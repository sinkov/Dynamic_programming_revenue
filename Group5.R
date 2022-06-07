# Loading packages
library(dplyr)
library(lpSolve)
library(nloptr)
library(mlogit)
library(matrixStats)
library(stargazer)
library(ggplot2)

### Protection Level (Eugeny) --------------------------------------------------

#Setting protection levels per week, based on dynamic prices

#Given the 5 fares twin room
prices=c(149,141,129,111,86)  # prices for each fare class
J=length(prices)  # number of fare classes
#estimated elastricity curve
exp_demand=500-prices*3
N<-350      # capacity 7 days * 50 twin rooms

#Typical week during high season
v<-matrix(0, nrow = (J+1), ncol = (N+1)); 
ybest<-matrix(0, nrow = J+1, ncol = (N+1));
# Dynamic Programming Recursion
for(i in 2:(J+1)){ # i=2 is stage 1 (i.e., p1 arrivals), i=3 is stage 2, etc.
  for(n in 1:(N+1)){
    x=n-1; # inventory level
    valuebest=-999;
    for(y in 0:x){ # protect for future stages
      avail=x-y; # available for this stage
      value=0; # to start computing the expected revenue
      for(d in 0:300){ # can also set range for each class
        sold=min(avail,d);
        value=value+
          dpois(d, exp_demand[i-1])*(prices[i-1]*sold+v[i-1,n-sold]);
      }
      if(value>valuebest){
        ybest[i,n]=y;
        valuebest=value;
      }
    }
    v[i,n]=valuebest;
  }
}
OptimalProtectionLimits<-c(ybest[3,100],ybest[4,100],ybest[5,100],ybest[6,100])
OptimalTotalExpectedRevenue=v[J+1,N+1]
print('Optimal protection levels are :')
OptimalProtectionLimits
print('Optimal expeced revenue (with levels) is :')
print(OptimalTotalExpectedRevenue)

# Testing the Performance of the Heuristic Policy
remDemand<-rep(0, J); 
remEffPrice<-rep(0, J);
CritFrac<-rep(0, J);
yheur<-rep(0, J);
for(i in 2:(J)){
  remDemand[i]=sum(exp_demand[1:(i-1)]);
  remEffPrice[i]=sum(exp_demand[1:(i-1)]*prices[1:(i-1)])/remDemand[i];
  CritFrac[i]=(remEffPrice[i]-prices[i])/remEffPrice[i];
  yheur[i]=qpois(CritFrac[i], remDemand[i]);
}
vheur<-matrix(0, nrow = (J+1), ncol = (N+1)); # i.e., j=0:5 and x=0:200
# Dynamic Programming Recursion
for(i in 2:(J+1)){ # i=2 is stage 1 (i.e., p1 arrivals), i=3 is stage 2, etc.
  for(n in 1:(N+1)){
    x=n-1; # inventory level
    y=yheur[i-1]; # protect for future
    avail=max(0,x-y); # available for this stage
    value=0; # to start computing the expected revenue
    for(d in 0:400){ # can also set range for each class
      sold=min(avail,d);
      value=value+
        dpois(d, exp_demand[i-1])*(prices[i-1]*sold+vheur[i-1,n-sold]);
    }
    vheur[i,n]=value;
  }
}
HeuristicTotalExpectedRevenue=vheur[J+1,N+1]
print('Heuristic revenue is:')
print(HeuristicTotalExpectedRevenue)
OptimalTotalExpectedRevenue

### Price differentiation (Maria) ----------------------------------------------

#1. We create a table with existing prices for each period and for each room type:

current_prices = data.frame("01/01-20.06" = c(68,68,89,105,147,105,221,344,344),
                            '21/06-04/07' = c(74,115,146,168,191,168,294,480,522),
                            '05/07-24.08' = c(81,125,156,189,221,189,338,522,627),
                            '25/08-22.09' = c(74,115,146,168,191,168,294,480,522),
                            '23/09-31.12' = c(68,68,89,105,147,105,221,344,344),
                            stringsAsFactors = FALSE)
rownames(current_prices) <- c('Twin Room economy', 
                              'Twin Room', 
                              'Junior Suite Room',
                              'Suite Room',
                              'Tower Suite Room',
                              'Standard Apartment',
                              'Family Apartment',
                              'Corona Villa',
                              'Royal Villa')

#First 5 types are residences and last 4 are cottages.

Lenth_of_the_periods = c(171, 14, 51, 29, 100)
Number_of_apartments = c(2, 50, 3, 10, 2, 11, 34, 4, 1)

Hotel_residence_revenue = 	1061149.25
Cottages_revenue = 	928776.02	
Total_revenue = Hotel_residence_revenue + Cottages_revenue

Hotel_residence_revenue_share = Hotel_residence_revenue/Total_revenue 
Cottages_revenue_share = Cottages_revenue/Total_revenue 

Percentage_of_total_revenue = c(25,	4,	43,	13,	15)

Revenue_per_period_total = Percentage_of_total_revenue*Total_revenue/100

Revenue_per_period_residences = Revenue_per_period_total*Hotel_residence_revenue_share
Revenue_per_period_cottages = Revenue_per_period_total*Cottages_revenue_share

R_per_period_res_per_day = Revenue_per_period_residences/Lenth_of_the_periods
R_per_period_cot_per_day = Revenue_per_period_cottages/Lenth_of_the_periods

#Let's assume that the proportion of occupancy is the same across all rooms inside cottage type and inside residence type. 

#Let's calculate maximum possible revenue per periods for reside:nces and for cottages:

max_revenue_residences = c(crossprod(Number_of_apartments[1:5], current_prices$X01.01.20.06[1:5]), 
                           crossprod(Number_of_apartments[1:5], current_prices$X21.06.04.07[1:5]),
                           crossprod(Number_of_apartments[1:5], current_prices$X05.07.24.08[1:5]),
                           crossprod(Number_of_apartments[1:5], current_prices$X25.08.22.09[1:5]),
                           crossprod(Number_of_apartments[1:5], current_prices$X23.09.31.12[1:5]))

max_revenue_cottages = c(crossprod(Number_of_apartments[6:9], current_prices$X01.01.20.06[6:9]), 
                         crossprod(Number_of_apartments[6:9], current_prices$X21.06.04.07[6:9]),
                         crossprod(Number_of_apartments[6:9], current_prices$X05.07.24.08[6:9]),
                         crossprod(Number_of_apartments[6:9], current_prices$X25.08.22.09[6:9]),
                         crossprod(Number_of_apartments[6:9], current_prices$X23.09.31.12[6:9]))

#Let's calculate real occupancy based on real revenue:
occupancy_residences = R_per_period_res_per_day/max_revenue_residences
occupancy_cottages = R_per_period_cot_per_day/max_revenue_cottages


#From real occupancy we can derive real demand:

demand_residence = matrix (-1, nrow = 5, ncol = 5)
for (i in 1:5){
  for (j in 1:5){
    demand_residence [j, i] = occupancy_residences[i]*Number_of_apartments[j]
  }
}

Number_of_apartments_cot = Number_of_apartments[6:9]

demand_cottages = matrix (-1, nrow = 4, ncol = 5)
for (i in 1:5){
  for (j in 1:4){
    demand_cottages [j, i] = occupancy_cottages[i]*Number_of_apartments_cot[j]
  }
}

#demand for rooms per type and per period:
demand = rbind(demand_residence, demand_cottages)

#Now we need to find demand function for each period and each room type based on the real price and real demand:
#Revenue = Demand*Price
#Demand = a - b*Price
#Revenue = (a-b*Price)*Price = a*Price - b*Price^2

#Let's include additional coefficients for 'a' per period:
coefficient_per_period = c(0.9, 1.10, 1.20, 1.10, 0.90)

a = matrix (-1, nrow = 9, ncol = 5)

for (i in 1:5){
  for (j in 1:9){
    a [j, i] =coefficient_per_period[i]*Number_of_apartments[j]
  }
}

b = matrix (-1, nrow = 9, ncol = 5)

b = (a-demand)/current_prices


Revenue_with_current_prices = matrix (-1, nrow = 9, ncol = 5)
for (i in 1:5){
  for (j in 1:9){
    Revenue_with_current_prices [j, i] =demand[j, i]*current_prices[j, i]*Lenth_of_the_periods[i]
  }
}

Revenue_current_prices = sum(Revenue_with_current_prices)
print(Revenue_current_prices)


#Let's apply the differentiation policy when we use two prices:
# - higher price = 20% up of the original price
# - discounted price = 10% down of the original price 

#In addition in order to be closer to the reality we use cannibalization level of 25% (those who could buy with higher price actually pay discounted price)

discount = 0.1
extra = 0.2
cannibalization = 0.25

Discounted_prices = current_prices*(1-discount)
Extra_prices = current_prices*(1+extra)

Demand_extra_prices = a-b*Extra_prices
#we have to replace negative values with zeros (so no people wanted this higher price)
Demand_extra_prices[Demand_extra_prices < 0] <- 0 
print(Demand_extra_prices)

#apply cannibalization
Demand_extra_prices_after_cannibalization = Demand_extra_prices*(1- cannibalization)

#calculate desirable demand for discounted price plus those who came from demand for higher price:
Desirable_demand_for_discounted_price_with_cannibalization = a-b*Discounted_prices -Demand_extra_prices+ cannibalization*Demand_extra_prices


Affordable_demand_for_discounted_price = matrix (-1, nrow = 9, ncol = 5)
for (i in 1:5){
  for (j in 1:9){
    Affordable_demand_for_discounted_price [j, i] =min(Number_of_apartments[j]-Demand_extra_prices_after_cannibalization[j, i], Desirable_demand_for_discounted_price_with_cannibalization[j, i])
  }
}

Revenue_with_two_prices = matrix (-1, nrow = 9, ncol = 5)

for (i in 1:5){
  for (j in 1:9){
    Revenue_with_two_prices [j, i] = Demand_extra_prices_after_cannibalization[j, i]*Extra_prices[j, i]*Lenth_of_the_periods[i]+
      Affordable_demand_for_discounted_price[j, i]*Discounted_prices[j, i]*Lenth_of_the_periods[i]
  }
}

print(sum(Revenue_with_two_prices))

Improvement = (sum(Revenue_with_two_prices)/sum(Revenue_with_current_prices)-1)*100
print(paste("The company can increase the revenue by", round(Improvement), "percent."))

### Data Simulation for WTP ----------------------------------------------------

types = 9
num_clients = 150
prices_mean = list(c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344),
                   c(74,115,146,168,191,168,294,480,522),
                   c(81,125,156,189,221,189,338,522,627),
                   c(74,115,146,168,191,168,294,480,522),
                   c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344),
                   c(68,68,89,105,147,105,221,344,344))
sd = 4

set.seed(123)

normal = rnorm(num_clients,260,50)

for (client in 1: num_clients) {
  for (month in 1:13) {
    prices_means = prices_mean[[month]]
    for (week in 1:4) {
      for (type in 1:types){
        Mon = min(rnorm(1, prices_means[type]*0.95,sd),normal[client])
        Tues = min(rnorm(1, prices_means[type]*0.95,sd ),normal[client]) 
        Wen = min(rnorm(1, prices_means[type]*1,sd ),normal[client]) 
        Thur = min(rnorm(1, prices_means[type]*1.1,sd ),normal[client]) 
        Fri = min(rnorm(1, prices_means[type]*1.2,sd ),normal[client]) 
        Sat = min(rnorm(1, prices_means[type]*1.20,sd ),normal[client]) 
        Sun = min(rnorm(1, prices_means[type]*1.18,sd ),normal[client]) 
        type_temp = c(Mon,Tues, Wen, Thur, Fri, Sat, Sun)
        # Finding the surplus for each one of them
        surplus_temp = c(Mon-prices_means[type],Tues-prices_means[type],
                         Wen-prices_means[type], Thur-prices_means[type],
                         Fri-prices_means[type], Sat-prices_means[type],
                         Sun-prices_means[type])
        if (type==1){
          client_week_WTP = type_temp
          client_week_SPS = surplus_temp
        }else {
          client_week_WTP = rbind(client_week_WTP, type_temp)
          client_week_SPS = rbind(client_week_SPS,surplus_temp)
        }
      }
      if (week==1){
        client_month_WTP = client_week_WTP
        client_month_SPS = client_week_SPS
      }else {
        client_month_WTP = cbind(client_month_WTP, client_week_WTP)
        client_month_SPS = cbind(client_month_SPS, client_week_SPS)
      }
    }
    if (month==1){
      client_Year_WTP = client_month_WTP
      client_Year_SPS = client_month_SPS
    }else {
      client_Year_WTP = cbind(client_Year_WTP, client_month_WTP)
      client_Year_SPS = cbind(client_Year_SPS, client_month_SPS)
    }
  }
  if (client==1){
    clients_WTP = client_Year_WTP
    clients_SPS = client_Year_SPS
  }else {
    clients_WTP = rbind(clients_WTP, client_Year_WTP)   
    clients_SPS = rbind(clients_SPS, client_Year_SPS) 
  }
} 

Room_Type = rep(c(1,2,3,4,5,6,7,8,9),num_clients)

for (i in 1:num_clients){
  client_ID = rep(i,9)
  if(i==1){ clients_ID = client_ID}
  else {clients_ID = append(clients_ID, client_ID)}
}

clients_WTP = data.frame(clients_WTP)
colnames(clients_WTP) = 1:364

clients_SPS = data.frame(clients_SPS)
colnames(clients_SPS) = 1:364

WTP = cbind(clients_ID,Room_Type)
WTP = cbind(WTP,clients_WTP)

SPS = cbind(clients_ID,Room_Type)
SPS = cbind(SPS,clients_SPS)

WTP = data.frame(WTP)
rownames(WTP) = seq(1,num_clients*9)

SPS = data.frame(SPS)
rownames(SPS) = seq(1,num_clients*9)

write.csv(WTP, 'Yearly_WTP.csv')

# Finding which room each client will choose per day

client_choice = matrix(-1, nrow = (num_clients), ncol = (364))
for (client in 1:num_clients){
  for (day in 1:364){
    if (max(subset(SPS,clients_ID==client)[day+2])<=0){
      client_choice[client,day] = 0  
    } else {
      client_choice[client,day] = max(which(subset(SPS,clients_ID==client)[day+2] == max(subset(SPS,clients_ID==client)[day+2])))
    }
  }
}

client_choice = data.frame(client_choice)
colnames(client_choice) = 1:364

# Finding the demand for each type of room per day

demand_day = matrix(-1, nrow = (9), ncol = (364))
for(day in 1:364){
  for (j in 1:9){
    demand_day[j,day] = sum(client_choice[,day] == j)
  }
}

write.csv(demand_day, 'demand_day.csv')

### Network Revenue Management using linear Programming (George) ---------------

WTP = read.csv('Yearly_WTP.csv')
demand_day = read.csv('demand_day.csv')
demand_days = demand_day[,-1]

### Finding the demands for the new prices

new_prices_non_peak = c(74,74,94,111,153,111,227,350,350)
new_prices_Jun_Aug = c(79,120,151,173,196,173,299,485,527)
new_prices_Jul = c(86,130,161,194,226,194,343,527,632)
# Finding the surpluses
for (type in 1:9){
  # I can take a 3 subset form Jan - Jun then Jun- Jul and Jul-August and Sep-Dec and c bind them (by using differne tprices)
  client_info = subset(WTP, Room_Type==type)[ ,c(2,3)]
  client_WTPs = subset(WTP, Room_Type==type)[ ,-c(1,2,3)]
  # From Jan-Jun
  client_surpluses_1 = subset(WTP, Room_Type==type)[ ,c(4:171)] - new_prices_non_peak[type]
  # For Jun
  client_surpluses_2 = subset(WTP, Room_Type==type)[ ,c(172:200)] - new_prices_Jun_Aug[type]
  # For Jul
  client_surpluses_3 = subset(WTP, Room_Type==type)[ ,c(201:229)] - new_prices_Jul[type]
  # For Aug
  client_surpluses_4 = subset(WTP, Room_Type==type)[ ,c(230:258)] - new_prices_Jun_Aug[type]
  # From Sept-Dec
  client_surpluses_5 = subset(WTP, Room_Type==type)[ ,c(259:367)] - new_prices_non_peak[type]
  # Combine them
  client_surpluses = cbind(client_surpluses_1,client_surpluses_2,client_surpluses_3,
                           client_surpluses_4,client_surpluses_5)
  
  client_surplus = cbind(client_info,client_surpluses)
  if (type == 1){
    SPS_New = client_surplus
  } else {SPS_New = rbind(SPS_New, client_surplus)}
}

# Finding which room each client will choose per day for the new price
client_choice_new = matrix(-1, nrow = (max(WTP$clients_ID)), ncol = (364))
for (client in 1:max(WTP$clients_ID)){
  for (day in 1:364){
    if (max(subset(SPS_New,clients_ID==client)[day+2])<=0){
      client_choice_new[client,day] = 0  
    } else {
      client_choice_new[client,day] = max(which(subset(SPS_New,clients_ID==client)[day+2] == max(subset(SPS_New,clients_ID==client)[day+2])))
    }
  }
}

# Finding the demand for each type of room per day for the new prices
New_demand_day = matrix(-1, nrow = (9), ncol = (364))
for(day in 1:364){
  for (j in 1:9){
    New_demand_day[j,day] = sum(client_choice_new[,day] == j)
  }
}


### Creating the function from January to April
revenue_method1 = matrix(0, nrow = (2), ncol = (168))
for (day in 1:168){
  # Objective Function Coefficients
  obj.fun = c(68,68,89,105,147,105,221,344,344,
              74,74,94,111,153,111,227,350,350)
  # Creating the left hand side of the constrains
  MaxAllocatePerRoomType = c(1,1,1,1,1,1,1,1,1)
  TwinRoomEcon = c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  TwinRoom = c(0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  JuniorSuiteRoom = c(0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  SuiteRoom = c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  TowerSuiteRoom = c(0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0)
  StandardApartment= c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0)
  FamilyApartment= c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0)
  CoronaVilla= c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0)
  RoyalVilla = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1)
  RoomTypeLessThanDemand = diag(1, 18, 18)
  # Bringing all constraint coefficients together to form the constraint coefficient matrix
  constr <- rbind(MaxAllocatePerRoomType,
                  TwinRoomEcon,TwinRoom,
                  JuniorSuiteRoom,SuiteRoom,
                  TowerSuiteRoom, StandardApartment,
                  FamilyApartment,CoronaVilla,RoyalVilla,
                  RoomTypeLessThanDemand);
  # Constraint directions
  constr.dir <- rep("<=", 28)
  # Creating the right hand side of the constraint
  rhs <- c(117,2,50,3,10,2,11,34,4,1, demand_days[1,day],demand_days[2,day],
           demand_days[3,day],demand_days[4,day],demand_days[5,day],demand_days[6,day],
           demand_days[7,day],demand_days[8,day],demand_days[9,day],
           New_demand_day[1,day],New_demand_day[2,day],New_demand_day[3,day],
           New_demand_day[4,day],New_demand_day[5,day],New_demand_day[6,day],
           New_demand_day[7,day],New_demand_day[8,day],New_demand_day[9,day])
  # Solving the LP: 
  optconvention <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
  # Finding the total revenue using the optimal allocation with 2 prices
  revenue_method1[1,day] = optconvention$objval
  # Finding the totla revenue with full allocation of the capacity with a fixed price
  revenue_method1[2,day] = min(2,demand_days[1,day])*68 + min(50,demand_days[2,day])*68 +
    min(3,demand_days[3,day])*89 +min(10,demand_days[4,day])*105 +min(2,demand_days[5,day])*147 +
    min(11,demand_days[6,day])*105 + min(34,demand_days[7,day])*221 + min(8,demand_days[8,day])*344 +
    min(1,demand_days[9,day])*344 
}

### Creating the function from June
revenue_method2 = matrix(0, nrow = (2), ncol = (29))
indx = 1
for (day in 169:197){
  # Objective Function Coefficients
  obj.fun = c(79,120,151,173,196,173,299,485,527,
              84,125,156,178,201,178,304,489,532)
  # Creating the left hand side of the constrains
  MaxAllocatePerRoomType = c(1,1,1,1,1,1,1,1,1)
  TwinRoomEcon = c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  TwinRoom = c(0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  JuniorSuiteRoom = c(0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  SuiteRoom = c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  TowerSuiteRoom = c(0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0)
  StandardApartment= c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0)
  FamilyApartment= c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0)
  CoronaVilla= c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0)
  RoyalVilla = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1)
  RoomTypeLessThanDemand = diag(1, 18, 18)
  # Bringing all constraint coefficients together to form the constraint coefficient matrix
  constr <- rbind(MaxAllocatePerRoomType,
                  TwinRoomEcon,TwinRoom,
                  JuniorSuiteRoom,SuiteRoom,
                  TowerSuiteRoom, StandardApartment,
                  FamilyApartment,CoronaVilla,RoyalVilla,
                  RoomTypeLessThanDemand);
  # Constraint directions
  constr.dir = rep("<=", 28)
  # Creating the right hand side of the constraint
  rhs <- c(117,2,50,3,10,2,11,34,4,1, demand_days[1,day],demand_days[2,day],
           demand_days[3,day],demand_days[4,day],demand_days[5,day],demand_days[6,day],
           demand_days[7,day],demand_days[8,day],demand_days[9,day],
           New_demand_day[1,day],New_demand_day[2,day],New_demand_day[3,day],
           New_demand_day[4,day],New_demand_day[5,day],New_demand_day[6,day],
           New_demand_day[7,day],New_demand_day[8,day],New_demand_day[9,day])
  # Solving the LP: 
  optconvention = lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
  # Finding the total revenue using the optimal allocation with 2 prices
  revenue_method2[1,indx] = optconvention$objval
  # Finding the total revenue with full allocation of the capacity with a fixed price
  revenue_method2[2,indx] = min(2,demand_days[1,day])*68 + min(50,demand_days[2,day])*68 +
    min(3,demand_days[3,day])*89 +min(10,demand_days[4,day])*105 +min(2,demand_days[5,day])*147 +
    min(11,demand_days[6,day])*105 + min(34,demand_days[7,day])*221 + min(8,demand_days[8,day])*344 +
    min(1,demand_days[9,day])*344 
  indx = indx + 1
}


### Creating the function from July
revenue_method3 = matrix(0, nrow = (2), ncol = (29))
indx = 1
for (day in 198:226){
  # Objective Function Coefficients
  obj.fun = c(86,130,161,194,226,194,343,527,632,
              91,135,166,199,231,199,348,532,637)
  # Creating the left hand side of the constrains
  MaxAllocatePerRoomType = c(1,1,1,1,1,1,1,1,1)
  TwinRoomEcon = c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  TwinRoom = c(0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  JuniorSuiteRoom = c(0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  SuiteRoom = c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  TowerSuiteRoom = c(0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0)
  StandardApartment= c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0)
  FamilyApartment= c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0)
  CoronaVilla= c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0)
  RoyalVilla = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1)
  RoomTypeLessThanDemand = diag(1, 18, 18)
  # Bringing all constraint coefficients together to form the constraint coefficient matrix
  constr <- rbind(MaxAllocatePerRoomType,
                  TwinRoomEcon,TwinRoom,
                  JuniorSuiteRoom,SuiteRoom,
                  TowerSuiteRoom, StandardApartment,
                  FamilyApartment,CoronaVilla,RoyalVilla,
                  RoomTypeLessThanDemand);
  # Constraint directions
  constr.dir <- rep("<=", 28)
  # Creating the right hand side of the constraint
  rhs <- c(117,2,50,3,10,2,11,34,4,1, demand_days[1,day],demand_days[2,day],
           demand_days[3,day],demand_days[4,day],demand_days[5,day],demand_days[6,day],
           demand_days[7,day],demand_days[8,day],demand_days[9,day],
           New_demand_day[1,day],New_demand_day[2,day],New_demand_day[3,day],
           New_demand_day[4,day],New_demand_day[5,day],New_demand_day[6,day],
           New_demand_day[7,day],New_demand_day[8,day],New_demand_day[9,day])
  # Solving the LP: 
  optconvention <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
  # Finding the total revenue using the optimal allocation with 2 prices
  revenue_method3[1,indx] = optconvention$objval
  # Finding the total revenue with full allocation of the capacity with a fixed price
  revenue_method3[2,indx] = min(2,demand_days[1,day])*68 + min(50,demand_days[2,day])*68 +
    min(3,demand_days[3,day])*89 +min(10,demand_days[4,day])*105 +min(2,demand_days[5,day])*147 +
    min(11,demand_days[6,day])*105 + min(34,demand_days[7,day])*221 + min(8,demand_days[1,day])*344 +
    min(1,demand_days[9,day])*344 
  indx = indx + 1
}


### Creating the function from August
revenue_method4 = matrix(0, nrow = (2), ncol = (29))
indx = 1
for (day in 227:255){
  # Objective Function Coefficients
  obj.fun = c(79,120,151,173,196,173,299,485,527,
              84,125,156,178,201,178,304,489,532)
  # Creating the left hand side of the constrains
  MaxAllocatePerRoomType = c(1,1,1,1,1,1,1,1,1)
  TwinRoomEcon = c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  TwinRoom = c(0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  JuniorSuiteRoom = c(0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  SuiteRoom = c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  TowerSuiteRoom = c(0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0)
  StandardApartment= c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0)
  FamilyApartment= c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0)
  CoronaVilla= c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0)
  RoyalVilla = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1)
  RoomTypeLessThanDemand = diag(1, 18, 18)
  # Bringing all constraint coefficients together to form the constraint coefficient matrix
  constr <- rbind(MaxAllocatePerRoomType,
                  TwinRoomEcon,TwinRoom,
                  JuniorSuiteRoom,SuiteRoom,
                  TowerSuiteRoom, StandardApartment,
                  FamilyApartment,CoronaVilla,RoyalVilla,
                  RoomTypeLessThanDemand);
  # Constraint directions
  constr.dir <- rep("<=", 28)
  # Creating the right hand side of the constraint
  rhs <- c(117,2,50,3,10,2,11,34,4,1, demand_days[1,day],demand_days[2,day],
           demand_days[3,day],demand_days[4,day],demand_days[5,day],demand_days[6,day],
           demand_days[7,day],demand_days[8,day],demand_days[9,day],
           New_demand_day[1,day],New_demand_day[2,day],New_demand_day[3,day],
           New_demand_day[4,day],New_demand_day[5,day],New_demand_day[6,day],
           New_demand_day[7,day],New_demand_day[8,day],New_demand_day[9,day])
  # Solving the LP: 
  optconvention <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
  # Finding the total revenue using the optimal allocation with 2 prices
  revenue_method4[1,indx] = optconvention$objval
  # Finding the totla revenue with full allocation of the capacity with a fixed price
  revenue_method4[2,indx] = min(2,demand_days[1,day])*68 + min(50,demand_days[2,day])*68 +
    min(3,demand_days[3,day])*89 +min(10,demand_days[4,day])*105 +min(2,demand_days[5,day])*147 +
    min(11,demand_days[6,day])*105 + min(34,demand_days[7,day])*221 + min(8,demand_days[1,day])*344 +
    min(1,demand_days[9,day])*344 
  indx = indx + 1
}


### Creating the function from September to December
revenue_method5 = matrix(0, nrow = (2), ncol = (109))
indx = 1
for (day in 256:364){
  # Objective Function Coefficients
  obj.fun = c(68,68,89,105,147,105,221,344,344,
              74,74,94,111,153,111,227,350,350)
  # Creating the left hand side of the constrains
  MaxAllocatePerRoomType = c(1,1,1,1,1,1,1,1,1)
  TwinRoomEcon = c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  TwinRoom = c(0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  JuniorSuiteRoom = c(0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  SuiteRoom = c(0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  TowerSuiteRoom = c(0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0)
  StandardApartment= c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0)
  FamilyApartment= c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0)
  CoronaVilla= c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0)
  RoyalVilla = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1)
  RoomTypeLessThanDemand = diag(1, 18, 18)
  # Bringing all constraint coefficients together to form the constraint coefficient matrix
  constr <- rbind(MaxAllocatePerRoomType,
                  TwinRoomEcon,TwinRoom,
                  JuniorSuiteRoom,SuiteRoom,
                  TowerSuiteRoom, StandardApartment,
                  FamilyApartment,CoronaVilla,RoyalVilla,
                  RoomTypeLessThanDemand);
  # Constraint directions
  constr.dir <- rep("<=", 28)
  # Creating the right hand side of the constraint
  rhs <- c(117,2,50,3,10,2,11,34,4,1, demand_days[1,day],demand_days[2,day],
           demand_days[3,day],demand_days[4,day],demand_days[5,day],demand_days[6,day],
           demand_days[7,day],demand_days[8,day],demand_days[9,day],
           New_demand_day[1,day],New_demand_day[2,day],New_demand_day[3,day],
           New_demand_day[4,day],New_demand_day[5,day],New_demand_day[6,day],
           New_demand_day[7,day],New_demand_day[8,day],New_demand_day[9,day])
  # Solving the LP: 
  optconvention <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
  # Finding the total revenue using the optimal allocation with 2 prices
  revenue_method5[1,indx] = optconvention$objval
  # Finding the total revenue with full allocation of the capacity with a fixed price
  revenue_method5[2,indx] = min(2,demand_days[1,day])*68 + min(50,demand_days[2,day])*68 +
    min(3,demand_days[3,day])*89 +min(10,demand_days[4,day])*105 +min(2,demand_days[5,day])*147 +
    min(11,demand_days[6,day])*105 + min(34,demand_days[7,day])*221 + min(8,demand_days[1,day])*344 +
    min(1,demand_days[9,day])*344 
  indx = indx + 1
}

### Combining the revenues for the 5 different periods
revenue_method = cbind(revenue_method1,revenue_method2,
                       revenue_method3,revenue_method4,revenue_method5)



### Plotting the results
df = data.frame('Methods'=c('Optimal Revenue', 'Fixed Revenue'),
                'Revenue'=c(sum(revenue_method[1,]),sum(revenue_method[2,])))

ggplot(data=df, aes(x=Methods, y=Revenue,width=0.5)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Revenue), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

print(paste('Revenue with full allocation of the capacity with a fixed price',sum(revenue_method[1,])))
print(paste('Revenue with optimal full allocation of the capacity with 2 prices',sum(revenue_method[2,])))


### MNL and Two prices in a week model (Lucy) ----------------------------------

# read our simulated WTP
data = read.csv("Yearly_WTP.csv")
data = data[,-1] # drop the index

# MNL model for single price in a week, eg, week 2 and room type 1
wtp = data
# e.g. for room type 1, week 2
wtp.week = wtp[, c(1:2, 10:16)] # 10=2*7-4, 16=2*7+2
wtpnew = subset(wtp.week, Room_Type==1)
N=nrow(wtpnew)

AvgWTPs=colMeans(wtpnew[3:9])
SdWTPs=colSds(as.matrix(wtpnew[3:9]))
VarWTPs=SdWTPs^2

AvgVarWTPs=mean(VarWTPs)

util=AvgWTPs # u_i
mu=sqrt(6*AvgVarWTPs)/pi

eval_f = function(x){
  price=x
  
  prices=rep(price, 7)
  
  attractions=exp((util-prices)/mu)
  
  purchaseProbs=attractions/(sum(attractions)+1)
  
  revenue=N*sum(prices*purchaseProbs)
  
  objfunction=-revenue
  return(objfunction)
}

lb=min(wtpnew[3:9])
ub=max(wtpnew[3:9])
x0=as.numeric(lb)

opts=list(
  "algorithm"="NLOPT_LN_COBYLA",
  "xtol_rel"=1.0e-6,
  "maxeval"=1000
)
result=nloptr(x0=ub, eval_f = eval_f, lb=lb, ub=ub, opts = opts)
priceOpt=round(result$solution,2) # fo x0=ub, priceOpt=107.85

prices=rep(priceOpt,7)
attractions=exp((util-prices)/mu)
purchaseProbs=attractions/(sum(attractions)+1)
barplot(purchaseProbs,cex.names=0.75)

noPurchaseprob=1/(sum(attractions)+1)
names(noPurchaseprob) <- "Do Not Buy"
barplot(c(purchaseProbs,noPurchaseprob),cex.names=0.75)

# Since WTP in weekend is higher than weekdays, people end up in only buying in weekend,
# so let's try the model with two different prices in a week, weekday price and weekend price. 

# define a function 
OptimalPriceForAWeek = function(roomType, week){
  # input: -roomType: input integer 1:9
  #        -month: input integer 1:52 (which is 4*13. 4 weeks and 13 month. 
  #                The reason is 13 month is because 4*13*7=364 which is closer to 365)
  # output: the optimal week day price and weekend price based on given room type and week, 
  #         and the optimal revenue of this week. 
  wtp = data
  ws = week*7-4 # the index number for the columns of the data (week start)
  we = week*7+2 # the index number for the columns of the data (week end)
  wtp.week = wtp[, c(1:2, ws:we)] # take the first two columns and the week we want to work with
  wtpnew = subset(wtp.week, Room_Type==roomType) # for room type
  N=nrow(wtpnew)
  
  weekdays.lb=min(wtpnew[3:7])
  weekend.lb=min(wtpnew[8:9])
  weekdays.ub=max(wtpnew[3:7])
  weekend.ub=max(wtpnew[8:9])
  
  np = ceiling((weekdays.ub - weekdays.lb)/3)
  p = ceiling(weekend.ub - weekend.lb)
  
  surplusNonPeak<-rep(0,N) 
  surplusPeak<-rep(0,N) 
  demandNonPeak<-rep(0,np^2)  
  demandPeak<-rep(0,np^2)
  
  index = 1
  for (basePrice in seq(from = as.numeric(weekdays.lb), to = as.numeric(weekdays.ub), by = 3)){
    for (weekendPrice in seq(from = as.numeric(weekend.lb), to = as.numeric(weekend.ub), by = 3)){
      for (i in 1:N){
        surplusNonPeak[i]=max(wtpnew[i,c(3:7)]-basePrice) # where warnings appeared
        surplusPeak[i]=wtpnew[i,8:9]-weekendPrice 
      }
      demandNonPeak[index]=sum((surplusNonPeak>surplusPeak)*(surplusNonPeak>=0)) 
      demandPeak[index]=sum((surplusPeak>=surplusNonPeak)*(surplusPeak>=0)) 
      index=index+1
    }
  }
  
  # capacity matrix for each room type
  capa.room = matrix(c(2,50,3,10,2,11,34,4,1))
  
  newdata<-data.frame(matrix(nrow=np^2,ncol = 5)) 
  colnames(newdata)=c("index","basePrice","weekendPrice","weekdaysDemand", "weekendDemand") 
  index=1
  for (basePrice in seq(from = as.numeric(weekdays.lb), to = as.numeric(weekdays.ub), by = 3)){
    for (weekendPrice in seq(from = as.numeric(weekend.lb), to = as.numeric(weekend.ub), by = 3)){
      newdata[index,1]=index
      newdata[index,2]=basePrice 
      newdata[index,3]=weekendPrice 
      newdata[index,4]=demandNonPeak[index] 
      newdata[index,5]=demandPeak[index]
      index=index+1 
    }
  }
  
  newdata$revenue=newdata$basePrice*newdata$weekdaysDemand+newdata$weekendPrice*newdata$weekendDemand 
  
  # regression for dependent variable weekdaysDemand
  fit2NonPeak <-lm(weekdaysDemand ~ basePrice+weekendPrice, data=newdata) 
  summary(fit2NonPeak)
  a1=coef(fit2NonPeak)[1] 
  b11=coef(fit2NonPeak)[2] 
  b12=coef(fit2NonPeak)[3]
  # regression for dependent variable weekendDemand
  fit2Peak <-lm(weekendDemand ~ basePrice+weekendPrice, data=newdata) 
  a2=coef(fit2Peak)[1]
  b21=coef(fit2Peak)[2]
  b22=coef(fit2Peak)[3]
  stargazer(fit2NonPeak,fit2Peak, type="text")
  
  demD = 5*capa.room[roomType] # demand week days
  demW = 2*capa.room[roomType]
  
  eval_f = function(x){
    basePrice=x[1]
    weekendPrice=x[2]
    weekdaysDemand=max(0,a1+b11*basePrice+b12*weekendPrice)
    weekendDemand=max(0,a2+b21*basePrice+b22*weekendPrice)
    choice_weekday = min(demD,weekdaysDemand) # 
    choice_weekend = min(demW,weekendDemand) # 
    revenue=basePrice*choice_weekday+weekendPrice*choice_weekend 
    
    objfunction=-revenue
    return(objfunction)
  }
  eval_g_ineq <- function(x) {
    basePrice=x[1]
    weekendPrice=x[2] 
    weekdaysDemand=max(0,a1+b11*basePrice+b12*weekendPrice)
    weekendDemand=max(0,a2+b21*basePrice+b22*weekendPrice)
    constraint <- c(-weekdaysDemand,
                    -weekendDemand,
                    weekdaysDemand-demD,
                    weekendDemand-demW,
                    x[1]-x[2]) 
    return(constraint)
  }
  
  x0=c(as.numeric(weekdays.lb),as.numeric(weekend.lb))
  lb=c(weekdays.lb,weekend.lb)
  ub=c(weekdays.ub,weekend.ub)
  opts=list(
    "algorithm"="NLOPT_LN_COBYLA",
    "xtol_rel"=1.0e-6,
    "maxeval"=1000
  )
  result <- nloptr(x0=x0,eval_f=eval_f,lb=lb,ub=ub,eval_g_ineq=eval_g_ineq,opts=opts)
  print(result)
  priceOpt<-result$solution 
  RevenueOpt<- -result$objective
  # print(paste("Optimal week days Price:",priceOpt[1]))
  # print(paste("Optimal weekend Price:",priceOpt[2]))
  # print(paste("Optimal Revenue:",RevenueOpt))
  return(c(priceOpt[1], priceOpt[2], RevenueOpt))
}

# for example: for room type 2, and week 30, which is reasonable and roughly 15% increase to the hotel's actual revenue
OptimalPriceForAWeek(2, 30) # the returned values are: optimal week days price, optimal weekend price, 
# and optimal revenue this week based on this two optimal values.
# the warnings is not fixed because it doesn't affect giving results and the reason for the warning is during calculating surplus,
# there have more than one value that are the same, so the output max() cannot be output only one value

