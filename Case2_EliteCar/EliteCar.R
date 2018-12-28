
####################################### GIVEN VARIABLES ###############################################

daily_km <- 250
min_fleet <- 280
summer_lease <- seq(80,120)
cars_owned <- 90
car_price <- c(25000,250000) # Euros
max_km_allowed <- 15000
value_lost <- c(0.01,0.02) # Between 1-2% monthly
lease_period <- c(18,24) #months when leasing from elsewhere
lease_cost <- c(300,2000) #monthly cost range per car
mean_cost_sports <- 450 # euros per month
mean_cost_4x4 <- 600  # euros per month
mean_cost_except <- 2000 # euros per month
short_lease_12 <- 1000 # For sport and 4x4 monthly when lease < 12 months
short_lease_12_except <- 3000 # Rarely found monthly fee. Duration  4 months
utilization_sport <- 0.5 # between 0.5 - 0.7
utilization_4x4 <- 0.4 # Generally they are leased 18-24 months between 0.4 - 0.55
# Company needs the cars for 6 months in summer, renters asking 9 or 12 months since they don't want cars back for winter

mean_insurance_sport <- 150  # monthly fee
mean_insurance_4x4 <- 200 # monthly fee
mean_insurance_except <- 400 # monthly fee
amortization <- c(0.01, 0.02) # between 1-2% monthly 
mean_fixed_sport <- 40000
mean_fixed_4x4 <- 60000
mean_fixed_except <- 200000
maintenance_vc <- 30 # Monthly maintenance and cleaning cost per car
budget <- 2000000
days_rented_1 <- c(63,43,42,3) # Number of instances for 1,2,3,4 days
days_rented_2 <- c(57,37,34) # Number of instances for 1,2,3 days

overhead_salaries <-  30000 # 3 branches - lets say total 12 employees 30000 mean yearly salary 
overhead_marketing <- 10000 # 1% of the gross revenue 
overhead_rent <- 30000 # 3 branches around 10000 each
overhead_administrative <- 10000 # 1% of the gross revenue 

overhead_costs <- overhead_salaries + overhead_marketing + overhead_rent + overhead_administrative
#Not directly related to service like marketing, wages and administrative expenses

mean_profit_sport_day <- 400
mean_profit_4x4_day <- 550
mean_profit_except_day <- 1350


############################## DATA PREPARATION   #######################################################
library(xlsx)
#install.packages("xlsx")
demand_data <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case2_EliteCar/Elite_v2.xls", sheetIndex = 1)


as_char_as_num <- function(x){
    return(as.numeric(as.character(x)))
}
# Function for converting columns to type numeric


demand_sport <- demand_data[,c(1:25)]
demand_4x4 <- demand_data[,c(1,26:30)]
demand_except <- demand_data[,c(1,31:34)]


demand_sport_april <- demand_sport[c(2:31),]
colnames(demand_sport_april)[1] <- "APRIL"
demand_sport_april$APRIL <- sapply(demand_sport_april$APRIL, as_char_as_num)
demand_sport_april$APRIL <- as.Date(demand_sport_april$APRIL, origin = "1899-12-30")
demand_sport_april$SPORT...CONVERTIBLE <- as_char_as_num(demand_sport_april$SPORT...CONVERTIBLE)

demand_sport_may <- demand_sport[c(33:63),]
colnames(demand_sport_may)[1] <- "MAY"
demand_sport_may$MAY <- sapply(demand_sport_may$MAY, as_char_as_num)
demand_sport_may$MAY <- as.Date(demand_sport_may$MAY, origin = "1899-12-30")
demand_sport_may$SPORT...CONVERTIBLE <- as_char_as_num(demand_sport_may$SPORT...CONVERTIBLE)

demand_sport_june <- demand_sport[c(65:94),]
colnames(demand_sport_june)[1] <- "JUNE"
demand_sport_june$JUNE <- sapply(demand_sport_june$JUNE, as_char_as_num)
demand_sport_june$JUNE <- as.Date(demand_sport_june$JUNE, origin = "1899-12-30")
demand_sport_june$SPORT...CONVERTIBLE <- as_char_as_num(demand_sport_june$SPORT...CONVERTIBLE)

demand_sport_july <- demand_sport[c(96:126),]
colnames(demand_sport_july)[1] <- "JULY"
demand_sport_july$JULY <- sapply(demand_sport_july$JULY, as_char_as_num)
demand_sport_july$JULY <- as.Date(demand_sport_july$JULY, origin = "1899-12-30")
demand_sport_july$SPORT...CONVERTIBLE <- as_char_as_num(demand_sport_july$SPORT...CONVERTIBLE)

demand_sport_aug <- demand_sport[c(128:158),]
colnames(demand_sport_aug)[1] <- "AUGUST"
demand_sport_aug$AUGUST <- sapply(demand_sport_aug$AUGUST, as_char_as_num)
demand_sport_aug$AUGUST <- as.Date(demand_sport_aug$AUGUST, origin = "1899-12-30")
demand_sport_aug$SPORT...CONVERTIBLE <- as_char_as_num(demand_sport_aug$SPORT...CONVERTIBLE)

demand_sport_sep <- demand_sport[c(160:189),]
colnames(demand_sport_sep)[1] <- "SEPTEMBER"
demand_sport_sep$SEPTEMBER <- sapply(demand_sport_sep$SEPTEMBER, as_char_as_num)
demand_sport_sep$SEPTEMBER <- as.Date(demand_sport_sep$SEPTEMBER, origin = "1899-12-30")
demand_sport_sep$SPORT...CONVERTIBLE <- as_char_as_num(demand_sport_sep$SPORT...CONVERTIBLE)



demand_4x4_april <- demand_4x4[c(2:31),]
colnames(demand_4x4_april)[1] <- "APRIL"
demand_4x4_april$APRIL <- sapply(demand_4x4_april$APRIL, as_char_as_num)
demand_4x4_april$APRIL <- as.Date(demand_4x4_april$APRIL, origin = "1899-12-30")
demand_4x4_april$X4.WHEEL.DRIVE <- as_char_as_num(demand_4x4_april$X4.WHEEL.DRIVE)

demand_4x4_may <- demand_4x4[c(33:63),]
colnames(demand_4x4_may)[1] <- "MAY"
demand_4x4_may$MAY <- sapply(demand_4x4_may$MAY, as_char_as_num)
demand_4x4_may$MAY <- as.Date(demand_4x4_may$MAY, origin = "1899-12-30")
demand_4x4_may$X4.WHEEL.DRIVE <- as_char_as_num(demand_4x4_may$X4.WHEEL.DRIVE)

demand_4x4_june <- demand_4x4[c(65:94),]
colnames(demand_4x4_june)[1] <- "JUNE"
demand_4x4_june$JUNE <- sapply(demand_4x4_june$JUNE, as_char_as_num)
demand_4x4_june$JUNE <- as.Date(demand_4x4_june$JUNE, origin = "1899-12-30")
demand_4x4_june$X4.WHEEL.DRIVE <- as_char_as_num(demand_4x4_june$X4.WHEEL.DRIVE)

demand_4x4_july <- demand_4x4[c(96:126),]
colnames(demand_4x4_july)[1] <- "JULY"
demand_4x4_july$JULY <- sapply(demand_4x4_july$JULY, as_char_as_num)
demand_4x4_july$JULY <- as.Date(demand_4x4_july$JULY, origin = "1899-12-30")
demand_4x4_july$X4.WHEEL.DRIVE <- as_char_as_num(demand_4x4_july$X4.WHEEL.DRIVE)

demand_4x4_aug <- demand_4x4[c(128:158),]
colnames(demand_4x4_aug)[1] <- "AUGUST"
demand_4x4_aug$AUGUST <- sapply(demand_4x4_aug$AUGUST, as_char_as_num)
demand_4x4_aug$AUGUST <- as.Date(demand_4x4_aug$AUGUST, origin = "1899-12-30")
demand_4x4_aug$X4.WHEEL.DRIVE <- as_char_as_num(demand_4x4_aug$X4.WHEEL.DRIVE)

demand_4x4_sep <- demand_4x4[c(160:189),]
colnames(demand_4x4_sep)[1] <- "SEPTEMBER"
demand_4x4_sep$SEPTEMBER <- sapply(demand_4x4_sep$SEPTEMBER, as_char_as_num)
demand_4x4_sep$SEPTEMBER <- as.Date(demand_4x4_sep$SEPTEMBER, origin = "1899-12-30")
demand_4x4_sep$X4.WHEEL.DRIVE <- as_char_as_num(demand_4x4_sep$X4.WHEEL.DRIVE)


demand_except_april <- demand_except[c(2:31),]
colnames(demand_except_april)[1] <- "APRIL"
demand_except_april$APRIL <- sapply(demand_except_april$APRIL, as_char_as_num)
demand_except_april$APRIL <- as.Date(demand_except_april$APRIL, origin = "1899-12-30")
demand_except_april$EXCEPTIONAL <- as_char_as_num(demand_except_april$EXCEPTIONAL)

demand_except_may <- demand_except[c(33:63),]
colnames(demand_except_may)[1] <- "MAY"
demand_except_may$MAY <- sapply(demand_except_may$MAY, as_char_as_num)
demand_except_may$MAY <- as.Date(demand_except_may$MAY, origin = "1899-12-30")
demand_except_may$EXCEPTIONAL <- as_char_as_num(demand_except_may$EXCEPTIONAL)

demand_except_june <- demand_except[c(65:94),]
colnames(demand_except_june)[1] <- "JUNE"
demand_except_june$JUNE <- sapply(demand_except_june$JUNE, as_char_as_num)
demand_except_june$JUNE <- as.Date(demand_except_june$JUNE, origin = "1899-12-30")
demand_except_june$EXCEPTIONAL <- as_char_as_num(demand_except_june$EXCEPTIONAL)

demand_except_july <- demand_except[c(96:126),]
colnames(demand_except_july)[1] <- "JULY"
demand_except_july$JULY <- sapply(demand_except_july$JULY, as_char_as_num)
demand_except_july$JULY <- as.Date(demand_except_july$JULY, origin = "1899-12-30")
demand_except_july$EXCEPTIONAL <- as_char_as_num(demand_except_july$EXCEPTIONAL)

demand_except_aug <- demand_except[c(128:158),]
colnames(demand_except_aug)[1] <- "AUGUST"
demand_except_aug$AUGUST <- sapply(demand_except_aug$AUGUST, as_char_as_num)
demand_except_aug$AUGUST <- as.Date(demand_except_aug$AUGUST, origin = "1899-12-30")
demand_except_aug$EXCEPTIONAL <- as_char_as_num(demand_except_aug$EXCEPTIONAL)

demand_except_sep <- demand_except[c(160:189),]
colnames(demand_except_sep)[1] <- "SEPTEMBER"
demand_except_sep$SEPTEMBER <- sapply(demand_except_sep$SEPTEMBER, as_char_as_num)
demand_except_sep$SEPTEMBER <- as.Date(demand_except_sep$SEPTEMBER, origin = "1899-12-30")
demand_except_sep$EXCEPTIONAL <- as_char_as_num(demand_except_sep$EXCEPTIONAL)


demand_sport_binded <- rbind(demand_sport_april[,c(2:ncol(demand_sport_april))], demand_sport_may[,c(2:ncol(demand_sport_april))],
                            demand_sport_june[,c(2:ncol(demand_sport_april))],demand_sport_july[,c(2:ncol(demand_sport_april))],
                            demand_sport_aug[,c(2:ncol(demand_sport_april))],demand_sport_sep[,c(2:ncol(demand_sport_april))])

####################################################################################################


############################# ONLY SPORTS. ALL CARS ARE LEASED SHORT TERM ##########################

g <- demand_sport_binded$SPORT...CONVERTIBLE

h <- hist(g, breaks = seq(from=0, to=max(g), by = 1), density = 20,
           xlab = "Number of daily demands", main = "Sport Cars Demand Frequencies", freq =FALSE) 

### ASSUMPTION WHEN A CUSTOMER ASKS FOR A SPORTS CAR THE MODEL IS NOT IMPORTANT

### 2 - How many cars to keep in the lot ? ####
no_of_cars <- 20

# 3 - How should the fleet be divided ?
# *** ALL SHORT TERM LEASES AS IT IS SPECIFIED *** 

### 1 - In order to break even how many days cars be rented out per month?

car_cost_month <- short_lease_12 + mean_insurance_sport + maintenance_vc
no_of_cars <- 20
days_in_month <- 30

total_costs <- (car_cost_month * no_of_cars) + overhead_costs #total costs
total_revenue <- mean_profit_sport_day * days_in_month * no_of_cars 

utilization_sport <- total_costs / total_revenue

days_rent_per_month <- ceiling(days_in_month * utilization_sport)






############################# 3 CATEGORIES. LEASES, LONG-TERM RENTALS & PURCHASES ##################

### 2 - How many cars to keep in the lot ? ####
no_of_cars_sport <- 20
no_of_cars_4wheel <- 3
no_of_cars_except <- 3

##### 3 - How should the fleet be divided ?

#Amortization was calculated by reducing each owned car by one to two per cent per month on the declining value of the car.

amortization_percent <- 0.015
# Below is proving that long term lease is better than purchasing when depreciation is 1.5% permonth

sports_deprecate_month <- 600
four_wheel_deprecate_month <- 900
except_deprecate_month <- 3000

# CRITERIA IS MEETING MOST OF THE DEMAND 98%+

no_sports_long_lease <- 11  
no_sports_short_lease <- 9

no_4wheel_long_lease <- 2
no_4wheel_short_lease <- 1

no_except_long_lease <- 2
no_except_short_lease <- 1


### 1 - In order to break even how many days cars be rented out per month?

sport_cost_month_long <- mean_cost_sports + mean_insurance_sport + maintenance_vc
sport_cost_month_short <- short_lease_12 + mean_insurance_sport + maintenance_vc

sports_variable_cost <- (sport_cost_month_long * no_sports_long_lease) + (sport_cost_month_short * no_sports_short_lease)


four_wheel_month_long <- mean_cost_4x4 + mean_insurance_4x4 + maintenance_vc
four_wheel_month_short  <- short_lease_12 + mean_insurance_4x4 + maintenance_vc

four_wheel_variable_cost <- (four_wheel_month_long * no_4wheel_long_lease) + (four_wheel_month_short * no_4wheel_short_lease)


collision_insurance <- 0.005 * mean_fixed_except 
except_cost_month_long <- mean_cost_except + mean_insurance_except + maintenance_vc + collision_insurance 
except_cost_month_short <- short_lease_12_except + mean_insurance_except + maintenance_vc + collision_insurance 

except_variable_cost <- (except_cost_month_long * no_except_long_lease) + (except_cost_month_short * no_except_short_lease)



total_costs_all <- sports_variable_cost + four_wheel_variable_cost + except_variable_cost + overhead_costs

revenue_sport <- mean_profit_sport_day * days_in_month * no_of_cars 
revenue_4x4 <- mean_profit_4x4_day * days_in_month * (no_4wheel_long_lease + no_4wheel_short_lease)
revenue_except <- mean_profit_except_day * days_in_month * (no_except_long_lease + no_except_short_lease)

# 42, 33, 25 are util weights
percent_util <- total_costs_all / ((revenue_sport * 42) + (revenue_4x4*33) + (revenue_except*25))


sports_util_perc <- round(percent_util * 42, 2)
four_wheel_util_perc <- round(percent_util * 33, 2)
except_util_perc <- round(percent_util * 25, 2) 

sports_util <- ceiling(percent_util * 42 * days_in_month)
four_wheel_util <- ceiling(percent_util * 33 * days_in_month)
except_util <- ceiling(percent_util * 25 * days_in_month)


