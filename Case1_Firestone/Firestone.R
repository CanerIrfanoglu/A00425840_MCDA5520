library(xlsx)
library(dplyr)

toronto <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case1_Firestone/Toronto_clean.xlsx", sheetIndex = 1)
vancouver <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case1_Firestone/Vancouver_clean.xlsx", sheetIndex = 1)
london <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case1_Firestone/london_clean.xlsx", sheetIndex = 1)


################################## DATA PREPARATION ################################## 
rearrange_months <- function(x){
    x_first <- x[,c(1:6)]
    x_second <- x[,c(7:13)]
    # Divide Dataset starting Jan-May, Jun-Dec
    
    x_first_half_drop_first <- x_first[-1,]
    x_second_half_drop_last <- x_second[-nrow(x_second),]
    # Drop first row of Jan-May & Last row of Jun-Dec
    
    x_jm <- cbind(x_second_half_drop_last, x_first_half_drop_first) %>% select(-YEAR)
    return(x_jm)
}

toronto_jm <- rearrange_months(toronto)
vancouver_jm <- rearrange_months(vancouver)
london_jm <- rearrange_months(london)

as_char_as_num <- function(x){
    return(as.numeric(as.character(x)))
}
# Function for converting columns to type numeric


london_jm$JUL[1] <- 0
london_jm$JUN[1] <- 0
# Convert M's to 0 

toronto_jm_num <- sapply(toronto_jm,as_char_as_num)
vancouver_jm_num <- sapply(vancouver_jm,as_char_as_num)
london_jm_num <- sapply(london_jm,as_char_as_num)
# Making sure all columns are type numeric

####################################################################################################


################################## VISUALIZATION ############################################## 
t_sums <- rowSums(toronto_jm_num)
v_sums <- rowSums(vancouver_jm_num)
l_sums <- rowSums(london_jm_num) 

t_50_80_mean <- 139.24
v_50_80_mean <- 60.34
l_50_80_mean <- 208.81


g <- v_sums

h <- hist(g, breaks = seq(from=min(g), to=max(g), by = ((max(g) - min(g))/ 6)), density = 5,
          col = "lightgray", xlab = "Snow Depth in cm's", main = "Snow Depth Frequencies in Vancouver") 
xfit <- seq(min(g), max(g), length = 43)        #Trying to fit normal curve on hist of data
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) #Histogram & actuals indicate data is not distrubed normally!!!
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "red", lwd = 2)


####################################################################################################


############################## 1 - CALCULATING PROBABILITIES #####################################

percent_calculate <- function(array, perc, mean_option = 1){
    
    mu_series <- mean(array)
    # Average of all array
    
    sd_series <- sd(array)
    # SD of all array
    
    if(mean_option == 1){
        
    # last_10_mean <- mean(array[(length(array)-9):length(array)])
    # # Mean of last 10 indices
    # mean_to_use <- last_10_mean #Updated with mean from pdf
        if(array[1] == 149.4){mean_to_use <- 137.94}
        else if(array[1] == 3.3){mean_to_use <- 44.1} 
        else if(array[1] == 141.6){mean_to_use <- 203.3} 
    }
    else if(mean_option == 2){
        
    mean_to_use <- mu_series
    # Use the mean of whole series
    }
    else if(mean_option == 3){
        
        if(array[1] == 149.4){mean_to_use <- t_50_80_mean}
        else if(array[1] == 3.3){mean_to_use <- v_50_80_mean} 
        else if(array[1] == 141.6){mean_to_use <- l_50_80_mean} 
    }
    
    z_to_calc <- perc*mean_to_use
    # corresponding value to given percentage to the mean
    
    z_score <- (z_to_calc - mu_series) / sd_series
    
    return(pnorm(z_score))
    
}
    
t_20 <- percent_calculate(t_sums, 0.2)
t_30 <- percent_calculate(t_sums, 0.3) - percent_calculate(t_sums, 0.2)
t_40 <- percent_calculate(t_sums, 0.4) - percent_calculate(t_sums, 0.3)
# t_30 <- percent_calculate(t_sums, 0.3)
# t_40 <- percent_calculate(t_sums, 0.4)

v_20 <- percent_calculate(v_sums, 0.2)
v_30 <- percent_calculate(v_sums, 0.3) - percent_calculate(v_sums, 0.2)
v_40 <- percent_calculate(v_sums, 0.4) - percent_calculate(v_sums, 0.3)

l_20 <- percent_calculate(l_sums, 0.2)
l_30 <- percent_calculate(l_sums, 0.3) - percent_calculate(l_sums, 0.2)
l_40 <- percent_calculate(l_sums, 0.4) - percent_calculate(l_sums, 0.3)

all_percs <- c('toronto%',100*round(t_20,4),100*round(t_30,4),round(t_40,4),
               'vancouver%',100*round(v_20,3),100*round(v_30,3),100*round(v_40,3),
               'london%',100*round(l_20,4),100*round(l_30,4),100*round(l_40,4))
####################################################################################################



################### DECIDE IF DISTRIBUTIONS ARE NORMAL ############################################

# Kurtosis needs to be btw -2 + 2
#write.xlsx(toronto_jm_num, "/Users/Caner/Desktop/java/Statistics_5520/Case1_Firestone/Toronto_jun_may.xlsx")

# TORONTO NORMAL DISTRIBUTION
# VANCOUVER NOT-NORMAL DISTRIBUTION
# LONDON histogram normalish
# moderately skewed
# ptest barely passes 0.05
# qqplot ~80% of values on the line


# Insufficient Data can cause a normal distribution to look completely scattered. 
# For example, classroom test results are usually normally distributed. 
# An extreme example: if you choose three random students and plot the results on a graph, 
# you won’t get a normal distribution. You might get a uniform distribution (i.e. 62 62 63) 
# or you might get a skewed distribution (80 92 99). If you are in doubt about whether you 
# have a sufficient sample size, collect more data.

# You have several options for handling your non normal data. Several tests,
# including the one sample Z test, T test and ANOVA assume normality.
# You may still be able to run these tests if your sample size is large enough (usually over 20 items).

####################################################################################################


#################### 3 - DOES THE MEAN AFFECT REFUNDS PROBABILITIES ? #############################
v_20_last_10 <- percent_calculate(v_sums, 0.2, mean_option = 1)
v_20_all <- percent_calculate(v_sums, 0.2, mean_option = 2)
v_20_toro <- percent_calculate(v_sums, 0.2, mean_option = 3)
v_30_last_10 <- percent_calculate(v_sums, 0.3, mean_option = 1)
v_30_all <- percent_calculate(v_sums, 0.3, mean_option = 2)
v_30_toro <- percent_calculate(v_sums, 0.3, mean_option = 3)
v_40_last_10 <- percent_calculate(v_sums, 0.4, mean_option = 1)
v_40_all <- percent_calculate(v_sums, 0.4, mean_option = 2)
v_40_toro <- percent_calculate(v_sums, 0.4, mean_option = 3)

vancouver_compare <- c('100% Refund last10 vs toro vs all', 100*round(v_20_last_10,3), 100*round(v_20_toro,3), 100*round(v_20_all,3), '|',
                       '75% Refund last10 vs toro vs all', 100*round(v_30_last_10,3), 100*round(v_30_toro,3),100*round(v_30_all,3), '|',
                       '50% Refund last10 vs toro vs all', 100*round(v_40_last_10,3), 100*round(v_40_toro,3),100*round(v_40_all,3))


t_20_last_10 <- percent_calculate(t_sums, 0.2, mean_option = 1)
t_20_all <- percent_calculate(t_sums, 0.2, mean_option = 2)
t_20_toro <- percent_calculate(t_sums, 0.2, mean_option = 3)
t_30_last_10 <- percent_calculate(t_sums, 0.3, mean_option = 1)
t_30_all <- percent_calculate(t_sums, 0.3, mean_option = 2)
t_30_toro <- percent_calculate(t_sums, 0.3, mean_option = 3)
t_40_last_10 <- percent_calculate(t_sums, 0.4, mean_option = 1)
t_40_all <- percent_calculate(t_sums, 0.4, mean_option = 2)
t_40_toro <- percent_calculate(t_sums, 0.4, mean_option = 3)


toronto_compare <- c('100% Refund last10 vs toro vs all', 100*round(t_20_last_10,5), 100*round(t_20_toro,5), 100*round(t_20_all,5), '|',
                       '75% Refund last10 vs toro vs all', 100*round(t_30_last_10,5), 100*round(t_30_toro,5), 100*round(t_30_all,5), '|',
                       '50% Refund last10 vs toro vsall', 100*round(t_40_last_10,5), 100*round(t_40_toro,5), 100*round(t_40_all,5))


l_20_last_10 <- percent_calculate(l_sums, 0.2, mean_option = 1)
l_20_all <- percent_calculate(l_sums, 0.2, mean_option = 2)
l_20_toro <- percent_calculate(l_sums, 0.2, mean_option = 3)
l_30_last_10 <- percent_calculate(l_sums, 0.3, mean_option = 1)
l_30_all <- percent_calculate(l_sums, 0.3, mean_option = 2)
l_30_toro <- percent_calculate(l_sums, 0.3, mean_option = 3)
l_40_last_10 <- percent_calculate(l_sums, 0.4, mean_option = 1)
l_40_all <- percent_calculate(l_sums, 0.4, mean_option = 2)
l_40_toro <- percent_calculate(l_sums, 0.4, mean_option = 3)


london_compare <- c('100% Refund last10 vs toro vs all', 100*round(l_20_last_10,4), 100*round(l_20_toro,4), 100*round(l_20_all,4), '|',
                       '75% Refund last10 vs toro vs all', 100*round(l_30_last_10,4), 100*round(l_30_toro,4), 100*round(l_30_all,4), '|',
                       '50% Refund last10 vs toro vs all', 100*round(l_40_last_10,4), 100*round(l_40_toro,4), 100*round(l_40_all,4))


# CONCLUSION YES IT WILL AFFECT.

####################################################################################################

########################################## EV CALCULATION ##########################################

# Initial investment = 1
invest <- 1

# 4 scenarios
full_refund <- 1
seventyfive_refund <- 0.75
half_refund <- 0.5
no_refund <- 0

ev_calculate <- function(perc20,perc30,perc40){
    case1 <- perc20 * full_refund
    case2 <- perc30 * seventyfive_refund
    case3 <- perc40 * half_refund
    total <- case1 + case2 + case3
    return(total)
}

t_ev <- ev_calculate(t_20,t_30,t_40)
l_ev <- ev_calculate(l_20,l_30,l_40)
v_ev <- ev_calculate(v_20,v_30,v_40)

####################################################################################################

############################ SHOULD FIRESTONE GO WITH THIS PROGRAM #################################




######################### PLOTS ########################
N <- 43
props = 1:N / (N+1)
PP_plot <- plot(props, sort(pnorm(t_sums, mean(t_sums), sd(t_sums))))


pp <- ggplot(data = data.frame(v_sums), mapping = aes(sample = v_sums)) +
    stat_pp_band() +
    stat_pp_line(color = 'red') +
    stat_pp_point() +
    labs(x = "Probability Points", y = "Cumulative Probability", title ="Vancouver P-P Plot")
pp


qq <- ggplot(data = data.frame(v_sums), mapping = aes(sample = v_sums)) +
    stat_qq_band() +
    stat_qq_line(color = 'blue') +
    stat_qq_point() +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles", title ="Vancouver Q-Q Plot")
qq



















