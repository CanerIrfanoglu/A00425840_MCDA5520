library(xlsx)
library(dplyr)
library(tidyr)

travel <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case6_BrandEquity/Brand Equity 7B10E023A.xlsx", sheetIndex = 1)
fast <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case6_BrandEquity/Brand Equity 7B10E023B.xlsx", sheetIndex = 1)

####################################### DATA PREPARATION ###############################################
na_counts_travel <-sapply(travel, function(y) sum(length(which(is.na(y)))))
na_counts_fast <-sapply(fast, function(y) sum(length(which(is.na(y)))))

for(i in 2:2){
    travel[is.na(travel[,i]), i] <- ceiling(mean(travel[,i], na.rm = TRUE))
}# Replace age column with column mean

for(i in 2:2){
    fast[is.na(fast[,i]), i] <- ceiling(mean(fast[,i], na.rm = TRUE))
}# Replace age column with column mean


travel <- (travel[complete.cases(travel), ]) # Deletes 15 rows w missing data
fast <- (fast[complete.cases(fast), ]) # Deletes 50 rows w missing data

for(i in 1:ncol(travel)){
    if(i != 2 && i != 18){
        travel[,i] <- as.factor(travel[,i])
        # Convert all columns except for age & brand_equity to factor
        
    }
}

for(i in 1:ncol(fast)){
    if(i != 2){
        fast[,i] <- as.factor(fast[,i])
        # Convert all columns except for age & brand_equity to factor
    }
}

########################################################################################################


####################################### DATA INSPECTION ###############################################
str(travel)
str(fast)

########################################################################################################


# 1.	Run a crosstabs using the variables BRAND and LOYALBIN. What do the results tell you? 
levels(travel$loyalbin) <- c('Not_Loyal', 'Loyal')
table(travel$brand, travel$loyalbin, dnn = c("Brands", "Loyality"))
# Counts for each brand
round(prop.table(table(travel$brand, travel$loyalbin, dnn = c("Brands", "Loyality")),1),2)
# Count percentages row wise
round(prop.table(table(travel$brand, travel$loyalbin, dnn = c("Brands", "Loyality")),2),2)
# Percentage of Total


levels(fast$loyalbin) <- c('Not_Loyal', 'Loyal')
table(fast$brand, fast$loyalbin, dnn = c("Brands", "Loyality"))
# Counts for each brand
round(prop.table(table(fast$brand, fast$loyalbin, dnn = c("Brands", "Loyality")),1),2)
# Count percentages row wise

# 2.	Delete the brands associated with UK and AirUSA. Rerun the crosstabs. What do the results tell you? 
a <- table(fast$brand, fast$loyalbin, dnn = c("Brands", "Loyality"))
fast_filtered <- a[-c(4,5),]
round(prop.table(fast_filtered,1),2)

# 3.	How can you measure brand equity with the collected data? 




# 4.	What statistical analysis is suitable to compare brand equity across brands? 
# Why? Compare brand equity across brands for your chosen category. 

anova_br_eq <- summary(aov(brand_equity ~ brand  ,data = travel))

vs_263_264 <- t.test(travel[travel$brand == 263,18], travel[travel$brand == 264,18],
                         paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternative = "less")
vs_263_265 <- t.test(travel[travel$brand == 263,18], travel[travel$brand == 265,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternative = "greater")
vs_263_266 <- t.test(travel[travel$brand == 263,18], travel[travel$brand == 266,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_263_267 <- t.test(travel[travel$brand == 263,18], travel[travel$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternative = "greater")
vs_264_265 <- t.test(travel[travel$brand == 264,18], travel[travel$brand == 265,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternative = "greater")
vs_264_266 <- t.test(travel[travel$brand == 264,18], travel[travel$brand == 266,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternative = "less")
vs_264_267 <- t.test(travel[travel$brand == 264,18], travel[travel$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternative = "greater")
vs_265_266 <- t.test(travel[travel$brand == 265,18], travel[travel$brand == 266,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_265_267 <- t.test(travel[travel$brand == 265,18], travel[travel$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternative = "less")
vs_266_267 <- t.test(travel[travel$brand == 266,18], travel[travel$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)

# 266 > 

#5.     Compare loyalty, 1ess and popularity 
#for the brands of your chosen category using the appropriate statistical analysis. 

# loyal # 
# 1- 10 as factor 
loyal_counts_t <- travel %>% group_by(brand, loyal) %>% tally() %>% spread(loyal, n)
loyal_counts_t <- as.data.frame(loyal_counts_t)
rownames(loyal_counts_t) <- loyal_counts_t[,1]
loyal_counts_t[,1] <- NULL
# Ready for chi-sq  266 > 263 > 264 > 267 > 265 // 4 > 1 > 2 > 5 > 3

chisq.test(loyal_counts_t[c(1,4),]) # Sig           # 4 > 1 > others statistically
chisq.test(loyal_counts_t[c(1,2),]) # Sig
chisq.test(loyal_counts_t[c(2,5),]) # No_sig
chisq.test(loyal_counts_t[c(2,3),]) # No_sig


# 0 - 1 
loyal_binary_counts <- travel %>% group_by(brand, loyalbin) %>% tally() %>% spread(loyalbin, n)
loyal_binary_counts <- as.data.frame(loyal_binary_counts)
rownames(loyal_binary_counts) <- loyal_binary_counts[,1]
loyal_binary_counts[,1] <- NULL
# Ready for chi-sq  266 > 264 > 267 > 265 > 263 // 4 > 2 > 5 > 3 > 1

chisq.test(loyal_binary_counts[c(4,2),]) # Sig       4 > others statistically
chisq.test(loyal_binary_counts[c(2,5),]) # No_Sig 


# Continious Assumption

travel$loyal <- as.numeric(as.character(travel$loyal))
anova_loyal <- summary(aov(loyal ~ brand  ,data = travel)) # Significant 

l_266_263 <- t.test(travel[travel$brand == 266,8], travel[travel$brand == 263,8],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE) # Significant

l_263_264 <- t.test(travel[travel$brand == 263,8], travel[travel$brand == 264,8],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

                                                    # 4 > others statistically



#### MANOVA 
travel$loyal <- as.numeric(as.character(travel$loyal))
travel$famil <- as.numeric(as.character(travel$famil))
travel$uniqu <- as.numeric(as.character(travel$uniqu))
travel$relev <- as.numeric(as.character(travel$relev))
travel$popul <- as.numeric(as.character(travel$popul))


travel_manova <- manova(cbind(loyal, famil, uniqu, relev, popul) ~ brand, data = travel)
summary.aov(travel_manova) # All dependent variables significant 

































