library(xlsx)
library(dplyr)
library(tidyr)

fast <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case6_BrandEquity/Brand Equity 7B10E023A.xlsx", sheetIndex = 1)
travel <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case6_BrandEquity/Brand Equity 7B10E023B.xlsx", sheetIndex = 1)
####################################### DATA PREPARATION ###############################################
na_counts_fast <-sapply(fast, function(y) sum(length(which(is.na(y)))))
na_counts_travel <-sapply(travel, function(y) sum(length(which(is.na(y)))))

for(i in 2:2){
    fast[is.na(fast[,i]), i] <- ceiling(mean(fast[,i], na.rm = TRUE))
}# Replace age column with column mean

for(i in 2:2){
    travel[is.na(travel[,i]), i] <- ceiling(mean(travel[,i], na.rm = TRUE))
}# Replace age column with column mean


fast <- (fast[complete.cases(fast), ]) # Deletes 15 rows w missing data
travel <- (travel[complete.cases(travel), ]) # Deletes 50 rows w missing data

for(i in 1:ncol(fast)){
    if(i != 2 && i != 18){
        fast[,i] <- as.factor(fast[,i])
        # Convert all columns except for age & brand_equity to factor
        
    }
}

for(i in 1:ncol(travel)){
    if(i != 2){
        travel[,i] <- as.factor(travel[,i])
        # Convert all columns except for age & brand_equity to factor
    }
}

########################################################################################################


####################################### DATA INSPECTION ###############################################
str(fast)
str(travel)

########################################################################################################


# 1.	Run a crosstabs using the variables BRAND and LOYALBIN. What do the results tell you? 
levels(fast$loyalbin) <- c('Not_Loyal', 'Loyal')
table(fast$brand, fast$loyalbin, dnn = c("Brands", "Loyality"))
# Counts for each brand
round(prop.table(table(fast$brand, fast$loyalbin, dnn = c("Brands", "Loyality")),1),2)
# Count percentages row wise
round(prop.table(table(fast$brand, fast$loyalbin, dnn = c("Brands", "Loyality")),2),2)
# Percentage of Total


levels(travel$loyalbin) <- c('Not_Loyal', 'Loyal')
table(travel$brand, travel$loyalbin, dnn = c("Brands", "Loyality"))
# Counts for each brand
round(prop.table(table(travel$brand, travel$loyalbin, dnn = c("Brands", "Loyality")),1),2)
# Count percentages row wise

# 2.	Delete the brands associated with UK and AirUSA. Rerun the crosstabs. What do the results tell you? 
a <- table(travel$brand, travel$loyalbin, dnn = c("Brands", "Loyality"))
travel_filtered <- a[-c(4,5),]
round(prop.table(travel_filtered,1),2)

# 3.	How can you measure brand equity with the collected data? 




# 4.	What statistical analysis is suitable to compare brand equity across brands? 
# Why? Compare brand equity across brands for your chosen category. 

anova_br_eq <- summary(aov(brand_equity ~ brand  ,data = fast))

vs_263_264 <- t.test(fast[fast$brand == 263,18], fast[fast$brand == 264,18],
                         paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_263_265 <- t.test(fast[fast$brand == 263,18], fast[fast$brand == 265,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_263_266 <- t.test(fast[fast$brand == 263,18], fast[fast$brand == 266,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_263_267 <- t.test(fast[fast$brand == 263,18], fast[fast$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_264_265 <- t.test(fast[fast$brand == 264,18], fast[fast$brand == 265,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_264_266 <- t.test(fast[fast$brand == 264,18], fast[fast$brand == 266,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_264_267 <- t.test(fast[fast$brand == 264,18], fast[fast$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_265_266 <- t.test(fast[fast$brand == 265,18], fast[fast$brand == 266,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_265_267 <- t.test(fast[fast$brand == 265,18], fast[fast$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)
vs_266_267 <- t.test(fast[fast$brand == 266,18], fast[fast$brand == 267,18],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE)


#5.     Compare loyalty, relevance, familiarity, uniqueness and popularity 
#for the brands of your chosen category using the appropriate statistical analysis. 




-------------------------------------------------------------------------------------------
    
# loyal # 
# 1- 10 as factor 
loyal_counts_t <- fast %>% group_by(brand, loyal) %>% tally() %>% spread(loyal, n)
loyal_counts_t <- as.data.frame(loyal_counts_t)
rownames(loyal_counts_t) <- loyal_counts_t[,1]
loyal_counts_t[,1] <- NULL
# Ready for chi-sq  266 > 263 > 264 > 267 > 265 // 4 > 1 > 2 > 5 > 3

chisq.test(loyal_counts_t[c(1,4),]) # Sig           # 4 > 1 > others statistically
chisq.test(loyal_counts_t[c(1,2),]) # Sig
chisq.test(loyal_counts_t[c(2,5),]) # No_sig
chisq.test(loyal_counts_t[c(2,3),]) # No_sig


# 0 - 1 
loyal_binary_counts <- fast %>% group_by(brand, loyalbin) %>% tally() %>% spread(loyalbin, n)
loyal_binary_counts <- as.data.frame(loyal_binary_counts)
rownames(loyal_binary_counts) <- loyal_binary_counts[,1]
loyal_binary_counts[,1] <- NULL
# Ready for chi-sq  266 > 264 > 267 > 265 > 263 // 4 > 2 > 5 > 3 > 1

chisq.test(loyal_binary_counts[c(4,2),]) # Sig       4 > others statistically
chisq.test(loyal_binary_counts[c(2,5),]) # No_Sig 
chisq.test(loyal_binary_counts[c(2,3),]) # No_Sig 
chisq.test(loyal_binary_counts[c(2,1),]) # No_Sig 
chisq.test(loyal_binary_counts[c(2,1),])
# Continious Assumption
# 266 > 263 > 264 > 267 > 265 // 4 > 1 > 2 > 5 > 3

fast$loyal <- as.numeric(as.character(fast$loyal))
anova_loyal <- summary(aov(loyal ~ brand  ,data = fast)) # Significant 

l_266_263 <- t.test(fast[fast$brand == 266,8], fast[fast$brand == 263,8],
                     paired = FALSE, conf.level = 0.95, var.equal = TRUE) # Significant

l_263_264 <- t.test(fast[fast$brand == 263,8], fast[fast$brand == 264,8],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_263_264 <- t.test(fast[fast$brand == 263,8], fast[fast$brand == 267,8],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_263_265 <- t.test(fast[fast$brand == 263,8], fast[fast$brand == 265,8],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternate = "greater") # No Significant

l_264_265 <- t.test(fast[fast$brand == 264,8], fast[fast$brand == 265,8],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE, alternate = "greater")

                                                    # 4 > others statistically

-------------------------------------------------------------------------------------------
#famil#


# 0 - 1 
famil_binary_counts <- fast %>% group_by(brand, familbin) %>% tally() %>% spread(familbin, n)
famil_binary_counts <- as.data.frame(famil_binary_counts)
rownames(famil_binary_counts) <- famil_binary_counts[,1]
famil_binary_counts[,1] <- NULL
# Ready for chi-sq  266 > 264 > 263 > 267 > 265 // 4 > 2 > 1 > 5 > 3

chisq.test(famil_binary_counts[c(4,2),]) # Sig       4 > others statistically
chisq.test(famil_binary_counts[c(2,1),]) # No_Sig 
chisq.test(famil_binary_counts[c(2,5),]) # No_Sig 
chisq.test(famil_binary_counts[c(2,3),]) # No_Sig 
chisq.test(famil_binary_counts[c(1,5),]) # No_Sig 
chisq.test(famil_binary_counts[c(1,3),]) # No_Sig 
chisq.test(famil_binary_counts[c(5,3),]) # No_Sig 


# Continious Assumption
# Ready for Anova  266 > 264 > 263 > 267 > 265 // 4 > 2 > 1 > 5 > 3
fast$famil <- as.numeric(as.character(fast$famil))
anova_famil <- summary(aov(famil ~ brand  ,data = fast)) # Significant 

#4 > 2 > 1 > others statistically

l_266_264 <- t.test(fast[fast$brand == 266,5], fast[fast$brand == 264,5],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # Significant

l_264_263 <- t.test(fast[fast$brand == 264,5], fast[fast$brand == 263,5],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_264_267 <- t.test(fast[fast$brand == 264,5], fast[fast$brand == 267,5],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) #  Significant

l_264_265 <- t.test(fast[fast$brand == 264,5], fast[fast$brand == 265,5],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) #  Significant

l_263_267 <- t.test(fast[fast$brand == 263,5], fast[fast$brand == 267,5],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_263_265 <- t.test(fast[fast$brand == 263,5], fast[fast$brand == 265,5],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) #  Significant

l_267_265 <- t.test(fast[fast$brand == 267,5], fast[fast$brand == 265,5],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant



-------------------------------------------------------------------------------------------
#relev#


# 0 - 1 
relev_binary_counts <- fast %>% group_by(brand, relevbin) %>% tally() %>% spread(relevbin, n)
relev_binary_counts <- as.data.frame(relev_binary_counts)
rownames(relev_binary_counts) <- relev_binary_counts[,1]
relev_binary_counts[,1] <- NULL
# Ready for chi-sq  266 > 267 > 265 > 264 > 263 // 4 > 5 > 3 > 2 > 1

chisq.test(relev_binary_counts[c(4,5),]) # Sig       4 > others statistically
chisq.test(relev_binary_counts[c(5,3),]) # No_Sig 
chisq.test(relev_binary_counts[c(5,2),]) # No_Sig 
chisq.test(relev_binary_counts[c(5,1),]) # No_Sig 


# Continious Assumption
# Ready for Anova  266 > 267 > 263 > 264 > 265 // 4 > 5 > 1 > 2 > 3

fast$relev <- as.numeric(as.character(fast$relev))
anova_relev <- summary(aov(relev ~ brand  ,data = fast)) # Significant 


#4 > others statistically

l_266_267 <- t.test(fast[fast$brand == 266,7], fast[fast$brand == 267,7],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # Significant

l_267_263 <- t.test(fast[fast$brand == 267,7], fast[fast$brand == 263,7],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_267_264 <- t.test(fast[fast$brand == 267,7], fast[fast$brand == 264,7],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_267_265 <- t.test(fast[fast$brand == 267,7], fast[fast$brand == 265,7],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant
-------------------------------------------------------------------------------------------
#uniqu#


# 0 - 1 
uniqu_binary_counts <- fast %>% group_by(brand, uniqubin) %>% tally() %>% spread(uniqubin, n)
uniqu_binary_counts <- as.data.frame(uniqu_binary_counts)
rownames(uniqu_binary_counts) <- uniqu_binary_counts[,1]
uniqu_binary_counts[,1] <- NULL
# Ready for chi-sq  266 > 267 > 263 > 265 > 264 // 4 > 5 > 1 > 3 > 2

chisq.test(uniqu_binary_counts[c(4,5),]) # Sig       4 > others statistically
chisq.test(uniqu_binary_counts[c(5,1),]) # No_Sig 
chisq.test(uniqu_binary_counts[c(5,3),]) # No_Sig 
chisq.test(uniqu_binary_counts[c(5,2),]) # No_Sig 


# Continious Assumption
# Ready for Anova  266 > 263 > 267 > 264 > 265 // 4 > 1 > 5 > 2 > 3
fast$uniqu <- as.numeric(as.character(fast$uniqu))
anova_uniqu <- summary(aov(uniqu ~ brand  ,data = fast)) # Significant 


#4 > others statistically
l_266_263 <- t.test(fast[fast$brand == 266,6], fast[fast$brand == 263,6],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # Significant

l_263_267 <- t.test(fast[fast$brand == 263,6], fast[fast$brand == 267,6],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_263_264 <- t.test(fast[fast$brand == 263,6], fast[fast$brand == 264,6],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant

l_263_265 <- t.test(fast[fast$brand == 263,6], fast[fast$brand == 265,6],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # No Significant
-------------------------------------------------------------------------------------------
#popul#


# 0 - 1 
popul_binary_counts <- fast %>% group_by(brand, populbin) %>% tally() %>% spread(populbin, n)
popul_binary_counts <- as.data.frame(popul_binary_counts)
rownames(popul_binary_counts) <- popul_binary_counts[,1]
popul_binary_counts[,1] <- NULL
# Ready for chi-sq  266 > 264 > 267 > 263 > 265 // 4 > 2 > 5 > 1 > 3

chisq.test(popul_binary_counts[c(4,2),]) # Sig       4 > 2 > others statistically
chisq.test(popul_binary_counts[c(2,5),]) # Sig 
chisq.test(popul_binary_counts[c(5,1),]) # NOSig
chisq.test(popul_binary_counts[c(5,3),]) # NOSig 



# Continious Assumption
# Ready for chi-sq  266 > 264 > 267 > 263 > 265 // 4 > 2 > 5 > 1 > 3
fast$popul <- as.numeric(as.character(fast$popul))
anova_popul <- summary(aov(popul ~ brand  ,data = fast)) # Significant 

#4 > 2 > 5 > others
l_266_264 <- t.test(fast[fast$brand == 266,9], fast[fast$brand == 264,9],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) # Significant

l_264_267 <- t.test(fast[fast$brand == 264,9], fast[fast$brand == 267,9],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) #  Significant

l_267_263 <- t.test(fast[fast$brand == 267,9], fast[fast$brand == 263,9],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) #No Significant

l_267_265 <- t.test(fast[fast$brand == 267,9], fast[fast$brand == 265,9],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) #  Significant

l_263_265 <- t.test(fast[fast$brand == 263,9], fast[fast$brand == 265,9],
                    paired = FALSE, conf.level = 0.95, var.equal = TRUE) #  Significant


































