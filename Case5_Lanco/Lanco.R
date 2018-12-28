library(xlsx)
library(ggplot2)
library(Hmisc)
library(MASS) #lda
library(tm) # KNN

####################################### DATA PREPARATION ###############################################

lanco <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case5_Lanco/Lanco5.xlsx", sheetIndex = 1)
lanco <- lanco[c(1:75),c(1:5)]
colnames(lanco)[5] <- "Days_Delivered"
colnames(lanco)[3] <- "Order_Size"
lanco <- lanco[,c("Received", "Order_Size", "Returned", "Days_Delivered")]

########################################################################################################


####################################### DATA INSPECTION ###############################################

descriptive_stats <- sapply(lanco[,c(2,4)], function(x) c( "Stand dev" = sd(x), 
                                                           "Mean"= mean(x,na.rm=TRUE),
                                                           "n" = length(x),
                                                           "Median" = median(x),
                                                           "CoeffofVariation" = sd(x)/mean(x,na.rm=TRUE),
                                                           "Minimum" = min(x),
                                                           "Maximun" = max(x),
                                                           "Upper Quantile" = quantile(x,1),
                                                           "LowerQuartile" = quantile(x,0)
)
)

data_density <- datadensity(lanco[,c(2,3,4)])

p <- ggplot(lanco, aes(x = Order_Size, y = Days_Delivered, label = Returned, colour = Returned)) + geom_point()
p + ggtitle("Scatter Plot Order Size vs. Delivery Time coloured by Returned")

########################################################################################################


####################################### ANALYSIS #######################################################

days_yes_vs_no <- t.test(lanco[lanco$Returned == "Yes",4], lanco[lanco$Returned == "No",4],
                        paired = FALSE, conf.level = 0.95, var.equal = TRUE)
# Proving yes&no days not coming from same population

qty_yes_vs_no <- t.test(lanco[lanco$Returned == "Yes",2], lanco[lanco$Returned == "No",2],
                        paired = FALSE, conf.level = 0.95, var.equal = TRUE)
# Proving yes&no quantities not coming from same population


independent_cor <- cor(lanco$Order_Size, lanco$Days_Delivered)
# No Multicollinearity between independent variables 

anova_qty <- summary(aov(Order_Size ~ Returned  ,data = lanco))
anova_days <- summary(aov(Days_Delivered ~ Returned  ,data = lanco))



levels(lanco$Returned) <- c(0, 1)
levels(lanco_s$Returned) <- c(0, 1)

#Randomly shuffle the data
#set.seed(43)
lanco_s<-lanco[sample(nrow(lanco)),]

#Create 5 equally size folds
folds <- cut(seq(1,nrow(lanco_s)),breaks=5,labels=FALSE)

log_accuracy <- numeric()
lda_accuracy <- numeric()
knn_accuracy <- numeric()

log_coef_i <- numeric() 
log_coef_o <- numeric() 
log_coef_s <- numeric() 
#Perform 5 fold cross validation
for(i in 1:5){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- lanco_s[testIndexes, ]
    trainData <- lanco_s[-testIndexes, ]
    ### test&train data is ready do analysis now ###
    
    ### Logistic Regression ###
    logmod <- glm(Returned ~ Order_Size + Days_Delivered, family = binomial(link = "logit"), data = trainData)
    pdata_log <- predict(logmod, testData, type = "response")
    cmat_log <- confusionMatrix(data = as.numeric(pdata_log>0.5), reference = testData$Returned)
    log_accuracy[i] <- cmat_log$overall[1]
    
    log_coef_i[i] <- logmod$coefficients[1]
    log_coef_o[i] <- logmod$coefficients[2]
    log_coef_s[i] <- logmod$coefficients[3]
    ####
    
    ### Linear Discriminant Analysis ### 
    ldamod <- lda(Returned ~ Order_Size + Days_Delivered, data = trainData)
    pdata_lda <- predict(ldamod, newdata = testData[,c(2,4)])$class
    cmat_lda <- confusionMatrix(data = pdata_lda, reference = testData$Returned)
    lda_accuracy[i] <- cmat_lda$overall[1]
    
    
    ### k - Nearest Neighbors ### 
    knn_output <- knn3Train(trainData[,c(2,4)], testData[,c(2,4)], trainData[,c(3)], k = 1)
    predictions_knn <- as.numeric(knn_output[1:nrow(testData)])
    cmat_knn <- confusionMatrix(predictions_knn, reference = testData[,c(3)])
    knn_accuracy[i] <- cmat_knn$overall[1]

}


#print(c("log accuracy", mean(log_accuracy), "lda accuracy", mean(lda_accuracy),"knn accuracy", mean(knn_accuracy))
accuracies <- c(mean(log_accuracy), mean(lda_accuracy), mean(knn_accuracy))
# knn > log > lda
# around 84 80 77

logmod <- glm(Returned ~ Order_Size + Days_Delivered, family = binomial(link = "logit"), data = lanco)
pdata_log <- predict(logmod, lanco, type = "response")
confusionMatrix(data = as.numeric(pdata>0.5), reference = lanco$Returned)
plot(lanco$Order_Size, lanco$Days_Delivered)
abline(logmod$linear.predictors)

ldamod <- lda(Returned ~ Order_Size + Days_Delivered, data = lanco)
pdata_lda <- predict(ldamod, newdata = lanco[,c(2,4)])$class
confusionMatrix(data = pdata_lda, reference = lanco$Returned)




train_lan <- lanco[16:75,]
test_lan <- lanco[1:15,]

#hede <- knn3Train(train_lan[,c(2,4)], test_lan[,c(2,4)], train_lan[,c(3)], k = 1)
hede <- knn3Train(lanco[,c(2,4)], lanco[,c(2,4)], lanco[,c(3)], k = 1)
predictions_knn <- as.numeric(hede[1:nrow(lanco)])
confusionMatrix(predictions_knn, reference = lanco[,c(3)])


kmeans <- kmeans(lanco[,c(2,4)], centers = 2)
plot(lanco$Order_Size, lanco$Days_Delivered)
points(lanco[kmeans$cluster ==1,], col = "red")


intercept <- mean(log_coef_i)
coef2 <- mean(log_coef_o)
coef3 <- mean(log_coef_s)

ab_slope <- coef2 / -coef3
ab_intercept <- intercept / -coef3

p + geom_abline(intercept=ab_intercept, slope=ab_slope)

t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
    if( equal.variance==FALSE ) 
    {
        se <- sqrt( (s1^2/n1) + (s2^2/n2) )
        # welch-satterthwaite df
        df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
    } else
    {
        # pooled standard deviation, scaled by the sample sizes
        se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
        df <- n1+n2-2
    }      
    t <- (m1-m2-m0)/se 
    dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
    names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
    return(dat) 
}











