library(readxl)
library(xlsx)

# read Professor.xlsx
diamond <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Project/Professor.xls", sheetIndex = 2, header = TRUE )

#View first 6 observations
head(diamond)

#Variables in the dataset
names(diamond)

#Reordering column names
diamond2 <- diamond[, c(1,2,3,4,5,6,7,9,8)]
diamond2

# Encoding categorical data
diamond2$Colour <- factor(diamond2$Colour,
                           levels = c('D','E','F','G','H','I','J','K','L'),
                           labels = c(1,1,1,2,2,2,3,3,4))
diamond2$Clarity <- factor(diamond2$Clarity,
                            levels = c('I2','I1','SI3','SI2','SI1', 'VS2',
                                       'VS1','VVS1','VVS2'),
                            labels =c(2,3,4,5,6,7,8,9,10))
diamond2$Cut <- factor(diamond2$Cut,
                        levels = c('F','G','V','X','I'),
                        labels = c(2,3,4,5,6))
diamond2$Certification <- factor(diamond2$Certification,
                                  levels = c('GIA','AGS','EGL','IGI','DOW'),
                                  labels =c(1,2,3,4,5))
diamond2$Polish <- factor(diamond2$Polish,
                           levels = c('F','G','V','v','X','I'),
                           labels = c(2,3,4,4,5,6))
diamond2$Symmetry <- factor(diamond2$Symmetry,
                             levels = c('F','G','V','X','I'),
                             labels = c(2,3,4,5,6))
diamond2$Wholesaler <- factor(diamond2$Wholesaler)

#View first few obs 
head(diamond2)

#histogram of price
library(ggplot2)
ggplot(data = diamond2, aes(x = Price)) +
  geom_histogram(aes(y = ..density..), fill = 'white', col = 'black') +
  geom_vline(xintercept = 3100, color = "red", size=1) +
  geom_vline(xintercept = mean(diamond2$Price), 
             linetype = 'dotted', color = 'blue', size = 1)

#histogram of carat
ggplot(data = diamond2, aes(x = Carat)) +
  geom_histogram(aes(y = ..density..), fill = 'white', col = 'black') +
  geom_vline(xintercept = 0.9, color = "red", size=1) +
  geom_vline(xintercept = mean(diamond2$Carat), 
             linetype = 'dotted', color = 'blue', size = 1)

#Scatterplot of Price vs Carat 
priceScatter = ggplot(diamond2, 
                       aes(y = Price, x = Carat,
                           shape = Wholesaler, color = Wholesaler)) +
  geom_point(size = 3) +
  xlab('Carat') +
  ylab('Price($)') + 
  ggtitle('Scatterplot of Price and Carat') +
  theme(plot.title = element_text(hjust = 0.5))
priceScatter

#Filter out Wholesaler = 3 from the dataset
new_diamond <- diamond2[diamond2$Wholesaler %in% c(1,2),] 

#Scatterplot of Price and Carat from new dataset
priceScatter2 = ggplot(new_diamond, 
                      aes(y = Price, x = Carat,
                          shape = Wholesaler, color = Wholesaler)) +
  geom_point(size = 3) +
  xlab('Carat') +
  ylab('Price ($)') + 
  ggtitle('Scatterplot of Price and Carat') +
  theme(plot.title = element_text(hjust = 0.5))
priceScatter2

#Scatterplot Matrix to see multicollinearity
car::scatterplotMatrix(new_diamond, diagonal = 'density', 
                       smooth = F, reg.line = F)
#Carat vs Colour
ggplot(new_diamond, aes(y=Carat, x=Colour, color = Wholesaler)) +
  geom_point(size = 2)
#Carat vs Clarity
ggplot(NewProfessor, aes(y=Carat, x=Clarity, color = Wholesaler)) +
  geom_point(size = 2)
#Carat vs Cut
ggplot(NewProfessor, aes(y=Carat, x=Cut, color = Wholesaler)) +
  geom_point(size = 2)

#Multiple Linear Regression
#Step 1: Take all independent variables
regressor1 = lm(formula = Price ~ ., data = new_diamond)
summary(regressor1)

# Results:
# Significant: Carat, Colour, Clarity
# Partial Significant: Cut, Symmetry, Certification, Wholesaler
# Not Significant: Polish, Wholesaler

#Step 2: Remove the variable that has largest p-value
# Cut3
# Regroup cut
cut_prop = table(new_diamond$Cut)
round(prop.table(cut_prop) , digits = 2)

new_diamond$Cut = factor(new_diamond$Cut,
                         levels = c(2,3,4,5,6),
                         labels = c(1,1,1,2,2))
diamondMLR = lm(formula = Price ~., data = new_diamond)
summary(diamondMLR)
#Cut is significant, adjusted R2 increased

#Step 3: Polish
#regroup
pol_prop = table(new_diamond$Polish)
round(prop.table(pol_prop), digits = 2)

new_diamond$Polish <- factor(new_diamond$Polish,
                          levels = c(2,3,4,5,6),
                          labels = c(1,1,2,3,3))


diamondMLR = lm(formula = Price ~ ., data = new_diamond)
summary(diamondMLR)
#Polish is not significant 


#Step 4: Remove Polish from the model
diamondMLR = lm(formula = Price ~ . -Polish, data = new_diamond)
summary(diamondMLR)
#adjusted R2 increased


#Step 5: regroup Certification
cert_prop = table(new_diamond$Certification)
round(prop.table(cert_prop), digits = 2)
new_diamond$Certification <- factor(new_diamond$Certification,
                                    levels = c(1,2,3,4,5),
                                    labels =c(1,1,2,2,2))

diamondMLR = lm(formula = Price ~ . -Polish, data = new_diamond)
summary(diamondMLR)
#certification is significant 
#adjusted R2 increased

#Step 6: symmetry
sym_prop = table(new_diamond$Symmetry)
round(prop.table(sym_prop) , digits = 2)

new_diamond$Symmetry <- factor(new_diamond$Symmetry,
                                levels = c(2,3,4,5,6),
                                labels = c(2,3,4,5,5))

diamondMLR = lm(formula = Price ~ . -Polish, data = new_diamond)
summary(diamondMLR)
#symmetry is significant 
#adjusted R2 increased

#Step 7: check the prop for colour and clarity
#prop table for colour and clarity
#colour
col_prop = table(new_diamond$Colour)
round(prop.table(col_prop), digits = 2)
#regroup is not needed because each category >= 5%

#clarity
cla_prop = table(new_diamond$Clarity)
round(prop.table(cla_prop), digits = 2)

new_diamond$Clarity <- factor(new_diamond$Clarity,
                               levels = c(2,3,4,5,6,7,8,9,10),
                               labels =c(2,3,4,5,6,7,7,7,7))

diamondMLR = lm(formula = Price ~ . -Polish, data = new_diamond)
summary(diamondMLR)
car::vif(diamondMLR)
#final MLR model
#Limitation: intercept is insignificant
#Clarity and Wholesaler are showing multicollinearity in this model (vif > 5)

#without Wholesaler
diamondMLR_xw = lm(formula = Price ~ . -Polish-Wholesaler, data = new_diamond)
summary(diamondMLR_xw)
car::vif(diamondMLR_xw)
#adjusted R2 decreased
#no multicollinearity in this model (all vifs < 5)

#Normal Q-Q plot
diamondMLR_res = rstandard(diamondMLR)
qqnorm(diamondMLR_res, 
       ylab = "Standardized Residuals",
       xlab = "Normal Percentiles",
       main = "Normal Q-Q Plot")
qqline(diamondMLR_res)

#Residuals vs Fitted Value
plot(diamondMLR, which = 1)

#Non-linearity and non-constant variance
#tranform response
diamond_log <- lm(log(Price) ~. -Polish, data = new_diamond)
summary(diamond_log)
car::vif(diamond_log)
# adjusted R2 increased and intercept is significant
# Clarity and Wholesaler are showing multicollinearity (vif > 5)

#remove Wholesaler
diamond_log_xw <- lm(log(Price) ~. -Polish-Wholesaler, data = new_diamond)
summary(diamond_log_xw)
car::vif(diamond_log_xw)
#adjusted R2 increased and significant
# vifs < 5, no multicollinearity is in this model
# In my opinion, this would be the best model

#Normal Q-Q plot
qqnorm(diamond_log$residuals, 
       ylab="Raw Residuals")
qqline(diamond_log$residuals)

#Residuals vs Fitted Value
plot(diamond_log$fit,diamond_log$res,
     xlab="Fitted",
     ylab="Residuals",
     main="Logged Response")
