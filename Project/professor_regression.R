library(xlsx)
library(plyr) # revalue func
professor <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Project/Professor.xls", sheetIndex = 2, header = TRUE )
professor_raw <- professor

professor_cluster <- professor[professor$Carat > 0.5,]
# Cluster to work with


##### DATA PREPARATION ##### 

professor_cluster <- dplyr::select(professor_cluster, -c(Wholesaler))
# Color # 
professor_cluster$Colour <- revalue(professor_cluster$Colour, c("L"=1, "J"=2, "K" = 2, "G" = 3, "H" = 3, "I" = 3, "F" = 4, "D" = 4, "E" = 4 ))
professor_cluster$Colour <- revalue(professor_cluster$Colour, c("1" = 1, "2" = 1, "3" = 2, "4" = 2))

# Clarity # 
professor_cluster$Clarity <- revalue(professor_cluster$Clarity, c("SI2" = 2, "SI1" = 2, "SI3" = 2, "VS2" = 3, "VS1" = 3, "I1" = 1, "I2" = 1, "VVS1" = 3, "VVS2" = 3))

# Cut # 
professor_cluster$Cut <- revalue(professor_cluster$Cut, c("F"=2, "G" = 3, "V" = 4, "X" = 5, "I" = 6))
#professor_cluster$Cut <- revalue(professor_cluster$Cut, c("2"=1, "3" = 2, "4" = 2, "5" = 3, "6" = 3))


# Symmetry # 
professor_cluster$Symmetry <- revalue(professor_cluster$Symmetry, c("F"=2, "G" = 3, "V" = 4, "X" = 5, "I" = 6))
professor_cluster$Symmetry <- revalue(professor_cluster$Symmetry, c("2" = 1, "3" = 2, "4" = 3, "5" = 3, "6" = 3))

# Polish # 
professor_cluster$Polish <- revalue(professor_cluster$Polish, c("F"=2, "G" = 3, "V" = 4, "v" = 4, "X" = 5, "I" = 6))
professor_cluster$Polish <- revalue(professor_cluster$Polish, c("2" = 1, "3" = 1, "4" = 2, "5" = 3, "6" = 3))

# Certification #
professor_cluster$Certification <- revalue(professor_cluster$Certification, c("AGS"=2, "DOW"=1, "EGL" = 1, "GIA" = 2, "IGI" = 1))

################


#### ANOVA #### 

anova_Cut <- summary(aov(Price ~ Cut  ,data = professor_cluster)) 
anova_Color <- summary(aov(Price ~ Colour  ,data = professor_cluster)) 
anova_Symmetry <- summary(aov(Price ~ Symmetry  ,data = professor_cluster)) 
anova_Polish <- summary(aov(Price ~ Polish  ,data = professor_cluster)) 
anova_Clarity <- summary(aov(Price ~ Clarity  ,data = professor_cluster)) 
anova_Certification <- summary(aov(Price ~ Certification  ,data = professor_cluster)) 

##### REGRESSION ####

lm1 <- lm(Price ~ . , data = professor_cluster)
# All variables - Certification + Cut insignificant
lm2 <- lm(Price ~ . -c(Certification), data = professor_cluster)
# Removed Certification

professor_cluster$Cut <- revalue(professor_cluster$Cut, c("2"=1, "3" = 1, "4" = 2, "5" = 2, "6" = 2))
# Reduce Cut to 2 groups
lm3 <- lm(Price ~ Carat+Colour+Clarity+Polish+Symmetry+Cut+Certification, data = professor_cluster)
# All variables significant









### CORR PLOT ###

prof_as_char <- sapply(professor_cluster, as.character)
professor_numeric <- sapply(as.data.frame(prof_as_char), as.numeric)

corrplot(cor(professor_numeric[,-8]))
