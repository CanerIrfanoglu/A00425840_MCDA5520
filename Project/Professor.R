library(xlsx)
library(plyr) # revalue func
library(dplyr)
library(Hmisc) # describe function
library(corrplot)

professor <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Project/Professor.xls", sheetIndex = 2, header = TRUE )
professor_raw <- professor
na_counts_professor <-sapply(professor, function(y) sum(length(which(is.na(y))))) # No NA Values
descriptive_professor <- describe(professor)
data_types_professor <- str(professor)

professor$Colour <- revalue(professor$Colour, c("L"=1, "J"=2, "K" = 2, "G" = 3, "H" = 3, "I" = 3, "F" = 4, "D" = 4, "E" = 4 ))
#professor$Colour <- revalue(professor$Colour, c("L"="1-L", "J"="2-J", "K" = "2-K", "G" = "3-G", "H" = "3-H", "I" = "3-I", "F" = "4-F", "D" = "4-D", "E" = "4-E" ))

#professor$Clarity <- revalue(professor$Clarity, c("SI2" = 5, "SI1" = 6, "SI3" = 4, "VS2" = 7, "VS1" = 8, "I1" = 3, "I2" = 2, "VVS1" = 10, "VVS2" = 9))
#professor$Clarity <- revalue(professor$Clarity, c("SI2" = "5-SI2", "SI1" = "6-SI1", "SI3" = "4-SI3", "VS2" = "7-VS2", "VS1" = "8-VS1", "I1" = "3-I1", "I2" = "2-I2", "VVS1" = "10-VVS1", "VVS2" = "9-VVS2"))
professor$Clarity <- revalue(professor$Clarity, c("SI2" = 2, "SI1" = 2, "SI3" = 2, "VS2" = 3, "VS1" = 3, "I1" = 1, "I2" = 1, "VVS1" = 3, "VVS2" = 3))

professor$Cut <- revalue(professor$Cut, c("P"=1, "F"=2, "G" = 3, "V" = 4, "X" = 5, "I" = 6))
#professor$Cut <- revalue(professor$Cut, c("P"="1-P", "F"="2-F", "G" = "3-G", "V" = "4-V", "X" = "5-X", "I" = "6-I"))

professor$Polish <- revalue(professor$Polish, c("P"=1, "F"=2, "G" = 3, "V" = 4, "v" = 4, "X" = 5, "I" = 6))
#professor$Polish <- revalue(professor$Polish, c("P"="1-P", "F"="2-F", "G" = "3-G", "V" = "4-V", "X" = "5-X", "I" = "6-I"))

professor$Symmetry <- revalue(professor$Symmetry, c("P"=1, "F"=2, "G" = 3, "V" = 4, "X" = 5, "I" = 6))
#professor$Symmetry <- revalue(professor$Symmetry, c("P"="1-P", "F"="2-F", "G" = "3-G", "V" = "4-V", "X" = "5-X", "I" = "6-I"))


lm1 <- lm(Price ~ Polish , data = professor)

wholesaler_means <- professor %>% group_by(Wholesaler) %>% dplyr::summarise(avg = mean(Price))

#write.csv(professor, "/Users/Caner/Desktop/SMU_Class/Statistics_5520/Project/professor_modified2.csv")
test1 <- describe(professor)


professor[,c(-5)]
sapply(professor[,c(-5)], numeric)
prof_as_char <- sapply(professor[,c(-5,-8)], as.character)
professor_numeric <- sapply(as.data.frame(prof_as_char), as.numeric)

corrplot(cor(professor_numeric))

### ANOVA ####
anova_Cut <- summary(aov(Price ~ Cut  ,data = professor)) # Significant 
anova_Color <- summary(aov(Price ~ Colour  ,data = professor)) # Significant 


#### REGRESSION #####
professor_cluster <- professor[professor$Carat > 0.5,]

# CARAT 

lm1 <- lm(Price ~ Colour , data = professor) # Initial

# COLOR #

professor$Colour <- revalue(professor$Colour, c("1" = 1, "2" = 1, "3" = 2, "4" = 2))
lm1 <- lm(Price ~ Colour , data = professor)
professor_cluster <- professor[professor$Carat > 0.5,]

# CLARITY # 

lm1 <- lm(Price ~ Clarity , data = professor_cluster)


##  CUT ## 

test1 <- within(professor_cluster, Cut <- relevel(Cut, ref = 5))
# Changing fixing variable to 5th one in df for regression


test2 <- within(professor_cluster, Cut <- relevel(Cut, ref = 3)) # 6 fixed
test2 <- within(professor_cluster, Cut <- relevel(Cut, ref = 2)) # 3 fixed

# POLISH # 

lm1 <- lm(Price ~ Polish , data = professor_cluster)
freqs_prof <- describe(professor_cluster$Polish)

#professor_cluster$Polish <- revalue(professor_cluster$Polish, c("2" = 1, "3" = 1, "4" = 2, "5" = 3, "6" = 3))
# 3 categories






# SYMMETRY # 

lm1 <- lm(Price ~ Symmetry , data = professor_cluster)
professor_cluster$Symmetry <- revalue(professor_cluster$Symmetry, c("2" = 1, "3" = 2, "4" = 3, "5" = 3, "6" = 3))
test2 <- within(professor_cluster, Symmetry <- relevel(Symmetry, ref = 2)) # 3 fixed



# CERTIFICATION # 

professor_cluster$Certification <- revalue(professor_cluster$Certification, c("AGS"=1, "DOW"=1, "EGL" = 2, "GIA" = 2, "IGI" = 1))
lm1 <- lm(Price ~ Certification , data = professor_cluster)








