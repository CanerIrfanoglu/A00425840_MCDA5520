library(xlsx)
library(dplyr)
orion <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case7_Orion/Orion_v2_noNA.xls", sheetIndex = 1 )

na_counts_orion <-sapply(orion, function(y) sum(length(which(is.na(y)))))
orion <- orion[-c(70:nrow(orion)),-c(13)] # Remove empty column

orion <- orion %>% mutate(orion_win = ifelse(orion$Winning.Bid.Price == orion$Orion.Bid, 1, 0))
# Mutate a new column if orion won 1 else 0

orion <- orion[-which(orion$Fuel_Type == "Hyb"),]


# + Fuel_Type + Length + Floor_Type
lm1 <- lm(Winning.Bid.Price ~ Orion.Cost..per.Bus + Quan. , data = orion)
lm2 <- lm(Winning.Bid.Price ~ Orion.Cost..per.Bus + Quan. + Fuel_Type + Length + Floor_Type, data = orion)

#write.csv(orion, "/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case7_Orion/Orion_v4.csv")

orion2 <- orion[orion$orion_win == 0,]
summary(aov(orion2$Winning.Bid.Price ~ orion2$Floor_Type + orion2$Fuel_Type +orion2$Length + orion2$Orion.Bid + orion2$Quan.))
