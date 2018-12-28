library(xlsx)
library(dplyr)
new_gold <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case4_Hospitals/HospitalEditable.xlsx", sheetIndex = 2)
old_blue <- read.xlsx("/Users/Caner/Desktop/SMU_Class/Statistics_5520/Case4_Hospitals/HospitalEditable.xlsx", sheetIndex = 3)

new_gold_RN <- new_gold %>% filter(Nurse.Type == "RN")
new_gold_NA <- new_gold %>% filter(Nurse.Type == "NA")

old_blue_RN <- old_blue %>% filter(Nurse.Type == "RN")
old_blue_NA <- old_blue %>% filter(Nurse.Type == "NA")

t.test(old_blue_RN$NURSES_ADMIN, new_gold_RN$NURSES_ADMIN, paired = FALSE, conf.level = 0.95, var.equal = TRUE)
t.test(old_blue_NA$NURSES_ADMIN, new_gold_NA$NURSES_ADMIN, paired = FALSE, conf.level = 0.95, var.equal = TRUE)

t.test(old_blue_RN$Direct.Patient.Care, new_gold_RN$Direct.Patient.Care, paired = FALSE, conf.level = 0.95, var.equal = TRUE)
t.test(old_blue_NA$Direct.Patient.Care, new_gold_NA$Direct.Patient.Care, paired = FALSE, conf.level = 0.95, var.equal = TRUE)

t.test(old_blue_RN$Indirect.Patient.Care, new_gold_RN$Indirect.Patient.Care, paired = FALSE, conf.level = 0.95, var.equal = TRUE)
t.test(old_blue_NA$Indirect.Patient.Care, new_gold_NA$Indirect.Patient.Care, paired = FALSE, conf.level = 0.95, var.equal = TRUE)

t.test(old_blue_RN$Non.Nursin.Tasks, new_gold_RN$Non.Nursin.Tasks, paired = FALSE, conf.level = 0.95, var.equal = TRUE)
t.test(old_blue_NA$Non.Nursin.Tasks, new_gold_NA$Non.Nursin.Tasks, paired = FALSE, conf.level = 0.95, var.equal = TRUE)



# visualize with box plots
# # Plot weight by group and color by group later
library(ggplot2)

df_binded <- as.data.frame(rbind(cbind(old_blue_NA$Indirect.Patient.Care,1),cbind(new_gold_NA$Indirect.Patient.Care,2)))
colnames(df_binded) <- c("data", "category")
df_binded$category <- as.factor(df_binded$category)
p10 <- ggplot(df_binded, aes(x = category, y = data)) +
    geom_boxplot()
p10
