library(tidyverse)
## MPG Regression
mpg_mecha <- read.csv('mechacar_mpg.csv',check.names = F,stringsAsFactors = F) #import dataset
head(mpg_mecha)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance,data=mpg_mecha))

## Suspension Coil Summary
suspensionCoil_data <- read.csv('Suspension_Coil.csv',check.names = F,stringsAsFactors = F) #import dataset
head(susp_coil)
summary <- suspensionCoil_data %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI)) 
lot_summary <- suspensionCoil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),SD=sd(PSI)) 

## Suspension Coil T-Tests
# Lot 1
t.test(subset(suspensionCoil_data,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# Lot 2
t.test(subset(suspensionCoil_data,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# Lot 3
t.test(subset(suspensionCoil_data,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)
# Across all Lots
t.test(suspensionCoil_data$PSI,mu = 1500)
