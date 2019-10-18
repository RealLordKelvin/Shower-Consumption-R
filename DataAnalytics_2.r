# load datasets

data <- read.csv2("file:///C:/Users/pc/Desktop/Master_Statistik/Energy-Analytics/asset-v1_WI-EESYS+DAE-M+2017-2018_WS+type@asset+block@Shower_data (1).csv")

data_survey <- read.csv2("file:///C:/Users/pc/Desktop/Master_Statistik/Energy-Analytics/asset-v1_WI-EESYS+DAE-M+2017-2018_WS+type@asset+block@Shower_survey_data (1).csv")



# calculating energy consumption., overview

data$energy_consumption <- data$Volume*((data$Avgtemperature-12)/0.65)*(4.184/3600)

summary(data$energy_consumption)


# remove outlieres


data_positive <- data[-data$energy_consumption < 0,]

summary(data_positive$energy_consumption)

regr_1 <- lm(data_positive$energy_consumption~data_positive$Avgtemperature + data_positive$Volume, data = data_positive)
plot(regr_1)

summary(regr_1)

plot(data_positive$Avgtemperature, data_positive$Volume)

# quantile approach

qnt <- quantile(data_positive$energy_consumption, probs=c(.25, .75))
summary(qnt)
head(qnt)

data_clean_quantile <- data_positive[data_positive$energy_consumption >=qnt[1] & data_positive$energy_consumption<=qnt[2],]
summary(data_clean_quantile)

# standard deviation approach

upper_sd <- mean(data_clean$energy_consumption + 2 * sd(data_clean$energy_consumption))
under_sd <- mean(data_clean$energy_consumption - 2 * sd(data_clean$energy_consumption))

data_clean_sd <- data_positive[data_positive$energy_consumption >= under_sd & data_positive$energy_consumption <= upper_sd,]

summary(data_clean_sd)

# compare two approaches with original data

windows()
layout(matrix(1:3, nrow = 1, byrow = TRUE))
plot(data_clean_quantile$Avgtemperature, data_clean_quantile$Volume)
plot(data_positive$Avgtemperature, data_positive$Volume)
plot(data_clean_sd$Avgtemperature, data_clean_sd$Volume)

#  choosing the standard deviation approach because it contains more values,
#  but there is no real difference in both approaches


# Regression Analysis

library(dplyr)
library(ggplot2)
#Remove the first shower, create groups
data_in_groups <- data_clean_sd %>% dplyr::filter(Shower != 1) %>%
  dplyr::mutate(isTreatment = ifelse(group %in% c("1","2"), FALSE, TRUE),
                isIntervention = ifelse(Shower <= 10, FALSE, TRUE))


#Create the model for the DiD effect 
regr_3 <- lm( energy_consumption ~ Volume + Avgtemperature +isTreatment * isIntervention, data = data_in_groups)
summary(regr_3)
