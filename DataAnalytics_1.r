# Import datasets
data <- read.csv2("file:///C:/Users/pc/Desktop/programming/Shower_data_EnergyDataAnalytics.csv")
data_survey <- read.csv2("file:///C:/Users/pc/Desktop/programming/Shower_survey_data_EnergyDataAnalytics.csv")

# inspect datasets
head(data)
str(data)
head(data_survey)
str(data_survey)

#load necessary libraries
library(dplyr)
library(magrittr)

install.packages("mice")
library(mice)
## Bonus exercise 1a ##

## Hypotheses
# H0**: Real-time information on personal water consumption does not trigger conservation behavior?
# H1**: Real-time information on personal water consumption triggers conservation behavior?

# split in Baseline und no Baseline Phase
data_with_baseline <- data %>%dplyr::filter(Shower <= 10)
data_no_baseline <- data %>%dplyr::filter(Shower > 10)

# built two dataframes: First dataframe = Control group, Second dataframe = Experimental group
data_control <- data_no_baseline %>% filter(group <= 2)
data_feedback <- data_no_baseline %>% filter(group > 2)

# group by Hh_id for control and feedback group
data_control_Volume <- data_control %>% group_by(Hh_ID) %>%
  summarise(SumVolume = sum(Volume),
            avgVolume = mean(Volume))

data_feedback_Volume <- data_feedback %>% group_by(Hh_ID) %>%
  summarise(SumVolume = sum(Volume),
            avgVolume = mean(Volume))

# two-sample and two-sided t-test of average Volume of each Hh_Id in the according group
t.test(data_control_Volume$avgVolume, data_feedback_Volume$avgVolume, mu = 0, conf.level = .95)


# plot histograms ? 
library(ggplot2)

gg_feedback <- ggplot(data = data_feedback_Volume, mapping = aes(data_feedback_Volume$SumVolume)) + geom_histogram(bins=30)
gg_feedback

gg_control <- ggplot(dat = data_control_Volume, mapping = aes(data_control_Volume$SumVolume)) + geom_histogram(bins = 30)
gg_control

gg_feedback_1 <- ggplot(data = data_feedback, mapping = aes(data_feedback$Volume)) + geom_histogram(bins = 30)
gg_feedback_1

###### ? 

data_control_Volume_avg_var <- data_control_Volume %>% summarise(avgVolume_Control = mean(SumVolume),
                                                             VARVolume_Control = var(SumVolume))

data_feedback_Volume_avg_var <- data_feedback_Volume %>% summarise(avgVolume_Feedback = mean(SumVolume),
                                                               VARVolume_Feedback = var(SumVolume))  


## b) Hawthorne - Effect ##
# Individuals are aware of being observed and thus modify an aspect of their behavior

# We assume that people in the Baseline phase are not aware of being observed. This changes in the upcoming intervention phase.

data_control_Bl <- data_with_baseline%>% filter(group <= 2)
data_control_Ip <- data_no_baseline %>% filter(group <= 2)


data_control_Bl_test <- data_control_Bl %>% group_by(Hh_ID) %>% summarise(avgVolume = mean(Volume))
data_control_Ip_test <- data_control_Ip %>% group_by(Hh_ID) %>% summarise(avgVolume = mean(Volume))


t.test(data_control_Bl_test$avgVolume, data_control_Ip_test$avgVolume, mu = 0, conf.level = .95)

## c) Do young people consume less water during showering ##
# Hypotheses:
# H0: People aged 20-29 do not consume less than older people during the baseline phase
# H1: People aged 20-29 do consume less than older people during the baseline phase

# combine the Shower- and Survey-dataset by a left_join
combined_dataset <- dplyr::left_join(data_with_baseline, data_survey)

# use this dataset and group it by Hh_ID, age, then summarise the average Volume
age_participants <- combined_dataset %>% dplyr::group_by(Hh_ID, alter) %>% 
  dplyr::summarise(avgVolume = mean(Volume))

# filter dataset by people aged 20-29 and older and save it in according new dataset
age_20_29 <- result%>% dplyr::filter(alter == "20-29")
age_older_20_29 <- result%>%filter(alter != "20-29")

#two-sample and two-sided t-test of people aged 20-29 and older and compare their average Volume
t.test(age_20_29$avgVolume, age_older_20_29$avgVolume, mu = 0, conf.level = .95)


## d) does gender influence water consumption ##
# Hypotheses
# H0: Gender of participants does not influence the average shower volume during the baseline phase
# H1: Gender of participants does influence the average shower volume during the baseline phase

# use combined dataset and compute average Volume for every Household and the according gender
gender <- combined_dataset %>% group_by(Hh_ID, gesl) %>% summarise(avgVolume=mean(Volume)) 

# filter the new dataset gender by sexuality (männlich & is not männlich = weiblich)
weiblich <- gender%>% filter(gesl == "weiblich") 
m�nnlich <- gender%>%filter(gesl != "weiblich")

#two-sample & two-sided t-test, compare the average volume of each gender and the according Household ID's on a confidence level of 95 %
t.test(m�nnlich$avgVolume, weiblich$avgVolume, mu = 0, conf.level = .95)


## checking influence of hair length on average water volume
# change data type of X03d_longhair to "factor"
data_survey$X03d_longhair <- as.factor(data_survey$X03d_longhair)

# make new data set out of combined_dataset and compute the average Volume for every Houshold with the according hair length
longhair <- combined_dataset %>% group_by(Hh_ID, X03d_longhair) %>% summarise (avgVolume = mean(Volume))

#filter new dataset by long and short hair
no_longhair <- longhair %>% filter (X03d_longhair == "0")
longhair_1 <- longhair%>% filter (X03d_longhair == "1")

# two-sided & two-sample t-test and compare the average Volume of long and short hair on a confidence level of 95 %
t.test(no_longhair$avgVolume, longhair_1$avgVolume, mu = 0, conf.level = .95)


## e) Do participants with an income < 4000 Fr show similar water consumption changes than the ones with an income >= 4000 ##
# Hypotheses
# H0: participants with an income <4000 show similar water consumption changes than the ones with a higher income
# H1: participants with an income <4000 do not show similar water consumption changes than the ones with a higher income

#


combined_dataset_no_baseline <- dplyr::left_join(data_no_baseline, data_survey)

c_d_baseline <- combined_dataset %>% dplyr::group_by(Hh_ID, einkommen) %>% 
  summarise(avgVolume = mean(Volume))
c_d_no_baseline <- combined_dataset_no_baseline %>% group_by(Hh_ID, einkommen) %>% 
  summarise(avgVolume = mean(Volume))

cd_bllower_4000 <- c_d_baseline %>% filter(einkommen %in% c("< 3000 Fr.", "3000 - 3999 Fr."))
cd_blabove_4000 <- c_d_baseline %>% filter(einkommen %in% c("10000 - 11999 Fr.", "12000 - 14999 Fr.", "4000 - 4999 Fr.", "5000 - 5999 Fr.",
                                                          "6000 - 6999 Fr.", "7000 - 7999 Fr.", "8000 - 8999 Fr.", "9000 - 9999 Fr.",
                                                          "Mehr als 15000 Fr."))
na.omit(cd_bllower_4000)
na.omit(cd_blabove_4000)

cd_Iplower4000 <- c_d_no_baseline %>% filter(einkommen %in% c("< 3000 Fr.", "3000 - 3999 Fr."))
cd_Ipabove4000 <- c_d_no_baseline %>% filter(einkommen %in% c("10000 - 11999 Fr.", "12000 - 14999 Fr.", "4000 - 4999 Fr.", "5000 - 5999 Fr.",
                                                           "6000 - 6999 Fr.", "7000 - 7999 Fr.", "8000 - 8999 Fr.", "9000 - 9999 Fr.",
                                                           "Mehr als 15000 Fr."))



comparison_bl_above <- c(cd_blabove_4000$avgVolume - cd_Ipabove4000$avgVolume)

diff_Volume_above <- cd_Ipabove4000
diff_Volume_above$Avarage_BL <- cd_blabove_4000$avgVolume
diff_Volume_above$diff <- c(comparison_bl_above)

# Setting names 
names(diff_Volume_above) <- c("Hh_ID", "einkommen", "Avarage_volume_IP", "Avarage_volume_BL", "diff_above")


comparison_below <- cd_bllower_4000$avgVolume - cd_Iplower4000$avgVolume


diff_volume_Below <- cd_bllower_4000
diff_volume_Below$Avarage_IP <- cd_Iplower4000$avgVolume 
diff_volume_Below$diff <- c(comparison_below)

# Setting names
names(diff_volume_Below) <- c("Hh_ID", "einkommen", "Avarage_volume_BL", "Avarage_volume_IP", "diff_below")


t.test(diff_volume_Below$diff_below, diff_Volume_above$diff_above, mu = 0, conf.level = .95)

## f) What is the preferred strategy for saving the most energy ? ##

data_with_baseline <- data %>%dplyr::filter(Shower <= 10)
data_no_baseline <- data %>%dplyr::filter(Shower > 10)

#data_control_IP_no <- data_no_baseline %>% filter(group <= 2)

data_feedback_group <- data %>% filter(group > 2) %>% group_by(Hh_ID, Shower <= 10) %>%
  summarise(avgShower = mean(Showertime),
            avgTemp = mean(Avgtemperature),
            avgFlowrate =mean(Flowrate),
            avgBreaktime = mean(Breaktime))
data_feedback_group

data_feedback_group_1 <- data_feedback_group %>% filter(`Shower <= 10`== FALSE) 
data_feedback_group_2 <- data_feedback_group %>% filter(`Shower <= 10` == TRUE)

# combine datasets

b <- right_join(data_feedback_group_1, data_feedback_group_2, by = "Hh_ID")

#get the differences of the phases

attach(b)
diff_showertime <- avgShower.x - avgShower.y
diff_Breaktime <- avgBreaktime.x - avgBreaktime.y
diff_flowrate <- avgFlowrate.x - avgFlowrate.y
diff_temperature <- avgTemp.x - avgTemp.y
detach(b)

#get the differences of the phases

data_feedback_group$diff_showertime <- diff_showertime
data_feedback_group$diff_Breaktime <- diff_Breaktime
data_feedback_group$diff_flowrate <- diff_flowrate
data_feedback_group$diff_temperature <- diff_temperature


avgShower_boolean <- case_when(data_feedback_group[,7] < 0 ~ TRUE,
                              data_feedback_group[,7] >= 0 ~ FALSE)
avgBreaktime_boolean <- case_when(data_feedback_group[,8] < 0 ~ FALSE,
                              data_feedback_group[,8] >= 0 ~ TRUE)
avgFlowrate_boolean <- case_when(data_feedback_group[,9] < 0 ~ TRUE,
                             data_feedback_group[,9] >= 0 ~ FALSE)
avgTemperature_boolean <- case_when(data_feedback_group[,10] < 0 ~ TRUE,
                             data_feedback_group[,10] >= 0 ~ FALSE)

windows()
layout(matrix(1:4,nrow=2,byrow=TRUE))
barplot(table(avgBreaktime_boolean), col = "orchid3", main = "Avarage Breaktime")
barplot(table(avgTemperature_boolean), col = "cornflowerblue", main = "Avarage Temperature")
barplot(table(avgFlowrate_boolean), col = "tomato", main = "Avarage Flowrate")
barplot(table(avgShower_boolean), col = "forestgreen", main = "Avarage Showertime")
?barplot
?col

#split_feedback_baseline <- data_feedback_group %>% filter(Shower <= 10)
#split_feedback_intervention <- data_feedback_group %>% filter(Shower > 10)







#diff_flowRate <- data_with_baseline$Flowrate - data_no_baseline$Flowrate
#diff_temperature <- data_with_baseline$Avgtemperature - data_no_baseline$Avgtemperature
#diff_breaktime <- data_with_baseline$Breaktime - data_no_baseline$Breaktime


