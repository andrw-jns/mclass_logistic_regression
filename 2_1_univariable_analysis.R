# UNIVARIABLE ANALYSIS ----------------------------------------------------



# 0 Set up ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(tsibble)
library(broom)

ae_synthetic_Prov <- readRDS('ae_synthetic_ProvID15318.rds')



# 1 Initial exploratory analysis ------------------------------------------

# lets view the top 20 rows
ae_synthetic_Prov %>% 
  head(n = 20) %>% 
  View()


# how many attendances are admitted?
ae_synthetic_Prov %>% 
  group_by(Admitted_Flag) %>% 
  summarise(attendances = n(),
            p_attendances = n() / nrow(ae_synthetic_Prov))


# how does the number of attendances change over time
ae_synthetic_Prov %>% 
  count(ym = yearmonth(AE_Arrive_Date), name = "attendances") %>%
  ggplot() +  
  geom_line(aes(x = ym, y = attendances)) +
  expand_limits(y = 0) +
  scale_x_date(breaks = as_date(c('2015-04-01', '2015-10-01', '2016-04-01', '2016-10-01')),
               date_labels = "%b %Y")


# how does the proportion of attendances that are admitted change over time
ae_synthetic_Prov %>% 
  group_by(ym = yearmonth(AE_Arrive_Date)) %>% 
  summarise(pAdmitted= sum(ifelse(Admitted_Flag == 1, 1, 0))/n()) %>%
  ggplot() +  
  geom_line(aes(x = ym, y = pAdmitted)) +
  expand_limits(y = 0) +
  scale_x_date(breaks = as_date(c('2015-04-01', '2015-10-01', '2016-04-01', '2016-10-01')),
               date_labels = "%b %Y")


# 2 Consider the potential relevance of each variable ---------------------
# What are the variables in the dataset that we might use to predict admission?  
# Which are these are candidates?
#  IMD_Decile_From_LSOA
#  Age_Band
#  Sex
#  AE_Arrive_Date
#  AE_Arrive_HourOfDay
#  AE_Time_Mins
#  AE_HRG
#  AE_Num_Diagnoses
#  AE_Num_Investigations
#  AE_Num_Treatments
#  AE_Arrival_Mode
#  Provider_Patient_Distance_Miles
#  Prov_ID
#  Admitted_Flag
#  Admission_Method
#  ICD10_Chapter_Code
#  Treatment_Function_Code
#  Length_Of_Stay_Days


# Let's engineer some variables that might be of interest from AE_Arrive_Date
ae_synthetic_Prov  <- 
  ae_synthetic_Prov %>% 
  mutate(der_Arrive_FinYear = factor(ifelse(month(AE_Arrive_Date) < 4, year(AE_Arrive_Date) - 1, year(AE_Arrive_Date))),
         der_Arrive_Quarter = factor(quarter(AE_Arrive_Date, with_year = FALSE)),
         der_Arrive_Month = factor(month(AE_Arrive_Date)),
         der_Arrive_Winter = factor(ifelse(month(AE_Arrive_Date) %in% c(11, 12, 1, 2), 1, 0)),
         der_Arrive_Weekday = factor(wday(AE_Arrive_Date, week_start = 1)))



# 3 Test each candidate variable in turn ----------------------------------
#  To what extent are the variables we selected related to the outcome (admitted y/n)
#  This will help us decide whether to include these variables in our preliminary model
#  Use p <= 0.25 as means of screening in potentially useful variables


# 3.1 AE_Arrival_Mode ------------------------------------------
#  Create table showing how the proprtion of patients admitted varies with AE_Arrival_Mode
ae_synthetic_Prov %>% 
  group_by(AE_Arrival_Mode, Admitted_Flag) %>% 
  summarise(attendances = n()) %>% 
  spread(Admitted_Flag, attendances) %>% 
  rename(admitted = `1`, not_admitted = `0`) %>% 
  mutate(pAdmitted = admitted / (admitted + not_admitted))

mod_AE_Arrival_Mode <- glm(formula = Admitted_Flag ~ AE_Arrival_Mode,
                           data = ae_synthetic_Prov,
                           family = binomial(link = 'logit'))

tidy(mod_AE_Arrival_Mode) %>% 
  mutate(oddsRatio = exp(estimate)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue = p.value)
# How should interpret these results?

predict(mod_AE_Arrival_Mode, type = 'response', newdata = data.frame(AE_Arrival_Mode = 'Ambulance'))
predict(mod_AE_Arrival_Mode, type = 'response', newdata = data.frame(AE_Arrival_Mode = 'Walkin'))

# We could derive these from the model odds ratios as follows
#  Probability of ambulance case being admitted  = 0.751 / (1 + 0.751) = 0.4289
#  Probability of walkin case being admitted = 0.751*0.154 / (1 + 0.751*0.154) = 0.1036


# 3.2 IMD_Decile_From_LSOA ------------------------------------------------
ae_synthetic_Prov %>% 
  group_by(IMD_Decile_From_LSOA, Admitted_Flag) %>% 
  summarise(attendances = n()) %>% 
  spread(Admitted_Flag, attendances) %>% 
  rename(admitted = `1`, not_admitted = `0`) %>% 
  mutate(pAdmitted = admitted / (admitted + not_admitted))

ae_synthetic_Prov %>% 
  group_by(IMD_Decile_From_LSOA, Admitted_Flag) %>% 
  summarise(attendances = n()) %>% 
  spread(Admitted_Flag, attendances) %>% 
  rename(admitted = `1`, not_admitted = `0`) %>% 
  mutate(pAdmitted = admitted / (admitted + not_admitted),
         attendances = admitted + not_admitted) %>% 
  ggplot() +
  geom_point(aes(x = IMD_Decile_From_LSOA, y = pAdmitted, size = attendances), colour = '#5881c1') +
  geom_smooth(aes(x = IMD_Decile_From_LSOA, y = pAdmitted), method = 'loess', colour = '#f9bf07') +
  scale_x_continuous(breaks = 1:10)

mod_IMD_Decile_From_LSOA <- glm(formula = Admitted_Flag ~ IMD_Decile_From_LSOA,
                                           data = ae_synthetic_Prov,
                                           family = binomial(link = 'logit'))

tidy(mod_IMD_Decile_From_LSOA) %>% 
  mutate(oddsRatio = exp(estimate)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue = p.value)
# How should interpret these results?
# Discuss altetrnative forms: categorical / scale and centred etc.

predict(mod_IMD_Decile_From_LSOA, type = 'response', newdata = data.frame(IMD_Decile_From_LSOA = 1))
predict(mod_IMD_Decile_From_LSOA, type = 'response', newdata = data.frame(IMD_Decile_From_LSOA = 10))



# 3.3 Provider_Patient_Distance_Miles -------------------------------------
ae_synthetic_Prov %>% 
  group_by(Provider_Patient_Distance_Miles, Admitted_Flag) %>% 
  summarise(attendances = n()) %>% 
  spread(Admitted_Flag, attendances) %>% 
  rename(admitted = `1`, not_admitted = `0`) %>% 
  mutate(pAdmitted = admitted / (admitted + not_admitted)) %>% 
  View()

ae_synthetic_Prov %>% 
  group_by(Provider_Patient_Distance_Miles, Admitted_Flag) %>% 
  summarise(attendances = n()) %>% 
  spread(Admitted_Flag, attendances) %>% 
  rename(admitted = `1`, not_admitted = `0`) %>% 
  mutate(pAdmitted = admitted / (admitted + not_admitted),
         attendances = admitted + not_admitted) %>% 
  ggplot() +
  geom_point(aes(x = Provider_Patient_Distance_Miles, y = pAdmitted, size = attendances), colour = '#5881c1') +
  geom_smooth(aes(x = Provider_Patient_Distance_Miles, y = pAdmitted), method = 'loess', colour = '#f9bf07') +
  scale_x_continuous(limits = c(0, 20))

mod_Provider_Patient_Distance_Miles <- glm(formula = Admitted_Flag ~ Provider_Patient_Distance_Miles,
                           data = ae_synthetic_Prov,
                           family = binomial(link = 'logit'))

tidy(mod_Provider_Patient_Distance_Miles) %>% 
  mutate(oddsRatio = exp(estimate)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue = p.value)
# How should interpret these results?

# Let's group mod_Provider_Patient_Distance_Miles into 4 groups: 0, 1, 2, 3-10, 11+
ae_synthetic_Prov <-
  ae_synthetic_Prov %>% 
  mutate(der_Dist_Group = factor(case_when(Provider_Patient_Distance_Miles == 0 ~ '0',
                                           Provider_Patient_Distance_Miles == 1 ~ '1',
                                           Provider_Patient_Distance_Miles == 2 ~ '2',
                                           Provider_Patient_Distance_Miles >= 3 & 
                                             Provider_Patient_Distance_Miles <= 10 ~ '3-10',
                                           TRUE ~ '11+')))
# Discuss alternatives to grouping; splines, polynomials, GAMs

mod_der_Dist_Group <- glm(formula = Admitted_Flag ~ der_Dist_Group,
                                           data = ae_synthetic_Prov,
                                           family = binomial(link = 'logit'))


tidy(mod_der_Dist_Group) %>% 
  mutate(oddsRatio = exp(estimate)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue = p.value)


predict(mod_der_Dist_Group, type = 'response', newdata = data.frame(der_Dist_Group = '0'))
predict(mod_der_Dist_Group, type = 'response', newdata = data.frame(der_Dist_Group = '3-10'))



# 4 EXERCISE - Repeat process for remaining variables --------------------------------
# Sex
# Age_Band
# AE_Arrive_HourOfDay
# AE_Num_Diagnoses
# der_Arrive_FinYear
# der_Arrive_Quarter
# der_Arrive_Month
# der_Arrive_Winter
# der_Arrive_Weekday
  
ae_synthetic_Prov %>% 
  group_by(XXXXXXXXXXXXXXXXXX, Admitted_Flag) %>% 
  summarise(attendances = n()) %>% 
  spread(Admitted_Flag, attendances) %>% 
  rename(admitted = `1`, not_admitted = `0`) %>% 
  mutate(pAdmitted = admitted / (admitted + not_admitted),
         attendances = admitted + not_admitted) %>% 
  ggplot() +
  geom_point(aes(x = XXXXXXXXXXXXXXXXXX, y = pAdmitted, size = attendances), colour = '#5881c1') +
  geom_smooth(aes(x = XXXXXXXXXXXXXXXXXX, y = pAdmitted), method = 'loess', colour = '#f9bf07') 

mod <- glm(formula = Admitted_Flag ~ XXXXXXXXXXXXXXXXXX,
           data = ae_synthetic_Prov,
           family = binomial(link = 'logit'))

tidy(mod) %>% 
  mutate(oddsRatio = exp(estimate)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue = p.value)  

ae_synthetic_Prov <-
  ae_synthetic_Prov %>% 
  mutate(der_Num_Diag_Group = factor(AE_Num_Diagnoses))  



# 5 Save updated dataframe ------------------------------------------------
saveRDS(ae_synthetic_Prov, 'ae_synthetic_Prov.rds')