# MODEL VALIDATION --------------------------------------------------------

# 0 Set up ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(scales)
library(broom)
library(lmtest)
library(PredictABEL)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(ModelMetrics)


memory.limit(size = 1500000)

ae_synthetic_Prov <- readRDS('ae_synthetic_ProvID15318.rds')

load(file = 'preliminary_final_model.RDA')


# 1 Predict admission and explore --------------------------
ae_synthetic_Prov <- 
  ae_synthetic_Prov %>% 
  mutate(predAdm = predict(preliminary_final_model, type = 'response'))

#  Lets look at the top 20 rows
ae_synthetic_Prov %>% 
  head(n = 20) %>% 
  select(Admitted_Flag, predAdm, everything()) %>% 
  View()

# Lets look at the distribution of predictions
ae_synthetic_Prov %>% 
  ggplot() +
  geom_freqpoly(aes(x = predAdm), bins = 50, colour = '#5881c1') +
  scale_x_continuous(name = 'Modelled probability of admission',
                     limits = c(0, 1)) +
  scale_y_continuous(name = 'count of attendances',
                     labels = comma_format())


# Lets look at the distribution of predictions for admitted and non-admitted attendances
ae_synthetic_Prov %>% 
  mutate(admittedyn = ifelse(Admitted_Flag == 0, 'not admitted', 'admitted')) %>% 
  ggplot() +
  geom_freqpoly(aes(x = predAdm, colour = admittedyn), bins = 50) +
  scale_x_continuous(name = 'Modelled probability of admission',
                     limits = c(0, 1)) +
  scale_y_continuous(name = 'count of attendances',
                     labels = comma_format()) +
  scale_colour_manual(values = c('#f9bf07', '#5881c1')) +
  facet_wrap(~ admittedyn) +
  theme(legend.position = 'none')


# 2 Validation ------------------------------------------------------------

# 2.1 Calibration plots ---------------------------------------------------
# How good are the predictions for low, medium and high risk (af admission) attendances?
plotCalibration(data = ae_synthetic_Prov, 
                cOutcome = 14, 
                predRisk = ae_synthetic_Prov$predAdm, 
                groups = 10, 
                plottitle = 'Calibration Plot')$Chi_square



# 2.2 Hosmer-Lemeshow test of goodness of fit -----------------------------
hoslem.test(x = ae_synthetic_Prov$Admitted_Flag, y = ae_synthetic_Prov$predAdm, g=10)
# Null hypothese - the model fits the data
# If p > 0.05 do not reject the null hypothesis



# 2.3 Confusion matrix  ---------------------------------------------------
# For a given cut point, how does the classified predictions (af admission) compare with actuals
cutpoint <- 0.5

ae_synthetic_Prov %>% 
  mutate(observed = ifelse(Admitted_Flag == 0, 'not admitted', 'admitted'),
         modelled = ifelse(predAdm < cutpoint, 'modelled not admitted', 'modelled admitted')) %>% 
  group_by(observed, modelled) %>% 
  summarise(attendances = n()) %>% 
  select(observed, modelled, attendances) %>% 
  spread(modelled, attendances)


# 2.4 Performance metrics for given cut-point -----------------------------
cm <- ae_synthetic_Prov %>% 
  mutate(observed = ifelse(Admitted_Flag == 0, 'not admitted', 'admitted'),
         modelled = ifelse(predAdm < cutpoint, 'modelled not admitted', 'modelled admitted')) %>% 
  group_by(observed, modelled) %>% 
  summarise(attendances = n()) %>% 
  ungroup()

truePos <- cm %>% 
  filter(observed == 'admitted', modelled == 'modelled admitted') %>% 
  select(attendances) %>% 
  unname() %>% unlist()

trueNeg <- cm %>% 
  filter(observed == 'not admitted', modelled == 'modelled not admitted') %>% 
  select(attendances) %>% 
  unname() %>% unlist()

falsePos <- cm %>% 
  filter(observed == 'not admitted', modelled == 'modelled admitted') %>% 
  select(attendances) %>% 
  unname() %>% unlist()

falseNeg <- cm %>% 
  filter(observed == 'admitted', modelled == 'modelled not admitted') %>% 
  select(attendances) %>% 
  unname() %>% unlist()

# sensitivity (probability of predicting admission when patient not admitted)
# also called 'recall'
sensitivity <- truePos / (truePos + falseNeg)

# speficity (probability of predicting no admission when patient not admitted)
specificity <- trueNeg / (trueNeg + falsePos)

# PPV positive predictive value (probability that the patient will be admitted when model suggests they will be admitted) 
# also called 'Precision'
PPV <- truePos / (truePos + falsePos)

# NPV positive predictive value (probability that the patient will not be admitted when model suggests they will not be admitted) 
NPV <- trueNeg / (trueNeg + falseNeg)



# 2.5 EXERCISE Produce confusion matrix and performance metrics for other cutpoints --------
# Try cutpoints 0.3, 0.4, 0.6, 0.7


# 2.6 Plot sensitivity and Specificity for all cutpoints ------------------
pred <- prediction(predictions = ae_synthetic_Prov$predAdm, labels = ae_synthetic_Prov$Admitted_Flag)
senspec <- performance(pred, "spec", "sens")

data.frame(cutpoint = senspec@alpha.values[[1]],
           specificity = senspec@y.values[[1]],
           sensitivity = senspec@x.values[[1]]) %>% 
  gather(sens_spec, value, 2:3) %>% 
  ggplot() +
  geom_line(aes(x = cutpoint, y = value, colour = sens_spec)) +
  scale_x_continuous(name= "% cut point", labels = percent_format(accuracy = 1)) + 
  scale_y_continuous(name="Sensitivity/Specificity") +
  scale_colour_manual(values = c('#f9bf07', '#5881c1')) +
  theme(legend.title = element_blank())
  

# 2.7 ROC (receiver operating characteristic) curve and C-statistic -----------
modelRoc <- roc(data = ae_synthetic_Prov, response = Admitted_Flag, predictor = predAdm)
plot.roc(modelRoc)

## Area under the (ROC) curve - sometimes called the C-statistic or concordance statistic
## The probability that a randomly selected admitted attendance will have a higher modelled probability of admission 
##  than a randomly selected non-admitted attendance
pROC::auc(modelRoc)


# 2.8 Brier score ---------------------------------------------------------
## The mean squared difference between the modelled probability of admission and the outcome (admitted 1/0)
##  the lower the Brier score is for a set of predictions, the better the predictions are calibrated
brier(preliminary_final_model)

## alternative way to call the same function
brier(actual = ae_synthetic_Prov$Admitted_Flag, predicted = ae_synthetic_Prov$predAdm)


# 3 Save final model ------------------------------------------------------
# Given staisfactory assessment against these chacks, this becomes our final model
final_model <- preliminary_final_model

save(final_model, file = 'final_model.RDA')
