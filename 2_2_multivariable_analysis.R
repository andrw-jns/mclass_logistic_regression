# MULTIVARIABLE ANALYSIS --------------------------------------------------


# 0 Set up ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(broom)
library(lmtest)

ae_synthetic_Prov <- readRDS('ae_synthetic_Prov.rds')


# 1 Main effects model --------------------------------------------
# variables to inlude in initial main effects model
#   AE_Arrival_Mode
#   Age_Band
#   Sex
#   IMD_Decile_From_LSOA
#   AE_Arrive_HourOfDay
#   der_Num_Diag_Group
#   der_Dist_Group

initial_main_effects_model <- glm(formula = Admitted_Flag ~ AE_Arrival_Mode + Age_Band + Sex + IMD_Decile_From_LSOA +
                       AE_Arrive_HourOfDay + der_Num_Diag_Group + der_Dist_Group,
                     data = ae_synthetic_Prov,
                     family = binomial(link = 'logit'))

tidy(initial_main_effects_model) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue) %>% 
  View()

# let's view the (exponentiated) model coefficients (i.e. the odds ratios)
tidy(initial_main_effects_model) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, oddsRatio) %>% 
  mutate(term = factor(term),
         term = factor(term, levels = rev(levels(term)))) %>% 
  ggplot() +
  geom_point(aes(x = term, y = oddsRatio), colour = '#5881c1') +
  geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  coord_flip()

# Should we exclude any variable from this initial model?
# Look for p > 0.05


# 1.1 Try removing IMD_Decile_From_LSOA -----------------------------------
initial_main_effects_model_upd <- update(initial_main_effects_model, . ~ . - IMD_Decile_From_LSOA )


# Examine p.value of added variable(s)
tidy(initial_main_effects_model_upd) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue) %>% 
  View()


# Compare coefficients of the current and upadted model
tidy(initial_main_effects_model) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, original = oddsRatio) %>% 
  full_join(tidy(initial_main_effects_model_upd) %>% 
              mutate(oddsRatio = exp(estimate),
                     pValue = round(p.value, digits = 4)) %>% 
              select(term, updated = oddsRatio),
            by = 'term') %>% 
  gather(model, oddsRatio, 3:2) %>% 
  mutate(term = factor(term),
         term = factor(term, levels = rev(levels(term))),
         model = factor(model, levels = c('original', 'updated'))) %>% 
  ggplot() +
  geom_point(aes(x = term, y = oddsRatio, colour = model)) +
  geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  scale_colour_manual(values = c('#f9bf07', '#5881c1')) +
  coord_flip()

# compare model fits
lrtest(initial_main_effects_model, initial_main_effects_model_upd)
# close call, but probably worth keeping this variable


# 1.2 Try adding in previously excluded variable -------------------------
# Now let's test whther inclusing any of the variables that we excluded 
#  in the univariable analysis add value to this initial model
# Note that the significance of some variable only become clear when other 
#  variables are included
#   der_Arrive_Quarter
#   der_Arrive_Month
#   der_Arrive_Winter
#   der_Arrive_Weekday
#   der_Arrive_FinYear 

# Start with der_Arrive_Quarter
# Add into model
initial_main_effects_model_upd <- update(initial_main_effects_model, . ~ . + der_Arrive_Quarter )


# Examine p.value of added variable(s)
tidy(initial_main_effects_model_upd) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue) %>% 
  View()


# Compare coefficients of the current and upadted model
tidy(initial_main_effects_model) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, original = oddsRatio) %>% 
  full_join(tidy(initial_main_effects_model_upd) %>% 
               mutate(oddsRatio = exp(estimate),
                      pValue = round(p.value, digits = 4)) %>% 
               select(term, updated = oddsRatio),
             by = 'term') %>% 
  gather(model, oddsRatio, 3:2) %>% 
  mutate(term = factor(term),
         term = factor(term, levels = rev(levels(term))),
         model = factor(model, levels = c('original', 'updated'))) %>% 
  ggplot() +
  geom_point(aes(x = term, y = oddsRatio, colour = model)) +
  geom_hline(aes(yintercept = 1)) +
  scale_y_log10() +
  scale_colour_manual(values = c('#f9bf07', '#5881c1')) +
  coord_flip()

# compare model fits
lrtest(initial_main_effects_model, initial_main_effects_model_upd)

## repeat for the other variables


# 1.3 EXERCISE - repeat for other variables -------------------------------
#   der_Arrive_Month
#   der_Arrive_Winter
#   der_Arrive_Weekday
#   der_Arrive_FinYear 


#  This is our main effects model
main_effects_model <- initial_main_effects_model


# 2 Preliminary final model -----------------------------------------------
# Next we need to consider interaction between our predictor variables
#   AE_Arrival_Mode
#   Age_Band
#   Sex
#   IMD_Decile_From_LSOA
#   AE_Arrive_HourOfDay
#   der_Num_Diag_Group
#   der_Dist_Group

# (7 choose 2 = ) 21 potential 2-way interactions between these variables
# Which have some plausibility?

# Consider the following
#   AE_Arrival_Mode:Age_Band
#   Age_Band:Sex
#   AE_Arrival_Mode:AE_Arrive_HourOfDay
#   IMD_Decile_From_LSOA:der_Dist_Group



# 2.1 Try AE_Arrival_Mode:Age_Band ----------------------------------------
main_effects_model_upd <- update(main_effects_model, . ~ . + IMD_Decile_From_LSOA:der_Dist_Group)

# Examine coefficient and p.value of added variable(s)
# Look for p.value of interaction terms < 0.05
tidy(main_effects_model_upd) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue) %>% 
  View()


# Compare model fits
lrtest(main_effects_model, main_effects_model_upd)


# 2.2 EXERCISE Repeat for other plausible interaction terms ---------------
#   Age_Band:Sex
#   AE_Arrival_Mode:AE_Arrive_HourOfDay
#   IMD_Decile_From_LSOA:der_Dist_Group)



# Add all accepted interaction terms together and look again at p.values
preliminary_final_model <- update(main_effects_model, . ~ . + 
                                    AE_Arrival_Mode:Age_Band +
                                    Age_Band:Sex +
                                    AE_Arrival_Mode:AE_Arrive_HourOfDay)

tidy(preliminary_final_model) %>% 
  mutate(oddsRatio = exp(estimate),
         pValue = round(p.value, digits = 4)) %>% 
  select(term, coefficient = estimate, oddsRatio, pValue) %>% 
  View()


# Compare model fits
lrtest(main_effects_model, preliminary_final_model)


# 3 Save our preliminary final model --------------------------------------
save(preliminary_final_model, file = 'preliminary_final_model.RDA')
