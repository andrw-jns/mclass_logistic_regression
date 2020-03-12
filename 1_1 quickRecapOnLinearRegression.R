# QUICK RECAP ON LINEAR REGRESSION ----------------------------------------

# 0 Set up ----------------------------------------------------------------
library(tidyverse)
library(broom)


# 1 Read the dataset ------------------------------------------------------
toyExampleDf <- readRDS(file = 'toyExampleDf.RDS')


# 2 Review the dataset ----------------------------------------------------
View(toyExampleDf)

# 100 records - each record represents a child
# 4 variables
#    age (continuous; years 5 to 12)
#    sex (binary; 0 = male, 1 = female)
#    place (categorical; A, B, C or D)
#    height(continuous; in cm)

# We want to know how height varies (in our sample) with age, sex and place

# Visualise the distribution of heights in our sample
toyExampleDf %>% 
  ggplot() +
  geom_histogram(aes(height), binwidth = 10, fill = '#5881c1') +
  geom_vline(aes(xintercept = mean(height, na.rm = TRUE)), colour = '#f9bf07', size = 1)

# Calculate some summary statistics
toyExampleDf %>% 
  summarise(minHeight = min(height, na.rm = TRUE),
            maxHeight = max(height, na.rm = TRUE),
            meanHeight = mean(height, na.rm = TRUE),
            sdHeight = sd(height, na.rm = TRUE))


# 3 Explore relationships -------------------------------------------------


# 3.1 How does height vary with age? --------------------------------------
toyExampleDf %>% 
  ggplot() +
  geom_point(aes(x = age, y = height), colour = '#5881c1')

# are age and height correlated?
cor(toyExampleDf$height, toyExampleDf$age, method = 'pearson')
cor(toyExampleDf$height, toyExampleDf$age, method = 'pearson')**2
# how do we interpret these values?

# If we knew a child's age, what would be our best guess at their height?
# What do we mean by 'best guess'?
# Lots of ways of interpretting 'best guess'
# For now lets take 'best guess' to mean that its a prediction that minimises the square of our errors accross our sample

# Simple linear regression will do this
modAge <- lm(formula = height ~ age, 
             data = toyExampleDf)

#  Lets look at some of the model cofficients
tidy(modAge)
#  How do we interpret the values in this table?

# In this simple model, the regression coefficients can be calculated long hand
regCoefs <- toyExampleDf %>% 
  mutate(y = height,
         x = age) %>% 
  mutate(xdev = x - mean(x, na.rm = TRUE),
         ydev = y - mean(y, na.rm = TRUE)) %>% 
  summarise(b1 = sum(xdev*ydev)/sum(xdev*xdev),
            b0 = mean(y, na.rm = TRUE) - sum(xdev*ydev)/sum(xdev*xdev)*mean(x, na.rm = TRUE))
regCoefs

# Let's show this visually
toyExampleDf %>% 
  ggplot() +
  geom_point(aes(x = age, y = height), colour = '#5881c1') +
  geom_abline(data = regCoefs, aes(intercept = b0, slope = b1))

# Lets predict some heights from children's ages
# Long hand for an 8 year old or an 11 year old
regCoefs$b0 + 8*regCoefs$b1
regCoefs$b0 + 11*regCoefs$b1

# Or using the predict function
predict(modAge, data.frame(age = 8))
predict(modAge, data.frame(age = 11))



# 3.2 How does height vary by sex? ----------------------------------------
toyExampleDf %>% 
  mutate(sexLabel = if_else(sex == 0, 'male', 'female')) %>% 
  ggplot() +
  geom_histogram(aes(height), binwidth = 10, fill = '#5881c1') +
  facet_wrap(~sexLabel, nrow = 2, ncol = 1) +
  geom_vline(data = toyExampleDf %>% 
               mutate(sexLabel = if_else(sex == 0, 'male', 'female')) %>% 
               group_by(sexLabel) %>% 
               summarise(meanHeight = mean(height, na.rm = TRUE)),
             aes(xintercept = meanHeight), colour = '#f9bf07', size = 1) +
  geom_text(data = toyExampleDf %>% 
              mutate(sexLabel = if_else(sex == 0, 'male', 'female')) %>% 
              group_by(sexLabel) %>% 
              summarise(meanHeight = mean(height, na.rm = TRUE)),
            aes(x = meanHeight + 5, y = 10, label = round(meanHeight, digits = 1)))

# Even though sex is a binary variable, we can still use regression
modSex <- lm(formula = height ~ sex, 
                data = toyExampleDf)

tidy(modSex) 
# How do we intepret the model coeficients?

# Let's predict a child's height given their gender
# Long hand - for male then female
129.7 + 0*7.11
129.7 + 1*7.11

# Or using predict
predict(modSex, data.frame(sex = 0))
predict(modSex, data.frame(sex = 1))


# And we can use regression to take account of both age and sex 
modAgeSex <- lm(formula = height ~ age + sex, 
             data = toyExampleDf)

tidy(modAgeSex) 
tidy(modAge)
tidy(modSex)
# What are the model coefficeints different in the three models?
# How do we interpret the coefficients in the AgeSex model?

# How do our predictions change when we use both age and sex?
predict(modAge, data.frame(age = 8, sex = 0))
predict(modAgeSex, data.frame(age = 8, sex = 0))


# 3.3 How do children's heights vary with place? -------------------------
toyExampleDf %>% 
  ggplot() +
  geom_histogram(aes(height), binwidth = 10, fill = '#5881c1') +
  facet_wrap(~place, nrow = 2, ncol = 2) +
  geom_vline(data = toyExampleDf %>% 
               group_by(place) %>% 
               summarise(meanHeight = mean(height, na.rm = TRUE)),
             aes(xintercept = meanHeight), colour = '#f9bf07', size = 1) +
  geom_text(data = toyExampleDf %>% 
              group_by(place) %>% 
              summarise(meanHeight = mean(height, na.rm = TRUE)),
            aes(x = meanHeight + 10, y = 12, label = round(meanHeight, digits = 1)))

# How can we incorporate place, a categorical variable, in our regression model?
# First we convert our categorical variable (with 4 levels) into 3 binary variables
# These are usually called dummy or design variables
toyExampleDf <- toyExampleDf %>% 
  mutate(placeB = ifelse(place == 'B', 1, 0),
         placeC = ifelse(place == 'C', 1, 0),
         placeD = ifelse(place == 'D', 1, 0))

# Then we can include these binary variables in our model
modPlace <- lm(formula = height ~ placeB + placeC + placeD, 
             data = toyExampleDf)

tidy(modPlace)
# How do we interpret the coefficients in the model?

# Fortunately, if we ask r to include a categorical varaible in our model
# Then it will create the design varaibles automatically
modPlace2 <- lm(formula = height ~ place, 
               data = toyExampleDf)

tidy(modPlace2)


# 3.4 All three preictor variables together -------------------------------
# Lets try adding place to our age+sex model
modAgeSexPlace <- lm(formula = height ~ age + sex + place, 
             data = toyExampleDf)

tidy(modAgeSexPlace)
# How to interpret the model coefficients?

# Let's predict some children's heights gievn age, sex and place
# First long hand; for male child aged 8 from place A and then
#  a female child and 11 from place D
73.9 + 8*6.77 + 0*8.93 + 0*5.02 + 0*-12.3 + 0*-6.17
73.9 + 11*6.77 + 1*8.93 + 0*5.02 + 0*-12.3 + 1*-6.17

predict(modAgeSexPlace, data.frame(age = 8, sex = 0, place = 'A'))
predict(modAgeSexPlace, data.frame(age = 11, sex = 1, place = 'D'))

# We can also get prediction intervals around our 'best guess'
predict(modAgeSexPlace, data.frame(age = 8, sex = 0, place = 'A'), interval = 'prediction', level = 0.50)
predict(modAgeSexPlace, data.frame(age = 8, sex = 0, place = 'A'), interval = 'prediction', level = 0.95)


# 4 Recap important messages ----------------------------------------------

# a Linear regression describes the relationship between an outcome variable 
#     (in our case height) and 1 or more predictor variables (e.g. age, sex, place)

# b Predictor varaibles can be continuous, binary or categorical (using design variables)

# c The model coefficients show how on average our outcome variable changes with respect 
#    to a predictor varaible, having controlled for the effects of all other predictor
#    variables in the model