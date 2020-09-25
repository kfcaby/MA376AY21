library(tidyverse)


wages <- read.table(file = "http://www.isi-stats.com/isi2/data/Wages.txt",
                    header = T)

# Is there an association between race and salary

wages %>% 
  group_by(race) %>% 
  summarize(mean = mean(wage),
            sd = sd(wage),
            n = n())

wages = wages %>% 
  mutate(race = factor(race))

contrasts(wages$race) = "contr.sum"

model <- lm(wage ~ race, data = wages)
summary(model)

# is there an association between race and education

wages %>% 
  count(race,educ) %>% 
  group_by(race) %>% 
  mutate(perc = n/sum(n)) %>% 
  pivot_wider(id_cols = race, names_from = educ, values_from = perc)

#So what: there is an association between race and education
# We would like an effect for race that's adjusted for education

#Fit a model that adjusts for education

contrasts(wages$educ) = "contr.sum"
modelAdjusted <-lm(wage ~ race + educ, data = wages)
summary(modelAdjusted)

# -$68 Interpretation: The effect of race on wages after adjusting for education.

# How does this compare to the unadjusted effect. The unadjusted effect was -$87
summary(model)

# The unadjusted effect include both the effect of education and the effect of race.
# The adjusted effect isolate the effect of race.





