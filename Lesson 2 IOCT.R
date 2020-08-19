
library(tidyverse)

ioct <- read_csv(file = "https://raw.githubusercontent.com/kfcaby/MA376AY21/master/data/IOCT_Data_Cleaned.csv")

# Do corps squad athletes do better on the IOCT?
ioct <- ioct %>% 
  filter(IOCT_Time < 400)

ioct %>% 
  ggplot(aes(color = factor(CS), y = IOCT_Time, x = sex)) +
  geom_boxplot()

# On average, non-CS did a little bit better

#single-mean model
ioct %>% 
  summarise(meanIOCT = mean(IOCT_Time))

# add a column of residuals
ioct = ioct %>% 
  mutate(single_mean = mean(IOCT_Time)) %>% 
  mutate(single_mean_residual = IOCT_Time - single_mean)

# standard error
ioct %>% 
  summarize(single_mean_se = sd(single_mean_residual))

## separate means model
ioct %>% 
  group_by(CS) %>% 
  summarise(meanIOCT = mean(IOCT_Time))

# add a column of residuals
ioct = ioct %>% 
  group_by(CS) %>% 
  mutate(CS_mean = mean(IOCT_Time)) %>% 
  mutate(CS_mean_residual = IOCT_Time - CS_mean)

# standard error
# residuals squared divided by the number of degrees of freedom
# the degrees of freedom is the number of data points minus
# the number of parameters in the model estimated 
# two parameters = a mean for CS and a mean for non-CS

ioct %>%
  group_by() %>% 
  summarise(CS_mean_se = sqrt(sum(CS_mean_residual^2)/(378)))




  