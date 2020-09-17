library(tidyverse)
drivers <- read_csv(file = "https://raw.githubusercontent.com/kfcaby/MA376AY21/master/data/DriverBrakingTimes.csv")

drivers = drivers %>% 
  mutate(difference = Facebook - Instagram)

t.test(drivers$difference) 

#convert from wide to long
drivers_long <- drivers %>% 
  select(-difference) %>% 
  pivot_longer(cols = -participant,
               names_to = "media",
               values_to = "time")

# convert to factor variables
drivers_long <- drivers_long %>% 
  mutate(participant = factor(participant),
         media = factor(media))

# change how the factors are encoded
contrasts(drivers_long$media) = "contr.sum"
contrasts(drivers_long$participant) = "contr.sum"

#fit the linear model
model <- drivers_long %>% 
  lm(time ~ media + participant, data = .)

summary(model)

anova(model)

meanTime = mean(drivers_long$time)

drivers_long %>% 
  ggplot(aes(x = participant, y = time, color = media)) +
  geom_point() +
  theme(text = element_text(size = 16)) +
  geom_hline(yintercept = meanTime)




