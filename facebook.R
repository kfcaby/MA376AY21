library(tidyverse)

# Note the file on the ISI website has some issues
braking <- read_csv(file = "data/DriverBrakingTimes.csv")

#convert to long format
braking_long = braking %>% 
  pivot_longer(cols = -participant,
               names_to = "media",
               values_to = "time")

# convert participant to factor
braking_long = braking_long %>% 
  mutate(participant = factor(participant),
         media = factor(media))

# model using indicator encoding
braking_long %>% 
  lm(time ~ participant + media, data = .) %>% 
  summary()



#model using effect encoding
contrasts(braking_long$participant) = contr.sum
contrasts(braking_long$media) = contr.sum

contrasts(braking_long$media)

braking_long %>% 
  lm(time ~ participant + media, data = .) %>% 
  summary()

braking_long %>% 
  group_by(media) %>% 
  summarise(mean(time))

mean(braking_long$time)
