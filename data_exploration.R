library(readr)
insurance <- read_csv("insurance.csv")
View(insurance)

library(ggplot2)
library(dplyr)

hist(insurance$charges)

ggplot(insurance, aes(x = insurance$age, y = insurance$charges, colour = smoker)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)


cor(insurance$age, insurance$charges)


ggplot(insurance, aes(x = charges)) +
  geom_histogram() +
  facet_wrap(~smoker)

insurance <- insurance %>%
  mutate(
    overweight = bmi > 25
  )

smokers <- insurance %>% filter(smoker == "yes")
nonsmokers <- insurance %>% filter(smoker == "no")

ggplot(smokers, aes(x = smokers$age, y = smokers$charges, colour = overweight)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(nonsmokers, aes(x = nonsmokers$age, y = nonsmokers$charges, colour = overweight)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)



mdl_charges <- lm(charges ~ age, data = insurance)

mdl_charges_smoker <- lm(charges ~ age + smoker, data = insurance)

mdl_charges_full <- lm(charges ~ age + smoker + overweight, data = insurance)


library(broom)

glance(mdl_charges_full)


install.packages("ggfortify")
library(ggfortify)

autoplot(mdl_charges_full, which=2)



mdl_charges_full


