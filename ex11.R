#Loading in libraries and data
library(tidyverse)
library(skimr)
library(naniar)
library(cowplot)
library(ggplot2)
library(MASS)
library(MuMIn)

f <- "https://raw.githubusercontent.com/difiore/ada-datasets/main/Mammal_lifehistories_v2.txt"
d <- read_tsv(f)
skim(d) #Categorical data: order, family, Genus, species
#Numeric data: mass(g), gestation(mo), newborn(g), weaning(mo), wean mass(g), AFR(mo), max.life(mo),
#litter size, litters/year, refs

#Step 1, replace -999 with NA
d <- d |>
  replace_with_na_all(condition = ~.x == -999)

#Step 2: Drop litter size and refs
names(d)
d <- d |>
  dplyr::select(-c(`litter size`, `refs`))

#Step 3, log transform all numeric variables
d <- d |>
  mutate(`mass(g)` = log(`mass(g)`), `gestation(mo)` = log(`gestation(mo)`), 
         `newborn(g)` = log(`newborn(g)`), `weaning(mo)` = log(`weaning(mo)`),
         `wean mass(g)` = log(`wean mass(g)`), `AFR(mo)` = log(`AFR(mo)`),
         `max. life(mo)` = log(`max. life(mo)`), `litters/year` = log(`litters/year`))

#Step 4, Regress values to get relative values
#Age values
relGest <- lm(data = d, `gestation(mo)` ~ `mass(g)`, na.action = na.exclude) #Gestation
relWean <- lm(data = d, `weaning(mo)` ~ `mass(g)`, na.action = na.exclude) #weaning
relAFR <- lm(data = d, `AFR(mo)` ~ `mass(g)`, na.action = na.exclude) #AFR
relLife <- lm(data = d, `max. life(mo)` ~ `mass(g)`, na.action = na.exclude) #Max life

#Mass
relNewbornMass <- lm(data = d, `newborn(g)` ~ `mass(g)`, na.action = na.exclude) #Newborn mass
relWeaningMass <- lm(data = d, `wean mass(g)` ~ `mass(g)`, na.action = na.exclude) #Weaning mass

d <- d |>
  mutate(relGest = resid(relGest), relWean = resid(relWean), relAFR = resid(relAFR), 
         relLife = resid(relLife), relNewbornMass = resid(relNewbornMass), relWeaningMass = resid(relWeaningMass))

#Step 5, plotting residules
lifeOrder <- ggplot(data = d |> drop_na(relLife), mapping = aes(x = order, y = relLife)) +
  geom_boxplot() +
  ggtitle("Order and Lifespan") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

newmassOrder <- ggplot(data = d |> drop_na(relNewbornMass), mapping = aes(x = order, y = relNewbornMass)) +
  geom_boxplot() +
  ggtitle("Order and Newborn Mass(g)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

weanmassOrder <- ggplot(data = d |> drop_na(relWeaningMass), mapping = aes(x = order, y = relWeaningMass)) +
  geom_boxplot() +
  ggtitle("Order and Weaning Mass") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(lifeOrder, newmassOrder, weanmassOrder)

#Step 6, model selection
d <- d |>
  drop_na(`max. life(mo)`, `AFR(mo)`, `gestation(mo)`, `newborn(g)`, `weaning(mo)`, `wean mass(g)`, `mass(g)`, `litters/year`)

#Max life(mo) models
lifeFull <- lm(data = d, `max. life(mo)` ~ `gestation(mo)` + `newborn(g)` + 
                 `weaning(mo)` + `wean mass(g)` + `litters/year` + `mass(g)`, na.action = na.fail)
summary(lifeFull)

lifeTest <- stepAIC(lifeFull, scope = .~., direction = "both")

lfMods <- dredge(lifeFull)
lfModsAvg <- summary(model.avg(lfMods, subset = delta <= 4, fit = TRUE))
plot(lfModsAvg)
lfCI <- confint(lfModsAvg)

#AFR(mo) models
AFRFull <- lm(data = d, `AFR(mo)` ~ `gestation(mo)` + `newborn(g)` + 
                 `weaning(mo)` + `wean mass(g)` + `litters/year` + `mass(g)`, na.action = na.fail)
summary(AFRFull)

AFRTest <- stepAIC(AFRFull, scope = .~., direction = "both")

AFRmods <- dredge(AFRFull)
AFRavg <- summary(model.avg(AFRmods, subset = delta <= 4, fit = TRUE))
plot(AFRavg)
