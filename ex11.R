#Loading in libraries and data
library(tidyverse)
library(skimr)
library(naniar)
library(cowplot)
library(ggplot2)
library(MASS)

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
  select(order, family, Genus, species, `mass(g)`, `gestation(mo)`, `newborn(g)`,
         `weaning(mo)`, `wean mass(g)`, `AFR(mo)`, `max. life(mo)`, `litters/year`)

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

#Step 5, plotting residules
lifeOrder <- ggplot(data = d, mapping = aes(x = Order, y = relLife)) +
  geom_boxplot() +
  ggtitle("Order and Lifespan")

newmassOrder <- ggplot(data = d, mapping = aes(x = Order, y = relNewbornMass)) +
  geom_boxplot() +
  ggtitle("Order and Newborn Mass(g)")

weanmassOrder <- ggplot(data = d, mapping = aes(x = Order, y = relWeaningMass)) +
  geom_boxplot() +
  ggtitle("Order and Weaning Mass")

plot_grid(lifeOrder, newmassOrder, weanmassOrder)

#Step 6, model selection

#Max life(mo) models
lifeFull <- lm(data = d, `max. life(mo)` ~ `gestation(mo)` + `newborn(g)` + 
                 `weaning(mo)` + `wean mass(g)` + `litters/year` + `mass(g)`)
summary(lifeFull)

lifeTest <- stepAIC(lifeFull, scope = .~., direction = "both")

#AFR(mo) models
AFRFull <- lm(data = d, `AFR(mo)` ~ `gestation(mo)` + `newborn(g)` + 
                 `weaning(mo)` + `wean mass(g)` + `litters/year` + `mass(g)`)
summary(AFRFull)

AFRTest <- stepAIC(AFRFull, scope = .~., direction = "both")




