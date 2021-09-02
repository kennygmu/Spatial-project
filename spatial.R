#### Library ####
setwd("/Users/kenny/Downloads/PHD degree preparation/GMU/tobacco_distance")
library(tidyverse)
library(corrplot)
library(car)
library(spdep)
library(nlme)
library(tidycensus)
library(viridis)
library(spatialreg)
library(xtable)

library(readxl)
final <- read_excel("spatial_final.xlsx")
View(final)

census_api_key('3f2cc7311e57e38311566ac6b2d44c7853454cae', install = TRUE, overwrite = TRUE)
va <- get_acs(geography = "county", 
              variables = "B01003_001", 
              state = "VA",
              geometry = TRUE) 

data.va <- merge(va,final,by="GEOID")

us <- get_acs(geography = "state", 
              variables = "B01003_001", 
              geometry = TRUE)
View(us[,1:2])
#### Exploratory ####
dev.new(width=10, height=10)
par(mfrow=c(4,4))
###smokers###
ggplot(data.va, aes(fill = `Smokers (%)`, color = `Smokers (%)`)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")+
  labs(title = "Smoking Rate in Virginia")


## # of tobacco outlets ###
ggplot(data.va, aes(fill = Freq, color = Freq))+
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma") +
  labs(title = "Number of Tobacco Retailers in Virginia")

###population###
ggplot(data.va, aes(fill = population, color = population)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "viridis") +
  labs(title = "County Level Population")

###Density###
ggplot(data.va, aes(fill = Density, color = Density)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma") +
  labs(title = "Density ( Number of retailers per 1000 persons)")

ggplot(data.va, aes(fill = `% Hispanic`, color = `% Hispanic`)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma") +
  labs(title = "`% Hispanic`")

coordinates(final) <- ~ INTPTLONG + INTPTLAT 
knea <- knearneigh(coordinates(final), longlat = TRUE)
neib <- knn2nb(knea)
nb.list <- nb2listw(neib)
moran.test(final$`Smokers (%)`,nb.list) # Exists spatial autocorrelation

fit <- lm(final$`Smokers (%)`~ scale(final$Density) , data = final)
summary( fit )
AIC(fit)
###table1###
fitx <- lm(final$`Smokers (%)`~ scale(final$Density) +scale(final$`% Female`)+ scale(final$`% Black`) + scale(final$`% Hispanic`) + scale(final$`% Asian`) + scale(final$`% Non-Hispanic White`) 
           + scale(final$`% Not Proficient in English`) + scale(final$`income_inequalityZ-Score`) + scale(final$`some college_Z-Score`) + scale(final$`unemployment_Z-Score`) + scale(final$`Median Household Income`)
           + scale(final$`% Homeowners`) + scale(final$`% Severe Housing Problems`) + scale(final$`Average Traffic Volume per Meter of Major Roadways`) + scale(final$`% With Access to Exercise Opportunities`)
           + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`)
           + scale(final$`% Food Insecure`) + scale(final$`violent crime_Z-Score`) + scale(final$`Excessive drinking_Z-Score`)
           + scale(final$`% Frequent Mental Distress`) + scale(final$`% Physically Inactive`)
           , data = final)

fitx.lagrange <- lm.LMtests(fitx,nb.list,test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(fitx.lagrange) # LMlag has the lowest p value
stargazer(fitx.lagrange,header=FALSE, type='latex',
          ci = F,title = "Lagrange Multiplier Test",
          column.labels = c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))

###density only
fit1 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density), data = final, nb.list)
summary(fit1)


###add access to health care

fit2 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density) + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`), data = final, nb.list)
summary(fit2)
###add access to care, demographic,
fit3 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density) +scale(final$`% Female`)+ scale(final$`% Black`) + scale(final$`% Hispanic`) + scale(final$`% Asian`) + scale(final$`% Non-Hispanic White`) 
                 + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`)
                 , data = final, nb.list)
summary.sarlm(fit3)

###add access to care, demographic, SES
fit4 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density) +scale(final$`% Female`)+ scale(final$`% Black`) + scale(final$`% Hispanic`) + scale(final$`% Asian`) + scale(final$`% Non-Hispanic White`) 
                 + scale(final$`% Not Proficient in English`) + scale(final$`income_inequalityZ-Score`) + scale(final$`some college_Z-Score`) + scale(final$`unemployment_Z-Score`) + scale(final$`Median Household Income`)
                 + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`)
                 , data = final, nb.list)
summary.sarlm(fit4)

###add access to care, demographic, SES, environment factors
fit5 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density) +scale(final$`% Female`)+ scale(final$`% Black`) + scale(final$`% Hispanic`) + scale(final$`% Asian`) + scale(final$`% Non-Hispanic White`) 
                 + scale(final$`% Not Proficient in English`) + scale(final$`income_inequalityZ-Score`) + scale(final$`some college_Z-Score`) + scale(final$`unemployment_Z-Score`) + scale(final$`Median Household Income`)
                 + scale(final$`% Homeowners`) + scale(final$`% Severe Housing Problems`) + scale(final$`Average Traffic Volume per Meter of Major Roadways`) + scale(final$`% With Access to Exercise Opportunities`)
                 + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`)
                 , data = final, nb.list)
summary.sarlm(fit5)

###add access to care, demographic, SES, environment factors, risk conditions/behaviors
fit6 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density) +scale(final$`% Female`)+ scale(final$`% Black`) + scale(final$`% Hispanic`) + scale(final$`% Asian`) + scale(final$`% Non-Hispanic White`) 
                 + scale(final$`% Not Proficient in English`) + scale(final$`income_inequalityZ-Score`) + scale(final$`some college_Z-Score`) + scale(final$`unemployment_Z-Score`) + scale(final$`Median Household Income`)
                 + scale(final$`% Homeowners`) + scale(final$`% Severe Housing Problems`) + scale(final$`Average Traffic Volume per Meter of Major Roadways`) + scale(final$`% With Access to Exercise Opportunities`)
                 + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`)
                 + scale(final$`% Food Insecure`) + scale(final$`violent crime_Z-Score`) + scale(final$`Excessive drinking_Z-Score`)
                 , data = final, nb.list)
summary.sarlm(fit6)

###add access to care, demographic, SES, environment factors, risk conditions/behaviors
fit6 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density) +scale(final$`% Female`)+ scale(final$`% Black`) + scale(final$`% Hispanic`) + scale(final$`% Asian`) + scale(final$`% Non-Hispanic White`) 
                 + scale(final$`% Not Proficient in English`) + scale(final$`income_inequalityZ-Score`) + scale(final$`some college_Z-Score`) + scale(final$`unemployment_Z-Score`) + scale(final$`Median Household Income`)
                 + scale(final$`% Homeowners`) + scale(final$`% Severe Housing Problems`) + scale(final$`Average Traffic Volume per Meter of Major Roadways`) + scale(final$`% With Access to Exercise Opportunities`)
                 + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`)
                 + scale(final$`% Food Insecure`) + scale(final$`violent crime_Z-Score`) + scale(final$`Excessive drinking_Z-Score`)
                 , data = final, nb.list)
summary.sarlm(fit6)


###add access to care, demographic, SES, environment factors, risk conditions/behaviors and personal health
fit7 <- lagsarlm(final$`Smokers (%)`~ scale(final$Density) +scale(final$`% Female`)+ scale(final$`% Black`) + scale(final$`% Hispanic`) + scale(final$`% Asian`) + scale(final$`% Non-Hispanic White`) 
                 + scale(final$`% Not Proficient in English`) + scale(final$`income_inequalityZ-Score`) + scale(final$`some college_Z-Score`) + scale(final$`unemployment_Z-Score`) + scale(final$`Median Household Income`)
                 + scale(final$`% Homeowners`) + scale(final$`% Severe Housing Problems`) + scale(final$`Average Traffic Volume per Meter of Major Roadways`) + scale(final$`% With Access to Exercise Opportunities`)
                 + scale(final$`Primary care physicians_Z-Score`) + scale(final$`Mental health providers_Z-Score`)
                 + scale(final$`% Food Insecure`) + scale(final$`violent crime_Z-Score`) + scale(final$`Excessive drinking_Z-Score`)
                 + scale(final$`% Frequent Mental Distress`) + scale(final$`% Physically Inactive`)
                 , data = final, nb.list)
summary.sarlm(fit7)

library(stargazer)
stargazer(fit1,fit2,fit3,fit4,fit5,fit6,fit7,header=FALSE, type='latex',
          ci = T,title = "Spatially autoregressive models",
          column.labels = c("Univariate", "add access to health care", "add demographic", "add SES", "add environment factors", "add risk conditions/behaviors", "add personal health"))


stargazer(fit1,fit2,fit3,header=FALSE, type='latex',
          ci = T,title = "Spatially autoregressive models",
          column.labels = c("Univariate", "add access to health care", "add demographic"))


stargazer(fit4,fit5,header=FALSE, type='latex',
          ci = T,title = "Spatially autoregressive models",
          column.labels = c("add SES", "add environment factors"))



stargazer(fit6,fit7,header=FALSE, type='latex',
          ci = T,
          column.labels = c("add risk conditions/behaviors", "add personal health"))

stargazer(fit,header=FALSE, type='latex',
          ci = T,title = "Spatially autoregressive models",
          column.labels = c("add SES"))
