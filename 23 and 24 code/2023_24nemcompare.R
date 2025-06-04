library(tidyverse)
library(emmeans)
library(glmmTMB)
library(dplyr)

#######How tou create a year compare
soil23comp = Soil23 %>%
  mutate(year =2023) %>%
  rename_at(5,~"rootknot")%>%
  rename_at(8,~"spiral")%>%
  rename_at(1,~"Farms")%>%
  rename_at(2,~"Treat")%>%
  select(Farms,Treat,year,rootknot,spiral)

names(soil23comp)[1] <- "farms"
names(soil23comp)[2] <- "treat"

soil23comp<- soil23comp[-1,]
  

soil24comp = `2024.sites.nematode.counts(Sheet1)` %>%
   mutate(year=2024)%>%
   rename_at(4,~"rootknot")%>%
   rename_at(8,~"spiral")%>%
   select(farms,treat,year,rootknot,spiral)

###########Making a bind
#mutating a row to change soil23comp rows to inetgers to it matches up#
soil23comp <- soil23comp %>%
  mutate(across(c(4,5),as.integer))

soilall=bind_rows(soil23comp,soil24comp)%>%
  mutate(year=as.character(year),
         irrigation = str_sub(treat,1,1),
         tillage = str_sub(treat,2,2))

view(soilall)

install.packages("glmmTMB")
library(glmmTMB)
library(tidyverse)
library(emmeans)

install.packages("glmmTMB")

mod.spiral=glmmTMB(spiral~irrigation*tillage+year,data=soilall,family=poisson())
car::Anova(mod.spiral,type=2)

emmeans(mod.spiral,~irrigation*tillage)

mod.spiral=glmmTMB(spiral~irrigation*tillage+year,data=soilall,family=nbinom2())
car::Anova(mod.spiral,type=2)

library(DHARMa)
testDispersion(mod.spiral)

mod.spiral=glmmTMB(spiral~irrigation*tillage+year,data=soilall,family=poisson())
testDispersion(mod.spiral)

#####Rootknot test now
mod.rootknot=glmmTMB(rootknot~irrigation*tillage+year,data=soilall,family=poisson())
testDispersion(mod.rootknot)
car::Anova(mod.rootknot,type=2)

emmeans(mod.rootknot,~irrigation*tillage)

mod.rootknot=glmmTMB(rootknot~irrigation+tillage+year,data=soilall,family=nbinom2())
DHARMa::simulateResiduals(mod.rootknot) %>% DHARMa::plotSimulatedResiduals(.)
testDispersion(mod.rootknot)

mod.rootknot=glmmTMB(rootknot~irrigation+tillage+year,data=soilall,family=nbinom2())
car::Anova(mod.rootknot)
testDispersion(mod.rootknot)
mod.rootknot                     

##########RootKnot analysis
emmeans(mod.rootknot, ~irrigation*tillage) %>%
  as.data.frame() %>%
  ggplot(aes(y = emmean, x= irrigation, group = tillage, color = tillage))+
  geom_point(position = position_dodge(width = 0.2))+
  geom_errorbar(aes(ymin = asymp.LCL,ymax =asymp.UCL),position = position_dodge(width = 0.2),width = 0.2)+
  labs(y ="ln-Root-Knot-Nematodes")+
  theme_classic()


#############Spiral analysis
emmeans(mod.spiral, ~treat) %>%
  as.data.frame() %>%
  ggplot(aes(y = emmean, x= treat))+
  geom_point()+
  geom_errorbar(aes(ymin = asymp.LCL,ymax =asymp.UCL))+
  labs(y ="ln-Spiral-Nematodes")+
  theme_classic()

emmeans(mod.rootknot, ~irrigation*tillage) %>%
  as.data.frame() %>%
  ggplot(aes(y = emmean, x= irrigation, group = tillage, color = tillage))+
  geom_point(position = position_dodge(width = 0.2))+
  geom_errorbar(aes(ymin = asymp.LCL,ymax =asymp.UCL),position = position_dodge(width = 0.2),width = 0.2)+
  labs(y ="ln-Spiral-Nematodes")+
  theme_classic()




##########trying to do the ndwi
install.packages("exifr")
library("exifr")
photo_files <- list.files("2024 farm phots", pattern = "*.HEIC", full.names = T)
dat <- read_exif(photo_files) %>% 
  rename(lat = GPSLatitude,lon = GPSLongitude)
#wow that worked sort of#

photo_files <- read_exif(photo_files)

library(exiftoolr)
photo_files <- list.files("2024 farm phots", pattern = "*.HEIC", full.names = T)
dat <- read_exif(photo_files) %>% 
  rename(lat = GPSLatitude,lon = GPSLongitude)
