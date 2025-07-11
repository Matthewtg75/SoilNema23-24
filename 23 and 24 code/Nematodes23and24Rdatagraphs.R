#Right click the run button at the top right then press select all, after that press run. It runs all the program including graphs to that point
library(dplyr)
library(esquisse)
library(ggplot2)
library(tidyr)
setwd("~/Documents/GitHub/SoilNema23-24/Working code")

#Getting everthing set up##################
# reading file in

#Sofie help on getting the graph jordan wanted 2023-24 compared###
soil23 = read.csv("soil23.csv")

SG23<- soil23 %>%
  pivot_longer(cols = 5:8, names_to = "Nematodes", values_to = "Value")

soil24 = read.csv("24 sites nematode counts(Sheet1).csv")

SG24<- soil24 %>%
  pivot_longer(cols = 4:8, names_to = "Nematodes", values_to = "Value")

SG23 = SG23 %>%
  mutate(Year=2023)%>%
  select(Farms, Treat, Nematodes, Value, Year)%>%
  mutate(Nematodes=case_when(Nematodes=="Helicotylenchus.....Spiral"~"spiral",
            Nematodes=="Meloidogyne.....Root.knot"~"root.knot",
            Nematodes=="Paratrichodorus.....Stubby.root"~"stubby.root",
            Nematodes=="Mesocriconema..etc......Ring"~"ring",
            .default = "other"))

SG24 = SG24 %>%
  mutate(Year=2024)%>%
  mutate(Farms=sites)%>%
  mutate(Treat=treatment)%>%
  select(Farms, Treat, Nematodes, Value, Year)

SG2324 = bind_rows(SG23, SG24)
SG2324= SG2324[SG2324$Nematodes %in% c("ring", "root.knot","spiral","stubby.root"), ]

library(stringr)
SG2324[c("Irrig","Till")] = str_split_fixed(SG2324$Treat, "",2 )

Nematode_obj = c(ring = "Ring (Mesocriconema)",
                 root.knot = "Root Knot (Meloidogyne)",
                 stubby.root = "Stubby Root (Paratrichodorus)",
                 spiral = "Spiral (Helicotylenchus)")

ggplot(SG2324, aes(x=Irrig, y=Value, fill = Till)) +
  geom_boxplot()+
  facet_wrap(vars(Nematodes), scales="free_y", labeller=labeller(Nematodes=Nematode_obj))+
  labs(title = "Nematodes 2023- 2024", x="Irrigation", y="Abundance") +
  scale_x_discrete(labels=c("Non-Irrigated", "Irrigated"))+
  scale_fill_discrete(name="Tillage",
                      labels=c("Conventional", "Reduced"))+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 23L, 
                                  face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=15, face = "bold"),
        axis.title.x = element_text(size=15, face = "bold"))

#making Plots or graphs####################
#most recent graph created for nematodes
ggplot(SG23) +
 aes(x = Nematodes, y = Value, fill = Nematodes, group = Treat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 labs(title = "2023 Nematode Samples", fill = "Treatment") +
 theme_minimal() +
 theme(plot.title = element_text(size = 23L, 
 face = "bold", hjust = 0.5)) +
 facet_wrap(vars(Nematodes), scales = "free_y")


#Getting Stuff thats needed####################
#Means across treatments
#SD across treatments
SG23=SG23%>% 
  mutate(Treat_Type=paste0(Treat,"_",Nematodes))%>%
  group_by(Treat_Type) %>% 
  mutate(mean_treats = mean(Value, na.rm = TRUE)) %>%
  mutate(sd_treat = sd(Value, na.rm = TRUE))

SG23 %>% 
  group_by(Treat) %>% 
  summarize( Min = min(Value),
             Q1 = quantile(Value, .25),
             Avg_Per_Treat = mean(Value), 
             Q3 = quantile(Value, .75),
             Max = max(Value))
SG23 %>%
  group_by(Treat) %>% 
  summarize( Min = min(Value),
             Q1 = quantile(Value, .25),
             Avg_Per_Treat = mean(Value), 
             Q3 = quantile(Value, .75),
             Max = max(Value)) %>%
  ggplot(., aes(x = Treat, y = Avg_Per_Treat)) + geom_col()

#making Plots or graphs####################


#press cmd + return to do esquessie
esquisser()


#Sofies magic graph she helped with

ggplot(data = SG23, aes(x = Treat, y = Value, fill = Treat)) +
  stat_summary(fun = mean, geom = "col") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  facet_wrap(vars(Nematodes), scale = "free_y") +
  labs(x = "Treatment", y = "Mean Number of Nematodes",
       title = "2023 Nematodes Means and Errorbars across Treatments") +
  theme_bw()

#this is the stuff with bar and errorbar

ggplot(data = SG23, aes(x = Treat, y = mean_treats, fill = Treat_Type)) +
 geom_col() +
 stat_summary(fun.data = mean_cl_boot, geom = "errorbar") +
 scale_fill_brewer(palette = "PuOr", 
 direction = 1) +
 labs(x = "Treatment", y = "Amount of Nematode", title = "Nematode Type at Each Treatment Bar", 
 fill = "Treatment") +
 theme_light() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5), 
 axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
 facet_wrap(vars(Nematodes), 
 scales = "free_y")


ggplot(SG23) +
 aes(x = Treat, y = Value, fill = Nematodes) +
 geom_boxplot() +
 scale_fill_brewer(palette = "Accent", 
 direction = 1) +
 labs(x = "Treatments", y = "Amount of Nematodes", title = "Nematode type at each Treatment") +
 theme_linedraw() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.title.y = element_text(face = "bold"), 
 axis.title.x = element_text(face = "bold")) +
 facet_wrap(vars(Nematodes), scales = "free_y")


ggplot(SG23) +
 aes(x = Treat, y = Value, fill = Nematodes) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 labs(title = "Nematodes Boxplot") +
 theme_minimal() +
 theme(plot.title = element_text(face = "bold", 
 hjust = 0.5)) +
 facet_wrap(vars(Nematodes), scales = "free_y")
SG23 %>% 
  reframe(mean=mean(count),SD=sd(count))
####### Year 23 #########

SG23= SG23[SG23$Nematodes %in% c("ring", "root.knot","spiral","stubby.root"), ]

SG23[c("Irrig","Till")] = str_split_fixed(SG23$Treat, "",2 )

ggplot(SG23, aes(x=Irrig, y=Value, fill = Till)) +
  geom_boxplot()+
  facet_wrap(vars(Nematodes), scales="free_y", labeller=labeller(Nematodes=Nematode_obj))+
  labs(title = "Nematodes 2023", x="Irrigation", y="Abundance") +
  scale_x_discrete(labels=c("Non-Irrigated", "Irrigated"))+
  scale_fill_discrete(name="Tillage",
                      labels=c("Conventional", "Reduced"))+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 23L, 
                                  face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=20, face = "bold"),
        axis.title.x = element_text(size=20, face = "bold"))

################## Year 24

SG24[c("Irrig","Till")] = str_split_fixed(SG24$Treat, "",2 )


ggplot(SG24, aes(x=Irrig, y=Value, fill = Till)) +
  geom_boxplot()+
  facet_wrap(vars(Nematodes), scales="free_y", labeller=labeller(Nematodes=Nematode_obj))+
  labs(title = "Nematodes 2024", x="Irrigation", y="Abundance") +
  scale_x_discrete(labels=c("Non-Irrigated", "Irrigated"))+
  scale_fill_discrete(name="Tillage",
                      labels=c("Conventional", "Reduced"))+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 23L, 
                                  face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=20, face = "bold"),
        axis.title.x = element_text(size=20, face = "bold"))

###Remove Lesion to match up with 2023 Data
SG24= SG24[SG24$Nematodes %in% c("ring", "root.knot","spiral","stubby.root"), ]

ggplot(SG24, aes(x=Irrig, y=Value, fill = Till)) +
  geom_boxplot()+
  facet_wrap(vars(Nematodes), scales="free_y", labeller=labeller(Nematodes=Nematode_obj))+
  labs(title = "Nematodes 2024", x="Irrigation", y="Abundance") +
  scale_x_discrete(labels=c("Non-Irrigated", "Irrigated"))+
  scale_fill_discrete(name="Tillage",
                      labels=c("Conventional", "Reduced"))+
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 23L, 
                                  face = "bold", hjust = 0.5),
        axis.title.y = element_text(size=20, face = "bold"),
        axis.title.x = element_text(size=20, face = "bold"))

######

ggplot(SG24) +
  aes(x = Treat, y = Value, fill = Treat) +
  geom_col() +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Treatments",
    y = "Nematode Count",
    title = "2024 Nematodes across Treaments"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 15L),
    axis.text.x = element_text(size = 15L),
    legend.text = element_text(size = 15L),
    legend.title = element_text(size = 15L)
  )


ggplot(data = SG24, aes(x = Treat, y = Value, fill = Treat)) +
  stat_summary(fun = mean, geom = "col") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  facet_wrap(vars(Nematodes), scale = "free_y") +
  labs(x = "Treatment", y = "Mean Number of Nematodes",
       title = "2024 Nematodes across Treatments") +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 15L),
    axis.text.x = element_text(size = 15L),
    legend.text = element_text(size = 15L),
    legend.title = element_text(size = 15L)
  )

#NWDI start######################################
library(sp)
library(raster)
library(ggplot2)
library(viridis)
library(rasterVis)

#idk some esquessie
esquisser()

#Trying to compare NDWI to nema from here on

#adding the ndwi avg to the NDWI 23
ndwi_2023=ndwi_2023%>%
  group_by(farm) %>%
  mutate(ndwi_avg = mean(NDWI, na.rm = TRUE))

#Trying to add NDWI data to the nematode set

  