#Right click the run button at the top right then press select all, after that press run. It runs all the program including graphs to that point
library(dplyr)
library(esquisse)
library(ggplot2)
library(tidyr)
setwd("/Users/snydermac/Desktop/Matthew/MatthewR")

#Getting everthing set up##################
# reading file in

soil23 = read.csv("soil23.csv")

SG23<- soil23 %>%
  pivot_longer(cols = 5:8, names_to = "Nematodes", values_to = "Value")

soil24 = read.csv("24 sites nematode counts(Sheet1).csv")

SG24<- soil24 %>%
  pivot_longer(cols = 4:8, names_to = "Nematodes", values_to = "Value")


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

SG23 %>%
  group_by(Treat) %>%
  summarise_at(vars(Value),list(name=sd)) %>%
  ggplot(., aes(x = Treat, y = SD)) + geom_col()


ggplot(SG23) +
 aes(x = Nematodes, y = Value, fill = Nematodes, group = Treat) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 labs(title = "2023 Nematode Samples Boxplot", fill = "Treatment") +
 theme_minimal() +
 theme(plot.title = element_text(size = 23L, 
 face = "bold", hjust = 0.5)) +
 facet_wrap(vars(Nematodes), scales = "free_y")

ggplot(soil23) +
 aes(x = Meloidogyne.....Root.knot, y = Treat) +
 geom_boxplot(fill = "#112446") +
 labs(title = "Box Plot of 2023 Soil Samples") +
 theme_minimal() +
 theme(plot.title = element_text(size = 21L, face = "bold", hjust = 0.5))



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


#press cmd + return to do esquessie
esquisser()



ggplot(SG23) +
 aes(x = Treat, y = mean_treats, fill = Treat) +
 geom_col() +
 scale_fill_hue(direction = 1) +
 labs(x = "Treatments", y = "Means Values of Nematodes", title = "Nematodes Types across Treatments", 
 fill = "Treatments") +
 theme_minimal() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5), 
 axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold")) +
 facet_wrap(vars(Nematodes), 
 scales = "free_y")

ggplot(SG23) +
 aes(x = Treat_Type, y = mean_treats, fill = Treat) +
 geom_col() +
 geom_line(colour = "#112446") +
 scale_fill_hue(direction = 1) +
 labs(x = "Nematode Type and Treatment", y = "Mean Number of Nematodes", 
 title = "Treatments vs means") +
 theme_minimal()

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
 aes(x = Treat, y = Value, fill = Farms) +
 geom_boxplot() +
 scale_fill_hue(direction = 1) +
 theme_minimal()
library(ggplot2)

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

ggplot(SG23) +
  aes(x = Treat, y = Value, fill = Nematodes, colour = Treat) +
  geom_boxplot() +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_minimal()

ggplot(SG23) +
  aes(x = Treat, y = Value, fill = Nematodes) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

ggplot(SG23) +
  aes(x = Farms, y = Value, colour = Treat) +
  geom_jitter() +
  geom_boxplot(aes(group = Treat), fill = "#112446") +
  scale_color_hue(direction = 1) +
  labs(
    y = "Amount of nema",
    title = "Across farms nema amounts"
  ) +
  theme_minimal()
ggplot(SG23) +
  aes(x = Value, y = Treat, fill = Treat, group = Farms) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Total of Nema",
    y = "Farm",
    title = "Nema farm sites grouped by treat"
  ) +
  coord_flip() +
  theme_minimal()
SG23 %>%
  group_by(Nematodes) %>%
  summarise(mean_nematodes = mean(Value, na.rm = TRUE)) %>%
  ggplot(., aes(x = Treat, y = mean_nematodes)) + geom_col()


################## Year 24
ggplot(SG24) +
  aes(x = treatment, y = root.knot, fill = treatment) +
  geom_col() +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Treatments",
    y = "Root Knot Nematode Count",
    title = "2024 Nematodes across Treaments"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 15L),
    axis.text.x = element_text(size = 15L),
    legend.text = element_text(size = 15L),
    legend.title = element_text(size = 15L)
  )

ggplot(data = SG24, aes(x = treatment, y = Value, fill = treatment)) +
  stat_summary(fun = mean, geom = "col") +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  facet_wrap(vars(Nematodes), scale = "free_y") +
  labs(x = "Treatment", y = "Mean Number of Nematodes",
       title = "2024 Nematodes Means and Errorbars across Treatments") +
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

  