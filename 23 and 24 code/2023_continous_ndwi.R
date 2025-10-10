library("tidyverse")
library("ggplot2")
library("dplyr")

# Step 1: Load the CSV file
data <- read.csv("C:/Users/Matth/Github/SoilNema23-24/Metadata/metad_ndwi_23.csv")

data_up <- read.csv("C:/Users/Matth/Github/SoilNema23-24/Metadata/Metadata spreadsheet w NDWI 2023.csv")

# Step 2: View column names to find your variables
colnames(data)

# Step 3: Run correlation test between two variables (replace with actual column names)

cor.test(data$NDWI.avg, data$rootknot, method = "pearson")

#Pearson's product-moment correlation: t = 1.7978, df = 18, p-value = 0.08901: percent confidence interval:-0.0632933  0.7100795#


cor.test(data$NDWI.avg, data$spiral, method = "pearson")

##t = 1.1101, df = 18, p-value = 0.2816: confidence interval:-0.2132756  0.6255805#

####LINEAR REGRESSIONS######

# Simple linear regression
model_simple_root <- lm(data$rootknot ~ data$NDWI.avg)

summary(model_simple_root)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-135.21  -82.58  -17.09   98.67  168.84 

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)  
#(Intercept)     -149.5      143.9  -1.039    0.313  
#data$NDWI.avg    583.5      324.6   1.798    0.089
#Residual standard error: 104.8 on 18 degrees of freedom
#F-statistic: 3.232 on 1 and 18 DF,  p-value: 0.08901

plot(model_simple_root)

# Simple linear regression
model_simple_spiral <- glm.nb(rootknot ~ NDWI.avg,data=data)

summary(model_simple_spiral)
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-168.93 -104.89  -67.79   -0.42  516.61 

#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
#(Intercept)     -189.2      256.7  -0.737    0.471
#data$NDWI.avg    642.7      578.9   1.110    0.282
#Residual standard error: 187 on 18 degrees of freedom
#F-statistic: 1.232 on 1 and 18 DF,  p-value: 0.2816

plot(model_simple_spiral)

model_simple_tot <- glm.nb(Nem_total ~ NDWI.avg,data=data_up)
