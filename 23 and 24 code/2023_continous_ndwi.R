library("tidyverse")
library("ggplot2")
library("dplyr")
install.packages("MASS")
library("MASS")

# Step 1: Load the CSV file
data <- read.csv("C:/Users/Matth/Github/SoilNema23-24/Metadata/metad_ndwi_23.csv")

data_up <- read.csv("C:/Users/Matth/Github/SoilNema23-24/Metadata/Metadata spreadsheet w NDWI 2023.csv")

# Step 2: View column names to find your variables
colnames(data)

colnames(data_up)

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

#doing a squareroot of the response

model_simple_root <- lm(sqrt(data$rootknot) ~ data$NDWI.avg)
summary(model_simple_root)
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


####Trying something to fix the rootknot problem####

# Load necessary package
library(MASS)     # for glm.nb
library(ggplot2)  # optional, for plotting

# Create the data
df <- data.frame(
  NDWI_avg = c(
    0.4973077, 0.3994075, 0.5572792, 0.4099959, 0.4041702,
    0.4572540, 0.4768103, 0.3971894, 0.4413446, 0.3902016,
    0.5263846, 0.4847200, 0.3033548, 0.4845739, 0.4879864,
    0.4725965, 0.4840152, 0.4758581, 0.3276186, 0.2717690
  ),
  rootknot = c(
    241, 16, 253, 119, 250,
    235, 118, 0, 0, 247,
    74, 254, 4, 0, 0,
    67, 231, 2, 4, 0
  )
)

# Fit negative binomial model
model_nb <- glm.nb(rootknot ~ NDWI_avg, data = df)

# View summary of model
summary(model_nb)

ggplot(df, aes(x = NDWI_avg, y = rootknot)) +
  geom_point() +
  stat_smooth(method = MASS::glm.nb, formula = y ~ x, se = TRUE) +
  labs(title = "Negative Binomial Regression: Rootknot vs NDWI",
       x = "NDWI.avg", y = "Rootknot Count") +
  theme_minimal()

#Whatever chat said
plot(df$NDWI_avg, df$rootknot, pch = 19, main = "Rootknot vs NDWI",
     xlab = "NDWI.avg", ylab = "Rootknot Count")

curve(predict(model_nb, newdata = data.frame(NDWI_avg = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Cook's Distance
plot(cooks.distance(model_nb), type = "h", main = "Cook's Distance")
abline(h = 4/length(df$rootknot), col = "red")  # heuristic threshold

# Influence plot
library(car)
influencePlot(model_nb)

# install.packages("pscl") if needed
install.packages("pscl")
library(pscl)

zinb_model <- zeroinfl(rootknot ~ NDWI_avg, data = df, dist = "negbin")
summary(zinb_model)

##### Trying vs the total###
# Negative Binomial Regression: Nematode_tot ~ NDWI_avg
library(MASS)

model_nb_tot <- glm.nb(Nematode_tot ~ NDWI_avg, data_up = df)

# View summary
summary(model_nb_tot)


####Trying to make log transform of data####
df <- data.frame(
  NDWI_avg = c(
    0.4973077, 0.3994075, 0.5572792, 0.4099959, 0.4041702,
    0.4572540, 0.4768103, 0.3971894, 0.4413446, 0.3902016,
    0.5263846, 0.4847200, 0.3033548, 0.4845739, 0.4879864,
    0.4725965, 0.4840152, 0.4758581, 0.3276186, 0.2717690
  ),
  rootknot = c(
    241, 16, 253, 119, 250,
    235, 118, 0, 0, 247,
    74, 254, 4, 0, 0,
    67, 231, 2, 4, 0
  )
)
Log_df_plus1 <- log(df + 1)
print(Log_df_plus1)


#stuff for checking
summary(df)
plot(df$NDWI_avg, df$rootknot,
     pch = 19, col = "steelblue",
     xlab = "NDWI_avg", ylab = "Rootknot (count)",
     main = "Rootknot counts vs NDWI_avg")
plot(df$NDWI_avg, log1p(df$rootknot),
     pch = 19, col = "darkgreen",
     xlab = "NDWI_avg", ylab = "log1p(Rootknot)",
     main = "Exploration: log1p-transformed Rootknot vs NDWI_avg")
abline(lm(log1p(rootknot) ~ NDWI_avg, data = df), col = "red")

m_pois <- glm(rootknot ~ log(NDWI_avg), family = poisson(link = "log"), data = df)
summary(m_pois)
##highly significant but cannot be trusted 

#check for overdispersion#
install.packages("AER")
library("AER")
dispersiontest(m_pois)
#alternative hypothesis: true dispersion is greater than 1
#sample estimates:dispersion 100.9457
#so yes its overdispersed#

#trying negative bi
library(MASS)
m_nb <- glm.nb(rootknot ~ log(NDWI_avg), data = df)
summary(m_nb)
#SAME ERROR OH MYYYYY###

new_data <- data.frame(NDWI_avg = seq(min(df$NDWI_avg), max(df$NDWI_avg), length.out = 100))
new_data$pred_rootknot <- predict(m_nb, new_data, type = "response")

plot(df$NDWI_avg, df$rootknot,
     pch = 19, col = "gray40",
     xlab = "NDWI_avg", ylab = "Rootknot")
lines(new_data$NDWI_avg, new_data$pred_rootknot, col = "blue", lwd = 2)

###trying chat###
Loglm<-lm(log1p(df$rootknot) ~ df$NDWI_avg)
summary(Loglm)

glm_trans<-glm.nb(df$rootknot ~ log(df$NDWI_avg))
summary(glm_trans)

#unfortunately that did not work either we can try doing more transformations

Lm_sqResponse<-lm(formula = df$NDWI_avg ~ df$rootknot + df$rootknot^2, data = df)
summary(Lm_sqResponse)


#cubing?
Lm_sqResponse<-lm(formula = df$NDWI_avg ~ df$rootknot + df$rootknot^2 + df$rootknot^3, data = df)
summary(Lm_sqResponse)
#not right either#

#the + 1 on the end of rootknot is to remove the infinite values from messing up(zeros are included in the count data)
Log_both <-lm(formula = log(df$NDWI_avg) ~ log(df$rootknot + 1), data = df)
summary(Log_both)
residuals.lm(Log_both)
#adjusted r squared is .113 thats rough buddy

Log_inde<-lm(formula = log(df$NDWI_avg) ~ df$rootknot, data = df)
summary(Log_inde)
#adjusted r squared is lower .107

Log_dep<-lm(formula = df$NDWI_avg ~ log(df$rootknot + 1), data = df)
summary(Log_dep)
##adjusted r squared is lower .106 are we cooked chat????

##make it backwards?
Log_back_ind<-lm(formula = df$rootknot ~ log(df$NDWI_avg), data = df)
summary(Log_back_ind)
#nah same results this is cooked what the hell#


lm(formula = data_up$rootknot ~ data_up$NDWI.avg + data_up$Treat, data = data_up)
