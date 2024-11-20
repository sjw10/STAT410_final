cat("\014")
rm(list=ls())
setwd("C:/Users/wwang/Downloads")

library(readxl)
library(stargazer)
library(faraway)
library(car)
library(leaps)
library(ggplot2)
library(dplyr)

# read in the data
Data = read_xlsx("2021WorldBank.xlsx", sheet = "Cleaned")

# remove all missing values by replacing zero values with NA
Data[Data == 0] = NA
cleanedData = na.omit(Data)

# condensing column names
colnames(cleanedData) = c("Country", "ACFT", "AE", "CO2", "EducationYears", "HealthSpendGDP", "HealthSpendPPP", "EducationAttainment", "Fertility", "GDPperCapitaUSD", "GDPperCapitaPPP", "Diptheria", "HepB3", "Measles", "LifeExpectancy", "Status")

# Descriptive Plots

topFifteen = cleanedData %>% arrange(desc(LifeExpectancy)) %>%
  slice_head(n = 15)

botFifteen = cleanedData %>% arrange(desc(LifeExpectancy)) %>%
  slice_tail(n = 15)

ggplot(subset(cleanedData, Country %in% topFifteen$Country | Country %in% botFifteen$Country), aes(x = reorder(Country, LifeExpectancy), y = LifeExpectancy))+
  geom_bar(stat = "identity", aes(fill = Status))+
  geom_vline(xintercept = 15.5, color = "black")+
  coord_flip()+
  scale_fill_manual(labels = c("Developed", "Developing"), values = c("#1f77b4", "#2ca02c"))+
  labs(title = "Life Expectancies By Country", subtitle = "Highest and Lowest 15 Countries, 2021", x = "Country", y = "Life Expectancy")

# remove the country name column from the data as we don't want it in the regressions
pureData = subset(cleanedData, select = -Country)

## SUBSET MODEL SELECTION, ALL POSSIBLE CASES

# separate the data in the design matrix and response matrix
X = subset(pureData, select = -LifeExpectancy)
Y = pureData$LifeExpectancy

subsetModel = regsubsets(LifeExpectancy ~ ., data = pureData, nvmax = 14, method = "exhaustive")
subsetSummary = summary(subsetModel)
subsetSummary

plot(subsetSummary$adjr2, xlab = "Number of Predictors", ylab = "Adjusted R-squared", type = "l", col = "blue", lwd = 2, )+
  abline(v = 7)+
  abline(v = 9)

plot(subsetSummary$bic, xlab = "Number of Predictors", ylab = "BIC", type = "l", col = "red", lwd = 2)+
  abline(v = 7)+
  abline(v = 9)


plot(subsetSummary$cp, xlab = "Number of Predictors", ylab = "Mallow's Cp", type = "l", col = "red", lwd = 2)+abline(v = 7)+
  abline(v = 9)


best8Model = lm(LifeExpectancy ~ ACFT + AE + HealthSpendPPP + EducationAttainment + Fertility + GDPperCapitaUSD + Diptheria + Measles, data = pureData) 

best7Model = lm(LifeExpectancy ~ ACFT + HealthSpendPPP + EducationAttainment + Fertility + GDPperCapitaUSD + Diptheria + Measles, data = pureData) 

best9Model = lm(LifeExpectancy ~ ACFT + AE + EducationYears + HealthSpendPPP + EducationAttainment + Fertility + GDPperCapitaUSD + Diptheria + Measles, data = pureData) 

### FORWARD SELECTION USING AIC

# for forward selection, we need a null model that only contains beta zero
forwardNull = lm(LifeExpectancy ~ 1, data = pureData)

# and then we need a full model that contains all predictors
forwardFull = lm(LifeExpectancy ~ ., data = pureData)

# use the step function to perform forward selection
forwardModelAIC = step(forwardNull, 
                      scope = list(lower = forwardNull, upper = forwardFull), 
                      direction = "forward", data = pureData)

# View the results of forward selection using the default AIC criterion
stargazer(forwardModelAIC, type = "text")

### BACKWARD SELECTION USING AIC

# for backward selection, we only need one model with all the predictors
backwardFull = lm(LifeExpectancy ~ ., data = pureData)

# use the step function to perform backward selection
backwardModelAIC = step(backwardFull, direction = "backward", data = pureData)

# View the results of forward selection using the default AIC criterion
stargazer(backwardModelAIC, type = "text")

# We see that the model resulting from forward and backward stepwise selection based on AIC is actually the same, so we can just use either one
stargazer(forwardModelAIC, type = "text")
stargazer(backwardModelAIC, type = "text")

stepModelAIC = forwardModelAIC

### FORWARD SELECTION USING BIC

# for forward selection, we need a null model that only contains beta zero
forwardNull = lm(LifeExpectancy ~ 1, data = pureData)

# and then we need a full model that contains all predictors
forwardFull = lm(LifeExpectancy ~ ., data = pureData)

# use the step function to perform forward selection
forwardModelBIC = step(forwardNull, 
                       scope = list(lower = forwardNull, upper = forwardFull), 
                       direction = "forward", data = pureData, k=log(length(pureData)))

# View the results of forward selection using the default BIC criterion
stargazer(forwardModelBIC, type = "text")

### BACKWARD SELECTION USING BIC

# for backward selection, we only need one model with all the predictors
backwardFull = lm(LifeExpectancy ~ ., data = pureData)

# use the step function to perform backward selection
backwardModelBIC = step(backwardFull, direction = "backward", data = pureData, k=log(length(pureData)))

# View the results of forward selection using the BIC criterion
stargazer(forwardModelBIC, type = "text")
stargazer(backwardModelBIC, type = "text")

### COMPARE ALL MODELS WE HAVE SO FAR

models = list(forwardModelBIC, backwardModelBIC, stepModelAIC, best7Model, best8Model, best9Model)  # Replace with your actual models

# Data storage for results
results = data.frame(
  Model = character(),
  BIC = numeric(),
  AIC = numeric(),
  Adj_R2 = numeric(),
  AICc = numeric(),
  Predictors = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each model
for (i in seq_along(models)) {
  model = models[[i]]
  
  # Calculate BIC, AIC, Adjusted R-squared, and AICc
  n = length(model$residuals)
  k = length(coef(model))
  aicc_value = AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
  bic_value = BIC(model)
  aic_value = AIC(model)
  adj_r2_value = summary(model)$adj.r.squared
  
  # Store results in the dataframe
  results = rbind(results, data.frame(
    Model = paste("Model", i),   # Customize this if needed
    BIC = bic_value,
    AIC = aic_value,
    Adj_R2 = adj_r2_value,
    AICc = aicc_value,
    Predictors = k - 1
  ))
}

results$Model = c("forwardModelBIC", "backwardModelBIC", "stepModelAIC", "best7Model", "best8Model", "best9Model")
results

stargazer(forwardModelBIC, type = "text")
# find the BIC, AIC and PRESS for all models

# plot diagnostic residual plots and the predictors against each other
par(mfrow = c(2,2))
# png("model_diagnostics.png", width = 800, height = 800)  # Set width and height
# plot(forwardModelBIC)
# dev.off()

plot(forwardModelBIC, which = 1)
plot(forwardModelBIC, which = 2)
plot(forwardModelBIC, which = 3)
par(mfrow = c(1,1))
plot(forwardModelBIC, which = 4)

# look at VIF

vif(forwardModelBIC)

# leverage plot

leverageValues = hatvalues(forwardModelBIC)

k = length(coef(forwardModelBIC)) - 1
n = nrow(cleanedData)
threshold = 2 * (k + 1) / n

highLeverage = which(leverageValues > threshold)


# plot(leverageValues, type = "h", main = "Leverage Values", ylab = "Leverage", xlab = "Observation Number")
# abline(h = threshold, col = "red", lty = 2)
# text(x = highLeverage, 
#      y = leverageValues[highLeverage], 
#      labels = cleanedData$Country[highLeverage],
#      pos = 3,  # Position of the label (4 = to the right)
#      cex = 0.8, # Size of the labels
#      col = "blue")

Leverage = data.frame(
  leverage = leverageValues,
  obs = 1:length(leverageValues),
  country = cleanedData$Country, 
  highLeverage = leverageValues > threshold 
)

library(ggrepel)
ggplot(test, aes(x = obs, y = leverage)) +
  geom_segment(aes(xend = obs, yend = 0), color = "black") +  # Vertical lines from each point to y = 0
  geom_point(aes(color = highLeverage), size = 2) +  # Points with color based on high leverage
  geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +  # Leverage threshold line
  geom_text_repel(data = subset(test, highLeverage == TRUE), 
            aes(label = country),  # Add vertical jitter to avoid overlap
            size = 3, color = "blue", box.padding = 0.5, point.padding = 0.3, max.overlaps = 10) +  
  labs(title = "Leverage Values for Linear Regression Model",
       x = "Observation Number",
       y = "Leverage", 
       color = "Leverage Threshold") +
  theme_minimal() + 
  scale_color_manual(labels = c("Below", "Above"), values = c("black", "red"))

                     