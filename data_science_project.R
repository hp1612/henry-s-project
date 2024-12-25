library(readr)
Datasetv2 <- read_csv("C:/Users/85251/Downloads/Datasetv2.csv")
library(dplyr)
rent <- Datasetv2 %>% filter(Rental== TRUE & `Public Housing`== FALSE)
sell <- Datasetv2 %>% filter(Rental== FALSE & `Public Housing`== FALSE)
rent$district <- factor(rent$district)
sell$district <- factor(sell$district)
rent_new <- rent %>% select(price, `saleable_area(ft^2)`, district, floor) # Price as responsive variable, saleable area, district and floor as explanatory variable. 
sell_new <- sell %>% select(price, `saleable_area(ft^2)`, district, floor)
pairs(rent_new[,], main = "Scatter Plot Matrix for Rental Housing") #Scatterplot between all 4 variables
pairs(sell_new[,], main = "Scatter Plot Matrix for Selling Houses")
model_0_rent <- lm(price ~ `saleable_area(ft^2)`+ district + floor, rent_new)
model_0_sell <- lm(price ~ `saleable_area(ft^2)`+ district + floor, sell_new)
library(MASS)
boxcox_result_rent <- boxcox(model_0_rent, lambda = seq(-2, 2, 0.1)) #Using Boxcox method for testing power transformation
optimal_lambda_rent <- boxcox_result_rent$x[which.max(boxcox_result_rent$y)]
print(optimal_lambda_rent)
boxcox_result_sell <- boxcox(model_0_sell, lambda = seq(-2, 2, 0.1))
optimal_lambda_sell <- boxcox_result_sell$x[which.max(boxcox_result_sell$y)]
print(optimal_lambda_sell)
model_1_rent <- lm(log(price) ~ `saleable_area(ft^2)`+ district + floor, rent_new) #The R squared value for log rental price transformation is not appealing.
model_1_sell <- lm(log(price) ~ `saleable_area(ft^2)`+ district + floor, sell_new) #The log transfer price transformation is suitable.
library(effects)
library(ggplot2)
library(alr4)
vif(model_1_rent) #Checking the multicollinearity of explanatory variable.
vif(model_1_sell)
full_model_rent <- model_0_rent
# Stepwise forward selection based on AIC
null_model_rent <- lm(price ~ 1, data = rent_new) 
# Perform forward selection
forward_model_rent <- step(null_model_rent, 
                      scope = list(lower = null_model_rent, upper = full_model_rent), 
                      direction = "forward", 
                      trace = 1)
full_model_sell <- model_1_sell
# Stepwise forward selection based on AIC
null_model_sell <- lm(log(price) ~ 1, data = sell_new) 
# Perform forward selection
forward_model_sell <- step(null_model_sell, 
                           scope = list(lower = null_model_sell, upper = full_model_sell), 
                           direction = "forward", 
                           trace = 1)
