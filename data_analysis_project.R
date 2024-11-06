library(readr)
df <- read_csv("C:/Users/85251/Downloads/gdp_versus_average_transfer_fee (1).csv")
pairs(df[, c(2, 3, 4)], main = "Inter-relationship Plot Matrix")
summary(lm(avg_fee ~ gdp_UK, data = df))
summary(lm(avg_fee ~ gdp_World, data = df))

