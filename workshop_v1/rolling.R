library(readr)
library(magrittr)
library(dplyr)

df <- read_csv("../Downloads/ww_dataUSC1.csv")
camden <- df[df$facility_name == "camden wwtf",]
camden$index <- as.numeric(rownames(camden))
plot(lm(camden$flowpop ~ camden$index))

library(zoo)
camden %>% mutate(rolling = rollapply(flowpop, 15, function(x) tail(x, 1)))
#sales_data <- sales_data %>% mutate(rolling_sales = rollapply(sales, window_size, function(x) tail(x, 1)))