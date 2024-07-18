### Generating frequency table, required a little bit of coding
library(readxl)
df <- read_excel("linelist_cleaned.xlsx")
install.packages("janitor")
library(janitor)
output <- tabyl(df$hospital, show_na = FALSE)
output['Cumulative Frequency'] <- cumsum(output$n)
output['Cumulative Percent'] <- cumsum(output$percent)
names(output)[1:3] <- c("Hospital","Frequency","Percent")

library(dplyr)
output <- output %>% mutate(Percent=round(Percent,3)) %>% mutate(`Cumulative Percent`=round(`Cumulative Percent`,3))



### Packaging up our code into a function
freq <- function (df,col_name){
  library(janitor)
  library(dplyr)
  
  col_sym <- rlang::sym(col_name)
  output <- tabyl(df,!!col_sym, show_na = FALSE)
  
  output['Cumulative Frequency'] <- cumsum(output$n)
  output['Cumulative Percent'] <- cumsum(output$percent)
  
  names(output)[1:3] <- c(col_name,"Frequency","Percent")
  output <- output %>% mutate(Percent=round(Percent,3)) %>% mutate(`Cumulative Percent`=round(`Cumulative Percent`,3))
  return(output)
}

frequency_table <- freq(df,"age")
frequency_table

freq(df,"hospital")
freq(df,"gender")