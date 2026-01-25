print("hello")
print("world")
x = 5
y = 3
print(x+y)

# library(RSQLite)
# sqlite.connect("//datawarehouse.dph")


library(readxl)
linelist_cleaned <- read_excel("dev/r-training/2025_March_27/linelist_cleaned.xlsx")
hist(linelist_cleaned$age)

hist(linelist_cleaned$hospital)

library(ggplot2)
ggplot(linelist_cleaned, aes(x=wt_kg)) + geom_histogram()

ggplot(linelist_cleaned, aes(x=hospital)) + geom_histogram()

table(linelist_cleaned$hospital)

ggplot(linelist_cleaned, aes(x=wt_kg,y=ht_cm)) + geom_point()

library(GGally)
ggpairs(linelist_cleaned[c('wt_kg','ht_cm','outcome')])

# Statistical Tests
t.test(ht_cm_f,ht_cm_m)
# Use Statistical Test
# Make Decision
# Write a report


