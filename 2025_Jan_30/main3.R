# Primitive Data Types
# string "andrew", "Rstudio", "1e5"
# float 4.32, pi, -1.1, 0
# int 1, 2, 3, -1, 0

# Less-Primitive Data Types
# numeric
# Date

as.integer(1) / as.integer(10) # R automatically casts integers to floats
1 / 10 # == 0
1e5 / 10

"1e5" / 10 # type error
"1e5" * 10

as.numeric("1e5")
round(1/10)

typeof(1e5)
typeof("1e5")
typeof(as.numeric("1e5"))

as.numeric("-1.1e-5")

string <- "apple,BANANA,ora$$nge,1e5  1e-5"
string <- gsub("\\$","",string)
string <- tolower(string)
strsplit(string,"\t")

string <- readLines("text.txt")
string <- gsub("\\$","",string)
string <- tolower(string)
string <- strsplit(string,"\t")[[1]]
age <- string[2]
string <- string[1]
# TODO cast age to numeric
string <- strsplit(string,",")[[1]]
q1 <- string[1]
q2 <- string[2]
q3 <- string[3]
weight <- string[4]
weight <- as.numeric(weight)
age <- as.numeric(age)

tmp <- list(c(1,2,3),4,5)
tmp

list(c(99,77,88),c(99,44),c(99,99,99,99,99))[[1]]
list(list(c(1,2,3),4,5),4)[[1]][[3]]





