library(readr)
df <- read_csv("andrew/R_training/data.csv")
View(df)
# data cleaning
# what do the values look like for each column
unique(df) # gives unique rows
unique(df$age) # gives unique ages
# what to do with -74 and NA - NA is fine either drop or change -74 to +74
df$age>=0
sum(df$age>=0)
sum(df$age>=0,na.rm = TRUE)
sum(is.na(df$age)) # i.e. there are 2 ages < 0
sum(df$age >= 0 | is.na(df$age)) # select only these ages
df <- df[df$age >= 0 | is.na(df$age),]
View(df)
sum(df$age < 0,na.rm=TRUE) # select only these ages
mean(df$age,na.rm = TRUE)
sd(df$age,na.rm = TRUE)
hist(df$age)
View(df)
# get easy ones out the way
df$sex
unique(df$sex)
# good to go
df$variant
unique(df$variant)
sum(df$variant == 'None',na.rm =TRUE)
# date
typeof(df$doc)
df$doc[1]+10
unique(df$doc)
na.omit(unique(df$doc))
#
table(df$id)
# which columns matter if they _are_ duplicated?
# all of them matter, but there is an order of importance
duplicated(df$id)
sum(duplicated(df$id))
View(df[duplicated(df$id),])
duplicated(df$id,fromLast=TRUE)
sum(duplicated(df$id,fromLast=TRUE))
View(df[duplicated(df$id,fromLast=TRUE),])
duplicated(df$id) | duplicated(df$id,fromLast=TRUE)
sum(duplicated(df$id) | duplicated(df$id,fromLast=TRUE))
df[duplicated(df$id) | duplicated(df$id,fromLast=TRUE),]
View(df[duplicated(df$id) | duplicated(df$id,fromLast=TRUE),])
# if IDs are not equal, would we consider a row a duplicate? no.
# so we can work with above
# if ids are equal, but others not equal what do we do?
# best thing to do is look
duplicates <- df[duplicated(df$id) | duplicated(df$id,fromLast=TRUE),]
View(duplicates)
# you can also do this, if NA values then will not reutrn
duplicated(df)
sum(duplicated(df))
!duplicated(df)
df[!duplicated(df),]
View(df[!duplicated(df),])
cleaned <- df[!duplicated(df),]
sum(duplicated(cleaned))
sum(duplicated(cleaned$id))