
library(GGally)
library(readxl)
library(rpart)
library(rpart.plot)

df <- read_excel("linelist_cleaned.xlsx")
df$gender <- as.factor(df$gender)
df <- df[c("gender","ht_cm","wt_kg","age","bmi")]
df <- na.omit(df)

ggpairs(df)

clf <- rpart(gender ~ ., data = df, method = "class")
rpart.plot(clf)

X <- df[,2:5]
y_true <- df$gender
y_pred <- predict(clf, X, type = "class")

sum(y_true == y_pred,na.rm=TRUE)/length(y_true) # compute accuracy

# is this better than random chance?
# 5610 observations
# 3750 observations of class 0 (m)
# always naively predict class 0
# -> our accuracy will be 3750 / 5610 = 66.8%

#sum(df$gender == "m") / length(df$gender)
library(caret)

conf_matrix <- confusionMatrix(y_pred, y_true)

print(conf_matrix)
# can we add more explanatory variables and increase performance?
# if 




