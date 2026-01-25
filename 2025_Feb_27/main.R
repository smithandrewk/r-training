# Machine Learning
# Powered by data
# Hypothetical model:
# Classification model
# Does this individual have heart disease or not? (dependent variable or target or label)
# inputs factors, features, independent variables
# 1. cholesterol measurement
# takes on a value from 0 to 100
# pick a threshold such that separates classes optimally
# for example 80 cholesterol less than means no heart disease
# greater than 80 there's a high chance of heart disease

library(ggplot2)
library(GGally)

cholesterol <- runif(1000,min=150,max=300)
age <- runif(1000,min=30,max=80)
blood_pressure <- runif(1000, 100, 180)

heart_disease <- ifelse(cholesterol > 200 & age > 50 & blood_pressure > 140,1,0)
noise <- rbinom(n, 1, 0.1) * sample(c(-1, 1), n, replace = TRUE)
heart_disease <- pmax(pmin(heart_disease + noise, 1), 0)
df <- data.frame(cholesterol,age,blood_pressure,heart_disease=as.factor(heart_disease))

ggpairs(df,columns=1:3,aes(color=heart_disease))

# Decision nice for public health
# It gives you the rules afterward ( interpretability )

library(rpart) # gini coefficient
library(rpart.plot)

tree <- rpart(data=df, heart_disease ~ .)
rpart.plot(tree)

y_pred <- predict(tree,df,type='class')

cm <- confusionMatrix(y_pred,df$heart_disease)
cm_df <- as.data.frame(cm$table)
ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() + geom_text(aes(label=Freq),color="white")










