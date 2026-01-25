# Separability
# Supposing we have some samples potentially from an outbreak

# There's an outbreak and we collect demographics from phone interviews
infected_dataframe = data.frame(
  age=rnorm(n=1000,mean=25,sd=10),
  true_label=factor(1)
  )
not_infected_dataframe = data.frame(
  age=rnorm(n=1000,mean=65,sd=10),
  true_label=factor(0)
  )
data = rbind(infected_dataframe,not_infected_dataframe)

library(ggplot2)
ggplot(data=data,aes(x=age,color=true_label)) + geom_histogram() + geom_vline(xintercept=45.6)

# Training a classifier model
# Training a logistic regression model (or fitting)
model = glm(data=data,formula = true_label ~ age,family = binomial)

data$predictions = predict(model,type="response")

ggplot(data=data,aes(x=predictions)) + geom_histogram()

data$predicted_label = ifelse(data$predictions > .5,0,1)

sum(data$true_label == data$predicted_label) / length(data$true_label)

table(True=data$true_label,Predicted=data$predicted_label)

predict(model,new_data=25)

coefficients <- coef(model)
coefficients

coefficients["(Intercept)"] + coefficients["age"] * 45.6

