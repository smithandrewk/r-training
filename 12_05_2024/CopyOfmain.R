# Text editors
# File extensions
# Running Rscripts
print("hello world")

cases_october = 500
cases_november = 500
cases_december = 1000
population_october = 5000
population_november = 6000
population_december = 7000
attack_rate_october = 100 * (cases_october / population_october)
attack_rate_november = 100 * (cases_november / population_november)
attack_rate_december = 100 * (cases_december / population_december)

if (attack_rate_october < 25) {
  print("low risk")
} else if (attack_rate_october < 75) {
  print("medium risk")
} else {
  print("high risk")
}
if (attack_rate_november < 25) {
  print("low risk")
} else if (attack_rate_november < 75) {
  print("medium risk")
} else {
  print("high risk")
}
if (attack_rate_december < 25) {
  print("low risk")
} else if (attack_rate_december < 75) {
  print("medium risk")
} else {
  print("high risk")
}


# TODO: mutate and a dataframe
# write our own function

calculate_attack_rate <- function(cases,population) {
  attack_rate = 100 * (cases/population)
  return(attack_rate)
}
determine_risk_from_attack_rate <- function(attack_rate) {
  if (attack_rate < 25) {
    print("low risk")
  } else if (attack_rate < 75) {
    print("medium risk")
  } else {
    print("high risk")
  }
}


build_forecasting_model_for_pertussis_data <- function(number_of_weeks_to_forecast) {
  
}
build_forecasting_models_for_pertussis_data_over_forecasting_weeks <- function(list_of_number_of_weeks_to_build_models_from) {
  for (number_of_weeks_to_forecast in list_of_number_of_weeks_to_build_models_from) {
    build_forecasting_model_for_pertussis_data(10)  
  }
}


build_forecasting_models_for_pertussis_data_over_forecasting_weeks(c(10,20,30,40,50))
build_forecasting_models_for_pertussis_data_over_forecasting_weeks(c(10,20,30,40,50,52,54,56))














cases_october = 2500
cases_november = 500
cases_december = 1000
population_october = 5000
population_november = 6000
population_december = 7000

attack_rate_october = calculate_attack_rate(cases_october,population_october) 
attack_rate_november = calculate_attack_rate(cases_november,population_november) 
attack_rate_december = calculate_attack_rate(cases_december,population_december) 

determine_risk_from_attack_rate(attack_rate = attack_rate_october)
determine_risk_from_attack_rate(attack_rate = attack_rate_november)
determine_risk_from_attack_rate(attack_rate = attack_rate_december)
