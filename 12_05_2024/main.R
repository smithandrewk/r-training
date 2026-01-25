# Text editors
# File extensions
# Running Rscripts
print("hello world")

# TODO: mutate and a dataframe
# write our own function
source("functions.R")

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
