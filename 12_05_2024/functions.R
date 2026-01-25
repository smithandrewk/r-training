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