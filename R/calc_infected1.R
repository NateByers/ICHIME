# https://penn-chime.phl.io/
# Calculate number of infected individuals

calc_infected1 <- function(beta, susceptible0, infected0, gamma) {
  (beta*susceptible0*infected0 - gamma*infected0) + infected0
}

