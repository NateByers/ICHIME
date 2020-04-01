# forecast one day

forecast1 <- function(susceptible0, infected0, recovered0, gamma, beta) {
  
  susceptible1 <- calc_susceptible1(beta, susceptible0, infected0)
  infected1 <- calc_infected1(beta, susceptible0, infected0, gamma)
  recovered1 <- calc_recovered1(gamma, infected0, recovered0)
  
  data.frame(susceptible = susceptible1,
             infected = infected1,
             recovered = recovered1)
}