# https://penn-chime.phl.io/
# Calculate number of susceptible individuals

calc_susceptible1 <- function(beta, susceptible0, infected0) {
  -beta*susceptible0*infected0 + susceptible0
}