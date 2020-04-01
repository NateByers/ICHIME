# https://penn-chime.phl.io/
# Calculate number of recovered individuals

calc_recovered1 <- function(gamma, infected0, recovered0) {
  gamma*infected0 + recovered0
}