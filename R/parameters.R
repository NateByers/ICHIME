# https://penn-chime.phl.io/

get_gamma <- function(mean_recovery = 14 # days
) {
  1/mean_recovery
}

get_beta <- function(Td = c(7:10), # 7 to 10 days, doubling time
                     gamma, susceptible_origin) {
  
  (gamma + (2^(1/Td[1])) - 1)/susceptible_origin

}

get_R0 <- function(beta, gamma) {
  beta/gamma # basic reproduction number
}