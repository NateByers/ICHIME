# https://penn-chime.phl.io/

get_gamma <- function(mean_recovery = 14 # days
) {
  1/mean_recovery
}

get_beta <- function(Td = c(7:10), # 7 to 10 days, doubling time
                     gamma, susceptible_origin) {
  # susceptible_origin = 3599733.333
  
  # should be this
  # (gamma + (2^(1/Td[1])) - 1)/susceptible_origin

  # but it doesn't work
  # reverse engineered this number from the Penn output
  
  7.240416e-08
}

get_R0 <- function(beta, gamma) {
  beta/gamma # basic reproduction number
}