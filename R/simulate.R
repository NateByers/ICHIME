library(dplyr)


simulate <- function(susceptible_origin, infected_origin, recovered_origin,
                     mean_recovery, doubling_time, day_total,
                     date_origin = as.Date("2020-03-15")) {
  
  gamma <- get_gamma(mean_recovery)
  beta <- get_beta(doubling_time, gamma, susceptible_origin)
  
  sir <- data.frame(day = 1,
                    date = date_origin,
                    susceptible = susceptible_origin,
                    infected = infected_origin,
                    recovered = recovered_origin) 
  
  for(i in 1:day_total) {
    # i <- 1
    sir_ <- sir %>%
      dplyr::slice(i)
    
    next_day <- forecast1(susceptible0 = sir_$susceptible,
                          infected0 = sir_$infected,
                          recovered0 = sir_$recovered, gamma, beta) %>%
      dplyr::mutate(day = sir_$day + 1,
                    date = sir_$date + 1)
    
    sir <- dplyr::bind_rows(sir, next_day)
  }
  
  sir
  
}