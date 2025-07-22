# ------------------------------------------------------------------------------
# Author:   Jake Williams
# Date:     3 Jun 2025
# Purpose:  To create a function to calculate carbon emissions from flight 
#           route data stored in cop_routes
#-------------------------------------------------------------------------------

library(tidyverse)
library(carbonr)

# Create function get_emissions()----
get_emissions <- function(from, to, layover, round_trip, class, rf, wtt) {
  
  # If no layovers are entered set via to NULL, otherwise generate a character vector
  if (is.na(layover)) {
    v <- NULL
  } else {
    v <- strsplit(layover, ";")[[1]]
  }
  
  # Run carbonr::airplane_emissions()
  carbonr::airplane_emissions(from = from,
                              to = to,
                              via = v,
                              round_trip = round_trip, 
                              class = class,
                              radiative_force = rf,
                              include_WTT = wtt)
}
