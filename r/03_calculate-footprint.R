# ------------------------------------------------------------------------------
# Author:   Jake Williams
# Date:     23 June 2025
# Purpose:  To estimate GHG from flight travel from all previous COPs
#-------------------------------------------------------------------------------


# Load functions----
source("code/get_emissions.R")

# Estimate travel emissions----

# Calculate emissions for a flight between the origin and destination via any 
# layovers per person
cop_summary <- cop_summary %>%
  mutate(
    emissions = pmap_dbl(
      list(Origin, Dest, layover),
      function(o, d, l) {
        if (any(is.na(c(o, d)))) return(NA_real_)
        get_emissions(o, 
                      d, 
                      l, 
                      round_trip = TRUE, 
                      class = "Average passenger",
                      rf = TRUE,
                      wtt = TRUE)
      },
      .progress = TRUE
    ),
    
    # Sensitivity analysis - no radiative force or wtt emissions
    emissions_sens_1 = pmap_dbl(
      list(Origin, Dest, layover),
      function(o, d, l) {
        if (any(is.na(c(o, d)))) return(NA_real_)
        get_emissions(o, 
                      d, 
                      l,
                      round_trip = TRUE,
                      class = "Average passenger",
                      rf = FALSE,
                      wtt = FALSE)
      },
      .progress = TRUE
    ),
    
    # Sensitivity analysis - yes radiative force no or wtt emissions
    emissions_sens_2 = pmap_dbl(
      list(Origin, Dest, layover),
      function(o, d, l) {
        if (any(is.na(c(o, d)))) return(NA_real_)
        get_emissions(o, 
                      d, 
                      l,
                      round_trip = TRUE,
                      class = "Average passenger",
                      rf = TRUE,
                      wtt = FALSE)
      },
      .progress = TRUE
    ),
    
    # Sensitivity analysis - no radiative force, yes wtt emissions
    emissions_sens_3 = pmap_dbl(
      list(Origin, Dest, layover),
      function(o, d, l) {
        if (any(is.na(c(o, d)))) return(NA_real_)
        get_emissions(o, 
                      d, 
                      l,
                      round_trip = TRUE,
                      class = "Average passenger",
                      rf = FALSE,
                      wtt = TRUE)
      },
      .progress = TRUE
    )
  )

# Set emissions for host countires to zero
cop_summary <- cop_summary %>% 
  dplyr::mutate(emissions = dplyr::if_else(is.na(emissions), 0, emissions),
                emissions_sens_1 = dplyr::if_else(is.na(emissions_sens_1), 0, emissions_sens_1),
                emissions_sens_2 = dplyr::if_else(is.na(emissions_sens_2), 0, emissions_sens_2),
                emissions_sens_3 = dplyr::if_else(is.na(emissions_sens_3), 0, emissions_sens_3))

# Write cop_summary dataset
readr::write_csv(cop_summary, "outputs/cop_summary.csv")