# ------------------------------------------------------------------------------
# Author:   Jake Williams
# Date:     23 Jun 2025
# Purpose:  To determine flight routes for each country and COP combination  
#-------------------------------------------------------------------------------

# Load packages----
library(tidyverse)

# Load data----

# Airports for each country
airport_country <- readxl::read_excel("data/country_unique_JWPC_final.xlsx")

# Airports for each COP
airport_cop <- dplyr::tibble(COP = paste0("COP ", 1:29),
                             Dest = c("SXF", "GVA", "KIX", "EZE", "CGN", "RTM",
                                      "RAK", "DEL", "MXP", "EZE", "YUL", "NBO",
                                      "DPS", "POZ", "CPH", "CUN", "DUR", "DOH", 
                                      "WAW", "LIM", "CDG", "RAK", "CGN", "KTW", 
                                      "MAD", "GLA", "SSH", "DXB", "GYD"
                             ))

# Load functions----

# Load get_layovers() function
source("code/get_layovers.R")

# Identify flight routes----
cop_summary <- cop_summary %>% 
  
  # Add origin and destination airports
  dplyr::left_join(airport_country, by = c("Delegation" = "Country")) %>% 
  dplyr::left_join(airport_cop, by = c("Meeting" = "COP")) %>%
  
  # For all host countries set airports to NA
  dplyr::mutate(Origin = dplyr::if_else(Host_ISO == Delegation_ISO &
                                          !is.na(Delegation_ISO), NA, Origin),
                Dest = dplyr::if_else(Host_ISO == Delegation_ISO &
                                        !is.na(Delegation_ISO), NA, Dest)) %>% 
  
  # Run get_layovers where origin or dest is not NA
  dplyr::mutate(layover = pmap_chr(list(Origin, Dest), function(o, d) {
    if (is.na(o) || is.na(d)) return(NA_character_)
    get_layovers(o, d)
  }, .progress = TRUE))
