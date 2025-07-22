# ------------------------------------------------------------------------------
# Author:   Jake Williams
# Date:     11 Jul 2025
# Purpose:  To organise UNFCCC COP attendance data for analysis 
#-------------------------------------------------------------------------------

# Load packages----
library(tidyverse)
library(janitor)
library(utils)
library(countrycode)
library(whoville)

# Load data----
# COP attendance data (https://doi.org/10.1038/s41597-024-03978-7)
cop_raw <- utils::read.table("data/cops.cleaned.translated.tab",
                             header = TRUE,
                             fill = TRUE)

# Filter data----
# Describe patients included in study
cop_raw_n <- cop_raw %>% 
  janitor::tabyl(Group_Type, Virtual) %>% 
  dplyr::rename("virtual" = "1",
                "in person" = "0")

# Filter data to only include delegates who attended in person, as a party or 
# observer delegate
cop_data <- cop_raw %>% 
  dplyr::filter(Group_Type %in% c("Parties", "Observer States"),
                Virtual == 0)

# Export data----
# Create Summary data for countries and cops
cop_summary <- cop_data %>% 
  dplyr::summarise(Number = n(),
                   .by = c("Meeting", "Delegation")) %>% 
  
  # Correct typo for Botswana
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == "Bostwana", "Botswana", Delegation)) %>% 
  
  # Rename Cape Verde as Cabo Verde
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == "Cape Verde", "Cabo Verde", Delegation)) %>% 
  
  # Rename Ivory Coast as Cote D'ivoire
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == "Ivory Coast", "Côte d'Ivoire", Delegation)) %>% 
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == "Cote D'ivoire", "Côte d'Ivoire", Delegation)) %>% 
  
  # Rename 'Swaziland' to 'Eswatini'
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == 'Swaziland', 'Eswatini', Delegation)) %>% 
  
  # Rename 'Former Yugoslav Republic of Macedonia' to 'North Macedonia'
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == 'The Former Yugoslav Republic of Macedonia', 'North Macedonia', Delegation)) %>% 
  
  # Rename 'Libyan Arab Jamahiriya' to 'Libya'
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == 'Libyan Arab Jamahiriya', 'Libya', Delegation)) %>% 
  
  # Rename 'European Community' to 'European Union'
  dplyr::mutate(Delegation = dplyr::if_else(Delegation == 'European Community', 'European Union', Delegation)) %>% 
  
  # Add ISO country code to delegation
  dplyr::mutate(Delegation_ISO = dplyr::case_when(Delegation == "European Union" ~ NA,
                                                  Delegation == "Yugoslavia" ~ "YUG",
                                                  TRUE ~ whoville::names_to_iso3(Delegation))) %>% 
  # Add World Bank group for delegation
  dplyr::mutate(Delegation_wb = dplyr::case_when(Delegation_ISO == "COK" ~ "East Asia & Pacific",
                                                 Delegation_ISO == "VAT" ~ "Europe & Central Asia",
                                                 Delegation_ISO == "NIU" ~ "East Asia & Pacific",
                                                 Delegation_ISO == "YUG" ~ "Europe & Central Asia",
                                                 Delegation == "European Union" ~ "Europe & Central Asia",
                                                 TRUE ~ whoville::iso3_to_regions(iso3 = Delegation_ISO, region = "wb_region"))) %>% 
  
  # Add ISO country code to host
  dplyr::mutate(Host_ISO = case_when(
    Meeting == "COP 1"  ~ "DEU", # Germany
    Meeting == "COP 2"  ~ "CHE", # Switzerland
    Meeting == "COP 3"  ~ "JPN", # Japan
    Meeting == "COP 4"  ~ "ARG", # Argentina
    Meeting == "COP 5"  ~ "DEU", # Germany
    Meeting == "COP 6"  ~ "NLD", # Netherlands
    Meeting == "COP 6-2"~ "DEU", # Germany
    Meeting == "COP 7"  ~ "MAR", # Morocco
    Meeting == "COP 8"  ~ "IND", # India
    Meeting == "COP 9"  ~ "ITA", # Italy
    Meeting == "COP 10" ~ "ARG", # Argentina
    Meeting == "COP 11" ~ "CAN", # Canada
    Meeting == "COP 12" ~ "KEN", # Kenya
    Meeting == "COP 13" ~ "IDN", # Indonesia
    Meeting == "COP 14" ~ "POL", # Poland
    Meeting == "COP 15" ~ "DNK", # Denmark
    Meeting == "COP 16" ~ "MEX", # Mexico
    Meeting == "COP 17" ~ "ZAF", # South Africa
    Meeting == "COP 18" ~ "QAT", # Qatar
    Meeting == "COP 19" ~ "POL", # Poland
    Meeting == "COP 20" ~ "PER", # Peru
    Meeting == "COP 21" ~ "FRA", # France
    Meeting == "COP 22" ~ "MAR", # Morocco
    Meeting == "COP 23" ~ "DEU", # Germany
    Meeting == "COP 24" ~ "POL", # Poland
    Meeting == "COP 25" ~ "ESP", # Spain
    Meeting == "COP 26" ~ "GBR", # United Kingdom
    Meeting == "COP 27" ~ "EGY", # Egypt
    Meeting == "COP 28" ~ "ARE", # United Arab Emirates
    Meeting == "COP 29" ~ "AZE")) %>% # Azerbaijan
  
  # Add country name
  dplyr::mutate(Host_country = countrycode::countrycode(sourcevar = Host_ISO,
                                                        origin = "iso3c",
                                                        destination = "country.name.en")) %>% 
  
  # Add world bank group for host
  dplyr::mutate(Host_wb = whoville::iso3_to_regions(iso3 = Host_ISO,
                                                    region = "wb_region"))
