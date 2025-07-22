# ------------------------------------------------------------------------------
# Author:   Jake Williams
# Date:     3 Jun 2025
# Purpose:  To create a function to identify possible airport layovers between
#           two stated airports. 
# Data:     OpenFlights routes data [1]
#-------------------------------------------------------------------------------

library(tidyverse)
library(airportr)

# Load data----

# [1] Route data from OpenFlights
routes <- readr::read_csv(
  file = "https://raw.githubusercontent.com/jpatokal/openflights/refs/heads/master/data/routes.dat",
  col_name = FALSE) %>% 
  dplyr::rename(airline = X1,
                airline_id = X2,
                source = X3,
                source_id = X4,
                dest = X5,
                des_id = X6,
                codeshare = X7,
                stops = X8,
                equipment = X9)


# Calculate route distances----
routes_unique_dist <- routes %>% 
  
  # Keep distinct routes
  dplyr::select(source, dest) %>% 
  dplyr::distinct() %>% 
  
  # Filter OpenFlights route data to only include routes to and from airports
  # that also appear in the airportr::airports dataset
  dplyr::filter(source %in% airportr::airports$IATA, 
                dest %in% airportr::airports$IATA) %>% 
  
  # Calculate distance for all routes
  dplyr::mutate(distance = purrr::pmap_dbl(list(source, dest), 
                                           airportr::airport_distance, 
                                           .progress = TRUE))

# Create get_distance() function----
# To look up distance in routes_unique_dist for use in get_layover() function
airport_distance2 <- function(origin, destination){
  routes_unique_dist %>% 
    dplyr::filter(source == origin,
                  dest == destination) %>% 
    dplyr::pull(distance)
}

# Create get_layovers() function----
get_layovers <- function(origin, destination){
  
  # Check for direct flight
  routes_origin <- routes_unique_dist %>% 
    dplyr::filter(source == origin)
  
  if(destination %in% routes_origin$dest){
    return(NA)
  }
  
  # Check for one layover
  legs <- dplyr::inner_join(
    routes_origin,
    dplyr::rename(routes_unique_dist, dest2 = dest),
    by = c('dest' = 'source')) %>% 
    dplyr::filter(source != dest2)
  
  if(destination %in% legs$dest2){
    out <- dplyr::filter(legs,
                         source == origin,
                         dest2 == destination) %>% 
      dplyr::mutate(dist = purrr::pmap_dbl(list(source, dest, dest2),
                                           ~airport_distance2(..1, ..2) +
                                             airport_distance2(..2, ..3))) %>% 
      dplyr::filter(dist == min(dist))
    
    return(paste(out$dest, sep = ";"))
  }
  
  # Check for two layovers
  legs <- dplyr::inner_join(
    legs,
    dplyr::rename(routes_unique_dist, dest3 = dest),
    by = c('dest2' = 'source'),
    relationship = 'many-to-many') %>%
    dplyr::filter(dest != dest3)
  
  if(destination %in% legs$dest3){
    out <- dplyr::filter(legs,
                         source == origin,
                         dest3 == destination) %>% 
      dplyr::mutate(dist = purrr::pmap_dbl(list(source, dest, dest2, dest3),
                                           ~airport_distance2(..1, ..2) +
                                             airport_distance2(..2, ..3) +
                                             airport_distance2(..3, ..4))) %>% 
      dplyr::filter(dist == min(dist))
    
    return(paste(out$dest, out$dest2, sep = ";"))
  }
  
  # Check for three layovers
  legs <- dplyr::inner_join(
    legs,
    dplyr::rename(routes_unique_dist, dest4 = dest),
    by = c('dest3' = 'source'),
    relationship = 'many-to-many') %>%
    dplyr::filter(dest2 != dest4)
  
  if(destination %in% legs$dest4){
    out <- dplyr::filter(legs,
                         source == origin,
                         dest4 == destination) %>% 
      dplyr::mutate(dist = purrr::pmap_dbl(list(source, dest, dest2, dest3, dest4),
                                           ~airport_distance2(..1, ..2) +
                                             airport_distance2(..2, ..3) +
                                             airport_distance2(..3, ..4) +
                                             airport_distance2(..4, ..5))) %>% 
      dplyr::filter(dist == min(dist))
    
    return(paste(out$dest, out$dest2, out$dest3, sep = ";"))
  }
  
  # Check for four layovers
  legs <- dplyr::inner_join(
    legs,
    dplyr::rename(routes_unique_dist, dest5 = dest),
    by = c('dest4' = 'source'),
    relationship = 'many-to-many') %>%
    dplyr::filter(dest3 != dest5)
  
  if(destination %in% legs$dest5){
    out <- dplyr::filter(legs,
                         source == origin,
                         dest5 == destination) %>% 
      dplyr::mutate(dist = purrr::pmap_dbl(list(source, dest, dest2, dest3, dest4, dest5),
                                           ~airport_distance2(..1, ..2) +
                                             airport_distance2(..2, ..3) +
                                             airport_distance2(..3, ..4) +
                                             airport_distance2(..4, ..5) +
                                             airport_distance2(..5, ..6))) %>% 
      dplyr::filter(dist == min(dist))
    
    return(paste(out$dest, out$dest2, out$dest3, out$dest4, sep = ";"))
  }
  
  # Check for five layovers
  legs <- dplyr::inner_join(
    legs,
    dplyr::rename(routes_unique_dist, dest6 = dest),
    by = c('dest5' = 'source'),
    relationship = 'many-to-many') %>%
    dplyr::filter(dest4 != dest6)
  
  if(destination %in% legs$dest5){
    out <- dplyr::filter(legs,
                         source == origin,
                         dest4 == destination) %>% 
      dplyr::mutate(dist = purrr::pmap_dbl(list(source, dest, dest2, dest3, dest4, dest5, dest6),
                                           ~airport_distance2(..1, ..2) +
                                             airport_distance2(..2, ..3) +
                                             airport_distance2(..3, ..4) +
                                             airport_distance2(..4, ..5) +
                                             airport_distance2(..5, ..6) +
                                             airport_distance2(..6, ..7))) %>% 
      dplyr::filter(dist == min(dist))
    
    return(paste(out$dest, out$dest2, out$dest3, out$dest4, out$dest5, sep = ";"))
  }
  
}