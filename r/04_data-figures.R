# ------------------------------------------------------------------------------
# Author:   Jake Williams
# Date:     15 July 2025
# Purpose:  To summarise data for figures
#-------------------------------------------------------------------------------

# Libraries----
library(geosphere)
library(maps)
library(sf)

# Data for Figure 1----

# Create summary data by world bank region
cop_summary_delegation_wb <- cop_summary %>% 
  dplyr::summarise(Number = sum(Number),
                   .by = c(Meeting, Host_country, Delegation_wb)) %>% 
  dplyr::mutate(Meeting = factor(Meeting,
                                 levels = c("COP 1", "COP 2", "COP 3", "COP 4", "COP 5", "COP 6", "COP 7", "COP 8", "COP 9", 
                                            "COP 10", "COP 11", "COP 12", "COP 13", "COP 14", "COP 15", "COP 16", "COP 17", 
                                            "COP 18", "COP 19", "COP 20", "COP 21", "COP 22", "COP 23", "COP 24", "COP 25", 
                                            "COP 26", "COP 27", "COP 28", "COP 29"))) %>% 
  dplyr::mutate(meeting_country = paste0(Meeting, " (", Host_country, ")"),
                meeting_country = if_else(Meeting %in% c("COP 2", "COP 5", "COP 9", 
                                                         "COP 23", "COP 25"),
                                          paste0(meeting_country, "^"),
                                          meeting_country),
                meeting_country = factor(meeting_country, 
                                         levels = unique(meeting_country[order(as.numeric(Meeting))])))

# Create cop host summary data
cop_summary_host <- cop_summary %>% 
  dplyr::mutate(emissions_total = emissions * Number) %>% 
  dplyr::summarise(emissions_total = sum(emissions_total),
                   Number = sum(Number),
                   .by = c(Meeting, Host_country, Host_wb, Host_country)) %>% 
  dplyr::mutate(Meeting = factor(Meeting,
                                 levels = c("COP 1", "COP 2", "COP 3", "COP 4", "COP 5", "COP 6", "COP 7", "COP 8", "COP 9", 
                                            "COP 10", "COP 11", "COP 12", "COP 13", "COP 14", "COP 15", "COP 16", "COP 17", 
                                            "COP 18", "COP 19", "COP 20", "COP 21", "COP 22", "COP 23", "COP 24", "COP 25", 
                                            "COP 26", "COP 27", "COP 28", "COP 29"))) %>% 
  dplyr::mutate(emissions_person = emissions_total / Number) %>% 
  dplyr::mutate(meeting_country = paste0(Meeting, " (", Host_country, ")"),
                meeting_country = if_else(Meeting %in% c("COP 2", "COP 5", "COP 9", 
                                                         "COP 23", "COP 25"),
                                          paste0(meeting_country, "^"),
                                          meeting_country),
                meeting_country = factor(meeting_country, 
                                         levels = unique(meeting_country[order(as.numeric(Meeting))])))


# Data for Figure 2----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Add data
cop_map <- cop_summary %>% 
  dplyr::mutate(emissions_total = emissions * Number) %>% 
  dplyr::select(Meeting, Delegation_ISO, emissions_total, Number) %>%
  dplyr::summarise(emissions_total = sum(emissions_total),
                   number_total = sum(Number),
                   .by = "Delegation_ISO") %>% 
  dplyr::mutate(emissions_attendee = emissions_total / number_total) %>% 
  dplyr::left_join(world,
                   by = c("Delegation_ISO" = "iso_a3_eh"))

# Set as sf object
cop_map <- sf::st_as_sf(cop_map)

# Airport data
airport_cop <- airport_cop %>% 
  left_join(airportr::airports %>% 
              dplyr::select(IATA, Longitude, Latitude),
            by = c("Dest" = "IATA"))

# Data for Appendix Table D2----
cop_summary_sens <- cop_summary %>% 
  dplyr::mutate(emissions_sens_1_total = emissions_sens_1 * Number,
                emissions_sens_2_total = emissions_sens_2 * Number,
                emissions_sens_3_total = emissions_sens_3 * Number) %>% 
  dplyr::summarise(emissions_sens_1_total = sum(emissions_sens_1_total),
                   emissions_sens_2_total = sum(emissions_sens_2_total),
                   emissions_sens_3_total = sum(emissions_sens_3_total),
                   number = sum(Number),
                   .by = "Meeting") %>% 
  dplyr::mutate(emissions_sens_1_person = emissions_sens_1_total / number,
                emissions_sens_2_person = emissions_sens_2_total / number,
                emissions_sens_3_person = emissions_sens_3_total / number)

# Data for Appendix Figure B2----

# Map data

# Create function to restructure data
structure_routes <- function(origin, dest, layover) {
  
  # If no layovers
  if(is.na(layover)) {
    
    tibble(A = origin,
           B = dest)
    
    # If layovers  
  } else {
    stops <- c(origin,
               unlist(stringr::str_split(layover, ";")),
               dest)
    
    tibble(A = head(stops, -1),
           B = tail(stops, -1))
  }
}

# Run restructure function
routes_figure <- cop_summary %>%
  dplyr::filter(Meeting == "COP 29",
                !is.na(Origin)) %>% 
  dplyr::select(Origin, Dest, layover) %>% 
  purrr::pmap_dfr(~ structure_routes(..1, ..2, ..3)) %>% 
  unique() %>% 
  
  # Add coordinates
  dplyr::left_join(airportr::airports %>% 
                     dplyr::select(IATA, Latitude, Longitude),
                   by = c("A" = "IATA")) %>% 
  dplyr::left_join(airportr::airports %>% 
                     dplyr::select(IATA, Latitude, Longitude),
                   by = c("B" = "IATA")) %>% 
  dplyr::rename(A_lat = Latitude.x,
                A_lon = Longitude.x,
                B_lat = Latitude.y,
                B_lon = Longitude.y)

# Data by flight segment
route_figure_segment <- pmap_dfr(routes_figure, function(A, B, A_lat, A_lon, B_lat, B_lon) {
  path <- geosphere::gcIntermediate(
    c(A_lon, A_lat), 
    c(B_lon, B_lat), 
    n = 100, 
    addStartEnd = TRUE, 
    sp = FALSE, 
    breakAtDateLine = TRUE
  )
  
  # Handle split paths (list output)
  if (is.list(path)) {
    path_df <- bind_rows(lapply(path, function(p) tibble(lon = p[, 1], lat = p[, 2])))
  } else {
    path_df <- tibble(lon = path[, 1], lat = path[, 2])
  }
  
  path_df %>%
    mutate(route_id = paste(A, B, sep = "-"))
})

# Correct when path crosses International Date Line
route_figure_segment <- route_figure_segment %>%
  group_by(route_id) %>%
  mutate(
    lon_diff = abs(lag(lon, default = first(lon)) - lon),
    segment = cumsum(if_else(lon_diff > 180, 1, 0))
  ) %>%
  ungroup()