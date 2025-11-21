# ------------------------------------------------------------------------------
# Author:   Jake Williams
# Date:     23 June 2025
# Purpose:  Create figures to visualise carbon emissions by COP
#-------------------------------------------------------------------------------

# Load libraries----
library(ggplot2)
library(patchwork)
library(geosphere)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(RColorBrewer)
library(cowplot)

# Figures---- 

# Figure 1

## Panel A
f1a <- ggplot(data = cop_summary_host,
              aes(fill = Source,
                  x = Meeting,
                  y = value)) +
  geom_bar(position = "stack", 
           stat = "identity") +
  scale_fill_manual(values = c("#A6CEE3", "#377eb8", "#245882")) +
  geom_line(aes(y = Number*max(emissions_total) / max(Number), group = 1), color = "black") +
  scale_y_continuous(
    name = expression("GHG (t CO"[2]*"-e)"),
    breaks = pretty(cop_summary_host$emissions_total, n = 10),
    sec.axis = sec_axis(~ . * max(cop_summary_host$Number, na.rm = TRUE) / max(cop_summary_host$emissions_total, na.rm = TRUE),
                        name = "No. attendees in study")
  ) +
  labs(x = "UNFCCC Conference of the Parties (COP) Meeting Number") +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )

## Panel B
cop_summary_host <- cop_summary_host %>% 
  dplyr::mutate(source_person = value / Number)

f1b <- ggplot(data = cop_summary_host,
              aes(x = meeting_country,
                  y = source_person,
                  fill = interaction(Source, Host_wb))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#fb9a99", "#e31a1c", "#991111", "#b2df8a", 
                               "#33a02c", "#1D5C1A", "#fdbf6f", "#ff7f00",
                               "#d95f02", "#cab2d6", "#984ea3", "#6a3d9a",
                               "#ffffb3", "#ffff33", "#BDBD26", "#B35B2788",
                               "#b15928", "#592D13", "#FA82C083", "#f781bf",
                               "#804463")) +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "UNFCCC Conference of the Parties Number",
                y = expression("GHG (t CO"[2]*"-e / person)"),
                fill = "Host region") +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  theme(legend.position = "none")
  

## Panel C
f1c <- ggplot(data = cop_summary_delegation_wb,
              aes(fill = Delegation_wb,
                  x = meeting_country,
                  y = Number)) +
  geom_bar(position = "fill",
           stat = "identity") +
  scale_fill_manual(values = c("#e31a1c", "#33a02c", "#984ea3", 
                               "#ff7f00", "#ffff33", "#b15928", "#f781bf")) +
  theme_classic() +
  labs(x = "UNFCCC Conference of the Parties Number",
       y = "Proportion of attendees",
       fill = "Delegation region") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(legend.key.size = unit(5, 'mm'))


## Draw Figure 1
f1 <- (f1a / f1b / f1c) + 
  plot_annotation(tag_levels = 'A',
                  theme = theme(plot.caption = element_text(hjust = 0)))

ggplot2::ggsave("outputs/figure_1.png",
                plot = f1,
                width = 180,
                height = 200,
                units = "mm",
                dpi = 300)
# NOTE: HOST FIGURE LEGEND ADDED MANUALLY

# Figure 2

## Panel A
f2a <- ggplot(cop_map) +
  geom_sf(aes(fill = emissions_attendee),
          col = "#b0b0b0",
          linewidth = 0.01) +
  scale_fill_viridis_c(na.value = "grey90") + 
  theme_void() +
  labs(title = "Emissions per attendee (all COPs)", fill = "GHG (tonnes CO2-e)")

## Panel B
f2b <- ggplot(data = world) +
  geom_sf(col = "#b0b0b0",
          fill = "grey90",
          linewidth = 0.1) +
  geom_point(data = airport_cop,
             aes(x = Longitude,
                 y = Latitude),
             color = "darkgreen",
             size = 0.5) +
  geom_text_repel(data = airport_cop,
                  aes(x = Longitude,
                      y = Latitude,
                      label = COP),
                  size = 2,
                  max.overlaps = Inf,
                  force = 3,
                  segment.size = 0.2,
                  min.segment.length = 0.1) + 
  theme_void() +
  labs(title = "COP host locations")

## Figure 2
f2 <- (f2a / f2b) +
  plot_annotation(tag_levels = "A")

ggplot2::ggsave("outputs/figure_2.png",
                plot = f2,
                width = 180,
                units = "mm",
                dpi = 300)

# Appendix Figure A

faa <- ggplot(data = world) +
  geom_sf(fill = "#e0e0e0", color = "#e0e0e0") +
  geom_point(data = routes_figure, aes(x = A_lon, y = A_lat), color = "darkgreen", size = 0.2) +
  geom_point(data = routes_figure, aes(x = B_lon, y = B_lat), color = "darkgreen", size = 0.2) +
  geom_path(data = route_figure_segment, aes(x = lon, y = lat, group = interaction(route_id, segment)), color = "darkgreen", size = 0.3, alpha = 0.5) +
  theme_void()

## Draw Appendix Figure A
ggplot2::ggsave("outputs/supplementary_figure_a.png",
                plot = faa,
                width = 180,
                height = 200,
                units = "mm",
                dpi = 300)
