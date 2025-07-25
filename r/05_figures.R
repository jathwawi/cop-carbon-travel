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

# Figures---- 

# Figure 1

## Panel A
f1a <- ggplot(data = cop_summary_host,
              aes(x = Meeting)) +
  geom_bar(aes(y = emissions_total), 
           stat = "identity",
           fill = "#cab2d6") +
  geom_line(aes(y = Number*max(emissions_total) / max(Number), group = 1), color = "#6a3d9a") +
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
f1b <- ggplot(data = cop_summary_host,
              aes(x = meeting_country,
                  y = emissions_person,
                  fill = Host_wb)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "UNFCCC Conference of the Parties Number",
                y = expression("GHG (t CO"[2]*"-e / person)"),
                fill = "Host region") +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank()
  )

## Panel C
f1c <- ggplot(data = cop_summary_delegation_wb,
              aes(fill = Delegation_wb,
                  x = meeting_country,
                  y = Number)) +
  geom_bar(position = "fill",
           stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  theme_classic() +
  labs(x = "UNFCCC Conference of the Parties Number",
       y = "% attendees in study",
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

# Figure 2

## Panel A
f2a <- ggplot(cop_map) +
  geom_sf(aes(fill = emissions_attendee),
          col = "#b0b0b0",
          linewidth = 0.01) +
  scale_fill_gradient(high = "#33a02c", low = "#a6cee3", na.value = "grey90") +
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