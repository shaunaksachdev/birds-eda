library(viridis)
library(sf)
library(geojsonio)
library(tidyverse)
library(rmapshaper)
library(paletteer)

# Read the GeoJSON file with the correct file path
canada_cd <- st_read("/Users/shaunaksachdev/Desktop/Lab5/canada_cd_sim.geojson", quiet = TRUE)
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Define the maps' theme
theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0))
}

test = list("CA.MB" = 1, "CA.BC" = 2, "CA.AB" = 3, "CA.SK" = 4,
            "CA.ON" = 5, "CA.QC" = 6, "CA.NL" = 7, "CA.NS" = 8, 
            "CA.NB" = 9, "CA.PE" = 10, "CA.NT" = 11, "CA.NU" = 12, "CA.YT" = 13)

# Function to create the map
canada = function(title, heat) {
  
  flat = unlist(heat)
  minf = min(flat)
  maxf = max(flat)
  
  hash = list("CA.MB" = "Manitoba", "CA.BC" = "British Columbia / Colombie-Britannique", 
              "CA.AB" = "Alberta", "CA.SK" = "Saskatchewan", "CA.ON" = "Ontario",
              "CA.QC" = "Quebec / Québec", "CA.NL" = "Newfoundland and Labrador / Terre-Neuve-et-Labrador",
              "CA.NS" = "Nova Scotia / Nouvelle-Écosse", "CA.NB" = "New Brunswick / Nouveau-Brunswick",
              "CA.PE" = "Prince Edward Island / Île-du-Prince-Édouard", 
              "CA.NT" = "Northwest Territories / Territoires du Nord-Ouest", 
              "CA.NU" = "Nunavut", "CA.YT" = "Yukon")
  
  col = canada_cd$PRNAME
  vals = rep(0, length(col))
  
  for (i in 1:13) {
    what = names(heat)[[i]]
    vals[hash[[what]] == col] = heat[[i]]
  }
  canada_cd$values = vals
  
  # Plot the map
  g = ggplot() +
    geom_sf(aes(fill = values), color = "gray60", size = 0.1, data = canada_cd) +
    coord_sf(crs = crs_string) +
    scale_fill_viridis(option = "plasma", name = title) +
    guides(fill = "colorbar") +
    labs(fill = title) + 
    theme_map() +
    theme(panel.grid.major = element_line(color = "white"),
          legend.key = element_rect(color = "gray40", size = 0.1))
  
  print(g)
}