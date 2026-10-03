# 9 - Create maps of true distributions and occurrence points ####
#--------------------------------------------------------------------------#
# This script creates maps of the six virtual species in the United States.
# The true presence-absence distributions are shown together with the
# environmentally filtered occurrence points used for modelling.
# The resulting figure is saved for the appendix.
#--------------------------------------------------------------------------#

# 9.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
#------------------------------------------------#

#Europe Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")

#USA Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data/usa")

#Europe Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")
#setwd("D:/SDM/Project/data/usa")


# 9.1 - Load packages ####
#--------------------------------------------------------------------------#

list.of.packages <- c("terra", "sf", "ggplot2", "dplyr", "geodata")

for (pkg in list.of.packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}


# 9.2 - Load country boundaries ####
#--------------------------------------------------------------------------#

world <- geodata::world(
  path = "bioclim/world",
  resolution = 5
)

world_sf <- sf::st_as_sf(world)

# Fix invalid geometries
sf::sf_use_s2(FALSE)
world_sf <- sf::st_make_valid(world_sf)

# Crop to USA
usa_extent <- sf::st_bbox(
  c(
    xmin = -125,
    xmax = -65,
    ymin = 25,
    ymax = 50
  ),
  crs = sf::st_crs(world_sf)
)

usa_sf <- sf::st_crop(
  world_sf,
  usa_extent
)


# 9.3 - Define species ####
#--------------------------------------------------------------------------#

species_names <- c(
  "Marmatharia_Medusa",
  "Stellaria_Petrus",
  "Idella_Wiesellus",
  "Alpina_Montis",
  "Luminaria_Aqua",
  "Deserta_Solensis"
)

species_labels <- c(
  "Marmatharia medusa",
  "Stellaria petrus",
  "Idella wiesellus",
  "Alpina montis",
  "Luminaria aqua",
  "Deserta solensis"
)


# 9.4 - Load virtual species ####
#--------------------------------------------------------------------------#

load_species <- function(species_name) {
  file <- paste0("virtual_species/", species_name, ".RData")
  load(file)
  species_object <- get(paste0(species_name, "PA"))
  terra::rast(species_object$pa.raster)
}

species_rasters <- lapply(species_names, load_species)
names(species_rasters) <- species_names


# 9.5 - Convert presence-absence rasters to data frames ####
#--------------------------------------------------------------------------#

create_distribution_df <- function(raster, species_name) {
  
  names(raster) <- "presence"
  
  df <- terra::as.data.frame(
    raster,
    xy = TRUE,
    na.rm = TRUE
  )
  
  df$species <- species_name
  
  return(df)
}

distribution_list <- list()

for (species in species_names) {
  
  distribution_list[[length(distribution_list) + 1]] <- 
    create_distribution_df(
      species_rasters[[species]],
      species
    )
}

distribution_df <- dplyr::bind_rows(distribution_list)


# 9.6 - Keep only true presence cells ####
#--------------------------------------------------------------------------#

distribution_df <- distribution_df[
  distribution_df$presence == 1,
]


# 9.7 - Add species labels ####
#--------------------------------------------------------------------------#

distribution_df$species_label <- factor(
  distribution_df$species,
  levels = species_names,
  labels = species_labels
)


# 9.8 - Create map ####
#--------------------------------------------------------------------------#

map_plot <- ggplot() +
  
  # Country boundaries
  geom_sf(
    data = usa_sf,
    fill = NA,
    linewidth = 0.25
  ) +
  
  # True distribution
  geom_raster(
    data = distribution_df,
    aes(
      x = x,
      y = y
    ),
    fill = "lightgreen"
  ) +
  
  facet_wrap(
    ~ species_label,
    ncol = 3
  ) +
  
  coord_sf(
    xlim = c(-125, -65),
    ylim = c(25, 50),
    expand = FALSE
  ) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 9)
  )


# 9.9 - Display map ####
#--------------------------------------------------------------------------#

plot(map_plot)


# 9.10 - Save map ####
#--------------------------------------------------------------------------#

output_dir <- "virtual_species/maps"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

ggsave(
  filename = file.path(
    output_dir,
    "true_distribution_USA.png"
  ),
  plot = map_plot,
  width = 6,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    output_dir,
    "true_distribution_USA.pdf"
  ),
  plot = map_plot,
  width = 10,
  height = 12
)
