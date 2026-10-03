# 4 - Generate background points for all 6 virtual species ####
#-----------------------------------------------------------------------------------#
# This script generates background points for all 6 virtual species, using the
# training data (80% of the original occurrence points) and the environmental layers.
# 3 values for the number of background points: (5000 / 1x occ / 5x occ)
# x 3 buffer-types (fix 100 km / fix 200 km / adaptive, 95%-quantile NN-distance)
# x 3 spatial-weighting (0 / 0.5 / 0.75)
# x 4 replicates (independent draws per combination)
# = 108 Background-Point-Sets per species, 648 Sets in total
#-----------------------------------------------------------------------------------#

# 4.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
#------------------------------------------------#

#Europe Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#europe_training <- terra::ext(-35, 60, 25, 75)
#bioclim <- terra::crop(bioclim, europe_training)

#USA Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data/usa")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#north_america <- terra::ext(-152, -58, 7, 68)
#bioclim <- terra::crop(bioclim, north_america)

#Europe Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#europe_training <- terra::ext(-35, 60, 25, 75)
#bioclim <- terra::crop(bioclim, europe_training)

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#north_america <- terra::ext(-152, -58, 7, 68)
#bioclim <- terra::crop(bioclim, north_america)


# 4.1 - Install and load packages ####
#----------------------------------------------------------#
# 4.1.1 - Define packages
list.of.packages <- c("terra", "remotes")

# 4.1.2 - If packages are missing, install them
for (pkg in list.of.packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# 4.1.3 - Load the packages
if (!requireNamespace("megaSDM", quietly = TRUE)) {
  remotes::install_github("brshipley/megaSDM", build_vignettes = FALSE) 
}
library(megaSDM)

# 4.1.4 - Clean up the environment
rm(list.of.packages, pkg)


# 4.2 - Prepare environmental data (needed for CRS/extent reference) ####
#-----------------------------------------------------------------------#
# 4.2.1 - Select the environmental variables used in the virtual species
names(bioclim) <- substr(names(bioclim), 11, 16)
env_vars <- c("bio_1", "bio_7", "bio_12", "bio_15")


# 4.3 - Create output directories for background points ####
#--------------------------------------------------------------------------#
# 4.3.1 - Define species names
species_names <- c(
  "Marmatharia_Medusa",
  "Stellaria_Petrus",
  "Idella_Wiesellus",
  "Alpina_Montis",
  "Luminaria_Aqua",
  "Deserta_Solensis"
)

# 4.3.2 - create output directories
output_dir <- "background/background_points"

# 4.3.3 - create the output directories if they don't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("background/occurrence_csv", recursive = TRUE, showWarnings = FALSE)
dir.create("background/buffers_100km", recursive = TRUE, showWarnings = FALSE)
dir.create("background/buffers_200km", recursive = TRUE, showWarnings = FALSE)
dir.create("background/buffers_adaptive", recursive = TRUE, showWarnings = FALSE)


# 4.4 - Loop over all 6 species ####
#--------------------------------------------------------------------------#
# 4.4.1 - Start the loop and define species-specific variables
for (s in seq_along(species_names)) {

  species_name <- species_names[s]
  cat("\n=== Species", s, "-", species_name, "===\n")
  
  train_sp <- train_sp <- read.csv(file.path("train_data", paste0(species_name, "_train.csv")))
  
  n_occ <- nrow(train_sp)

  occ_export <- data.frame(
    species = species_name,
    x = train_sp$x,
    y = train_sp$y
  )

  write.csv(
    occ_export,
    file.path("background/occurrence_csv", paste0(species_name, ".csv")),
    row.names = FALSE
  )

  # 4.4.2 - Create fixed-width buffers (100 km & 200 km)
  occ_list <- file.path("background/occurrence_csv", paste0(species_name, ".csv"))

  set.seed(1284)
  BackgroundBuffers(
    occlist = occ_list,
    envdata = bioclim,
    output = "background/buffers_100km",
    buff_distance = 100000,   # 100 km in meters
    ncores = 1
  )

  BackgroundBuffers(
    occlist = occ_list,
    envdata = bioclim,
    output = "background/buffers_200km",
    buff_distance = 200000,   # 200 km in meters
    ncores = 1
  )

  # 4.4.3 - Re-import the generated shapefiles so that they can be
  # passed to BackgroundPoints()
  buffer_100km <- terra::vect(file.path("background/buffers_100km", paste0(species_name, ".shp")))
  buffer_200km <- terra::vect(file.path("background/buffers_200km", paste0(species_name, ".shp")))

  # 4.4.4 - Create Nearest-Neighbour-Distance buffer
  BackgroundBuffers(
    occlist = occ_list,
    envdata = bioclim,
    output = "background/buffers_adaptive",
    buff_distance = NA,   # NA = automatisch, 95%-Quantil NN-Distanz
    ncores = 1
  )

  buffer_adaptive <- terra::vect(file.path("background/buffers_adaptive", paste0(species_name, ".shp")))

  # 4.4.5 - Define parameter combinations
  nbg_values <- c(
    "5000" = 5000,
    "1xocc" = 1 * n_occ,
    "5xocc" = 5 * n_occ
  )

  buffer_list <- list(
    "100km" = buffer_100km,
    "200km" = buffer_200km,
    "adaptive" = buffer_adaptive
  )

  spatial_weights <- c(0, 0.5, 0.75)
  n_replicates <- 4

  # 4.4.6 - Create a parameter grid for all combinations
  param_grid <- expand.grid(
    nbg_name = names(nbg_values),
    buffer_name = names(buffer_list),
    weight = spatial_weights,
    replicate = seq_len(n_replicates),
    stringsAsFactors = FALSE
  )
  

  # 4.4.7 - Generate background points for every combination ####
  #------------------------------------------------------------------------#
  # Each “species-combination-replicate” row is assigned its own, but
  # reproducible seed (derived from BASE_SEED, the species index, and
  # the row number). This way, each of the 4 replicates generates an 
  # independent set of background points, but the entire run remains fully
  # reproducible.
  #------------------------------------------------------------------------#
  BASE_SEED <- 1284

  for (i in seq_len(nrow(param_grid))) {

    nbg_name <- param_grid$nbg_name[i]
    buffer_name <- param_grid$buffer_name[i]
    weight <- param_grid$weight[i]
    replicate_id <- param_grid$replicate[i]

    nbg <- nbg_values[[nbg_name]]
    buffer_sel <- buffer_list[[buffer_name]]

    set.seed(BASE_SEED + (s - 1) * nrow(param_grid) + i)

    # 4.4.8 - Create a temporary directory for intermediate files 
    temp_dir <- file.path(output_dir, "temp")
    dir.create(temp_dir, showWarnings = FALSE)

    BackgroundPoints(
      spplist = species_name,
      envdata = bioclim,
      output = temp_dir,
      nbg = nbg,
      spatial_weights = weight,
      buffers = buffer_sel,
      method = "random",
      ncores = 1
    )

    # 4.4.9 - Read the generated background points and save them with a descriptive name
    temp_file <- file.path(temp_dir, paste0(species_name, "_background.csv"))
    bg_points <- read.csv(temp_file)

    final_name <- sprintf(
      "%s_n%s_buf%s_w%s_rep%d.csv",
      species_name, nbg_name, buffer_name, weight, replicate_id
    )

    write.csv(
      bg_points,
      file.path(output_dir, final_name),
      row.names = FALSE
    )

    cat("Erstellt:", final_name, "-", nrow(bg_points), "Background Points\n")
  }

  unlink(file.path(output_dir, "temp"), recursive = TRUE)

}


# 4.5 - Quick check ####
#----------------------------------------------------------------------------------#
# 4.5.1 - List all generated background point files and print the total count
generated_files <- list.files(output_dir, pattern = "\\.csv$", full.names = FALSE)
cat("\nerzeugte Background-Point-Sets (alle Arten):", length(generated_files), "\n")
print(generated_files)


# End of script ####
#----------------------------------------------------------------------------#
# Output:
# - Background points for all 6 virtual species, with 108 combinations per species,
#   saved as CSV files in the "background/background_points" directory.
#----------------------------------------------------------------------------#