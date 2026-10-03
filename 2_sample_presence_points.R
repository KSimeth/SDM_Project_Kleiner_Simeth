# 2 - Sample presence points ####
#--------------------------------------------------------------------------#
# This script samples presence points from the virtual species generated 
# in the previous script (1_create_virtual_species_paper.R). It performs two main tasks:
# 1. Extraction of presence and absence points from the virtual species'
#    presence-absence rasters.
# 2. Filtering of presence points to remove environmentally clustered points,
#    ensuring a more uniform representation of environmental conditions.
#--------------------------------------------------------------------------#

# 2.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
#------------------------------------------------#

#Europe Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#europe <- terra::ext(-25, 45, 34, 72)
#bioclim <- terra::crop(bioclim, europe)

#USA Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data/usa")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#usa <- terra::ext(-125, -65, 25, 50)
#bioclim <- terra::crop(bioclim, usa)

#Europe Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#europe <- terra::ext(-25, 45, 34, 72)
#bioclim <- terra::crop(bioclim, europe)

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#usa <- terra::ext(-125, -65, 25, 50)
#bioclim <- terra::crop(bioclim, usa)


# 2.1 - install and load packages  ####
#-----------------------------------#

# 2.1.1 - Define the required packages for the script
list.of.packages <- c("terra", "sf", "virtualspecies", "predicts")

# 2.1.2 - Loop to check if packages are installed, and install any that are missing
for (pkg in list.of.packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) { # Check if the package is not installed
    install.packages(pkg, dependencies = TRUE) # Install the package with dependencies
  }
  library(pkg, character.only = TRUE) # Load the package
}
# 2.1.3 - clean up the environment by removing the package list and loop variable
rm(list.of.packages,pkg)


# 2.2 - load virtual species and environmental data ####
#--------------------------------------------------------#

# 2.2.1 - Set a random seed for reproducibility in sampling
set.seed(21112024)

# 2.2.2 - Load the virtual species objects from pre-saved RData files
load("virtual_species/Marmatharia_Medusa.RData")
load("virtual_species/Stellaria_Petrus.RData")
load("virtual_species/Idella_Wiesellus.RData")
load("virtual_species/Alpina_Montis.RData")
load("virtual_species/Luminaria_Aqua.RData")
load("virtual_species/Deserta_Solensis.RData")


# 2.2.3 - Select the environmental variables used in the virtual species
names(bioclim) <- substr(names(bioclim), 11, 16)
env_vars <- c("bio_1", "bio_7", "bio_12", "bio_15")


# 2.3 - Extraction of presence and absence points ####
#------------------------------------------------------------------------------#
# Every raster cell with a presence value of 1 is taken as a presence point,
# and every cell with a value of 0 is taken as an absence point
# (= complete census, no sampling). 
#------------------------------------------------------------------------------#

# 2.3.1 - Loop through each virtual species object and extract presence-absence points
extract <- function(pa_object, species_label) {

 pa_raster <- pa_object$pa.raster
 
 if (inherits(pa_raster, "PackedSpatRaster")) {
    pa_raster <- terra::unwrap(pa_raster)
 }

 pa_df <- terra::as.data.frame(pa_raster, xy = TRUE, na.rm = TRUE)
 names(pa_df)[3] <- "pa"

 presence <- pa_df[pa_df$pa == 1, c("x", "y")]
 presence$type <- "presence"

 absence <- pa_df[pa_df$pa == 0, c("x", "y")]
 absence$type <- "absence"

 # 2.3.2 Check the number of presence and absence points extracted for each species
 cat(
   species_label,
   "- Presence-Zellen:", nrow(presence),
   "| Absence-Zellen:", nrow(absence), "\n"
 )

  list(presence = presence, absence = absence)
}

# 2.3.3 - Extract presence and absence points for each species
op_species1 <- extract(Marmatharia_MedusaPA, "Marmatharia_Medusa")
op_species2 <- extract(Stellaria_PetrusPA,   "Stellaria_Petrus")
op_species3 <- extract(Idella_WiesellusPA,   "Idella_Wiesellus")
op_species4 <- extract(Alpina_MontisPA,      "Alpina_Montis")
op_species5 <- extract(Luminaria_AquaPA,     "Luminaria_Aqua")
op_species6 <- extract(Deserta_SolensisPA,   "Deserta_Solensis")


# 2.4 - Filter environmentally clustered presence points ####
#-------------------------------------------------------------------------------#
# Each of the 4 environmental variables is divided into 25 classes of equal 
# width; of all points that fall within the same combination of environmental 
# classes (i.e., “environmentally clustered”), only one is retained.
# This drastically reduces the number of points without restricting the range
# of values for the environmental variables—all combinations remain represented,
# only redundancy within the same environmental class is removed.
#-------------------------------------------------------------------------------#

# 2.4.1 - Thin presence points based on environmental clusters
thin_environmental_clusters <- function(points_df, env_raster, env_vars, n_classes = 25) {

  # 2.4.2 - Extract environmental values for each point
  env_values <- terra::extract(
    env_raster[[env_vars]],
    points_df[, c("x", "y")],
    ID = FALSE
  )

  # 2.4.3 - Create a matrix of class indices for each environmental variable
  class_matrix <- sapply(env_vars, function(v) {
    v_range <- terra::minmax(env_raster[[v]])
    breaks <- seq(v_range[1], v_range[2], length.out = n_classes + 1)
    cut(env_values[[v]], breaks = breaks, include.lowest = TRUE, labels = FALSE)
  })

  # 2.4.4 - Remove points with NA values in any of the environmental variables
  valid <- stats::complete.cases(class_matrix)
  points_df <- points_df[valid, ]
  class_matrix <- class_matrix[valid, , drop = FALSE]

  # 2.4.5 - Create a unique key for each combination of environmental classes
  class_key <- apply(class_matrix, 1, paste, collapse = "_")

  # 2.4.6 - Shuffle the points to randomize the order before filtering
  shuffle_idx <- sample(seq_len(nrow(points_df)))
  points_df <- points_df[shuffle_idx, ]
  class_key <- class_key[shuffle_idx]

  # 2.4.7 - Keep only the first (previously randomized) occurrence of each
  # unique environmental class combination
  keep <- !duplicated(class_key)

  points_df[keep, ]
}

# 2.4.8 - Filter presence points for each species and report the number of
# points before and after filtering
filter_species <- function(op_species, species_label) {

  n_before <- nrow(op_species$presence)

  op_species$presence <- thin_environmental_clusters(
    op_species$presence,
    bioclim,
    env_vars,
    n_classes = 25
  )

  n_after <- nrow(op_species$presence)

  cat(
    species_label,
    "- Presence-Punkte vor Filterung:", n_before,
    "| nach Filterung:", n_after, "\n"
  )

  op_species
}

# 2.4.9 - Apply the filtering function to each species
op_species1 <- filter_species(op_species1, "Marmatharia_Medusa")
op_species2 <- filter_species(op_species2, "Stellaria_Petrus")
op_species3 <- filter_species(op_species3, "Idella_Wiesellus")
op_species4 <- filter_species(op_species4, "Alpina_Montis")
op_species5 <- filter_species(op_species5, "Luminaria_Aqua")
op_species6 <- filter_species(op_species6, "Deserta_Solensis")


# 2.5 - Save environmentally filtered occurrence points ####
#-------------------------------------------------------------------#
save(op_species1, file = "virtual_species/op_species1.RData")
save(op_species2, file = "virtual_species/op_species2.RData")
save(op_species3, file = "virtual_species/op_species3.RData")
save(op_species4, file = "virtual_species/op_species4.RData")
save(op_species5, file = "virtual_species/op_species5.RData")
save(op_species6, file = "virtual_species/op_species6.RData")

# 2.6 - End of script ####
#---------------------------------------------------------------------------#
# Output: presence points for all virtual species saved as RData
# files in the "virtual_species" directory.
#---------------------------------------------------------------------------#
