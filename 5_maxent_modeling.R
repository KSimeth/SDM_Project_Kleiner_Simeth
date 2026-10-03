# 5 - Run MaxEnt models for all background-point combinations ####
#----------------------------------------------------------------#
# This script runs MaxEnt models for all six virtual species using 
# the generated background-point datasets. It extracts environmental
# variables for presence and background points, fits MaxEnt models, 
# and saves the models and predictions as raster files.
#----------------------------------------------------------------#

# 5.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
#------------------------------------------------#

#Europe Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#europe <- terra::ext(-25, 45, 34, 72)
#europe_training <- terra::ext(-35, 60, 25, 75)
#bioclim_training <- terra::crop(bioclim, europe_training)
#bioclim_prediction <- terra::crop(bioclim, europe)

#USA Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data/usa")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#usa <- terra::ext(-125, -65, 25, 50)
#north_america <- terra::ext(-152, -58, 7, 68)
#bioclim_training <- terra::crop(bioclim, north_america)
#bioclim_prediction <- terra::crop(bioclim, usa)

#Europe Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#europe <- terra::ext(-25, 45, 34, 72)
#europe_training <- terra::ext(-35, 60, 25, 75)
#bioclim_training <- terra::crop(bioclim, europe_training)
#bioclim_prediction <- terra::crop(bioclim, europe)

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#usa <- terra::ext(-125, -65, 25, 50)
#north_america <- terra::ext(-152, -58, 7, 68)
#bioclim_training <- terra::crop(bioclim, north_america)
#bioclim_prediction <- terra::crop(bioclim, usa)



# 5.1 - Install and load required packages
#----------------------------------------------------------------#
# 5.1.1 - Define required packages
list.of.packages <- c("terra", "maxnet", "pROC")

# 5.1.2 - Install missing packages and load them
for (pkg in list.of.packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# 5.1.3 - Clean up the environment
rm(list.of.packages, pkg)


# 5.2 - Prepare bioclimatic variables for model training and prediction
#----------------------------------------------------------------#
# 5.2.1 - Define extent for usa and crop the bioclimatic variables
names(bioclim_training) <- substr(names(bioclim_training), 11, 16)
names(bioclim_prediction) <- substr(names(bioclim_prediction), 11, 16)


# 5.2.2 - Select the environmental variables used in the virtual species
env_vars <- c("bio_1", "bio_7", "bio_12", "bio_15")
bioclim_training <- bioclim_training[[env_vars]]
bioclim_prediction <- bioclim_prediction[[env_vars]]



# 5.3 - Define species names and create output directories
#----------------------------------------------------------------#
# 5.3.1 - Define species names
species_names <- c(
  "Marmatharia_Medusa",
  "Stellaria_Petrus",
  "Idella_Wiesellus",
  "Alpina_Montis",
  "Luminaria_Aqua",
  "Deserta_Solensis"
)

# 5.3.2 - Create output directories for models and predictions
model_dir <- "maxent/models"
prediction_dir <- "maxent/predictions"

dir.create(
model_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  prediction_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# 5.4 - Loop through species and background datasets to fit MaxEnt models
#------------------------------------------------------------------------#
# 5.4.1 - Set seed for reproducibility
set.seed(1284)
# 5.4.2 - Loop through each species
for (s in seq_along(species_names)) {
  
  species_name <- species_names[s]
  
  cat("\n====================================\n")
  cat("Species:", species_name, "\n")
  cat("====================================\n")
  
  # 5.4.3 - Read training presence points for the species
  train_file <- file.path(
    "train_data",
    paste0(species_name, "_train.csv")
  )
  
  train_sp <- read.csv(train_file)
  
  # 5.4.4 - Extract presence coordinates
  presence_xy <- train_sp[, c("x", "y")]
  
  cat(
    "Training presence points:",
    nrow(presence_xy),
    "\n"
  )
  
  
  # 5.4.5 - List background datasets for the species
  background_files <- list.files(
    "background/background_points",
    pattern = paste0(
      "^",
      species_name,
      "_n.*\\.csv$"
    ),
    full.names = TRUE
  )
  
  # 5.4.6 - Loop through each background dataset
  for (bg_file in background_files) {
    
    bg_name <- tools::file_path_sans_ext(
      basename(bg_file)
    )
    
    cat("\nRunning:", bg_name, "\n")
    
    
    # 5.4.7 - Read background points
    background <- read.csv(bg_file)
    
    # Rename coordinate columns
    # Longitude = x, Latitude = y
    
    if (all(c("Longitude", "Latitude") %in% names(background))) {
      names(background)[names(background) == "Longitude"] <- "x"
      names(background)[names(background) == "Latitude"] <- "y"
    }
    
    background_xy <- background[, c("x", "y")]
    
    # 5.4.8 - Extract environmental variables for presence and background points
    presence_env <- terra::extract(
      bioclim_training,
      presence_xy,
      ID = FALSE
    )
    
    background_env <- terra::extract(
      bioclim_training,
      background_xy,
      ID = FALSE
    )
    
    
    # 5.4.9 - Remove rows with NA values in environmental variables
    keep_presence <- complete.cases(presence_env)
    
    keep_background <- complete.cases(background_env)
    
    presence_env <- presence_env[keep_presence, , drop = FALSE]
    
    background_env <- background_env[
      keep_background,
      ,
      drop = FALSE
    ]
      
    
    # 5.4.10 - Combine presence and background environmental data into a single dataset
    model_data <- rbind(
      presence_env,
      background_env
    )
    
    p <- c(
      rep(1, nrow(presence_env)),
      rep(0, nrow(background_env))
    )
    
    # 5.4.11 - Fit MaxEnt model using maxnet
     model <- maxnet(
      p = p,
      data = model_data,
      f = maxnet.formula(
        p,
        model_data,
        classes = "default"
      )
    )
    
    # 5.4.12 - Save the fitted model as an RDS file
    model_file <- file.path(
      model_dir,
      paste0(
        bg_name,
        "_model.RDS"
      )
    )
    
    saveRDS(
      model,
      model_file
    )
    
    
    # 5.4.13 - Predict species distribution across Europe/USA using the fitted model
    prediction <- terra::predict(
      bioclim_prediction,
      model,
      type = "cloglog",
      na.rm = TRUE
    )
    
    # 5.4.14 - Save the prediction raster as a GeoTIFF file
    prediction_file <- file.path(
      prediction_dir,
      paste0(
        bg_name,
        "_prediction.tif"
      )
    )
    
    terra::writeRaster(
      prediction,
      prediction_file,
      overwrite = TRUE
    )
    
    
    cat(
      "Model saved:",
      model_file,
      "\n"
    )
    
    cat(
      "Prediction saved:",
      prediction_file,
      "\n" 
    )
  }
}

cat("\n====================================\n")
cat("ALL MAXENT MODELS COMPLETED\n")
cat("====================================\n")


# End of script####
#-------------------------------------------------------------------------------------#
# Output:
# - MaxEnt models for all six virtual species and all background-point combinations,
#   saved as RDS files in the "maxent/models" directory.
# - Predicted species distributions for all models, saved as GeoTIFF files in the
#   "maxent/predictions" directory.
#-------------------------------------------------------------------------------------#
