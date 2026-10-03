# 6 - Evaluate all MaxEnt predictions
#--------------------------------------------------------------------#
# This script evaluates all MaxEnt predictions (n = 648) generated 
# in the previous step with the same evaluation metrics used in 
# Wihtford et al. 2024. The evaluation metrics include:
# 1. AUC
# 2. Continuous Boyce Index
# 3. Overlap with true virtual species distribution
# 4. Overprediction
# 5. Underprediction
#--------------------------------------------------------------------#

# 6.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
#------------------------------------------------#

#Europe Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")

#USA Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data/usa")

#Europe Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")


# 6.1 Install and load packages####
#--------------------------------------------------------------------#
# 6.1.1 Define required packages
list.of.packages <- c(
  "terra",
  "pROC",
  "ecospat"
)

# 6.1.2 Install missing packages and load them
for (pkg in list.of.packages) {
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  
  library(pkg, character.only = TRUE)
}

# 6.1.3 Clean up the environment
rm(list.of.packages, pkg)


# 6.2 Prepare data and output directories
#-------------------------------------------------------------------#
# 6.2.1 Define species names
species_names <- c(
  "Marmatharia_Medusa",
  "Stellaria_Petrus",
  "Idella_Wiesellus",
  "Alpina_Montis",
  "Luminaria_Aqua",
  "Deserta_Solensis"
)

# 6.2.2 Create output directory for evaluation results
prediction_dir <- "maxent/predictions"

output_dir <- "maxent/evaluation"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# 6.2.3 Load true virtual species distributions by creating a new environment 
load_species_object <- function(path) {
  e <- new.env()
  load(path, envir = e)
  objs <- ls(e)
  if (length(objs) != 1) {
    stop("Erwartet genau ein Objekt in ", path, ", gefunden: ", length(objs))
  }
  get(objs[1], envir = e)
}

# 6.2.4 Load true virtual species distributions into a list
true_species <- list()

# 6.2.5 Loop through species names and load the corresponding true distribution objects
for (s in seq_along(species_names)) {
  
  # Get the current species name
  species_name <- species_names[s]
  
  # Load the true distribution object for the current species and store it in the list
  true_species[[species_name]] <- load_species_object(
    paste0(
      "virtual_species/",
      species_name,
      ".RData"
    )
  )
}


# 6.3 Evaluate model predictions####
#---------------------------------------------------------------------#
# 6.3.1 Start the function

# Define a function to evaluate model predictions against true distributions and test points
evaluate_model <- function(
    prediction,
    true_pa,
    test_points,
    n_bins = 20
) {
  # Resample true presence-absence raster to match the prediction raster
  true_pa <- terra::resample(
    true_pa,
    prediction,
    method = "near"
  )
  # Extract x and y coordinates from test points
  test_xy <- test_points[, c("x", "y")]
  
  # Extract predicted values at test point locations
  test_prediction <- terra::extract(
    prediction,
    test_xy,
    ID = FALSE
  )[, 1]
  
  # Remove any non-finite values from the test predictions
  test_prediction <- test_prediction[
    is.finite(test_prediction)
  ]

  # 6.3.2 AUC 
  #-----------------------------------------------------------------#
  # For presence-only evaluation we use background locations
  # as the comparison class. We sample the same number of background 
  # cells as test presence points.
  
  # Determine the number of test points
  n_test <- length(test_prediction)
  
  # Sample background cells from the prediction raster
  bg_cells <- sample(
    terra::ncell(prediction),
    size = n_test,
    replace = FALSE
  )
  
  # Extract predicted values at the sampled background cells
  bg_prediction <- terra::values(
    prediction
  )[
    bg_cells
  ]
  
  # Remove any non-finite values from the background predictions
  bg_prediction <- bg_prediction[
    is.finite(bg_prediction)
  ]
  
  # Calculate AUC using the pROC package
  auc <- NA_real_
  
  if (
    length(test_prediction) > 0 &&
    length(bg_prediction) > 0
  ) {
    
    response <- c(
      rep(1, length(test_prediction)),
      rep(0, length(bg_prediction))
    )
    
    scores <- c(
      test_prediction,
      bg_prediction
    )
    
    auc <- as.numeric(
      pROC::auc(
        response,
        scores,
        quiet = TRUE
      )
    )
  }
  
  # 6.3.3 Continuous Boyce Index
  #-----------------------------------------------------------------#
  
  # Initialize Boyce index variable
  boyce <- NA_real_
  
  # Ensure there are enough test points for Boyce index calculation
  if (length(test_prediction) >= 5) {
    
    # Extract predicted values from the prediction raster
    prediction_values <- terra::values(
      prediction
    )
    
    # Remove any non-finite values from the prediction values
    prediction_values <- prediction_values[
      is.finite(prediction_values)
    ]
    
    # Calculate the Boyce index using the ecospat.boyce function
    boyce_result <- try(
      ecospat::ecospat.boyce(
        fit = prediction_values,
        obs = test_prediction,
        PEplot = FALSE,
        rm.duplicate = TRUE
      ),
      silent = TRUE
    )
    
    # Check if the Boyce index calculation was successful and extract the correlation value
    if (!inherits(
      boyce_result,
      "try-error"
    )) {
      
      boyce <- as.numeric(
        boyce_result$cor
      )
    }
  }
  
  # 6.3.4 Determine binary threshold
  #-------------------------------------------------------------------#
  # Extract values from the true presence-absence raster and the prediction raster
  true_values <- terra::values(
    true_pa
  )
  
  pred_values <- terra::values(
    prediction
  )
  
  # Remove any non-finite values from the true and predicted values
  valid <- complete.cases(
    true_values,
    pred_values
  )
  
  # Keep only the valid values for true and predicted values
  true_values <- true_values[valid]
  pred_values <- pred_values[valid]
  
  
  # initialize threshold variable
  threshold <- NA_real_
  
  # Ensure that there are exactly two unique values in the presence-absence data
  if (
    length(unique(true_values)) == 2 &&
    length(pred_values) > 0
  ) {
    
    # Calculate the ROC curve using the pROC package
    roc_true <- try(
      pROC::roc(
        response = true_values,
        predictor = pred_values,
        quiet = TRUE
      ),
      silent = TRUE
    )
    
    # Check if the ROC curve calculation was successful and extract the 
    # threshold using Youden's J statistic
    if (!inherits(
      roc_true,
      "try-error"
    )) {
      
      threshold <- as.numeric(
        pROC::coords(
          roc_true,
          x = "best",
          best.method = "youden",
          ret = "threshold",
          transpose = FALSE
        )
      )
    }
  }
  
  
  # 6.3.5 Binary prediction
  #--------------------------------------------------------------#
  
  # Initialize binary prediction variable with NA values
  binary_prediction <- rep(
    NA_integer_,
    length(pred_values)
  )
  
  # if a valid threshold was determined, convert predicted values to binary predictions
  if (!is.na(threshold)) {
    
    binary_prediction <- ifelse(
      pred_values >= threshold,
      1,
      0
    )
  }
  

  # 6.3.6 True distribution
  #--------------------------------------------------------------#
  # Convert true presence-absence values to binary format
  true_binary <- ifelse(
    true_values > 0,
    1,
    0
  )
  
  
  # 6.3.7 Calculate areas
  #--------------------------------------------------------------#
  # Calculate the area of each cell in the true presence-absence raster in square kilometers
  cell_area <- terra::cellSize(
    true_pa,
    unit = "km"
  )
  
  # Remove any non-finite values from the cell area raster
  area_values <- terra::values(
    cell_area
  )[valid]
  
  # Calculate the total area of true presence
  true_area <- sum(
    area_values[
      true_binary == 1
    ],
    na.rm = TRUE
  )
  
  # Calculate the total of the predicted presence
  predicted_area <- sum(
    area_values[
      binary_prediction == 1
    ],
    na.rm = TRUE
  )
  
  # Calculate the total of the overlap
  overlap_area <- sum(
    area_values[
      true_binary == 1 &
        binary_prediction == 1
    ],
    na.rm = TRUE
  )
  
  # Calculate the total of the overprediction
  overprediction_area <- sum(
    area_values[
      true_binary == 0 &
        binary_prediction == 1
    ],
    na.rm = TRUE
  )
  
  # Calculate the total of the underprediction
  underprediction_area <- sum(
    area_values[
      true_binary == 1 &
        binary_prediction == 0
    ],
    na.rm = TRUE
  )
  
  
  # 6.3.8 Convert to percentages
  #--------------------------------------------------------------#
  # Initialize percentage variables with NA values
  overlap_percent <- NA_real_
  overprediction_percent <- NA_real_
  underprediction_percent <- NA_real_
  
  # Calculate overlap and underprediction percentages relative to the true area
  if (true_area > 0) {
    
    overlap_percent <-
      100 *
      overlap_area /
      true_area
    
    underprediction_percent <-
      100 *
      underprediction_area /
      true_area
  }
  
  # Calculate overprediction percentages relative to the true area
  if (predicted_area > 0) {
    
    overprediction_percent <-
      100 *
      overprediction_area /
      true_area
  }
  

  # 6.3.9 Return results
  #--------------------------------------------------------------#
  
  return(
    data.frame(
      AUC = auc,
      Boyce = boyce,
      threshold = threshold,
      true_area_km2 = true_area,
      predicted_area_km2 = predicted_area,
      overlap_area_km2 = overlap_area,
      overlap_percent = overlap_percent,
      overprediction_area_km2 = overprediction_area,
      overprediction_percent = overprediction_percent,
      underprediction_area_km2 = underprediction_area,
      underprediction_percent = underprediction_percent
    )
  )
}


# 6.4 Evaluate predictions
#--------------------------------------------------------------------#
# 6.4.1 Find all prediction raster files 
prediction_files <- list.files(
  prediction_dir,
  pattern = "_prediction\\.tif$",
  full.names = TRUE
)

# 6.4.2 report the number of files found
cat(
  "Prediction files found:",
  length(prediction_files),
  "\n"
)

# 6.4.3 initialize an empty list for the evaluation metrics
results_list <- vector(
  "list",
  length(prediction_files)
)

# 6.4.4 Loop for the evaluation
for (i in seq_along(prediction_files)) {
  
  prediction_file <- prediction_files[i]
  
  cat(
    "\n====================================\n"
  )
  
  cat(
    "Evaluating:",
    basename(prediction_file),
    "\n"
  )
  
  # Extract model name
  prediction_name <- tools::file_path_sans_ext(
    basename(prediction_file)
  )
  
  model_name <- sub(
    "_prediction$",
    "",
    prediction_name
  )
  
  # Identify species
  species_name <- species_names[
    sapply(
      species_names,
      function(x) {
        startsWith(
          model_name,
          x
        )
      }
    )
  ][1]
  
  
  if (is.na(species_name)) {
    
    warning(
      "Species could not be identified for ",
      model_name
    )
    
    next
  }
  
  # Load prediction
  prediction <- terra::rast(
    prediction_file
  )
  
  # Load true distribution
  #true_pa <- terra::unwrap(true_species[[species_name]]$pa.raster)
  #true_pa <- true_species[[species_name]]$pa.raster
  true_pa <- terra::unwrap(true_species[[species_name]]$pa.raster)
  
  # Load test data
  test_file <- file.path(
    "test_data",
    paste0(species_name, "_test.csv")
  )
  
  test_points <- read.csv(
    test_file
  )
  
  # Evaluate
   evaluation <- evaluate_model(
    prediction = prediction,
    true_pa = true_pa,
    test_points = test_points
  )
  
  # Extract treatment information
  treatment_name <- sub(
    paste0(
      "^",
      species_name,
      "_"
    ),
    "",
    model_name
  )
  
  treatment_parts <- strsplit(
    treatment_name,
    "_"
  )[[1]]
  
  n_background <- sub(
    "^n",
    "",
    treatment_parts[1]
  )
  
  buffer <- sub(
    "^buf",
    "",
    treatment_parts[2]
  )
  
  weighting <- sub(
    "^w",
    "",
    treatment_parts[3]
  )
  
  replicate <- sub(
    "^rep",
    "",
    treatment_parts[4]
  )
  
  # Combine metadata + evaluation
  results_list[[i]] <- data.frame(
    species = species_name,
    background_number = n_background,
    buffer = buffer,
    weighting = as.numeric(weighting),
    replicate = as.integer(replicate),
    model = model_name,
    evaluation
  )
}


# 6.5 Combine and save all results
#----------------------------------------------------------------#
# 6.5.1 combine all results
evaluation_results <- do.call(
  rbind,
  results_list
)

# 6.5.2 Save results
write.csv(
  evaluation_results,
  "maxent/evaluation/maxent_evaluation_results.csv",
  row.names = FALSE
)

# 6.5.3 Output 
cat(
  "Models evaluated:",
  nrow(evaluation_results),
  "\n"
)

# End of script####
#---------------------------------------------------------------------#
# Output: A variety of CSV files. Each row of the output file 
# corresponds to a combination and contains both the metadata 
# (species, parameters) and the associated metric values. 
# This file serves as the input for Script 7 (statistical evaluation).
#---------------------------------------------------------------------#