# 8 - Random Forest: variable importance
# ================================================================#


# 8.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
#------------------------------------------------#

#Europe Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data")

#USA Kora
#setwd("C:/Users/koras/OneDrive/Dokumente/Unikram/Physische_Geographie/SDM/Project/data/usa")

#Europe Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data")

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")

# 8.1 Packages
packages <- c("randomForest", "dplyr", "ggplot2")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# 8.2. Load evaluation results
results <- read.csv("maxent/evaluation/maxent_evaluation_results.csv")

# 8.3 Standardize evaluation metrics within each species
results <- results %>%
  group_by(species) %>%
  mutate(
    AUC = as.numeric(scale(AUC)),
    Boyce = as.numeric(scale(Boyce)),
    overlap_percent = as.numeric(scale(overlap_percent)),
    overprediction_percent = as.numeric(scale(overprediction_percent)),
    underprediction_percent = as.numeric(scale(underprediction_percent))
  ) %>%
  ungroup()

cat("Number of models:", nrow(results), "\n")
cat("Number of species:", length(unique(results$species)), "\n")

# 8.4 Evaluation metrics
metrics <- c(
  "AUC",
  "Boyce",
  "overlap_percent",
  "overprediction_percent"
)

# 8.5 Check required variables
cat("\nRequired variables:\n")
print(c(
  "species",
  "background_number",
  "buffer",
  "weighting",
  metrics
))

# 8.6 Prepare predictors
species_names <- c(
  "Marmatharia_Medusa",
  "Stellaria_Petrus",
  "Idella_Wiesellus",
  "Alpina_Montis",
  "Luminaria_Aqua",
  "Deserta_Solensis"
)

occ_numbers <- sapply(
  species_names,
  function(species_name) {
    
    train_file <- file.path(
      "train_data",
      paste0(species_name, "_train.csv")
    )
    
    if (!file.exists(train_file)) {
      stop(
        "Training file not found: ",
        normalizePath(train_file, mustWork = FALSE)
      )
    }
    
    nrow(read.csv(train_file))
  }
)

names(occ_numbers) <- species_names


rf_data <- results %>%
  mutate(
    background_number = case_when(
      background_number == "1xocc" ~ as.numeric(occ_numbers[species]),
      background_number == "5xocc" ~ 5 * as.numeric(occ_numbers[species]),
      background_number == "5000" ~ 5000,
      TRUE ~ NA_real_
    ),
    buffer = as.factor(buffer),
    weighting = as.factor(weighting),
    range_size = true_area_km2
  )

# 8.7 Check conversion
cat("\nBackground-point numbers:\n")
print(
  rf_data %>%
    select(species, background_number) %>%
    distinct() %>%
    arrange(species, background_number)
)


# 8.8 Random Forest models
importance_results <- data.frame()

set.seed(1284)

for (metric in metrics) {
  
  cat("\nRunning Random Forest for:", metric, "\n")
  
  model_data <- rf_data %>%
    select(
      all_of(metric),
      background_number,
      buffer,
      weighting,
      range_size
    ) %>%
    na.omit()
  
  names(model_data)[1] <- "response"
  
  rf_model <- randomForest(
    response ~ background_number + buffer + weighting + range_size,
    data = model_data,
    ntree = 1000,
    importance = TRUE
  )
  
  importance_values <- importance(
    rf_model,
    type = 1
  )
  
  importance_df <- data.frame(
    metric = metric,
    variable = rownames(importance_values),
    IncMSE = importance_values[, "%IncMSE"],
    row.names = NULL
  )
  
  importance_results <- rbind(
    importance_results,
    importance_df
  )
}

# 8.9 Print results
cat("\n====================================================\n")
cat("RANDOM FOREST VARIABLE IMPORTANCE\n")
cat("====================================================\n")

print(importance_results)

# 8.10 Save results
write.csv(
  importance_results,
  "maxent/evaluation/random_forest_variable_importance.csv",
  row.names = FALSE
)
