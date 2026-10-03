# 3 - Split training and test data ####
#-----------------------------------------------------------------#
# This script splits the presence-only data for each of the six
# virtual species into training and test datasets.
#-----------------------------------------------------------------#

# 3.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
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
#bioclim <- terra::crop(bioclim, europe)#bioclim <- terra::crop(bioclim, usa)

#USA Lilly
#setwd("C:/Users/phi/Documents/Uni/Master/SoSe2026/SDM/Project/data/usa")
#bioclim <- terra::rast(list.files("bioclim/climate/wc2.1_10m", pattern = "^wc2\\.1_10m_bio_[0-9]+\\.tif$", full.names = TRUE))
#usa <- terra::ext(-125, -65, 25, 50)
#bioclim <- terra::crop(bioclim, usa)


# 3.1 - Load packages and prepare working space ####
#------------------------------------------------#

# 3.1.1 - Define packages
list.of.packages <- c("terra", "sf", "predicts", "virtualspecies", "geodata", "ggplot2")

# 3.1.2 - If packages are missing, install them
for (pkg in list.of.packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # Install the package if not already installed
    install.packages(pkg, dependencies = TRUE)
  }
  # Load the package
  library(pkg, character.only = TRUE)
}

# 3.1.3 - Load the packages
lapply(list.of.packages, library, character.only = TRUE)

# 3.1.4 - create path
dir.create("test_data")
dir.create("train_data")


# 3.2 - Load presence-only data for all species ####
#---------------------------------------------------#

# 3.2.1 - Load the presence-only data for each species
load("virtual_species/op_species1.RData")
load("virtual_species/op_species2.RData")
load("virtual_species/op_species3.RData")
load("virtual_species/op_species4.RData")
load("virtual_species/op_species5.RData")
load("virtual_species/op_species6.RData")

# 3.2.2 - Extract presence points for each species
points1 <- op_species1$presence
points2 <- op_species2$presence
points3 <- op_species3$presence
points4 <- op_species4$presence
points5 <- op_species5$presence
points6 <- op_species6$presence


# 3.3 - Split data into training and test data ####
#--------------------------------------------------#

# 3.3.1 - Set seed for reproducibility
set.seed(1284)

# 3.3.2 - Determine number of points for each species
n1 <- nrow(points1)
n2 <- nrow(points2)
n3 <- nrow(points3)
n4 <- nrow(points4)
n5 <- nrow(points5)
n6 <- nrow(points6)

# 3.3.3 - Randomly select 80% of the points for training
train_index1 <- sample(
  seq_len(n1),
  size = floor(0.8 * n1)
)

train_index2 <- sample(
  seq_len(n2),
  size = floor(0.8 * n2)
)

train_index3 <- sample(
  seq_len(n3),
  size = floor(0.8 * n3)
)

train_index4 <- sample(
  seq_len(n4),
  size = floor(0.8 * n4)
)

train_index5 <- sample(
  seq_len(n5),
  size = floor(0.8 * n5)
)

train_index6 <- sample(
  seq_len(n6),
  size = floor(0.8 * n6)
)

# 3.3.4 - Create training datasets

train_species1 <- points1[train_index1, ]
train_species2 <- points2[train_index2, ]
train_species3 <- points3[train_index3, ]
train_species4 <- points4[train_index4, ]
train_species5 <- points5[train_index5, ]
train_species6 <- points6[train_index6, ]


# 3.3.5 - Create test datasets

test_species1 <- points1[-train_index1, ]
test_species2 <- points2[-train_index2, ]
test_species3 <- points3[-train_index3, ]
test_species4 <- points4[-train_index4, ]
test_species5 <- points5[-train_index5, ]
test_species6 <- points6[-train_index6, ]


# 3.4 - Extract environmental variables for training data ####
#-------------------------------------------------------------#

# 3.4.1 - Rename variables
names(bioclim) <- substr(names(bioclim),11,16)

# 3.4.2 - Select the environmental variables used in the virtual species
env_vars <- c("bio_1", "bio_7", "bio_12", "bio_15")

# 3.4.3 - Extract environmental variables for training data
train_env1 <- terra::extract(
  bioclim[[env_vars]],
  train_species1[, c("x", "y")],
  ID = FALSE
)

train_env2 <- terra::extract(
  bioclim[[env_vars]],
  train_species2[, c("x", "y")],
  ID = FALSE
)

train_env3 <- terra::extract(
  bioclim[[env_vars]],
  train_species3[, c("x", "y")],
  ID = FALSE
)

train_env4 <- terra::extract(
  bioclim[[env_vars]],
  train_species4[, c("x", "y")],
  ID = FALSE
)

train_env5 <- terra::extract(
  bioclim[[env_vars]],
  train_species5[, c("x", "y")],
  ID = FALSE
)

train_env6 <- terra::extract(
  bioclim[[env_vars]],
  train_species6[, c("x", "y")],
  ID = FALSE
)


# 3.5 - Add environmental variables to training data ####
#---------------------------------------------------------#
# 3.5.1 - Combine training data with environmental variables
train_species1 <- cbind(train_species1, train_env1)
train_species2 <- cbind(train_species2, train_env2)
train_species3 <- cbind(train_species3, train_env3)
train_species4 <- cbind(train_species4, train_env4)
train_species5 <- cbind(train_species5, train_env5)
train_species6 <- cbind(train_species6, train_env6)

# 3.5.2 - Remove unnecessary environmental variable dataframes
rm(
  train_env1,
  train_env2,
  train_env3,
  train_env4,
  train_env5,
  train_env6
)


# 3.6 - Check number of training and test points, prepare dataset ####
#--------------------------------------------------------------------#
# 3.6.1 - Print the number of training and test points for each species
cat("Species 1:", nrow(train_species1), "training /",
    nrow(test_species1), "test points\n")

cat("Species 2:", nrow(train_species2), "training /",
    nrow(test_species2), "test points\n")

cat("Species 3:", nrow(train_species3), "training /",
    nrow(test_species3), "test points\n")

cat("Species 4:", nrow(train_species4), "training /",
    nrow(test_species4), "test points\n")

cat("Species 5:", nrow(train_species5), "training /",
    nrow(test_species5), "test points\n")

cat("Species 6:", nrow(train_species6), "training /",
    nrow(test_species6), "test points\n")

# 3.6.2 - Delete unnessessary columns
train_species1 <- train_species1[, !names(train_species1) %in% c("type")]
train_species2 <- train_species2[, !names(train_species2) %in% c("type")]
train_species3 <- train_species3[, !names(train_species3) %in% c("type")]
train_species4 <- train_species4[, !names(train_species4) %in% c("type")]
train_species5 <- train_species5[, !names(train_species5) %in% c("type")]
train_species6 <- train_species6[, !names(train_species6) %in% c("type")]


# 3.7 - Save training and test data as CSV ####
#---------------------------------------------#
# 3.7.1 - Save training data as CSV
write.csv(train_species1, "train_data/Marmatharia_Medusa_train.csv", row.names = FALSE)
write.csv(train_species2, "train_data/Stellaria_Petrus_train.csv", row.names = FALSE)
write.csv(train_species3, "train_data/Idella_Wiesellus_train.csv", row.names = FALSE)
write.csv(train_species4, "train_data/Alpina_Montis_train.csv", row.names = FALSE)
write.csv(train_species5, "train_data/Luminaria_Aqua_train.csv", row.names = FALSE)
write.csv(train_species6, "train_data/Deserta_Solensis_train.csv", row.names = FALSE)

# 3.7.2 - Save test data as CSV
write.csv(test_species1, "test_data/Marmatharia_Medusa_test.csv", row.names = FALSE)
write.csv(test_species2, "test_data/Stellaria_Petrus_test.csv", row.names = FALSE)
write.csv(test_species3, "test_data/Idella_Wiesellus_test.csv", row.names = FALSE)
write.csv(test_species4, "test_data/Alpina_Montis_test.csv", row.names = FALSE)
write.csv(test_species5, "test_data/Luminaria_Aqua_test.csv", row.names = FALSE)
write.csv(test_species6, "test_data/Deserta_Solensis_test.csv", row.names = FALSE)


# End of script ####
#------------------------------------------------------------------------------#
# Output:
# - Training and test datasets for all six virtual species saved as CSV files
#   in the "train_data" and "test_data" directories, respectively.
#------------------------------------------------------------------------------#
