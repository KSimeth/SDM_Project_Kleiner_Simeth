# 1 - Generate virtual species ####
#--------------------------------------------------------------------------#
# This script generates six virtual species based on predefined
# environmental response functions from the paper Wihtford et al. 2024 
#(https://doi.org/10.1016/j.ecolmodel.2023.110604) for BIO1, BIO7, BIO12 and BIO15.
# The continuous suitability distributions are converted into binary
# presence-absence distributions and saved for subsequent analyses.
#--------------------------------------------------------------------------#

# 1.0 - Change here for Europe/USA extant and Kora/Lilly directory ####
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


# 1.1 - Install and load packages ####
#--------------------------------------------------------------------------#

# 1.1.1 - Define the required packages for the script 
list.of.packages <- c("terra", "sf", "predicts", "virtualspecies", "geodata", "ggplot2")

# 1.1.2 - check if packages are installed, if not install them
for (pkg in list.of.packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)
}


# 1.2 - Prepare bioclimatic variables europe/USA ####
#--------------------------------------------------------------------------#

# 1.2.1 - Standardise variable names
names(bioclim) <- substr(names(bioclim), 11, 16)

# 1.2.2 - Select the environmental variables used in the virtual species
terra::plot(bioclim)



# 1.3 - Define environmental response functions exactly like Whitford et al. 2024 ####
#-------------------------------------------------------------------------------------#
# BIO1  = Annual Mean Temperature
# BIO7  = Annual Temperature Range
# BIO12 = Annual Precipitation
# BIO15 = Precipitation Seasonality
#-------------------------------------------------------------------------------------#

# 1.3.1 - Species 1: Marmatharia_Medusa

params_S1 <- formatFunctions(
  bio_1 = c(fun = "quadraticFun", a = -13.57657, b = 338.57246, c = 0),
  bio_7 = c(fun = "quadraticFun", a = -3.584569, b = 245.008343, c = 0),
  bio_12 = c(fun = "logisticFun", alpha = -292.1815, beta = 1006.8523),
  bio_15 = c(fun = "dnorm", mean = 17.41111, sd = 40.41186)
)


# 1.3.2 - Species 2: Stellaria_Petrus

params_S2 <- formatFunctions(
  bio_1 = c(fun = "quadraticFun", a = -7.620954, b = -41.899100, c = 0),
  bio_7 = c(fun = "quadraticFun", a = -1.891247, b = 160.066161, c = 0),
  bio_12 = c(fun = "logisticFun", alpha = 228.2668, beta = 853.6718),
  bio_15 = c(fun = "linearFun", a = -0.5353535, b = 3.4714826)
)


# 1.3.3 - Species 3: Idella_Wiesellus

params_S3 <- formatFunctions(
  bio_1 = c(fun = "linearFun", a = 0.4343434, b = -2.3162422),
  bio_7 = c(fun = "dnorm", mean = 24.98215, sd = 32.45617),
  bio_12 = c(fun = "linearFun", a = -0.3131313, b = 408.6168485),
  bio_15 = c(fun = "logisticFun", alpha = 6.237355, beta = 82.323931)
)


# 1.3.4 - Species 4: Alpina_Montis

params_S4 <- formatFunctions(
  bio_1 = c(fun = "logisticFun", alpha = -0.3478124, beta = -1.0277679),
  bio_7 = c(fun = "linearFun", a = -0.4141414, b = 47.4501621),
  bio_12 = c(fun = "linearFun", a = 1.000, b = 3259.335),
  bio_15 = c(fun = "linearFun", a = -0.8787879, b = 87.2119282)
)


# 1.3.5 - Species 5: Luminaria_Aqua

params_S5 <- formatFunctions(
  bio_1 = c(fun = "linearFun", a = 0.4747475, b = 4.3447790),
  bio_7 = c(fun = "logisticFun", alpha = -0.6656435, beta = 15.8963152),
  bio_12 = c(fun = "linearFun", a = -0.5151515, b = 1111.1164251),
  bio_15 = c(fun = "dnorm", mean = 98.32964, sd = 53.02093)
)


# 1.3.6 - Species 6: Deserta_Solensis
params_S6 <- formatFunctions(
  bio_1 = c(fun = "linearFun", a = 0.6565657, b = 8.3890015),
  bio_7 = c(fun = "dnorm", mean = 10.75380, sd = 48.11187),
  bio_12 = c(fun = "linearFun", a = -0.6363636, b = 1277.5093573),
  bio_15 = c(fun = "dnorm", mean = 106.22005, sd = 21.81929)
)


# 1.4 - Generate all six virtual species ####
#--------------------------------------------------------------------------#

# 1.4.1 - Define the environmental stack for the virtual species generation
set.seed(1284)
env_stack <- bioclim[[c("bio_1", "bio_12", "bio_7", "bio_15")]]

# 1.4.2 - Generate the species
Marmatharia_Medusa <- generateSpFromFun(raster.stack = env_stack, parameters = params_S1, formula = "bio_1 * bio_12 * bio_7 * bio_15", plot = TRUE)
Stellaria_Petrus <- generateSpFromFun(raster.stack = env_stack, parameters = params_S2, formula = "bio_1 * bio_12 * bio_7 * bio_15", plot = TRUE)
Idella_Wiesellus <- generateSpFromFun(raster.stack = env_stack, parameters = params_S3, formula = "bio_1 * bio_12 * bio_7 * bio_15", plot = TRUE)
Alpina_Montis <- generateSpFromFun(raster.stack = env_stack, parameters = params_S4, formula = "bio_1 * bio_12 * bio_7 * bio_15", plot = TRUE)
Luminaria_Aqua <- generateSpFromFun(raster.stack = env_stack, parameters = params_S5, formula = "bio_1 * bio_12 * bio_7 * bio_15", plot = TRUE)
Deserta_Solensis <- generateSpFromFun(raster.stack = env_stack, parameters = params_S6, formula = "bio_1 * bio_12 * bio_7 * bio_15", plot = TRUE)


# 1.5 - Convert suitability to presence-absence ####
#--------------------------------------------------------------------------#

# 1.5.1 - Convert all species using the logistic probability method

set.seed(1284)

Marmatharia_MedusaPA <- convertToPA(Marmatharia_Medusa, prob.method = "logistic", beta = "random", alpha = -0.1, species.prevalence = 0.227310981231339, plot = TRUE)
Stellaria_PetrusPA   <- convertToPA(Stellaria_Petrus, prob.method = "logistic", beta = "random", alpha = -0.1, species.prevalence = 0.430869329859848, plot = TRUE)
Idella_WiesellusPA   <- convertToPA(Idella_Wiesellus, prob.method = "logistic", beta = "random", alpha = -0.1, species.prevalence = 0.0368289505958775, plot = TRUE)
Alpina_MontisPA      <- convertToPA(Alpina_Montis, prob.method = "logistic", beta = "random", alpha = -0.1, species.prevalence = 0.027412902079862, plot = TRUE)
Luminaria_AquaPA     <- convertToPA(Luminaria_Aqua, prob.method = "logistic", beta = "random", alpha = -0.1, species.prevalence = 0.235908702602576, plot = TRUE)
Deserta_SolensisPA   <- convertToPA(Deserta_Solensis, prob.method = "logistic", beta = "random", alpha = -0.1, species.prevalence = 0.428553908093615, plot = TRUE)


# 1.6 - Save virtual species ####
#--------------------------------------------------------------------------#

# 1.6.1 - Save presence-absence distributions ####

save(Marmatharia_MedusaPA, file = "virtual_species/Marmatharia_Medusa.RData")
save(Stellaria_PetrusPA,   file = "virtual_species/Stellaria_Petrus.RData")
save(Idella_WiesellusPA,   file = "virtual_species/Idella_Wiesellus.RData")
save(Alpina_MontisPA,      file = "virtual_species/Alpina_Montis.RData")
save(Luminaria_AquaPA,     file = "virtual_species/Luminaria_Aqua.RData")
save(Deserta_SolensisPA,   file = "virtual_species/Deserta_Solensis.RData")


# End of script ####
#--------------------------------------------------------------------------#
# Output:
# Six virtual species with continuous suitability and binary
# presence-absence distributions saved in the virtual_species folder.
#--------------------------------------------------------------------------#