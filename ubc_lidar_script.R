
# Using LiDAR to characterize urban forest structure and composition and locate hotspots based on derived individual tree attributes
# Dimitrios Markou
# 2024-03-13


# Processing 2015 and 2021 Campus LiDAR Data for tree metrics 

# STEP 1 ------------------------------------------------------------------
# Install and/or load packages 

library(lidR)
library(terra)
library(tidyverse)
library(sf)
library(rgdal)
library(dplyr)
library(whitebox)
library(foreign)
library(randomForest)
library(ggplot2)
library(read.dbf)

# STEP 2 Data conversion ------------------------------------------------------------------

# Laz to Las conversion (2015)
in_folder <- "C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_LAZ" 
out_folder <- "C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_LAS"

laz_to_las <- function(in_folder, out_folder){
  if(!dir.exists(out_folder)){
    dir.create(out_folder)
  }
  
  file_list <- list.files(path = in_folder, pattern = "*.laz")
  
  for (f in file_list){
    if (!file.exists(file.path(out_folder, gsub(".laz", ".las", f)))){
      print(f)
      tryCatch({
        las <- lidR::readLAS(file.path(in_folder, f))
        lidR::writeLAS(las, file.path(out_folder, gsub(".laz", ".las", f)))
      }, warning = function(w){
        print(w)
      }, error = function(e){
        print(e)
      })
    } else {
      print(paste0(f, " already exists"))
    }
    gc()
  }
}

laz_to_las(in_folder, out_folder)


# Laz to Las conversion (2021)
in_folder <- "C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_LAZ" 
out_folder <- "C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_LAS"

laz_to_las <- function(in_folder, out_folder){
  if(!dir.exists(out_folder)){
    dir.create(out_folder)
  }
  
  file_list <- list.files(path = in_folder, pattern = "*.laz")
  
  for (f in file_list){
    if (!file.exists(file.path(out_folder, gsub(".laz", ".las", f)))){
      print(f)
      tryCatch({
        las <- lidR::readLAS(file.path(in_folder, f))
        lidR::writeLAS(las, file.path(out_folder, gsub(".laz", ".las", f)))
      }, warning = function(w){
        print(w)
      }, error = function(e){
        print(e)
      })
    } else {
      print(paste0(f, " already exists"))
    }
    gc()
  }
}

laz_to_las(in_folder, out_folder)


# STEP 3 Visualize study area ------------------------------------------------------------------
# Download a UBC Orthophoto to visualize tree distribution

rgb_UBC <- rast()

str(rgb_UBC)

plot(rgb_UBC)


# STEP 4 Create LAScatalog objects ------------------------------------------------------------------

# Create LAScatalog object from the UBC 2015 las tiles (Catalog A)

# load the 18 tiles collected over UBC into a LAScatalog object 
las_catalog_A <- readLAScatalog("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_LAS")

# inspect the object 
las_check(las_catalog_A)

# provides a more succinct report
print(las_catalog_A)
summary(las_catalog_A)

# visualize the data tiles 
plot(las_catalog_A)


# Create LAScatalog object from the UBC 2021 las tiles (Catalog B)

# load the 18 tiles collected over UBC into a LAScatalog object 
las_catalog_B <- readLAScatalog("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_LAS")

# inspect the object
las_check(las_catalog_B)

# provides a more succinct report
print(las_catalog_B)
summary(las_catalog_B)

# visualize the data tiles 
plot(las_catalog_B)


# STEP 5a Filtering duplicates -----------------------------------------------------------------

# Set the output directory for the filtered .las data
work_dir <- "C:/Users/dimit/campus_lidar_2015/Data"
opt_output_files(las_catalog_A) <- paste(work_dir, "/2015_UBC_LAS_filtered/filt_ubcA_{ID}", sep = "")

work_dir2 <- "C:/Users/dimit/campus_lidar_2021/Data"
opt_output_files(las_catalog_B) <- paste(work_dir2, "/2021_UBC_LAS_filtered/filt_ubcB_{ID}", sep = "")

# remove duplicate points and speed up processing
las_catalog_A <- filter_duplicates(las_catalog_A)
las_catalog_B <- filter_duplicates(las_catalog_B)

# confirm that duplicates were removed for Tile 1
# load the filtered object
filtered_t1_ubcA <- readLAS("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_LAS_filtered/filt_ubcA_1.las")
filtered_t1_ubcB <- readLAS("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_LAS_filtered/filt_ubcB_1.las")

# inspect to confirm that duplicates were removed 
las_check(filtered_t1_ubcA)
las_check(filtered_t1_ubcB)

# read filtered .las into LAScatalog
las_catalog_A_filt <- readLAScatalog("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_LAS_filtered")
las_catalog_B_filt <- readLAScatalog("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_LAS_filtered")

# 2015 LAs catalog is unclassified. We will remove the building polygons.
opt_filter(las_catalog_B_filt) <- '-drop_classification 6'

# print the report
summary(las_catalog_A_filt)
summary(las_catalog_B_filt)

# visualize to make sure spatial info was preserved 
plot(las_catalog_A_filt)
plot(las_catalog_B_filt)


# STEP 5b Classify buildings for 2015 LAS catalog -------------------------

# Completed in ArcGIS pro ...


# Define the file paths
buildings <- "C:/Users/dimit/campus_lidar_2015/Data/buildings/buildings.shp"
output_dir <- "C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_X_buildings"

# Define a function to process each LAS file
process_las_file <- function(las_file) {
  # Construct the output file path
  output_file <- file.path(output_dir, paste0("erased_", basename(las_file)))
  
  # Run the erase operation
  wbt_classify_buildings_in_lidar(las_file, buildings, output_file)
}

# Loop through each LAS file in the LAScatalog and process it
for (las_file in list.files("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_LAS_filtered", pattern = "\\.las$", full.names = TRUE)) {
  process_las_file(las_file)
}


# STEP 5c # Ground classification ------------------------------------------------------------------

opt_output_files(las_catalog_A_filt) <- paste(work_dir, "/2015_UBC_grdclass/grdclass_ubcA_seg{ID}", sep = "")
mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1, cloth_resolution = 1, time_step = 1)
las_catalog_A_class<- classify_ground(las_catalog_A_filt, mycsf)

las_catalog_A_class <- readLAScatalog("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_grdclass")

opt_output_files(las_catalog_B_filt) <- paste(work_dir2, "/2021_UBC_grdclass/grdclass_ubcA_seg{ID}", sep = "")
mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1, cloth_resolution = 1, time_step = 1)
las_catalog_B_class<- classify_ground(las_catalog_B_filt, mycsf)

las_catalog_B_class <- readLAScatalog("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_grdclass")


# STEP 6 DTM ------------------------------------------------------------------

dtm_ubc_2015 <- rasterize_terrain(las_catalog_A_class, res = 1, algorithm = tin())
dtm_ubc_2021 <- rasterize_terrain(las_catalog_B_class, res = 1, algorithm = tin())

plot(dtm_ubc_2015)
plot(dtm_ubc_2021)

dtmA <- terra::terrain(dtm_ubc_2015, v = c("slope", "aspect"), unit = "radians")
dtm_hillshade <- terra::shade(slope = dtmA$slope, aspect = dtmA$aspect)
plot(dtm_hillshade, col = gray(0:50/50), legend = FALSE)

dtmB <- terra::terrain(dtm_ubc_2021, v = c("slope", "aspect"), unit = "radians")
dtm_hillshade <- terra::shade(slope = dtmB$slope, aspect = dtmB$aspect)
plot(dtm_hillshade, col = gray(0:50/50), legend = FALSE)

terra::writeRaster(dtm_ubc_2015, "C:/Users/dimit/campus_lidar_2015/Data/dtm_ubc_2015.tif", overwrite = FALSE)
terra::writeRaster(dtm_ubc_2021, "C:/Users/dimit/campus_lidar_2021/Data/dtm_ubc_2021.tif", overwrite = TRUE)

dtm_ubc_2015 <- rast("C:/Users/dimit/campus_lidar_2015/Data/dtm_ubc_2015.tif")
dtm_ubc_2021 <- rast("C:/Users/dimit/campus_lidar_2021/Data/dtm_ubc_2021.tif")


# STEP 7 Height normalization ------------------------------------------------------------------

# Apply point cloud normalization algorithm

work_dir <- "C:/Users/dimit/campus_lidar_2015/Data"
opt_output_files(las_catalog_A_class) <- paste(work_dir, "/2015_UBC_LAS_norm/norm_las_A_{ID}", sep = "")
norm_las_ctg_A <- normalize_height(las_catalog_A_class, dtm_ubc_2015)
                         
work_dir2 <- "C:/Users/dimit/campus_lidar_2021/Data"
opt_output_files(las_catalog_B_class) <- paste(work_dir2, "/2021_UBC_LAS_norm/norm_las_B_{ID}", sep = "")
norm_las_ctg_B <- normalize_height(las_catalog_B_class, dtm_ubc_2021)

opt_filter(norm_las_ctg_A) <- '-drop_z_below 0 -drop_z_above 65'
opt_filter(norm_las_ctg_B) <- '-drop_z_below 0 -drop_z_above 65'

norm_las_ctg_A <- readLAScatalog("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_LAS_norm")
plot(norm_las_ctg_A)
summary(norm_las_ctg_A)

norm_las_ctg_B <- readLAScatalog("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_LAS_norm")
plot(norm_las_ctg_B)
summary(norm_las_ctg_B)

opt_filter(norm_las_ctg_A) <- '-drop_z_below 0 -drop_z_above 65'
opt_filter(norm_las_ctg_B) <- '-drop_z_below 0 -drop_z_above 65'


# STEP 8 CHM ------------------------------------------------------------------

# Canopy Height Model

# We want a CHM across the whole study area, for 2015 and 2021, respectively. 
# Generate CHM with pit free algorithm
chm_ubc_2015 <- rasterize_canopy(norm_las_ctg_A, res = 0.5, pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5)))

chm_ubc_2021 <- rasterize_canopy(norm_las_ctg_B, res = 0.5, pitfree(thresholds = c(0, 10, 20), max_edge = c(0, 1.5)))

plot(chm_ubc_2015, col = height.colors(50))
terra::writeRaster(chm_ubc_2015, "C:/Users/dimit/campus_lidar_2015/Data/chm_ubc_2015.tif", overwrite = TRUE)

plot(chm_ubc_2021, col = height.colors(50))
terra::writeRaster(chm_ubc_2021, "C:/Users/dimit/campus_lidar_2021/Data/chm_ubc_2021.tif", overwrite = FALSE)

chm_ubc_2015 <- rast("C:/Users/dimit/campus_lidar_2015/Data/chm_ubc_2015.tif")
chm_ubc_2021 <- rast("C:/Users/dimit/campus_lidar_2021/Data/chm_ubc_2021.tif")


# STEP 9a Individual Tree Segmentation (2015) -----------------------------------------------

work_dir <- "C:/Users/dimit/campus_lidar_2015/Data"

# locate the tree tops (2015)
opt_output_files(norm_las_ctg_A) <- paste(work_dir, "/2015_UBC_ttops/ttops_ubcA_{ID}", sep = "")
ttops_ubc_2015 <- locate_trees(norm_las_ctg_A, lmf(4), uniqueness = "bitmerge")

# read in the treetops shapefiles as a spatial object
ttops2015 <- list.files("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_ttops",
                        pattern = "shp$",
                        full.names = TRUE)

# Initialize an empty list to store individual spatial objects
ttops2015_objects <- list()

# Loop through each shapefile and read it into a spatial object
for (file in ttops2015) {
  ttops2015_objects[[length(ttops2015_objects) + 1]] <- st_read(file)
}

# Combine all spatial objects into a single object
treetops2015 <- do.call(rbind, ttops2015_objects)

# Change the values for treeID column in ttops?
treetops2015 <- treetops2015 %>%
  mutate(treeID = (row_number()))

# apply dalponted2016 to segment the trees 
opt_output_files(norm_las_ctg_A) <- paste(work_dir, "/2015_UBC_ttops_seg/ttops_ubcA_seg{ID}", sep = "")
algo <- dalponte2016(chm_ubc_2015, treetops2015)
ctg_A_segmented <- segment_trees(norm_las_ctg_A, algo)

# read in the catalog if needed
ctg_A_segmented <- readLAScatalog("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_ttops_seg")

plot(ctg_A_segmented)
summary(ctg_A_segmented)


# STEP 9b Individual Tree Segmentation (2021) ----------------------------------------------

work_dir2 <- "C:/Users/dimit/campus_lidar_2021/Data"

# locate the tree tops (2021)
opt_output_files(norm_las_ctg_B) <- paste(work_dir2, "/2021_UBC_ttops/ttops_ubcB_{ID}", sep = "")
ttops_ubc_2021 <- locate_trees(norm_las_ctg_B, lmf(4), uniqueness = "bitmerge")

# read in the treetops shapefiles as a spatial object
ttops2021 <- list.files("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_ttops",
                        pattern = "shp$",
                        full.names = TRUE)

# Initialize an empty list to store individual spatial objects
ttops2021_objects <- list()

# Loop through each shapefile and read it into a spatial object
for (file in ttops2021) {
  ttops2021_objects[[length(ttops2021_objects) + 1]] <- st_read(file)
}

# Combine all spatial objects into a single object
treetops2021 <- do.call(rbind, ttops2021_objects)

# Change the values for treeID column in ttops?
treetops2021 <- treetops2021 %>%
  mutate(treeID = (row_number()))

library(sf)

# Check the CRS of norm_las_ctg_B
st_crs(norm_las_ctg_B)

# Check the CRS of chm_ubc_2021
st_crs(chm_ubc_2021)
st_crs(treetops2021)
st_crs(norm_las_ctg_B)

library(sf)

epsg_code <- 6653

# Set the CRS for norm_las_ctg_B
st_crs(norm_las_ctg_B) <- st_crs(epsg_code)

# Set the CRS for treetops2021
st_crs(treetops2021) <- st_crs(epsg_code)

# apply dalponte2016 to segment the trees 
opt_output_files(norm_las_ctg_B) <- paste(work_dir2, "/2021_UBC_ttops_seg/ttops_ubcB_seg{ID}", sep = "")
algo <- dalponte2016(chm_ubc_2021, treetops2021)
ctg_B_segmented <- segment_trees(norm_las_ctg_B, algo)

# read in the catalog if needed
ctg_B_segmented <- readLAScatalog("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_ttops_seg")

plot(ctg_B_segmented)
summary(ctg_B_segmented)


# STEP 10a Crown Metrics & Segmentation (2015) ------------------------------------------------------

work_dir <- "C:/Users/dimit/campus_lidar_2015/Data"

opt_filter(ctg_A_segmented) <- '-drop_z_below 0 -drop_z_above 65'
opt_stop_early(ctg_A_segmented) <- FALSE # where ctg is the variable for your las catalog
summary(ctg_A_segmented)

opt_output_files(ctg_A_segmented) <- paste(work_dir, "/2015_UBC_crowns_stdmetrics/crowns_ubcA_stdmetrics_{ID}", sep = "")
stdmetrics_ubc_crowns_2015 <- crown_metrics(ctg_A_segmented, func = .stdmetrics, geo = "convex")


crowns2015 <- list.files("C:/Users/dimit/campus_lidar_2015/Data/2015_UBC_crowns_stdmetrics",
                         pattern = "shp$",
                         full.names = TRUE)
# Initialize an empty list to store individual spatial objects
crowns2015_objects <- list()

# Loop through each shapefile and read it into a spatial object
for (file in crowns2015) {
  crowns2015_objects[[length(crowns2015_objects) + 1]] <- st_read(file)
}

# Combine all spatial objects into a single object
crowns2015 <- do.call(rbind, crowns2015_objects)

# Assuming your DataFrame is named df
crowns2015 <- crowns2015[order(crowns2015$treeID), ]


# STEP 10b Crown Metrics & Segmentation (2021) ----------------------------------------------------------------

work_dir2 <- "C:/Users/dimit/campus_lidar_2021/Data"

opt_filter(ctg_B_segmented) <- '-drop_z_below 0 -drop_z_above 65'
opt_stop_early(ctg_B_segmented) <- FALSE # where ctg is the variable for your las catalog
summary(ctg_B_segmented)

opt_output_files(ctg_B_segmented) <- paste(work_dir2, "/2021_UBC_crowns_stdmetrics/crowns_ubcB_stdmetrics_{ID}", sep = "")
stdmetrics_ubc_crowns_2021 <- crown_metrics(ctg_B_segmented, func = .stdmetrics, geo = "convex")

crowns2021 <- list.files("C:/Users/dimit/campus_lidar_2021/Data/2021_UBC_crowns_stdmetrics",
                        pattern = "shp$",
                        full.names = TRUE)
# Initialize an empty list to store individual spatial objects
crowns2021_objects <- list()

# Loop through each shapefile and read it into a spatial object
for (file in crowns2021) {
  crowns2021_objects[[length(crowns2021_objects) + 1]] <- st_read(file)
}

# Combine all spatial objects into a single object
crowns2021 <- do.call(rbind, crowns2021_objects)

# Assuming your DataFrame is named df
crowns2021 <- crowns2021[order(crowns2021$treeID), ]


# STEP 11 More metrics (DBH, Shape_Area, and Shape_Leng) ----------------------------------------------

# Define your own new metrics function
calculate_dbh <- function(Z) {
  # Calculate DBH at a specific height above ground level (e.g., 1.3 meters)
  dbh <- 2 * sqrt((Z / 2) ^ 2 - (1.3) ^ 2)  # Formula for DBH estimation
  
  return(dbh)
}

# Add a new column 'DBH' to the dataframe with the calculated DBH values
crowns2015$DBH <- calculate_dbh(crowns2015$zmax)
crowns2021$DBH <- calculate_dbh(crowns2021$zmax)

st_write(crowns2015, 
         "C:/Users/dimit/campus_lidar_2015/Data/crown_metrics_2015/crown_metrics_2015.shp", 
         driver = "ESRI Shapefile")

st_write(crowns2021, 
         "C:/Users/dimit/campus_lidar_2021/Data/crown_metrics_2021/crown_metrics_2021.shp", 
         driver = "ESRI Shapefile")

# STEP 12 ArcGIS crown filtering + species sampling (outside of R)------------------------------------------------------------------



# STEP 13 Read in Filtered Crown files w/ coniferous / deciduous samples -----------------------------------------------------------------

sp_classified_crowns2015 <- st_read("C:/Users/dimit/campus_lidar_2015/Data/crown_metrics_2015/crown_metrics_classified/sp_classified_crowns_2015.shp")

sp_classified_crowns2021 <- st_read("C:/Users/dimit/campus_lidar_2021/Data/crown_metrics_2021/crown_metrics_classified/sp_classified_crowns_2021.shp")


# Filtering data
# Drop intensity metrics and/or other unnecessary metrics 
# For sp_classified_crowns2015
filtered_crowns2015 <- sp_classified_crowns2015 %>%
  select(-starts_with("i"), -Shape_Area, -Shape_Leng, -treeID)

# For sp_classified_crowns2021
filtered_crowns2021 <- sp_classified_crowns2021 %>%
  select(-starts_with("i"), -Shape_Area, -Shape_Leng, -treeID)

# Drop geometry
no_geom_crowns2015 <- sf::st_drop_geometry(filtered_crowns2015)
no_geom_crowns2021 <- sf::st_drop_geometry(filtered_crowns2021)

# Filter for coniferous and deciduous species for 2015
coniferous2015 <- subset(no_geom_crowns2015, Species == "C")
deciduous2015 <- subset(no_geom_crowns2015, Species == "D")

# Filter for coniferous and deciduous species for 2021
coniferous2021 <- subset(no_geom_crowns2021, Species == "C")
deciduous2021 <- subset(no_geom_crowns2021, Species == "D")
  

# STEP 14 Random Forest PART 1 ---------------------------------------------------

# 2015
# Split the data into training and validation for coniferous species
# Determine the number of samples for training and validation
n_train_conif_2015 <- round(0.7 * nrow(coniferous2015))
n_valid_conif_2015 <- nrow(coniferous2015) - n_train_conif_2015

# Randomly sample indices for training and validation
train_indices_conif_2015 <- sample(1:nrow(coniferous2015), n_train_conif_2015)
valid_indices_conif_2015 <- setdiff(1:nrow(coniferous2015), train_indices_conif_2015)

# Assign data to training and validation datasets
train_conif_2015 <- coniferous2015[train_indices_conif_2015, ]
valid_conif_2015 <- coniferous2015[valid_indices_conif_2015, ]
  
# Split the data into training and validation for deciduous species
# Determine the number of samples for training and validation
n_train_decid_2015 <- round(0.7 * nrow(deciduous2015))
n_valid_decid_2015 <- nrow(deciduous2015) - n_train_decid_2015

# Randomly sample indices for training and validation
train_indices_decid_2015 <- sample(1:nrow(deciduous2015), n_train_decid_2015)
valid_indices_decid_2015 <- setdiff(1:nrow(deciduous2015), train_indices_decid_2015)

# Assign data to training and validation datasets
train_decid_2015 <- deciduous2015[train_indices_decid_2015, ]
valid_decid_2015 <- deciduous2015[valid_indices_decid_2015, ]

# Bring species training / validation data together
training_2015 <- rbind(train_conif_2015, train_decid_2015)
validation_2015 <- rbind(valid_conif_2015, valid_decid_2015)

training_2015$Species = as.factor(training_2015$Species)
validation_2015$Species = as.factor(validation_2015$Species)


# 2021
# Split the data into training and validation for coniferous species
# Determine the number of samples for training and validation
n_train_conif_2021 <- round(0.7 * nrow(coniferous2021))
n_valid_conif_2021 <- nrow(coniferous2021) - n_train_conif_2021

# Randomly sample indices for training and validation
train_indices_conif_2021 <- sample(1:nrow(coniferous2021), n_train_conif_2021)
valid_indices_conif_2021 <- setdiff(1:nrow(coniferous2021), train_indices_conif_2021)

# Assign data to training and validation datasets
train_conif_2021 <- coniferous2021[train_indices_conif_2021, ]
valid_conif_2021 <- coniferous2021[valid_indices_conif_2021, ]

# Split the data into training and validation for deciduous species
# Determine the number of samples for training and validation
n_train_decid_2021 <- round(0.7 * nrow(deciduous2021))
n_valid_decid_2021 <- nrow(deciduous2021) - n_train_decid_2021

# Randomly sample indices for training and validation
train_indices_decid_2021 <- sample(1:nrow(deciduous2021), n_train_decid_2021)
valid_indices_decid_2021 <- setdiff(1:nrow(deciduous2021), train_indices_decid_2021)

# Assign data to training and validation datasets
train_decid_2021 <- deciduous2021[train_indices_decid_2021, ]
valid_decid_2021 <- deciduous2021[valid_indices_decid_2021, ]

# Bring species training / validation data together
training_2021 <- rbind(train_conif_2021, train_decid_2021)
validation_2021 <- rbind(valid_conif_2021, valid_decid_2021)

training_2021$Species = as.factor(training_2021$Species)
validation_2021$Species = as.factor(validation_2021$Species)



# STEP 15 Random Forest PART 2 -----------------------------------------------------------------

# Train Random Forest models for each year and species type
rf_model_2015 <- randomForest(Species ~ ., data = training_2015, mtry=4, ntree=501, importance=TRUE)
rf_model_2021 <- randomForest(Species ~ ., data = training_2021, mtry=4, ntree=501, importance=TRUE)

# Predict species for the validation data
valid_prediction2015 <- data.frame(TrueSpecies = validation_2015$Species, PredictedSpecies = predict(rf_model_2015, newdata = validation_2015, type = "response"))
valid_prediction2021 <- data.frame(TrueSpecies = validation_2021$Species, PredictedSpecies = predict(rf_model_2021, newdata = validation_2021, type = "response"))

# Assuming prediction2015 and prediction2021 contain your predicted species labels
# and the true species labels are stored in validation_2015$Species and validation_2021$Species respectively

# For 2015
accuracy_2015 <- mean(valid_prediction2015$TrueSpecies == valid_prediction2015$PredictedSpecies)

# For 2021
accuracy_2021 <- mean(valid_prediction2021$TrueSpecies == valid_prediction2021$PredictedSpecies)

# Print accuracies
print(accuracy_2015)
print(accuracy_2021)

# Now, apply random forest classifier to entire study area 
# Predict species for the entire data set
predicted_species_2015 <- data.frame(TrueSpecies = no_geom_crowns2015$Species, PredictedSpecies = predict(rf_model_2015, newdata = no_geom_crowns2015, type = "response"))
predicted_species_2021 <- data.frame(TrueSpecies = no_geom_crowns2021$Species, PredictedSpecies = predict(rf_model_2021, newdata = no_geom_crowns2021, type = "response"))



# Merge PredictedSpecies column to crowns files 

campustrees2015 <- cbind(filtered_crowns2015, predicted_species_2015)

campustrees2021 <- cbind(filtered_crowns2021, predicted_species_2021)

st_write(campustrees2015, 
         "C:/Users/dimit/campus_lidar_2015/Data/crown_metrics_2015/campus_trees_final/campustrees_2015.shp", 
         driver = "ESRI Shapefile")

st_write(campustrees2021, 
         "C:/Users/dimit/campus_lidar_2021/Data/crown_metrics_2021/campus_trees_final/campustrees_2021.shp", 
         driver = "ESRI Shapefile")


# read the shapefiles
campustrees2015 <- st_read("C:/Users/dimit/campus_lidar_2015/Data/crown_metrics_2015/campus_trees_final/campustrees_2015.shp")
campustrees2021 <- st_read("C:/Users/dimit/campus_lidar_2021/Data/crown_metrics_2021/campus_trees_final/campustrees_2021.shp")

campustrees2015 <- sf::st_drop_geometry(campustrees2015)
campustrees2021 <- sf::st_drop_geometry(campustrees2021)

# read to csv file for data management 
write.csv(campustrees2015, file = "C:/Users/dimit/campus_lidar_dataverse/campustrees2015.csv", row.names = FALSE)
write.csv(campustrees2021, file = "C:/Users/dimit/campus_lidar_dataverse/campustrees2021.csv", row.names = FALSE)


# Assuming your data frame is called "df"
# Rename the column
colnames(campustrees2015)[colnames(campustrees2015) == "PrdctdS"] <- "PrdSpecies"
colnames(campustrees2021)[colnames(campustrees2021) == "PrdctdS"] <- "PrdSpecies"


coniferous2015 <- subset(campustrees2015, PrdSpecies == "C")
deciduous2015 <- subset(campustrees2015, PrdSpecies == "D")

coniferous2021 <- subset(campustrees2021, PrdSpecies == "C")
deciduous2021 <- subset(campustrees2021, PrdSpecies == "D")

# For coniferous2015
coniferous2015_above50 <- subset(coniferous2015, zmax > 50 & zmax < 60)
pairs(~ zmean + area, data = coniferous2015_above50)

# For deciduous2015
deciduous2015_above50 <- subset(deciduous2015, zmax > 35 & zmax < 60)
pairs(~ zmax + area, data = deciduous2015_above50)

# For coniferous2021
coniferous2021_above50 <- subset(coniferous2021, zmax > 50 & zmax < 60)
pairs(~ zmax + area, data = coniferous2021_above50)

# For deciduous2021
deciduous2021_above50 <- subset(deciduous2021, zmax > 50 & zmax < 60)
pairs(~ zmax + area, data = deciduous2021_above50)


# Subset the tallest coniferous trees
tallest_coniferous <- campustrees2015 %>%
  filter(PrdSpecies == "C") %>%
  arrange(desc(zmax)) %>%
  head(50)

# Subset the tallest deciduous trees
tallest_deciduous <- campustrees2015 %>%
  filter(PrdSpecies == "D") %>%
  arrange(desc(zmax)) %>%
  head(50)

# Combine the subsets
tallest_trees <- rbind(tallest_coniferous, tallest_deciduous)

# Convert DBH from centimeters to meters
tallest_trees$DBH_m <- tallest_trees$DBH / 100

# Calculate the radius of the crown (assuming half of the DBH)
tallest_trees$crown_radius <- tallest_trees$DBH_m / 2

# Calculate crown volume using the cylinder approximation formula
tallest_trees$crown_volume <- pi * tallest_trees$crown_radius^2 * tallest_trees$zmax


# Plot zmax vs area, grouped by PrdSpecies
ggplot(tallest_trees, aes(x = area, y = crown_volume, color = PrdSpecies)) +
  geom_point() +
  ggtitle("Scatterplot of zmax vs area by PrdSpecies (Tallest Trees)") +
  xlab("") +
  ylab("")

pairs(~ zmax + crown_volume + area + pzabov2, data = tallest_trees)


# Read the DBF file into a data frame
conif_hex_stats2015 <- read.dbf("C:/Users/dimit/campus_lidar_results/conif_hex_stats_2015.dbf")
decid_hex_stats2015 <- read.dbf("C:/Users/dimit/campus_lidar_results/decid_hex_stats_2015.dbf")
conif_hex_stats2021 <- read.dbf("C:/Users/dimit/campus_lidar_results/conif_hex_stats_2021.dbf")
decid_hex_stats2021 <- read.dbf("C:/Users/dimit/campus_lidar_results/decid_hex_stats_2021.dbf")

# Create subsets for each year and species with the 10 highest "MEAN_zmax"
conif_hex_stats2015_subset <- conif_hex_stats2015[order(-conif_hex_stats2015$MEAN_zmax), ][1:20, ]
decid_hex_stats2015_subset <- decid_hex_stats2015[order(-decid_hex_stats2015$MEAN_zmax), ][1:20, ]
conif_hex_stats2021_subset <- conif_hex_stats2021[order(-conif_hex_stats2021$MEAN_zmax), ][1:20, ]
decid_hex_stats2021_subset <- decid_hex_stats2021[order(-decid_hex_stats2021$MEAN_zmax), ][1:20, ]

# Combine all subset dataframes into one
all_subset <- rbind(conif_hex_stats2015_subset, decid_hex_stats2015_subset, 
                    conif_hex_stats2021_subset, decid_hex_stats2021_subset)

# Create a vector to indicate the grouping of each subset
group <- rep(c("conif_hex_stats2015", "decid_hex_stats2015", 
               "conif_hex_stats2021", "decid_hex_stats2021"), each = 20)

# Create a factor for the 'group' variable to specify the desired order
all_subset$group <- factor(group, levels = c("conif_hex_stats2015", "decid_hex_stats2015", 
                                             "conif_hex_stats2021", "decid_hex_stats2021"))



# Create a boxplot for zmax by Species for the Hexagon Hectares with the Tallest Trees 
ggplot(all_subset, aes(x = group, y = MEAN_zmax, fill = substr(group, 1, 6), group = group)) +
  geom_boxplot(coef = 1.5, position = position_dodge(width = 0.8)) +  # Specify whiskers length and dodge position
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +  # Add all points
  labs(title = "Mean zmax of the Tallest Trees Grouped by Hexagon Hectare",
       x = "Year", y = "Mean zmax",
       fill = "Species Type") +  # Change legend title
  scale_x_discrete(labels = function(x) substr(x, -4, -1)) +  # Simplify x-axis labels
  scale_fill_manual(values = c("darkgreen", "orange"),
                    labels = c("Coniferous", "Broadleaf")) +  # Specify custom legend labels
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # Remove grid lines
        plot.title = element_text(hjust = 0.5),  # Center plot title
        axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
        axis.title.y = element_text(hjust = 0.5),  # Center y-axis label
        legend.title = element_text(hjust = 0.5),  # Center legend title
        axis.line = element_line(color = "black", size = 0.5))  # Add x and y axis lines

# Create a boxplot for Tree Count by Species for the Hexagon Hectares with the Tallest Trees 
ggplot(all_subset, aes(x = group, y = COUNT_Prdc, fill = substr(group, 1, 6), group = group)) +
  geom_boxplot(coef = 1.5, position = position_dodge(width = 0.8)) +  # Specify whiskers length and dodge position
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +  # Add all points
  labs(title = "Mean Tree Count of the Tallest Trees Grouped by Hexagon Hectare",
       x = "Year", y = "Mean Tree Count",
       fill = "Species Type") +  # Change legend title
  scale_x_discrete(labels = function(x) substr(x, -4, -1)) +  # Simplify x-axis labels
  scale_fill_manual(values = c("forestgreen", "maroon"),
                    labels = c("Coniferous", "Broadleaf")) +  # Specify custom legend labels
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # Remove grid lines
        plot.title = element_text(hjust = 0.5),  # Center plot title
        axis.title.x = element_text(hjust = 0.5),  # Center x-axis label
        axis.title.y = element_text(hjust = 0.5),  # Center y-axis label
        legend.title = element_text(hjust = 0.5),  # Center legend title
        axis.line = element_line(color = "black", size = 0.5))  # Add x and y axis lines


library(gridExtra)

# Create a boxplot for zmax by Species for the Hexagon Hectares with the Tallest Trees 
plot1 <- ggplot(all_subset, aes(x = group, y = MEAN_zmax, fill = substr(group, 1, 6), group = group)) +
  geom_boxplot(coef = 1.5, position = position_dodge(width = 0.8)) +  # Specify whiskers length and dodge position
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +  # Add all points
  labs(title = "A",
       x = "", y = "Mean zmax",
       fill = "Species Type") +  # Change legend title
  scale_x_discrete(labels = function(x) substr(x, -4, -1)) +  # Simplify x-axis labels
  scale_fill_manual(values = c("forestgreen", "maroon"),
                    labels = c("Coniferous", "Broadleaf")) +  # Specify custom legend labels
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # Remove grid lines
        plot.title = element_text(size = 11, hjust = 0.5),  # Change title font size and center title
        axis.title.x = element_text(size = 9, hjust = 0.5),  # Center x-axis label
        axis.title.y = element_text(size = 9, hjust = 0.5),  # Center y-axis label
        legend.title = element_text(size = 9, hjust = 0.5),  # Center legend title
        axis.line = element_line(color = "black", size = 0.5))  # Add x and y axis lines

# Create a boxplot for Tree Count by Species for the Hexagon Hectares with the Tallest Trees 
plot2 <- ggplot(all_subset, aes(x = group, y = COUNT_Prdc, fill = substr(group, 1, 6), group = group)) +
  geom_boxplot(coef = 1.5, position = position_dodge(width = 0.8)) +  # Specify whiskers length and dodge position
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +  # Add all points
  labs(title = "B",
       x = "", y = "Mean Tree Count",
       fill = "Species Type") +  # Change legend title
  scale_x_discrete(labels = function(x) substr(x, -4, -1)) +  # Simplify x-axis labels
  scale_fill_manual(values = c("forestgreen", "maroon"),
                    labels = c("Coniferous", "Broadleaf")) +  # Specify custom legend labels
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # Remove grid lines
        plot.title = element_text(size = 11, hjust = 0.5),  # Change title font size and center title
        axis.title.x = element_text(size = 9, hjust = 0.5),  # Center x-axis label
        axis.title.y = element_text(size = 9, hjust = 0.5),  # Center y-axis label
        legend.title = element_text(size = 9, hjust = 0.5),  # Center legend title
        axis.line = element_line(color = "black", size = 0.5))  # Add x and y axis lines

plot3 <- ggplot(all_subset, aes(x = group, y = MEAN_area, fill = substr(group, 1, 6), group = group)) +
  geom_boxplot(coef = 1.5, position = position_dodge(width = 0.8)) +  # Specify whiskers length and dodge position
  geom_jitter(position = position_dodge(width = 0.8), alpha = 0.5) +  # Add all points
  labs(title = "C",
       x = "", y = "Mean Area",
       fill = "Species Type") +  # Change legend title
  scale_x_discrete(labels = function(x) substr(x, -4, -1)) +  # Simplify x-axis labels
  scale_fill_manual(values = c("forestgreen", "maroon"),
                    labels = c("Coniferous", "Broadleaf")) +  # Specify custom legend labels
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # Remove grid lines
        plot.title = element_text(size = 11, hjust = 0.5),  # Change title font size and center title
        axis.title.x = element_text(size = 9, hjust = 0.5),  # Center x-axis label
        axis.title.y = element_text(size = 9, hjust = 0.5),  # Center y-axis label
        legend.title = element_text(size = 9, hjust = 0.5, family = "sans"),  # Center legend title
        axis.line = element_line(color = "black", size = 0.5))  # Add x and y axis lines


# Arrange the plots vertically
grid.arrange(plot1, plot2, plot3, ncol = 1)










