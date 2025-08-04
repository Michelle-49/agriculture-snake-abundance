# loop for analyzing images

library(readr)
library(imageviewer)
library(magick)
library(rsvg)
library(beepr)
library(hexView)
library(tiff)
library (EBImage)
library(rio)

# source in the functions
source("functions.R")


# CLOUD ANALYSIS

folder <- "clouds"
path2 <- file.path("images", paste("VC", folder, sep = "_"), paste("VC", folder, "images", sep = "_"), fsep = "/")
images_folders <- list.dirs(path = path2, recursive = FALSE)

df_clouds <- data.frame(images = character(), pixels = numeric())

for (n in 25:29) {
  path3 <- file.path(path2, paste(folder, n, sep = "_"), fsep = "/")
  test_images <- list.files(path = path3, recursive = FALSE, pattern = "vc", full.names = FALSE)

  images_list_blue <- format.image(test_images, path3)
  images_list_wg <- format.image.grs(test_images, path3)
  
  # standardizing images
  stand_path <- file.path("images", paste("VC", folder, sep = "_"), paste("stand", "VC", folder, sep = "_"), paste("stand", folder, n, sep = "_"), fsep="/")
  dir.create(path = stand_path)
  standardize.image(images_list_blue, stand_path)
  stand_images <- list.files(path = stand_path, recursive = FALSE, pattern = "vc", full.names = FALSE)
  stand_list_blue <- format.image(stand_images, stand_path)

  # FOR CLOUDS: USE 0.37 THRESHOLD FOR BLUE, 0.5 THRESHOLD FOR WHITE (STANDARDIZED BLUE)
  # FOR CLOUDS: USE 0.37 THRESHOLD FOR BLUE, 0.45 THRESHOLD FOR WHITE (STANDARDIZED BLUE)
  # FOR DARKSKY: 1-3) USE 0.36 BLUE, 0.45 WHITE; 4) 0.36 B, 0.4 WG; 5) 0.35 B, 0.39 
  output_folder <- file.path("images", paste("VC", folder, sep = "_"), paste("binarized", "VC", folder, sep = "_"), fsep="/")
  dir.create(output_folder)
  
  df_temp <- binarize.image.comb(stand_list_blue, images_list_wg, output_folder, 0.37, 0.45)
  
  colnames(df_temp) <- c("image", "prop_blk_pixels")
  
  df_clouds <- rbind(df_clouds, df_temp)
}

write_csv(df_clouds, file.path("output", paste("VCpixels_", folder, ".csv", sep = ""), fsep = "/"))



#######################################################################################

# BLUE ANALYSIS

folder <- "bluehigh"
path2 <- file.path("images", paste("VC", folder, sep = "_"), paste("VC", folder, "images", sep = "_"), fsep = "/")
images_folders <- list.dirs(path = path2, recursive = FALSE)

df_bluehigh <- data.frame(images = character(), pixels = numeric())

for (n in 1:length(images_folders)) {
  pathb <- file.path(path2, paste(folder, n, sep = "_"), fsep = "/")
  test_images <- list.files(path = pathb, recursive = FALSE, pattern = "vc", full.names = FALSE)
  
  images_list_blue <- format.image(test_images, pathb)
  
  # standardizing images
  stand_path <- file.path("images", paste("VC", folder, sep = "_"), paste("stand", "VC", folder, sep = "_"), paste("stand", folder, n, sep = "_"), fsep="/")
  dir.create(path = stand_path)
  standardize.image(images_list_blue, stand_path)
  stand_images <- list.files(path = stand_path, recursive = FALSE, pattern = "vc", full.names = FALSE)
  stand_list_blue <- format.image(stand_images, stand_path)
  
  # FOR CLOUDS: USE 0.37 THRESHOLD FOR BLUE, 0.45 THRESHOLD FOR WHITE (STANDARDIZED BLUE)
  # FOR DARKSKY: 1-3) USE 0.36 BLUE, 0.45 WHITE; 4) 0.36 B, 0.4 WG; 5) 0.35 B, 0.39 
  output_folder <- file.path("images", paste("VC", folder, sep = "_"), paste("binarized", "VC", folder, sep = "_"), fsep="/")
  dir.create(output_folder)
  
  df_temp <- binarize.image.blue(stand_list_blue, output_folder, 0.38)
  
  colnames(df_temp) <- c("image", "prop_blk_pixels")
  
  df_bluehigh <- rbind(df_bluehigh, df_temp)
}

write_csv(df_bluehigh, file.path("output", paste("VCpixels_", folder, ".csv", sep = ""), fsep = "/"))

