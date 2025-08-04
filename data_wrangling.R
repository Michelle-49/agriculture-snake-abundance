# Organize and format the landcover data from the attribute tables (ArcGIS)
# and the survey data to get abundance data and svl data

library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(car)
library(tidyr)
library(reshape2)
library(forcats)  # for fct_recode()

## Get land cover data


# Ag inventory land covers for each buffer size
# create a list of the land cover files
landcover_files <- list.files(path = "data/transectbuffer_landcovers", pattern = "landcovers_", full.names = TRUE)
# change the order so landcover_100 is first and _1000 is last
landcover_files <- landcover_files[c(2:10, 1)]

# make a list of data frames corresponding to the landcover files in the list
landcover_list <- lapply(landcover_files, read.csv)
# change the names of each list element
names(landcover_list) <- paste(rep("landcover_"), seq(from = 100, to = 1000, by = 100), sep = "")

# Road data for each buffer size
# create list of road length files
road_files <- list.files(path = "data/road_lengths", pattern = "road_", full.names = TRUE)
# change the order so road_100 is first and _1000 is last
road_files <- road_files[c(1, 3:10, 2)]

# make a list of data frames corresponding to the road files in the list
road_list <- lapply(road_files, read.csv)
# change the names of each list element
names(road_list) <- paste(rep("road_"), seq(from = 100, to = 1000, by = 100), sep = "")

# merge the road and landcover data frames for each buffer size
landroad_list <- list()
for (i in 1:length(landcover_list)) {
  # landcover_df <- as.data.frame(landcover_list[i])
  # road_df <- as.data.frame(road_list[i])
  landroad_list[[i]] <- merge(landcover_list[[i]], road_list[[i]], by = "site", all = TRUE)
}
names(landroad_list) <- paste(rep("lr_"), seq(from = 100, to = 1000, by = 100), sep = "")

# replace any NA values that came from merging the landcover and road data frames with 0
# and remove un-needed columns from road data
for (i in 1:length(landroad_list)) {
  landroad_list[[i]] <- replace(landroad_list[[i]], is.na(landroad_list[[i]]), 0)
  landroad_list[[i]] <- landroad_list[[i]] %>% 
    select(-c(total_kms, area_m2, area_km2)) %>% 
    filter(site != "Nat QC 1 old" & site != "Nat-ag 7 old") %>%  # remove old site
    rename(urban = "urban_developed")  # rename urban landcover 
  landroad_list[[i]]$site <- fct_recode(landroad_list[[i]]$site,"Nat QC 1" = "Nat QC 1 alt")   # match the naming scheme of survey data
}


# combine crop and pasture into agriculture - not much variation by themselves
for (i in 1:length(landcover_list)) {
  landcover_list[[i]] <- landcover_list[[i]] %>% 
    mutate(agriculture = crop + pasture)
}

for (i in 1:length(landroad_list)) {
  landroad_list[[i]] <- landroad_list[[i]] %>% 
    mutate(agriculture = crop + pasture)
}


################################################################################

## Veg cover data

# append vegetation cover pixel data
# read in the veg cover file
pixel_data <- read.csv("output/VC_pixels_total1.csv")

# get the average vegetation cover for each site
vc_data <- pixel_data %>% 
  group_by(site, year, day_of_year) %>%
  summarise(mean_vc = mean(prop_wht_pixels))

vc_data <- vc_data %>% 
  mutate_at("site", as.factor) %>% 
  filter(site != "Nat-ag 7 old" & site != "Nat QC 1 old")  # remove old sites
  

# match the naming scheme of survey data  
vc_data$site <- fct_recode(vc_data$site, "Nat-ag 5" = "Nat-ag 5 new",
                           "Nat QC 1" = "Nat QC 1 alt",
                           "Nat-ag 7" = "Nat-ag 7 new")
vc_data <- droplevels(vc_data)


################################################################################

## Abundance data

# Get the snake survey data formatted into abundance data and svl data for modelling

# read in snake survey data
snakesurvey_data <- read.csv("data/survey_data_1.csv")
num_boards <- read.csv("data/number_boards.csv")

# remove sites that were surveyed in 2023 and moved in 2024 but too close to be independent
snakesurvey_data <- snakesurvey_data %>% 
  subset(!(site %in% c("Nat-ag 7 old", "Nat QC 1 old", "Nat QC 6 old"))) %>% 
  rename("time_of_day" = "t_midpoint",
         "sex" = "sex_final")

# get the number of garters and redbellies per survey
abund_freq <- snakesurvey_data %>%
  group_by(year, day_of_year, site, temp_c, time_of_day) %>%
  summarise(
    garter_count = sum(spp == "garter" & status == "unique", na.rm = FALSE),
    redbelly_count = sum(spp == "redbelly" & status == "unique", na.rm = FALSE),
    .groups = 'keep')


# append the number of cover boards 
abund_df <- merge(abund_freq, num_boards, by = "site", all.x = TRUE)


# append veg cover
abund_data <- merge(abund_df, as.data.frame(vc_data), all.x = TRUE)

abund_data <- abund_data %>% mutate_at(c("site", "year"), as.factor)
abund_data <- abund_data %>% mutate_at("time_of_day", as.numeric)

# adjust veg cover data in abundance data frame

# fill in missing veg cover data (days that were surveyed but not photographed)
# use the vc data that corresponds to the date closest to a survey day
# create a new column for vc_mod (the filled in vc values)
abund_data[ , "vc_mod"] <- NA

# get the vc_values and the day (with all NA rows omitted)
vc_values <- na.omit(abund_data %>% select(site, day_of_year, mean_vc))

for (n in 1:nrow(abund_data)) {
  value <- abund_data[n, "mean_vc"]
  if (is.na(value)){
    # if value is NA, vc_mod becomes the vc value with the closest matching day in vc_values
    site_n <- abund_data[n, "site"]
    # check if the site is in vc data (had vc pictures taken)
    if (site_n %in% vc_values$site) {
      vc_values_site <- vc_values %>% 
        subset(vc_values$site == site_n)
      abund_data[n, "vc_mod"] <- vc_values_site[which.min(abs(abund_data[n, "day_of_year"] - vc_values_site$day_of_year)), "mean_vc"]
    } 
  } else {
    # if value isn't NA, vc_mod = vc (ie. it has a vc value, so it remains what it is)
    abund_data[n, "vc_mod"] <- abund_data[n, "mean_vc"]
  }
}
# set the vc_mod value to NA for surveys in 2023
abund_data$vc_mod[abund_data$year == 2023] <- NA

# remove mean_vc column
abund_data <- abund_data %>% 
  select(-c("mean_vc"))

################################################################################

## SVL data

# get the pertinent svl data from the survey data
svl_data <- snakesurvey_data %>%
  select(site, year, day_of_year, temp_c, time_of_day, spp, sex, svl_cm, status)
svl_data <- svl_data %>% 
  filter(
    status == "unique" & 
      (spp == "garter" | spp == "redbelly")
  )
  
svl_data <- replace(svl_data, svl_data == "", NA)
svl_data <- na.omit(svl_data) %>% 
  mutate_at(c("site", "spp", "sex", "status"), as.factor)

# rename J (juveniles) to U (un-sexed)
svl_data$sex <- fct_recode(svl_data$sex, U = "J")

# # append the veg cover data

# fill in missing veg cover data (days that were surveyed but not photographed)
# get the columns from the abund_data, which filled vc for all the survey dates
vc_allsurveys <- abund_data %>% 
  select(site, year, day_of_year, vc_mod)

# merge the filled in vc for all survey dates and the svl data
svl_data <- merge(svl_data, vc_allsurveys, by = c("site", "year", "day_of_year"))
svl_data$vc_mod[svl_data$year == 2023] <- NA
# remove duplicated rows (that for some reason arose?)
svl_data <- svl_data %>% 
  distinct(.keep_all = TRUE)

# order the factor levels for sex
svl_data$sex <- factor(svl_data$sex, levels = c("F", "M", "U"))

################################################################################

## Land covers scale of effect

# get correlation coefficients btwn abundance and landcovers at different buffer
# sizes to determine which buffer size to include in the model (scale of effect)


# merge all land road data frames into one to get a single data frame with the abundance
# and land cover data for all buffer sizes
abund_lr <- abund_data
for (i in 1:length(landroad_list)) {
  landroad_df <- landroad_list[[i]] %>% select(site, agriculture, crop, pasture, forested, wetland, urban)
  # change landcovers from proportions to percentages
  landroad_df <- landroad_df %>% 
    mutate(agriculture = agriculture*100,
              crop = crop*100,
              pasture = pasture*100,
              forested = forested*100,
              wetland = wetland*100,
              urban = urban*100)
  
  landroad_df <- landroad_df %>% 
    # rename the column names to correspond to the buffer size for the data being added
    # use !! to "unquote the expression" to evaluate paste() first for the new name
    # use := for renaming columns using a dynamic name (dplyr syntax)
    rename(!!paste("agriculture_", i*100, sep = "") := agriculture,
           !!paste("crop_", i*100, sep = "") := crop,
           !!paste("pasture_", i*100, sep = "") := pasture,
           !!paste("forested_", i*100, sep = "") := forested,
           !!paste("wetland_", i*100, sep = "") := wetland,
           !!paste("urban_", i*100, sep = "") := urban)
  
  abund_lr <- merge(abund_lr, landroad_df, by = "site", all.x = TRUE)
  # df_list <- list(abund_lr, landroad_df)
  # abund_lr %>% reduce(full_join, by = 'site')
}


# merge all land road data frames into one to get a single data frame with the svl
# and land cover data for all buffer sizes
svl_mod <- svl_data %>% select(site, spp, svl_cm) # get just the pertinent columns for svl
svl_lr <- svl_mod
for (i in 1:length(landroad_list)) {
  landroad_df <- landroad_list[[i]] %>% select(site, agriculture, crop, pasture, forested, wetland, urban)
  landroad_df <- landroad_df %>% 
    # rename the column names to correspond to the buffer size for the data being added
    # use !! to "unquote the expression" to evaluate paste() first for the new name
    # use := for renaming columns using a dynamic name (dplyr syntax)
    rename(!!paste("agriculture_", i*100, sep = "") := agriculture,
           !!paste("crop_", i*100, sep = "") := crop,
           !!paste("pasture_", i*100, sep = "") := pasture,
           !!paste("forested_", i*100, sep = "") := forested,
           !!paste("wetland_", i*100, sep = "") := wetland,
           !!paste("urban_", i*100, sep = "") := urban)
  svl_lr <- merge(svl_lr, landroad_df, by = "site", all.x = TRUE)
}


# # check normality
# hist(abund_lr$garter_count)
# # garter abundance not normal
# hist(abund_lr$redbelly_count)
# #redbelly abundance not normal

# get Pearson's correlation coefficient for abundance vs all the landcovers
# first remove the site column to have an all numerical data frame to evaluate correlation
abund_lr_mod <- abund_lr %>% select(-c(site, year)) 
corr_p <- cor(abund_lr_mod)

# # since not normally distributed, get Spearman's correlation coefficient
# corr_s <- cor(abund_lr_mod, method = "spearman")
# 
# # also try Kendall's
# corr_k <- cor(abund_lr_mod, method = "kendall")


## Abundance correlations

## garters

# get sequences that contain all the names for each land cover buffer size
ag_seq <- paste(rep("agriculture_"), seq(from = 100, to = 1000, by = 100), sep = "")
crop_seq <- paste(rep("crop_"), seq(from = 100, to = 1000, by = 100), sep = "")
pasture_seq <- paste(rep("pasture_"), seq(from = 100, to = 1000, by = 100), sep = "")
forested_seq <- paste(rep("forested_"), seq(from = 100, to = 1000, by = 100), sep = "")
wetland_seq <- paste(rep("wetland_"), seq(from = 100, to = 1000, by = 100), sep = "")
urban_seq <- paste(rep("urban_"), seq(from = 100, to = 1000, by = 100), sep = "")

# pearson
# use the sequences to get all the correlation values for each land cover buffer size
agcorrs_gp <- corr_p["garter_count", c(ag_seq)]
cropcorrs_gp <- corr_p["garter_count", c(crop_seq)]
pasturecorrs_gp <- corr_p["garter_count", c(pasture_seq)]
forestedcorrs_gp <- corr_p["garter_count", c(forested_seq)]
wetlandcorrs_gp <- corr_p["garter_count", c(wetland_seq)]
urbancorrs_gp <- corr_p["garter_count", c(urban_seq)]

# find the maximum absolute value correlation coefficient
ag_g_max_p <- which.max(abs(agcorrs_gp))
crop_g_max_p <- which.max(abs(cropcorrs_gp))
pasture_g_max_p <- which.max(abs(pasturecorrs_gp))
forested_g_max_p <- which.max(abs(forestedcorrs_gp))
wetland_g_max_p <- which.max(abs(wetlandcorrs_gp))
urban_g_max_p <- which.max(abs(urbancorrs_gp))

# look at how different the correlation values are for each buffer size
summary(agcorrs_gp)
summary(cropcorrs_gp)
summary(pasturecorrs_gp)
summary(forestedcorrs_gp)
summary(wetlandcorrs_gp)
summary(urbancorrs_gp)


# # spearman 
# # use the sequences to get all the correlation values for each land cover buffer size
# agcorrs_gs <- corr_s["garter_count", c(ag_seq)]
# cropcorrs_gs <- corr_s["garter_count", c(crop_seq)]
# pasturecorrs_gs <- corr_s["garter_count", c(pasture_seq)]
# forestedcorrs_gs <- corr_s["garter_count", c(forested_seq)]
# wetlandcorrs_gs <- corr_s["garter_count", c(wetland_seq)]
# 
# # find the maximum absolute value correlation coefficient
# ag_g_max_s <- which.max(abs(agcorrs_gs))
# crop_g_max_s <- which.max(abs(cropcorrs_gs))
# pasture_g_max_s <- which.max(abs(pasturecorrs_gs))
# forested_g_max_s <- which.max(abs(forestedcorrs_gs))
# wetland_g_max_s <- which.max(abs(wetlandcorrs_gs))
# urban_g_max_s <- which.max(abs(urbancorrs_gs))


# # kendall 
# # use the sequences to get all the correlation values for each land cover buffer size
# agcorrs_gk <- corr_k["garter_count", c(ag_seq)]
# forestedcorrs_gk <- corr_k["garter_count", c(forested_seq)]
# wetlandcorrs_gk <- corr_k["garter_count", c(wetland_seq)]
# 
# # find the maximum absolute value correlation coefficient
# ag_g_max_k <- which.max(abs(agcorrs_gk))
# forested_g_max_k <- which.max(abs(forestedcorrs_gk))
# wetland_g_max_k <- which.max(abs(wetlandcorrs_gk))

### while the scale of effect for pearson and spearman varied, the actual difference in 
### correlation values for each buffer size is very small 


## redbellies

# pearson
# use the sequences to get all the correlation values for each land cover buffer size
agcorrs_rbp <- corr_p["redbelly_count", c(ag_seq)]
cropcorrs_rbp <- corr_p["redbelly_count", c(crop_seq)]
pasturecorrs_rbp <- corr_p["redbelly_count", c(pasture_seq)]
forestedcorrs_rbp <- corr_p["redbelly_count", c(forested_seq)]
wetlandcorrs_rbp <- corr_p["redbelly_count", c(wetland_seq)]
urbancorrs_rbp <- corr_p["redbelly_count", c(urban_seq)]

# find the maximum absolute value correlation coefficient
ag_rb_max_p <- which.max(abs(agcorrs_rbp))
crop_rb_max_p <- which.max(abs(cropcorrs_rbp))
pasture_rb_max_p <- which.max(abs(pasturecorrs_rbp))
forested_rb_max_p <- which.max(abs(forestedcorrs_rbp))
wetland_rb_max_p <- which.max(abs(wetlandcorrs_rbp))
urban_rb_max_p <- which.max(abs(urbancorrs_rbp))

summary(agcorrs_rbp)
summary(cropcorrs_rbp)
summary(pasturecorrs_rbp)
summary(forestedcorrs_rbp)
summary(wetlandcorrs_rbp)
summary(urbancorrs_rbp)



## SVL correlations

# get correlation coefficient for svl vs all the landcovers 

# garters
svl_lr_g <- subset(svl_lr, spp == "garter")
# remove columns that are not numerical
svl_lr_g <- svl_lr_g %>% select(-c(site, spp)) 

# # check normality
# hist(svl_lr_g$svl_cm)
# # not normally distributed

# redbellies
svl_lr_rb <- subset(svl_lr, spp == "redbelly")
svl_lr_rb <- svl_lr_rb %>% select(-c(site, spp)) 

# # check normality
# hist(svl_lr_rb$svl_cm)
# # not normally distributed


## garters

# Pearson's coefficient
corrsvl_gp <- cor(svl_lr_g)

# # since not normally distributed, get Spearman's correlation coefficient
# corrsvl_gs <- cor(svl_lr_g, method = "spearman")

# # also try Kendall's
# corrsvl_gk <- cor(svl_lr_g, method = "kendall")

# pearson
# use the sequences to get all the correlation values for each land cover buffer size
agcorrs_svl_gp <- corrsvl_gp["svl_cm", c(ag_seq)]
cropcorrs_svl_gp <- corrsvl_gp["svl_cm", c(crop_seq)]
pasturecorrs_svl_gp <- corrsvl_gp["svl_cm", c(pasture_seq)]
forestedcorrs_svl_gp <- corrsvl_gp["svl_cm", c(forested_seq)]
wetlandcorrs_svl_gp <- corrsvl_gp["svl_cm", c(wetland_seq)]
urbancorrs_svl_gp <- corrsvl_gp["svl_cm", c(urban_seq)]

# find the maximum absolute value correlation coefficient
ag_gsvl_max_p <- which.max(abs(agcorrs_svl_gp))
crop_gsvl_max_p <- which.max(abs(cropcorrs_svl_gp))
pasture_gsvl_max_p <- which.max(abs(pasturecorrs_svl_gp))
forested_gsvl_max_p <- which.max(abs(forestedcorrs_svl_gp))
wetland_gsvl_max_p <- which.max(abs(wetlandcorrs_svl_gp))
urban_gsvl_max_p <- which.max(abs(urbancorrs_svl_gp))

# look at how different the correlation values are for each buffer size
summary(agcorrs_svl_gp)
summary(cropcorrs_svl_gp)
summary(pasturecorrs_svl_gp)
summary(forestedcorrs_svl_gp)
summary(wetlandcorrs_svl_gp)
summary(urbancorrs_svl_gp)


# # spearman 
# # use the sequences to get all the correlation values for each land cover buffer size
# agcorrs_svl_gs <- corrsvl_gs["svl_cm", c(ag_seq)]
# forestedcorrs_svl_gs <- corrsvl_gs["svl_cm", c(forested_seq)]
# wetlandcorrs_svl_gs <- corrsvl_gs["svl_cm", c(wetland_seq)]
# 
# # find the maximum absolute value correlation coefficient
# ag_gsvl_max_s <- which.max(abs(agcorrs_svl_gs))
# forested_gsvl_max_s <- which.max(abs(forestedcorrs_svl_gs))
# wetland_gsvl_max_s <- which.max(abs(wetlandcorrs_svl_gs))


# # kendall 
# # use the sequences to get all the correlation values for each land cover buffer size
# cropcorrs_gk <- corr_k["garter_count", c(crop_seq)]
# pasturecorrs_gk <- corr_k["garter_count", c(pasture_seq)]
# forestedcorrs_gk <- corr_k["garter_count", c(forested_seq)]
# wetlandcorrs_gk <- corr_k["garter_count", c(wetland_seq)]
# 
# # find the maximum absolute value correlation coefficient
# crop_g_max_k <- which.max(abs(cropcorrs_gk))
# pasture_g_max_k <- which.max(abs(pasturecorrs_gk))
# forested_g_max_k <- which.max(abs(forestedcorrs_gk))
# wetland_g_max_k <- which.max(abs(wetlandcorrs_gk))


## redbellies

# Pearson's coefficient
corrsvl_rbp <- cor(svl_lr_rb)

# pearson
# use the sequences to get all the correlation values for each land cover buffer size
agcorrs_svl_rbp <- corrsvl_rbp["svl_cm", c(ag_seq)]
cropcorrs_svl_rbp <- corrsvl_rbp["svl_cm", c(crop_seq)]
pasturecorrs_svl_rbp <- corrsvl_rbp["svl_cm", c(pasture_seq)]
forestedcorrs_svl_rbp <- corrsvl_rbp["svl_cm", c(forested_seq)]
wetlandcorrs_svl_rbp <- corrsvl_rbp["svl_cm", c(wetland_seq)]
urbancorrs_svl_rbp <- corrsvl_rbp["svl_cm", c(urban_seq)]

# find the maximum absolute value correlation coefficient
ag_rbsvl_max_p <- which.max(abs(agcorrs_svl_rbp))
crop_rbsvl_max_p <- which.max(abs(cropcorrs_svl_rbp))
pasture_rbsvl_max_p <- which.max(abs(pasturecorrs_svl_rbp))
forested_rbsvl_max_p <- which.max(abs(forestedcorrs_svl_rbp))
wetland_rbsvl_max_p <- which.max(abs(wetlandcorrs_svl_rbp))
urban_rbsvl_max_p <- which.max(abs(urbancorrs_svl_rbp))

# look at how different the correlation values are for each buffer size
summary(agcorrs_svl_rbp)
summary(cropcorrs_svl_rbp)
summary(pasturecorrs_svl_rbp)
summary(forestedcorrs_svl_rbp)
summary(wetlandcorrs_svl_rbp)
summary(urbancorrs_svl_rbp)

###########################################

# Append the landcover data for the buffers for the scale of max effect

# append to the abundance data
# get the land covers that correspond to the scale of max effect for abundance
buffers_abund <- c(names(ag_g_max_p), names(ag_rb_max_p), 
                   names(crop_g_max_p), names(crop_rb_max_p),
                   names(pasture_g_max_p), names(pasture_rb_max_p),
                   names(forested_g_max_p), names(forested_rb_max_p), 
                   names(wetland_g_max_p), names(wetland_rb_max_p),
                   names(urban_g_max_p), names(urban_rb_max_p))
landcovers_sme_abund <- abund_lr %>% 
  select(site, all_of(buffers_abund))
landcovers_sme_abund <- unique(landcovers_sme_abund) # have a single row for each site

abund_data <- merge(abund_data, landcovers_sme_abund, by = "site")

write.csv(abund_data, "output/abund_data.csv")

# append to the svl data
# get the land covers that correspond to the scale of max effect for svl
buffers_svl <- c(names(ag_gsvl_max_p), names(ag_rbsvl_max_p), 
                 names(crop_gsvl_max_p), names(crop_rbsvl_max_p),
                 names(pasture_gsvl_max_p), names(pasture_rbsvl_max_p),
                 names(forested_gsvl_max_p), names(forested_rbsvl_max_p), 
                 names(wetland_gsvl_max_p), names(wetland_rbsvl_max_p),
                 names(urban_gsvl_max_p), names(urban_rbsvl_max_p))
landcovers_sme_svl <- abund_lr %>% 
  select(site, all_of(buffers_svl))
landcovers_sme_svl <- unique(landcovers_sme_svl) # have a single row for each site

svl_data <- merge(svl_data, landcovers_sme_svl, by = "site")

write.csv(svl_data, "output/svl_data.csv")

################################################################################

# Abundance by sex

# get the number of garters and redbellies per survey
abund_sex_data <- snakesurvey_data %>%
  group_by(year, day_of_year, site, temp_c, time_of_day) %>%
  summarise(
    m_garter_count = sum(spp == "garter" & status == "unique" & sex == "M", na.rm = FALSE),
    f_garter_count = sum(spp == "garter" & status == "unique" & sex == "F", na.rm = FALSE),
    j_garter_count = sum(spp == "garter" & status == "unique" & sex == "J", na.rm = FALSE),
    m_redbelly_count = sum(spp == "redbelly" & status == "unique" & sex == "M", na.rm = FALSE),
    f_redbelly_count = sum(spp == "redbelly" & status == "unique" & sex == "F", na.rm = FALSE),
    j_redbelly_count = sum(spp == "redbelly" & status == "unique" & sex == "J", na.rm = FALSE),
    .groups = 'keep')

# append the number of cover boards 
abund_sex_data <- merge(abund_sex_data, num_boards, by = "site", all.x = TRUE)

# append the landcovers data
abund_sex_data <- merge(abund_sex_data, landcovers_sme_abund, by = "site")

# append the veg cover
abund_sex_data <- merge(abund_sex_data, as.data.frame(vc_data), all.x = TRUE)
abund_sex_data <- abund_sex_data %>% mutate_at(c("site", "year"), as.factor)
abund_sex_data <- abund_sex_data %>% mutate_at("time_of_day", as.numeric)

# fill in missing veg cover data (days that were surveyed but not photographed)
abund_sex_data <- abund_sex_data
# create a new column for vc_mod (the filled in vc values)
abund_sex_data[ , "vc_mod"] <- NA

# get the vc_values and the day (with all NA rows omitted)
vc_values <- na.omit(abund_sex_data %>% select(site, day_of_year, mean_vc))

for (n in 1:nrow(abund_sex_data)) {
  value <- abund_sex_data[n, "mean_vc"]
  if (is.na(value)){
    # if value is NA, vc_mod becomes the vc value with the closest matching day in vc_values
    site_n <- abund_sex_data[n, "site"]
    # check if the site is in vc data (had vc pictures taken)
    if (site_n %in% vc_values$site) {
      vc_values_site <- vc_values %>% 
        subset(vc_values$site == site_n)
      abund_sex_data[n, "vc_mod"] <- vc_values_site[which.min(abs(abund_sex_data[n, "day_of_year"] - vc_values_site$day_of_year)), "mean_vc"]
    } 
  } else {
    # if value isn't NA, vc_mod = vc (ie. it has a vc value, so it remains what it is)
    abund_sex_data[n, "vc_mod"] <- abund_sex_data[n, "mean_vc"]
  }
}
# set the vc_mod value to NA for surveys in 2023
abund_sex_data$vc_mod[abund_sex_data$year == 2023] <- NA


# standardized total abundance by spp by sex

# get the total number of garters and redbellies by sex caught at each site (not per survey)
total_abund_sex <- snakesurvey_data %>%
  select(site, spp, status, sex)
abund_sex_unq <- total_abund_sex %>%
  filter(spp == "garter" & status == "unique" |  # only looking at unique snakes
           spp == "redbelly" & status == "unique"
         )
# change necessary columns to factors
abund_sex_unqf <- abund_sex_unq %>% 
  mutate_at(c("site", "spp", "sex"), as.factor)

abund_sex_df <- abund_sex_unqf %>% count(site, spp, sex, .drop = FALSE)

# remove rows where sex is blank
abund_sex_df <- abund_sex_df %>% 
  filter(sex != "")

# append the number of coverboards
abund_sex_df <- merge(abund_sex_df, num_boards, by = "site", all.x = TRUE)

# get the number of times each site was surveyed
survey_days <- snakesurvey_data %>%   
  count(site, year, day_of_year)  
# get a count for each unique day a site was surveyed by year
# (don't want to re-count observations of the same survey or miscount sites surveyed on the same day both years)
num_surveys_yr <- survey_days %>%  
  group_by(site, year) %>%     
  summarize(num_surveys = length(unique(day_of_year)))  
num_surveys <- num_surveys_yr %>% 
  group_by(site) %>%        
  summarize(num_surveys = sum(num_surveys))   # sum up the incidences across both years

# append the number of surveys
abund_sex_df <- merge(abund_sex_df, num_surveys, by = "site", all.x = TRUE)

abund_sex_df <- abund_sex_df %>% 
  rename("abundance" = "n")

# standardize snake/sex counts 
std_abund_sex <- abund_sex_df %>% 
  mutate(std_abundance = abundance/num_coverboards/num_surveys)

# merge all the land road data
std_abund_sex_lr <- std_abund_sex
for (i in 1:length(landroad_list)) {
  landroad_df <- landroad_list[[i]] %>% select(site, agriculture, forested, wetland, urban)
  landroad_df <- landroad_df %>% 
    # rename the column names to correspond to the buffer size for the data being added
    # use !! to "unquote the expression" to evaluate paste() first for the new name
    # use := for renaming columns using a dynamic name (dplyr syntax)
    rename(!!paste("agriculture_", i*100, sep = "") := agriculture,
           !!paste("forested_", i*100, sep = "") := forested,
           !!paste("wetland_", i*100, sep = "") := wetland,
           !!paste("urban_", i*100, sep = "") := urban)
  std_abund_sex_lr <- merge(std_abund_sex_lr, landroad_df, by = "site", all.x = TRUE)
}


################################################################################

# long data frame of abundance data to get species as a factor
# (for plotting)

abund_data_dfl <- abund_data %>% 
  gather(spp, abundance, garter_count:redbelly_count)
abund_data_dfl$spp <- fct_recode(abund_data_dfl$spp, 
                                 Garter = "garter_count",
                                 Redbelly = "redbelly_count")
abund_data_dfl <- abund_data_dfl %>% 
  rename(Species = spp)

