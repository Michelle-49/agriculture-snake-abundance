# Final models - svl

library(lmerTest)
library(lmtest)
library(nlme)
library(tidyverse)
library(performance)
library(statmod)
library(stats)


# source in the data
source("data_wrangling.R")


# land cover buffers for scale of max effect for garter svl
ag_gsvl_max_p
crop_gsvl_max_p
pasture_gsvl_max_p
forested_gsvl_max_p
wetland_gsvl_max_p
urban_gsvl_max_p

# land cover buffers for scale of max effect for redbelly svl
ag_rbsvl_max_p
crop_rbsvl_max_p
pasture_rbsvl_max_p
forested_rbsvl_max_p
wetland_rbsvl_max_p
urban_rbsvl_max_p

# set the landcover variables for the scale of max effect for garter and redbelly
svl_data <- svl_data %>% 
  mutate(agriculture_g = agriculture_1000,
         agriculture_rb = agriculture_100,
         crop_g = crop_800,
         crop_rb = crop_800,
         pasture_g = pasture_900,
         pasture_rb = pasture_800,
         forested_g = forested_200,
         forested_rb = forested_600,
         wetland_g = wetland_300,
         wetland_rb = wetland_1000,
         urban_g = urban_1000,
         urban_rb = urban_300
  )

# add a quadratic term for day of year
svl_data <- svl_data %>% 
  mutate(day_of_year2 = I(svl_data$day_of_year^2))

# filter out snakes too small to sex
svl_data_ad <- svl_data %>%
  filter(sex != "U")


### Garter models

# get a subset of the data for garter snakes
svl_data_g <- svl_data_ad %>% 
  filter(spp == "garter")


## Fit model with vegetation cover

# full model
gsvl_crop_vc <- lme(svl_cm ~ crop_g + pasture_g + forested_g + wetland_g + urban_g
                    + day_of_year + temp_c + time_of_day + sex + vc_mod, 
                    random = ~ 1 | site,            # random effects structure
                    weights = varIdent(form = ~ 1 | sex),  # variance weight for sex
                    data = svl_data_g,
                    control =list(msMaxIter = 1000, msMaxEval = 1000),
                    na.action = na.exclude)
summary(gsvl_crop_vc)

# reduced model
gsvl_crop_vc2 <- update(gsvl_crop_vc, .~. - forested_g)
summary(gsvl_crop_vc2)
final_mod_gsvl_cropvc <- gsvl_crop_vc2


## Model without vegetation cover

gsvl_crop <- update(gsvl_crop_vc, .~. - vc_mod)
summary(gsvl_crop)

gsvl_crop2 <- update(gsvl_crop, .~. - forested_g)
summary(gsvl_crop2)
final_mod_gsvl_crop <- gsvl_crop2


################################################################################

### Redbelly models

svl_data_rb <- svl_data_ad %>% 
  filter(spp == "redbelly")


## Fit model with vegetation cover

# full model
rbsvl_crop_vc <- lme(svl_cm ~ crop_rb + pasture_rb + forested_rb + wetland_rb + urban_rb
                     + day_of_year + temp_c + time_of_day + sex + vc_mod, 
                     random = ~ 1 | site,
                     weights = varIdent(form = ~ 1 | sex),  # weights for variance structure
                     data = svl_data_rb,
                     control =list(msMaxIter = 1000, msMaxEval = 1000),
                     na.action = na.exclude)
summary(rbsvl_crop_vc)

# reduced model
rbsvl_crop_vc2 <- update(rbsvl_crop_vc, .~. - forested_rb)
summary(rbsvl_crop_vc2)
final_mod_rbsvl_cropvc <- rbsvl_crop_vc2


## Fit model without vegetation cover

# full model
rbsvl_crop <- update(rbsvl_crop_vc, .~. - vc_mod)

# reduced model
rbsvl_crop2 <- update(rbsvl_crop, .~. - forested_rb)
summary(rbsvl_crop2)
final_mod_rbsvl_crop <- rbsvl_crop2


rbsvl_crop_h <- update(final_mod_rbsvl_crop, .~. + het_index_rb)
summary(rbsvl_crop_h)
# correlated and non-sig

