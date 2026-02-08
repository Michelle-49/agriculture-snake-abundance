# Final models - abundance


library(lme4)
library(lmerTest)
library(tidyverse)
library(performance)
library(clubSandwich)
# library(DHARMa)
# library(AICcmodavg)


# source in the data
source("data_wrangling.R")


# land cover buffers for scale of max effect for garter abundance:
ag_g_max_p
crop_g_max_p
pasture_g_max_p
forested_g_max_p
wetland_g_max_p
urban_g_max_p

# land cover buffers for scale of max effect for redbelly abundance:
ag_rb_max_p
crop_rb_max_p
pasture_rb_max_p
forested_rb_max_p
wetland_rb_max_p
urban_rb_max_p


# center and scale the variables
# set the landcover variables for the scale of max effect for garter and redbelly
abund_data <- abund_data %>%
  mutate(
    agriculture_gsc = scale(agriculture_400),
    agriculture_rbsc = scale(agriculture_900),
    crop_gsc = scale(crop_1000),
    crop_rbsc = scale(crop_300),
    pasture_gsc = scale(pasture_900),
    pasture_rbsc = scale(pasture_900),
    forested_gsc = scale(forested_800),
    forested_rbsc = scale(forested_100),
    wetland_gsc = scale(wetland_1000),
    wetland_rbsc = scale(wetland_1000),
    urban_gsc = scale(urban_700),
    urban_rbsc = scale(urban_100),
    day_of_yearsc = scale(day_of_year),
    time_of_daysc = scale(time_of_day),
    temp_csc = scale(temp_c),
    vc_modsc = scale(vc_mod)
  )
abund_data <- abund_data %>% 
  mutate(day_of_year2 = I(abund_data$day_of_year^2),
         day_of_yearsc2 = I(abund_data$day_of_yearsc^2))


### Garter models

## Model with vegetation cover

# full model
gcrop_vc <- glmer.nb(garter_count ~ crop_gsc + pasture_gsc + forested_gsc + wetland_gsc + urban_gsc
                       + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc
                       + offset(log(num_coverboards))
                       + (1|site),
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10e5)),
                       data = abund_data,
                       na.action = na.exclude)


# reduced model
gcrop_vc_m2 <- update(gcrop_vc, .~. -forested_gsc)
final_mod_gcrop_vc <- gcrop_vc_m2



## Without vegetation cover
gcrop <- update(gcrop_vc, .~. - vc_modsc)

# reduced model
gcrop_m2 <- update(gcrop, .~. - forested_gsc)
final_mod_gcrop <- gcrop_m2

################################################################################

### Redbelly models

## Model with vegetation cover

# full model
rbcrop_vc <- glmer.nb(redbelly_count ~ crop_rbsc + pasture_rbsc + forested_rbsc + wetland_rbsc + urban_rbsc
                             + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc 
                             + offset(log(num_coverboards))
                             + (1|site),
                             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                             data = abund_data)
# reduced model
rbcrop_vc_m2 <- update(rbcrop_vc, .~. - forested_rbsc)
final_mod_rbcrop_vc <- rbcrop_vc_m2


## Model without vegetation cover

# full model
rbcrop <- update(rbcrop_vc, .~. - vc_modsc)

# reduced model
rbcrop_vc_m2 <- update(rbcrop_vc, .~. - forested_rbsc)
final_mod_rbcrop <- rbcrop_vc_m2

