# Final models - abundance


library(lme4)
library(lmerTest)
library(tidyverse)
library(performance)
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
    agriculture_rbsc = scale(agriculture_1000),
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


## 2024 veg cover 

abund_data_2024 <- abund_data %>% 
  filter(year == 2024)

# full model
g2024_poly <- glmer.nb(garter_count ~ agriculture_gsc + forested_gsc + wetland_gsc + urban_gsc
                        + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc + offset(log(num_coverboards)) 
                        + (1|site),
                        data = abund_data_2024)
# reduced model
g2024_m3 <- update(g2024_poly, .~. - temp_csc - forested_gsc)
final_mod_g2024 <- g2024_m3


## novc

# full model
gnovc_poly <- glmer.nb(garter_count ~ agriculture_gsc + forested_gsc + wetland_gsc + urban_gsc
                        + year + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + offset(log(num_coverboards))
                        + (1|site),
                        data = abund_data)
# reduced model
gnovc_m6 <- update(gnovc_poly, .~. - temp_csc - year - forested_gsc)
final_mod_gnovc <- gnovc_m6



## crop and pasture

# full model
gcrop_poly <- glmer.nb(garter_count ~ crop_gsc + pasture_gsc + forested_gsc + wetland_gsc + urban_gsc
                            + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc + offset(log(num_coverboards))
                            + (1|site),
                            control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10e5)),
                            data = abund_data)
# reduced model
gcrop_m3 <- update(gcrop_poly, .~. - temp_csc - forested_gsc)
final_mod_gcrop <- gcrop_m3

################################################################################

### Redbelly models


## 2024 veg cover

# full model
rb2024_poly <- glmer.nb(redbelly_count ~ agriculture_rbsc + forested_rbsc + wetland_rbsc + urban_rbsc
                         + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc
                         + offset(log(num_coverboards))
                         + (1|site),
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10e5)),
                         data = abund_data_2024)
#reduced model
rb2024_m2 <- update(rb2024_poly, .~. - forested_rbsc - temp_csc)
final_mod_rb2024 <- rb2024_m2


## novc

# full model
rbnovc_poly <- glmer.nb(redbelly_count ~ agriculture_rbsc + forested_rbsc + wetland_rbsc + urban_rbsc
                         + year + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + offset(log(num_coverboards)) 
                         + (1|site),
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10e5)),
                         data = abund_data)
# reduced model
rbnovc_m2 <- update(rbnovc_poly, .~. - forested_rbsc - year - temp_csc)
final_mod_rbnovc <- rbnovc_m2


## crop and pasture

# full model
rbcrop_poly <- glmer.nb(redbelly_count ~ crop_rbsc + pasture_rbsc + forested_rbsc + wetland_rbsc + urban_rbsc
                             + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc + offset(log(num_coverboards))
                             + (1|site),
                             control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                             data = abund_data)
# reduced model
rbcrop_m3 <- update(rbcrop_poly, .~. - forested_rbsc - temp_csc)
final_mod_rbcrop <- rbcrop_m3
