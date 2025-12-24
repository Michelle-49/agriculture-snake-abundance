# Final models - svl

library(lmerTest)
library(lmtest)
library(nlme)
library(statmod)
library(stats)
# library(tidyverse)
# library(performance)
# library(DHARMa)
# library(AICcmodavg)

# source in the data
source("data_wrangling.R")


# land cover buffers for scale of max effect for garter svl
ag_gsvl_max_p
crop_gsvl_max_p
pasture_gsvl_max_p
forested_gsvl_max_p
wetland_gsvl_max_p
urban_gsvl_max_p
het_gsvl_max_p

# land cover buffers for scale of max effect for redbelly svl
ag_rbsvl_max_p
crop_rbsvl_max_p
pasture_rbsvl_max_p
forested_rbsvl_max_p
wetland_rbsvl_max_p
urban_rbsvl_max_p
het_rbsvl_max_p

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
         urban_rb = urban_300,
         het_index_g = het_index_100,
         het_index_rb = het_index_300
  )
# add a quadratic term for day of year
svl_data <- svl_data %>% 
  mutate(day_of_year2 = I(svl_data$day_of_year^2))

# filter out neonates from data set
svl_data_ad <- svl_data %>%
  filter(age_class != "N")


### Garter models

# get a subset of the data for garter snakes
svl_data_g <- svl_data_ad %>% 
  filter(spp == "garter")


## Model with vegetation cover

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
gsvl_crop_vc3 <- update(gsvl_crop_vc, .~. - forested_g - temp_c)
summary(gsvl_crop_vc2)

final_mod_gsvl_cropvc <- gsvl_crop_vc2


# LRT to compare models
gsvl_crop_vc_ML <- update(gsvl_crop_vc, .~., method = "ML")  #change the model fitting method to ML for comparison
gsvl_crop_vc2_ML <- update(gsvl_crop_vc2, .~., method = "ML")
anova(gsvl_crop_vc_ML, gsvl_crop_vc2_ML)
# no sig diff btwn models


# check model fit

plot(gsvl_crop_vc2, xlab = "fitted(gsvl_crop_vc)")
# fitted values still somewhat clustered due to sexes, but much better

# check that residuals are normally distributed
qqnorm(residuals(gsvl_crop_vc2))
qqline(residuals(gsvl_crop_vc2))
# deviates a bit at the extremes, but pretty good

# collinearity
vif(gsvl_crop_vc2)
# all VIFs < 1.5 - good


# null hypothesis testing
gsvl_crop_vc_null <- update(gsvl_crop_vc_ML, .~. - crop_g)
anova(gsvl_crop_vc_null, gsvl_crop_vc_ML)
# model without crop actually nearly significantly better


# test with heterogeneity and connectivity indices

gsvl_cropvc_ch <- update(final_mod_gsvl_cropvc, .~. + het_index_g + connected)
summary(gsvl_cropvc_ch)
# no correlation or other issues, but highly non-significant

gsvl_cropvc_h <- update(final_mod_gsvl_cropvc, .~. + het_index_g)
summary(gsvl_cropvc_h)
gsvl_cropvc_c <- update(final_mod_gsvl_cropvc, .~. + connected)
summary(gsvl_cropvc_c)

#################################

## Model without vegetation cover

gsvl_crop <- update(gsvl_crop_vc, .~. - vc_mod)
summary(gsvl_crop)

gsvl_crop2 <- update(gsvl_crop, .~. - forested_g)
gsvl_crop3 <- update(gsvl_crop, .~. - forested_g - temp_c)
summary(gsvl_crop2)

final_mod_gsvl_crop <- gsvl_crop2


# LRT to compare models
gsvl_crop_ML <- update(gsvl_crop, .~., method = "ML")
final_mod_gsvl_crop_ML <- update(gsvl_crop2, .~., method = "ML")
anova(gsvl_crop_ML, final_mod_gsvl_crop_ML)
# no sig diff btwn models


# check model fit

plot(final_mod_gsvl_crop, xlab = "fitted(gsvl_crop)")
# fitted values still somewhat clustered due to sexes, but much better

# check that residuals are normally distributed
qqnorm(residuals(final_mod_gsvl_crop))
qqline(residuals(final_mod_gsvl_crop))
# deviates a bit at the extremes, but not bad

# collinearity
vif(final_mod_gsvl_crop)
# all VIFs < 1.5 - good


# null hypothesis testing
gsvl_crop_null <- update(gsvl_crop_ML, .~. - crop_g)
anova(gsvl_crop_null, final_mod_gsvl_crop_ML)
# no sig diff


# test with heterogeneity and connectivity indices

gsvl_crop_ch <- update(final_mod_gsvl_crop, .~. + het_index_g + connected)
summary(gsvl_crop_ch)
# no correlation or other issues, but highly non-significant

gsvl_crop_h <- update(final_mod_gsvl_crop, .~. + het_index_g)
summary(gsvl_crop_h)
gsvl_crop_c <- update(final_mod_gsvl_crop, .~. + connected)
summary(gsvl_crop_c)

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
rbsvl_crop_vc3 <- update(rbsvl_crop_vc, .~. - forested_rb - time_of_day)
summary(rbsvl_crop_vc2)

final_mod_rbsvl_cropvc <- rbsvl_crop_vc2


# LRT to compare models
rbsvl_crop_vc_ML <- update(rbsvl_crop_vc, .~., method = "ML")
rbsvl_crop_vc2_ML <- update(rbsvl_crop_vc2, .~., method = "ML")
anova(rbsvl_crop_vc_ML, rbsvl_crop_vc2_ML)
# no sig diff btwn models


# check model fit

plot(rbsvl_crop_vc2, xlab = "fitted(rbsvl_crop_vc2)")
# fitted values clustered due to sexes

# check that residuals are normally distributed
qqnorm(residuals(rbsvl_crop_vc2))
qqline(residuals(rbsvl_crop_vc2))
# deviates a bit, not bad

# collinearity
vif(rbsvl_crop_vc2)
# VIFs < 2.8 - still good


# null hypothesis testing
rbsvl_crop_vc_null <- update(rbsvl_crop_vc2_ML, .~. - crop_rb)
anova(rbsvl_crop_vc_null, rbsvl_crop_vc2_ML)
# no sig diff


# test with heterogeneity and connectivity indices

rbsvl_cropvc_h <- update(final_mod_rbsvl_cropvc, .~. + het_index_rb)
summary(rbsvl_cropvc_h)
# correlated and highly non-sig

# cannot include connectivity b/c redbellys only found at connected sites, 
# so interpreting variable as having only one level of factors


####################################

## Model without vegetation cover
rbsvl_crop <- update(rbsvl_crop_vc, .~. - vc_mod)
summary(rbsvl_crop)

# reduce the model - forested correlated with crop
rbsvl_crop2 <- update(rbsvl_crop, .~. - forested_rb)
summary(rbsvl_crop2)

final_mod_rbsvl_crop <- rbsvl_crop2


# LRT to compare models
rbsvl_crop_ML <- update(rbsvl_crop, .~., method = "ML")
rbsvl_crop2_ML <- update(rbsvl_crop2, .~., method = "ML")
anova(rbsvl_crop_ML, rbsvl_crop2_ML)
# no sig diff btwn models


# check model fit

plot(rbsvl_crop2, xlab = "fitted(rbsvl_crop2)")
# fitted values clustered due to sexes

# check that residuals are normally distributed
qqnorm(residuals(rbsvl_crop2))
qqline(residuals(rbsvl_crop2))
# deviates a bit, not bad

# collinearity
vif(rbsvl_crop2)
# VIFs <= 2.0 - good


# null hypothesis testing
rbsvl_crop_null <- update(rbsvl_crop2_ML, .~. - crop_rb)
anova(rbsvl_crop_null, rbsvl_crop2_ML)
# no sig diff


# test with heterogeneity and connectivity indices

rbsvl_crop_h <- update(final_mod_rbsvl_crop, .~. + het_index_rb)
summary(rbsvl_crop_h)
# correlated and non-sig
