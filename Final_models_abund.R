# Modelling abundance - final models

library(lme4)
library(lmerTest)
library(tidyverse)
library(performance)
library(DHARMa)
library(AICcmodavg)
library(ape)


source("data_wrangling.R")

# # read in the formatted data
# abund_data <- data.frame(read.csv("output/abund_data.csv"))
# abund_data <- abund_data %>%
#   mutate_at(c("site", "year"), as.factor)

ag_g_max_p
crop_g_max_p
pasture_g_max_p
forested_g_max_p
wetland_g_max_p
urban_g_max_p
het_g_max_p

ag_rb_max_p
crop_rb_max_p
pasture_rb_max_p
forested_rb_max_p
wetland_rb_max_p
urban_rb_max_p
het_rb_max_p


# center and scale the variables and set a general variable name for the landcovers for scale of max effect
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
    het_index_gsc = scale(het_index_600),
    het_index_rbsc = scale(het_index_100),
    day_of_yearsc = scale(day_of_year),
    time_of_daysc = scale(time_of_day),
    temp_csc = scale(temp_c),
    vc_modsc = scale(vc_mod)
  )

# add a quadratic day of year term
abund_data <- abund_data %>% 
  mutate(day_of_year2 = I(abund_data$day_of_year^2),
         day_of_yearsc2 = I(abund_data$day_of_yearsc^2))


## Garter models

# Fit full model looking at crop and pasture with vegetation cover
gcrop_vc <- glmer.nb(garter_count ~ crop_gsc + pasture_gsc + forested_gsc + wetland_gsc + urban_gsc
                       + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc
                       + offset(log(num_coverboards))
                       + (1|site),
                       control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10e5)),
                       data = abund_data)
summary(gcrop_vc)


# reduce the model - remove forest b/c correlated
gcrop_vc2 <- update(gcrop_vc, .~. - forested_gsc)
gcrop_vc3 <- update(gcrop_vc, .~. - forested_gsc - temp_csc)
summary(gcrop_vc2)

final_mod_gcrop_vc <- gcrop_vc2


# check model fit

# plot residuals with DHARMa
sim_gcrop_vc <- simulateResiduals(final_mod_gcrop_vc)
plot(sim_gcrop_vc)
# looks good

# test dispersion with DHARMa
testDispersion(sim_gcrop_vc, plot = TRUE)
# no over dispersion

# test zero inflation
testZeroInflation(sim_gcrop_vc, plot = TRUE)
# no zero inflation


# look at VIFs (variance inflation factors) for collinearity
vif(final_mod_gcrop_vc)
# all vifs < 2


# LRT to compare reduced models
anova(gcrop_vc, gcrop_vc2, gcrop_vc3)
# gcrop_vc3 does best


# null hypothesis testing
gcrop_vc_null <- update(final_mod_gcrop_vc, .~. - crop_gsc)
anova(gcrop_vc_null, final_mod_gcrop_vc)
# no sig diff btwn models


# test with connectivity and heterogeneity

gcrop_vc_ch <- update(gcrop_vc2, .~. + het_index_gsc + connected)
summary(gcrop_vc_ch)
# heterogeneity and connectivity are correlated with crop and other land covers
gcrop_vc_h <- update(gcrop_vc2, .~. + het_index_gsc)
summary(gcrop_vc_h)
gcrop_vc_c <- update(gcrop_vc2, .~. + connected)
summary(gcrop_vc_c)
# just connectivity causes convergence issues

#####################################

## Fit model without vegetation cover
gcrop <- update(gcrop_vc, .~. - vc_modsc)
summary(gcrop)

# reduce the model - remove forest b/c of correlation
gcrop_2 <- update(gcrop, .~. - forested_gsc)
gcrop_3 <- update(gcrop, .~. - forested_gsc - temp_csc)
summary(gcrop_2)

final_mod_gcrop <- gcrop_2


# check model fit

# plot residuals with DHARMa
sim_gcrop <- simulateResiduals(final_mod_gcrop)
plot(sim_gcrop)
# looks good

# test dispersion with DHARMa
testDispersion(sim_gcrop, plot = TRUE)
# no over dispersion

# test zero inflation
testZeroInflation(sim_gcrop, plot = TRUE)
# no zero inflation


# look at VIFs (variance inflation factors) for collinearity
vif(final_mod_gcrop)
# all vifs < 2


# LRT to compare reduced models
anova(gcrop, final_mod_gcrop)


# null hypothesis testing
gcrop_null <- update(final_mod_gcrop, .~. - crop_gsc)
anova(gcrop_null, final_mod_gcrop)
# model with crop nearly significantly better
# sig with an alpha of 0.10


# test with connectivity and heterogeneity

gcrop_ch <- update(gcrop_2, .~. + het_index_gsc + connected)
summary(gcrop_ch)
# het and connectivity correlated

gcrop_h <- update(gcrop_2, .~. + het_index_gsc)
summary(gcrop_h)
gcrop_c <- update(gcrop_2, .~. + connected)
summary(gcrop_c)

################################################################################

## Redbelly models

## crop and pasture with vegetation cover
rbcrop_vc <- glmer.nb(redbelly_count ~ crop_rbsc + pasture_rbsc + forested_rbsc + wetland_rbsc + urban_rbsc
                         + day_of_yearsc + day_of_yearsc2 + time_of_daysc + temp_csc + vc_modsc 
                         + offset(log(num_coverboards))
                         + (1|site),
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
                         data = abund_data)
summary(rbcrop_vc)


# reduce the model
rbcrop_vc2 <- update(rbcrop_vc, .~. - forested_rbsc)
rbcrop_vc3 <- update(rbcrop_vc, .~. - forested_rbsc - temp_csc)

final_mod_rbcrop_vc <- rbcrop_vc2

summary(final_mod_rbcrop_vc)

# check model fit

# plot residuals with DHARMa
sim_rbcrop_vc <- simulateResiduals(final_mod_rbcrop_vc)
plot(sim_rbcrop_vc)

# test dispersion with DHARMa
testDispersion(sim_rbcrop_vc, plot = TRUE)
# tests look good - don't seem to have over dispersion

# test zero inflation
testZeroInflation(sim_rbcrop_vc, plot = TRUE)
# no zero inflation

# look at collinearity
vif(final_mod_rbcrop_vc)
# good


# null hypothesis test
rbcrop_vc_null <- update(final_mod_rbcrop_vc, .~. - crop_rbsc)
anova(final_mod_rbcrop_vc, rbcrop_vc_null)


# test with heterogeneity index and connectivity

rbcrop_vc_ch <- update(rbcrop_vc2, .~. + het_index_rbsc + connected)
summary(rbcrop_vc_ch)
# Model nearly unidentifiable: large eigenvalue ratio

rbcrop_vc_h <- update(rbcrop_vc2, .~. + het_index_rbsc)
summary(rbcrop_vc_h)
rbcrop_vc_c <- update(rbcrop_vc2, .~. + connected)
summary(rbcrop_vc_c)
# model is nearly unidentifiable: large eigenvalue ratio

#################################

## Model without vegetation cover

rbcrop <- update(rbcrop_vc, .~. - vc_modsc)
summary(rbcrop)


# reduce the model
rbcrop_2 <- update(rbcrop, .~. - forested_rbsc)
rbcrop_3 <- update(rbcrop, .~. - forested_rbsc - temp_csc)

final_mod_rbcrop <- rbcrop_2

summary(final_mod_rbcrop)

# check model fit

# plot residuals with DHARMa
sim_rbcrop <- simulateResiduals(final_mod_rbcrop)
plot(sim_rbcrop)

# test dispersion with DHARMa
testDispersion(sim_rbcrop, plot = TRUE)
# tests look good - don't seem to have over dispersion

# test zero inflation
testZeroInflation(sim_rbcrop, plot = TRUE)
# no zero inflation

# look at collinearity
vif(final_mod_rbcrop)
# good


# null hypothesis test
rbcrop_null <- update(rbcrop_2, .~. - crop_rbsc)
anova(final_mod_rbcrop, rbcrop_null)


# test with heterogeneity index and connectivity

rbcrop_ch <- update(rbcrop_2, .~. + het_index_rbsc + connected)
# Unable to evaluate scaled gradient and model failed to converge

rbcrop_h <- update(rbcrop_2, .~. + het_index_rbsc)
summary(rbcrop_h)
rbcrop_c <- update(rbcrop_2, .~. + connected)
summary(rbcrop_c)
# Unable to evaluate scaled gradient and model failed to converge



# comparing R2 of models

performance::r2(final_mod_gcrop)
performance::r2(final_mod_gcrop_vc)


performance::r2(final_mod_rbcrop)
performance::r2(final_mod_rbcrop_vc)
# 2024 model has higher R2 for redbelly models

