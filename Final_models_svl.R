# Final models - svl

library(lmerTest)
library(lmtest)
library(nlme)
library(tidyverse)
library(performance)
library(statmod)
library(stats)
# library(DHARMa)
library(AICcmodavg)

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


### Garter models

# get a subset of the data for garter snakes
svl_data_g <- svl_data %>% 
  filter(spp == "garter")


## 2024 veg cover

svl_data_g2024 <- svl_data_g %>% 
  filter(year == 2024)
svl_data_g2024$sex <- as.factor(svl_data_g2024$sex)

# full model
gsvl_2024 <- lme(svl_cm ~ agriculture_g + forested_g + wetland_g + urban_g
                      + day_of_year + temp_c + time_of_day + sex + vc_mod,
                      random = ~ 1 | site,                   # random effects structure
                      weights = varIdent(form = ~ 1 | sex),  # weights for variance structure
                      data = svl_data_g2024,
                      control =list(msMaxIter = 1000, msMaxEval = 1000),
                      method = "REML"
                 )
# reduced model
gsvl_2024_red <- update(gsvl_2024, .~. - forested_g - temp_c)
final_mod_gsvl_2024 <- gsvl_2024_red


# test without unsexed individuals
svl_data_g2024_nou <- svl_data_g2024 %>% 
  filter(sex != "U")

gsvl_2024_nou <- update(gsvl_2024_red, .~., data = svl_data_g2024_nou)



# novc

# full model
gsvl_novc <- lme(svl_cm ~ agriculture_g + forested_g + wetland_g + urban_g
                        + year + day_of_year + temp_c + time_of_day + sex, 
                      random = ~ 1 | site,                   # random effects structure
                      weights = varIdent(form = ~ 1 | sex),  # variance weight for sex
                      data = svl_data_g,
                      control =list(msMaxIter = 1000, msMaxEval = 1000),
                      na.action = na.exclude)
# reduced model
gsvl_novc_red <- update(gsvl_novc, .~. - temp_c - year - forested_g)
final_mod_gsvl_novc <- gsvl_novc_red


# test without unsexed individuals
svl_data_g_nou <- svl_data_g %>% 
  filter(sex != "U")

gsvl_novc_nou <- update(gsvl_novc_red, .~., data = svl_data_g_nou)



## crop and pasture

# full model
gsvl_crop <- lme(svl_cm ~ crop_g + pasture_g + forested_g + wetland_g + urban_g
                   + day_of_year + temp_c + time_of_day + sex + vc_mod, 
                 random = ~ 1 | site,            # random effects structure
                 weights = varIdent(form = ~ 1 | sex),  # variance weight for sex
                 data = svl_data_g2024,
                 control =list(msMaxIter = 1000, msMaxEval = 1000),
                 na.action = na.exclude)
# reduced model
gsvl_crop_red <- update(gsvl_crop, .~. - temp_c - forested_g)
final_mod_gsvl_crop <- gsvl_crop_red


# test without unsexed individuals
gsvl_crop_nou <- update(gsvl_crop_red, .~., data = svl_data_g2024_nou)
summary(gsvl_crop_nou)


################################################################################

### Redbelly models

svl_data_rb <- svl_data %>% 
  filter(spp == "redbelly")


## 2024 veg cover

svl_data_rb2024 <- svl_data_rb %>% 
  filter(year == 2024)


# full model
rbsvl_2024 <- lme(svl_cm ~ agriculture_rb + forested_rb + wetland_rb + urban_rb
                  + day_of_year + temp_c + time_of_day + sex + vc_mod, 
                  random = ~ 1 | site,
                  weights = varIdent(form = ~ 1 | sex),  # weights for variance structure
                  data = svl_data_rb2024,
                  control =list(msMaxIter = 1000, msMaxEval = 1000),
                  method = "ML")
# reduced model
rbsvl_2024_red <- update(rbsvl_2024, .~. - time_of_day)
final_mod_rbsvl_2024 <- rbsvl_2024_red


# test without unsexed individuals
svl_data_rb2024_nou <- svl_data_rb2024 %>% 
  filter(sex != "U")

rbsvl_2024_nou <- update(rbsvl_2024_red, .~. , data = svl_data_rb2024_nou)


# novc

# full model
rbsvl_novc <- lme(svl_cm ~ agriculture_rb + forested_rb + wetland_rb + urban_rb
                         + year + day_of_year + temp_c + time_of_day + sex, 
                       random = ~ 1 | site,
                       weights = varIdent(form = ~ 1 | sex),  # weights for variance structure
                       data = svl_data_rb,
                       # control =list(msMaxIter = 1000, msMaxEval = 1000),
                       na.action = na.exclude)
# reduced model
rbsvl_novc_red <- update(rbsvl_novc, .~. - year - time_of_day)
final_mod_rbsvl_novc <- rbsvl_novc_red


# test without unsexed individuals
svl_data_rb_nou <- svl_data_rb %>% 
  filter(sex != "U")

rbsvl_novc_nou <- update(rbsvl_novc_red, .~., data = svl_data_rb_nou)



## crop and pasture

# full model
rbsvl_crop <- lme(svl_cm ~ crop_rb + pasture_rb + forested_rb + wetland_rb + urban_rb
                    + day_of_year + temp_c + time_of_day + sex + vc_mod, 
                  random = ~ 1 | site,
                  weights = varIdent(form = ~ 1 | sex),  # weights for variance structure
                  data = svl_data_rb2024,
                  control =list(msMaxIter = 1000, msMaxEval = 1000),
                  na.action = na.exclude)
# reduced model
rbsvl_crop_red <- update(rbsvl_crop, .~. - time_of_day - forested_rb)
final_mod_rbsvl_crop <- rbsvl_crop_red


# testing without unsexed individuals
rbsvl_crop_nou <- update(rbsvl_crop_red, .~., data = svl_data_rb2024_nou)


# model comparisons

gsvl_2024_ML <- update(gsvl_2024, .~., method = "ML")
gsvl_2024_redML <- update(gsvl_2024_red, .~., method = "ML")
anova(gsvl_2024_ML, gsvl_2024_redML)
aictab(list(gsvl_2024_ML, gsvl_2024_redML), modnames = c("gsvl_2024_ML", "gsvl_2024_redML"))

gsvl_novc_ML <- update(gsvl_novc, .~., method = "ML")
gsvl_novc_redML <- update(gsvl_novc_red, .~., method = "ML")
anova(gsvl_novc_ML, gsvl_novc_redML)
aictab(list(gsvl_novc_ML, gsvl_novc_redML), modnames = c("gsvl_novc_ML", "gsvl_novc_redML"))

gsvl_crop_ML <- update(gsvl_crop, .~., method = "ML")
gsvl_crop_redML <- update(gsvl_crop_red, .~., method = "ML")
anova(gsvl_crop_ML, gsvl_crop_redML)
aictab(list(gsvl_crop_ML, gsvl_crop_redML), modnames = c("gsvl_crop_ML", "gsvl_crop_redML"))


rbsvl_2024_ML <- update(rbsvl_2024, .~., method = "ML")
rbsvl_2024_redML <- update(rbsvl_2024_red, .~., method = "ML")
anova(rbsvl_2024_ML, rbsvl_2024_redML)
aictab(list(rbsvl_2024_ML, rbsvl_2024_redML), modnames = c("rbsvl_2024_ML", "rbsvl_2024_redML"))

rbsvl_novc_ML <- update(rbsvl_novc, .~., method = "ML")
rbsvl_novc_redML <- update(rbsvl_novc_red, .~., method = "ML")
anova(rbsvl_novc_ML, rbsvl_novc_redML)
aictab(list(rbsvl_novc_ML, rbsvl_novc_redML), modnames = c("rbsvl_novc_ML", "rbsvl_nocv_redML"))

rbsvl_crop_ML <- update(rbsvl_crop, .~., method = "ML")
rbsvl_crop_redML <- update(rbsvl_crop_red, .~., method = "ML")
anova(rbsvl_crop_ML, rbsvl_crop_redML)
aictab(list(rbsvl_crop_ML, rbsvl_crop_redML), modnames = c("rbsvl_crop_ML", "rbsvl_crop_redML"))


