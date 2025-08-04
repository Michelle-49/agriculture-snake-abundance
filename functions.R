# functions

# format images into arrays
  # added filepath argument, to y2 <- line, ie. paste(filepath,...))
    # because function would only work when full.names = TRUE for the image list
    # but then run into errors later with other functions, so this fixes that
  # added image_convert() since my files were DNG, not TIFF
format.image <- function(y, filepath) {
  # creates an empty list
  list_array <- list()
  for (n in 1:length(y)) {
    # transforms the name of the pictures
    #image <- paste("Array", y[n], sep = "") 
    # read the image file
    y2 <- image_read(paste(filepath, y[n], sep = "/")) 
    y2 <- image_convert(y2, format = "tiff") # convert files to TIFF format
    #print(dim(y2))
    #print(y2) 
    #print(image_info(y2))
    # converts to an array to be able to change the color values
    y_array <- as.integer(y2[[1]]) 
    y_array <- transpose(y_array)
    y_array <- y_array / 255 
    #print(y_array)
    list_array[[as.character(y[n])]] <- y_array 
  }
  return(list_array)
}


# format image to greyscale then to an array
format.image.grs <- function(y, filepath) {
  # creates an empty list
  list_array <- list()
  for (n in 1:length(y)) {
    # transforms the name of the pictures
    #image <- paste("Array", y[n], sep = "") 
    # read the image file
    y2 <- image_read(paste(filepath, y[n], sep = "/")) 
    y2 <- image_convert(y2, format = "tiff", colorspace = "gray") # convert files to TIFF format and to greyscale
    #print(dim(y2))
    #print(image_info(y2))
    # converts to an array to be able to change the color values
    y_array <- as.integer(y2[[1]]) 
    y_array <- transpose(y_array)
    y_array <- y_array / 255 
    #print(y_array)
    list_array[[as.character(y[n])]] <- y_array 
  }
  return(list_array)
}


# standardize images - removes variation in pixel values (brightness)
standardize.image <- function(x, output_path) {
  for(m in 1:length(x)){
    # name red layer 
    r <- x[[m]][, , 1]
    # name green layer
    g <- x[[m]][, , 2]
    # name blue layer
    b <- x[[m]][, , 3] 
    # Standardize red channel
    pr <- r / (r + g + b)
    # Standardize green channel
    pg <- g / (r + g + b)
    # Standardize blue channel
    pb <- b / (r + g + b) 
    # Dividing by 0 outputs NaN in R. We need to replace NaN with 0 (so that they are equal to the darkest pixel value).
    pr[is.na(pr)] <- 0
    pg[is.na(pg)] <- 0
    pb[is.na(pb)] <- 0
    # stack and convert to image. Order is important in the three layers
    # this image is now standardized (colors might seem weird because of the standardization)
    z <- Image(array(dim = c(nrow(pr), ncol(pr), 3), data = cbind(pr, pg, pb))) 
    # set to color mode
    colorMode(z) <- Color
    # the new picture will use the origin name, but will add "_stand" at the end and will be saved in the .TIFF format
    name <- gsub(".DNG", '', names(x[m]))  # remove old file extensions from file name
    name <- gsub(".jpg", '', name)
    output_filename <- paste(name, "_stand", ".TIFF", sep = "")
    # specify the path where the standardized pictures will be saved
    output_fullpath <- file.path(output_path, output_filename)
    # save as new image (with "_stand" added at the end) in 16-bit TIFF format
    writeImage(z, output_fullpath) 
  }
}  


# binarize - isolate the colour of interest
binarize.blue <- function(x, output_path) {
  # creates an empty database to compile proportion of colored pixels at the end
  Prop_list <- NA 
  for(m in 1:length(x)){
    # name red channel
    pr <- x[[m]][, , 1]
    # name green channel
    pg <- x[[m]][, , 2]
    # name blue channel
    pb <- x[[m]][, , 3] 
    # stack and convert to image. Order is important. Create a new picture with the colour of interest isolated 
    z <- Image(array(dim = c(nrow(pb), ncol(pb), 1), data = pb))   
    # the new picture will use the origin name, but will add "_blue" at the end and will be saved in .TIFF format
    output_filename <- paste(names(x[m]), "_blue", ".TIFF", sep = "") 
    # specify the path where the pictures will be saved
    output_fullpath <- file.path(output_path, output_filename) 
    # save as a new picture in .TIFF format with "_blue" added at the end of the name in the folder selected
    writeImage(z, output_fullpath) 
  }
} 


# binarize - isolate white-grey
binarize.wg <- function(x, output_path) {
  # creates an empty database to compile proportion of colored pixels at the end
  Prop_list <- NA 
  for(m in 1:length(x)){
    # greyscale channel
    pgr <- x[[m]][, , 1]
    # stack and convert to image. Order is important. Create a new picture with the colour of interest isolated 
    z <- Image(array(dim = c(nrow(pgr), ncol(pgr), 1), data = pgr))   
    # the new picture will use the origin name, but will add "_wg" at the end and will be saved in .TIFF format
    output_filename <- paste(names(x[m]), "_wg", ".TIFF", sep = "") 
    # specify the path where the pictures will be saved
    output_fullpath <- file.path(output_path, output_filename) 
    # save as a new picture in .TIFF format with "_wg" added at the end of the name in the folder selected
    writeImage(z, output_fullpath) 
  }
} 


# function to get the reflectance values for a list of pictures to determine the threshold
reflectance.values.blue <- function(x, coords, output_path) {
  Prop_list_wg <- NA 
  for(m in 1:length(x)){
    pr <- x[[m]][, , 1]
    pg <- x[[m]][, , 2]
    pb <- x[[m]][, , 3]
    z <- Image(array(dim = c(nrow(pb), ncol(pb), 1), data = pb))   
    output_filename <- paste(names(x[m]), "_blue", ".TIFF", sep = "") 
    output_fullpath <- file.path(output_path, output_filename)
    #writeImage(z, output_fullpath)
    # using the coordinates data frame we gathered from ImageJ
    print(c(z[coords[m,1], coords[m,2], 1], paste("Reflectance values - blue pixel")))
  }
  return(Prop_list_wg)
} 


# function to get the reflectance values for a list of pictures to determine the threshold
reflectance.values.wg <- function(x, coords, output_path) {
  Prop_list_wg <- NA 
  for(m in 1:length(x)){
    pgr <- x[[m]][, , 1] 
    z <- Image(array(dim = c(nrow(pgr), ncol(pgr), 1), data = pgr))   
    output_filename <- paste(names(x[m]), "_wg", ".TIFF", sep = "") 
    output_fullpath <- file.path(output_path, output_filename)
    #writeImage(z, output_fullpath)
    # using the coordinates data frame we gathered from ImageJ
    print(c(z[coords[m,1], coords[m,2], 1], paste("Reflectance values - white grey pixel")))
  }
  return(Prop_list_wg)
}  


# final binarization function using the determine blue threshold
    # modified the function to return a data frame that can be exported as a file
    # rather than just returning a list of the values
    # also added threshold as an argument in case you need to change the value
binarize.image.blue <- function(x, output_path, threshold) {
  # Prop_list_blue <- NA 
  # list_black <- NA
  # list_white <- NA
  propblue_df <- data.frame(images = character(), prop_blue_pixels = numeric())
  for(m in 1:length(x)){
    pr <- x[[m]][, , 1]
    pg <- x[[m]][, , 2]
    pb <- x[[m]][, , 3]
    z <- Image(array(dim = c(nrow(pb), ncol(pb), 1), data = pb))   
    ## Need to modify this according to the reflectance value obtained. Select only regions > threshold for blue and create a new binarized picture
    # All blue pixels will be black (0) and all other pixels will be white (1)
    z[which(z[] <= threshold)] <- 1 
    z[which(z[] > threshold & z[] != 1)] <- 0 
    # Number of pixels that are not considered blue
    White_pixels <- length(z[which(z[] == 1)]) 
    Black_pixels <- length(z[which(z[] == 0)]) # Number of blue pixels
    Prop_white <- White_pixels / (White_pixels + Black_pixels) # Proportion of blue pixels
    # print(c(Prop_blue, paste("Proportion of blue pixels"))) 
    # The new picture will be saved in .TIFF with the "b_0.3" at the end of the name
    name <- gsub(".DNG", '', names(x[m]))
    name <- gsub(".jpg", '', name)
    name <- gsub(".TIFF", '', name)
    output_filename <- paste(name, "_b", threshold, ".TIFF", sep = "")  
    output_fullpath <- file.path(output_path, output_filename)
    writeImage(z, output_fullpath)
    # Add the proportion of blue pixels (and the count of black and white pixels) to the empty table created at the beginning of the function
    # Prop_list_blue[[m]] <- Prop_blue 
    # list_black[[m]] <- Black_blue
    # list_white[[m]] <- White_blue
    propblue_df <- rbind(propblue_df, c(names(x[m]), Prop_white))
  }
  # return_list <- list(Prop_list_blue, list_black, list_white)
  return(propblue_df)
}


# binarize images using the white-grey threshold
binarize.image.wg <- function(x, output_path, threshold) {
  prop_wg_df <- data.frame(images = character(), prop_wg_pixels = numeric())
  for(m in 1:length(x)){
    pgr <- x[[m]][, , 1]
    z <- Image(array(dim = c(nrow(pgr), ncol(pgr), 1), data = pgr))   
    # Select only regions > threshold for white-grey and create a new binarized picture
    # All white-grey pixels will be black (0) and all other pixels will be white (1)
    z[which(z[] > threshold)] <- 0
    z[which(z[] <= threshold & z[] != 0)] <- 1  # changed the order so that pixels with value of 1 will be selected
    # Number of pixels that are not considered white-grey
    White_pixels <- length(z[which(z[] == 1)]) 
    Black_pixels <- length(z[which(z[] == 0)]) # Number of white-grey pixels
    Prop_white <- White_pixels / (White_pixels + Black_pixels) # Proportion of white pixels (veg)
    # The new picture will be saved in .TIFF with the "wg_0.4" at the end of the name
    name <- gsub(".DNG", '', names(x[m]))
    name <- gsub(".jpg", '', name)
    name <- gsub(".TIFF", '', name)
    output_filename <- paste(name, "_wg", threshold, ".TIFF", sep = "")  
    output_fullpath <- file.path(output_path, output_filename)
    writeImage(z, output_fullpath)
    prop_wg_df <- rbind(prop_wg_df, c(output_filename, Prop_white))
  }
  
  return(prop_wg_df)
}


# combine the binarized blue and binarized white arrays
binarize.image.comb <- function(list_blue, list_wg, output_path, threshold_blue, threshold_wg) {
  prop_b_wg_df <- data.frame(images = character(), prop_wg_pixels = numeric())
  for(m in 1:length(list_blue)){
    # first binarize the array for blue
    pr <- list_blue[[m]][, , 1]
    pg <- list_blue[[m]][, , 2]
    pb <- list_blue[[m]][, , 3]
    z1 <- Image(array(dim = c(nrow(pb), ncol(pb), 1), data = pb))   
    z1[which(z1[] <= threshold_blue)] <- 1 
    z1[which(z1[] > threshold_blue & z1[] != 1)] <- 0

    # then binarize the array for white-grey
    pgr <- list_wg[[m]][, , 1]
    z2 <- Image(array(dim = c(nrow(pgr), ncol(pgr), 1), data = pgr))   
    z2[which(z2[] > threshold_wg)] <- 0
    z2[which(z2[] <= threshold_wg & z2[] != 0)] <- 1 
        
    # combine the arrays
    z3 <- z1 * z2  # any values that are not blue or white will remain a 1 (1*1 = 1)
                   # any values that are either blue, white, or both, will be 0 (1*0 AND 0*0 = 0)
    
    # look at number of black vs white pixels
    White_pixels <- length(z3[which(z3[] == 1)]) # number non-interest colour pixels
    Black_pixels <- length(z3[which(z3[] == 0)]) # number of blue and white-grey pixels
    Prop_white <- White_pixels / (White_pixels + Black_pixels) # Proportion of white pixels (veg)
    
    # The new picture will be saved in .TIFF with the "bwg" at the end of the name
    output_filename <- paste(names(list_blue[m]), "_bwg",threshold_blue, threshold_wg, ".TIFF", sep = "")  
    output_fullpath <- file.path(output_path, output_filename)
    writeImage(z3, output_fullpath)
    prop_b_wg_df <- rbind(prop_b_wg_df, c(output_filename, Prop_white))
  }
  
  return(prop_b_wg_df)
}



# function to get the ggcorrplot visual correlation matrix from a given model
model.corr.matrix <- function(model) {
  model_mat <- as.matrix(cov2cor(vcov(model)))
  model_mat <- model_mat[, colnames(model_mat) != '(Intercept)']
  model_mat <- model_mat[rownames(model_mat) != '(Intercept)', ]
  
  model_cormat <- ggcorrplot(model_mat, type = "lower", lab = TRUE)
  print(model_cormat)
}


# function to return "smaller" if the inputted value is negative or larger is the inputted value is positive
smaller.larger <- function (value) {
  case_when(value > 0 ~ "larger",
            value < 0 ~ "smaller")
  
}


# function to return "increase" (option 1) or "more" (option2) if the inputted value is positive, or "decrease" (option 1) or "fewer" (option 2)  or "decline" (option 3) if the inputted value is negative

inc.dec <- function (value, opt) {
  case_when(value > 0 & opt == 1 ~ "increase",
            value > 0 & opt == 2 ~ "more",
            value > 0 & opt == 3 ~ "increase",
            value < 0 & opt == 1 ~ "decrease",
            value < 0 & opt == 2 ~ "fewer",
            value < 0 & opt == 3 ~ "decline")
}

# # get a list of the back-transformed parameter estimates (count scale) per unit increase
# bt.param.est.list <- function(model, mod_data) {
#   param_est <- data.frame(parameter = as.character(), estimate = as.numeric())
#   var_names <- row.names(coef(summary(model)))[2:nrow(coef(summary(model)))] # get all variables except the intercept
#   for (i in 1:length(var_names)) {
#     unsc_variable <- gsub("sc", "", var_names[i])
#     est <- exp(coef(summary(model))[as.character(var_names[i]), "Estimate"]/sd(mod_data[ ,unsc_variable]))
#     param_est <- rbind(param_est, list(var_names[i], est))
#   }
#   
#   colnames(param_est) <- c("parameter", "estimate")
#   rownames(param_est) <- c(param_est$parameter)
#   return(param_est)
# }


# modified version to use _gsc _rbsc global variable names

# get a list of the back-transformed parameter estimates (count scale) per unit increase
bt.param.est.list <- function(model, mod_data, variables_list) {
  param_est <- data.frame(parameter = as.character(), estimate = as.numeric())
  var_names <- row.names(coef(summary(model)))[2:nrow(coef(summary(model)))] # get all variables except the intercept
  for (i in 1:length(var_names)) {
    est <- exp(coef(summary(model))[var_names[i], "Estimate"]/sd(mod_data[ ,variables_list[[var_names[i]]]]))
    param_est <- rbind(param_est, list(var_names[i], est))
  }
  
  colnames(param_est) <- c("parameter", "estimate")
  rownames(param_est) <- c(param_est$parameter)
  return(param_est)
}



# get a list of the back-transformed parameter estimates (count scale) per 10% or unit increase
bt10.param.est.list <- function(model, mod_data, variables_list) {
  param_est <- data.frame(parameter = as.character(), estimate = as.numeric())
  var_names <- row.names(coef(summary(model)))[2:nrow(coef(summary(model)))] # get all variables except the intercept
  for (i in 1:length(var_names)) {
    est <- exp(coef(summary(model))[as.character(var_names[i]), "Estimate"]/sd(mod_data[ ,variables_list[[var_names[i]]]])*10)
    param_est <- rbind(param_est, list(var_names[i], est))
  }
  
  colnames(param_est) <- c("parameter", "estimate")
  rownames(param_est) <- c(param_est$parameter)
  return(param_est)
}



# # get a data frame with the CI and the std dev of each variable
# CI.SD.df <- function(model, mod_data) {
#   # 95% CI (SD scaled and log scale)
#   CI_orig <- as.data.frame(confint(model, method = "Wald"))
#   CI_vars <- CI_orig[-c(1:2), ] # get rid of the .sig and intercept rows
#   
#   # list the variables in the model (removing "sc" to get the un-scaled variable names)
#   vars_names <- gsub("sc", "", row.names(CI_vars))
#   variables <- mod_data %>% select(all_of(vars_names))
#   variables_SD <- sapply(variables, sd)
#   variables_SD <- as.data.frame(variables_SD)
#   variables_SD <- rownames_to_column(variables_SD, "variable")
#   # append to the CI dataframe
#   SD <- variables_SD[order(variables_SD[, "variable"]), "variables_SD"]
#   CI_sd <- cbind(CI_vars[order(row.names(CI_vars)), ], SD)
#   CI_sd <- CI_sd %>%
#     rename("lower" = "2.5 %",
#            "upper" = "97.5 %")
#   
#   return(CI_sd)
# }



# modified for using global _gsc _rbsc variables

# get a data frame with the CI and the std dev of each variable
CI.SD.df <- function(model, mod_data, variables_list) {
  # 95% CI (SD scaled and log scale)
  CI_orig <- as.data.frame(confint(model, method = "Wald"))
  CI_vars <- CI_orig[-c(1:2), ] # get rid of the .sig and intercept rows
  
  vars_names <- row.names(CI_vars)
  # get the un-scaled variables from the full variables list that appear in the model
  unsc_vars_names <- variables_list[which(names(variables_list) %in% vars_names)]
  unsc_vars_names <- unname(unsc_vars_names)
  unsc_variables <- mod_data %>% select(all_of(unsc_vars_names))
  # take the std dev of the un-scaled variables to append each variables std dev to the CI df
  variables_SD <- sapply(unsc_variables, sd)
  variables_SD <- as.data.frame(variables_SD)
  variables_SD <- rownames_to_column(variables_SD, "variable")
  # append to the CI dataframe
  SD <- variables_SD[order(variables_SD[, "variable"]), "variables_SD"]
  CI_sd <- cbind(CI_vars[order(row.names(CI_vars)), ], SD)
  CI_sd <- CI_sd %>%
    rename("lower" = "2.5 %",
           "upper" = "97.5 %")
  
  return(CI_sd)
}



# ### WORK IN PROGRESS
# # get a plot of abundance model predictions for specified variable of interest
# abund.pred.plot <- function(model, variable, variable_name, mod_colour) {
#   # get model predictions
#   preds <- predict_response(model, 
#                               terms = as.character(variable_name),
#                               condition = c(num_coverboards = 1),
#                               margin = "empirical",
#                               vcov = get_varcov(model))
#   
#   # construct an agriculture column (for setting the geom_ribbon on the x-axis)
#   preds <- preds %>% 
#     mutate(variable_name = seq(min(variable), 
#                           max(variable), 
#                           length.out = nrow(preds)))
#   
#   
#   # marginal = "empirical" 
#   # answers: What is the predicted (or: expected) value of the response at meaningful 
#   # values or levels of my focal terms for the ‘average’ observation in the population?”.
#   # Not only refers to actual data in your sample, but also “what would be if” 
#   # we had more data, or if we had data from a different population
#   
#   
#   # plot using ggeffects base plot and sjPlot mechanics
#   pred_plot <- plot(preds,
#                       show_data = TRUE,
#                       show_title = FALSE,
#                       show_x_title = FALSE,
#                       show_y_title = FALSE,
#                       jitter = 0.025,
#                       colors = as.character(mod_colour)) +
#     # labs(tag = "A") +
#     set_theme(base = theme_classic(),     # need sjPlot for set_theme
#               axis.linecolor = "black",
#               axis.textcolor = "black",
#               axis.textsize = 0.9,
#               axis.title.size = 1
#     )
#   return(pred_plot)
# }



# ### WORK IN PROGRESS
# # get a plot of svl model predictions for specified variable of interest
# svl.pred.plot <- function(model, variable, variable_name, colour_list) {
#   # get predictions from the model
#   preds <- predict_response(model,
#                             terms = c(paste(.data[[variable_name]], "[all]", sep = " "), "sex"),
#                             margin = "empirical",
#                             vcov = get_varcov(model))
#   
#   # construct an agriculture column (for setting the geom_ribbon on the x-axis)
#   preds <- preds %>% 
#     mutate(variable_name = seq(min(variable), 
#                           max(variable), 
#                           length.out = nrow(preds)))
#   
#   
#   # plotting using ggeffects base plot and sjPlot mechanics
#   pred_plot <- plot(preds,
#                           show_data = TRUE,
#                           show_title = FALSE,
#                           show_x_title = FALSE,
#                           show_y_title = FALSE,
#                           jitter = 0.025,
#                           # colors = c("#913143", "#314391", "#439131")
#   ) +
#     scale_y_continuous(breaks = seq(10, 70, by = 10)) +
#     scale_colour_manual(values = colour_list,
#                         labels = c("Female", "Male", "Unsexed")) +
#     scale_fill_manual(values = colour_list) +
#     labs(tag = "A",
#          colour = "Sex") +
#     set_theme(base = theme_classic(),
#               axis.linecolor = "black",
#               axis.textcolor = "black",
#               axis.textsize = 0.9,
#               axis.title.size = 1
#     )
#   
# }


# function to extract the legend from a plot
get.legend <- function(plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  # Mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend) 
}



# function to plot exploratory abundance plots
abund.explor.plots <- function(x, y, colours, axis_labels) {
  ggplot(data = abund_data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(shape = 1, colour =  colours[2], ) +
    geom_jitter(shape = 1, colour =  colours[1]) +
    # geom_smooth(colour =  "#062d0d") +
    # labs(x = axis_labels[[1]][[2]],
    #      y = "Garter snake count",
    #      tag = axis_labels[[1]][[1]]) +
    theme_classic() 
    # theme(
    #   plot.background = element_blank(),
    #       panel.grid.minor = element_blank(),
    #       panel.grid.major = element_blank(),
    #       axis.title = element_text(size = 9),
    #       axis.text = element_text(size = 6))
}



# function to plot exploratory svl plots
svl.explor.plots <- function(plot_data, x, y, colours, axis_labels) {
  ggplot(data = plot_data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(shape = 1, colour =  colours[2], ) +
    geom_jitter(shape = 1, colour =  colours[1]) +
    theme_classic()

}



poly.refit <- function(model_list) {
  poly_mods_list <- list()
  new_mods_names <- list()
  for (i in 1:length(model_list)) {
    # update each model in the list
    poly_model <- update(model_list[[i]], .~. - day_of_yearsc - day_of_yearsc2 
                             + poly(day_of_yearsc, 2, raw = TRUE))
    
    # append each new updated model to the list
    poly_mods_list <- append(poly_mods_list, poly_model)
    
    # create a new name for the updated model based on the original model name
    new_name <- paste("plotting", "mod", gsub("final_mod_", "", names(model_list[i])), sep = "_")
    # append each new model name to the list of new model names
    new_mods_names <- append(new_mods_names, new_name)
    
  }
  
  # rename each model in the list of updated plotting models by the new names list
  names(poly_mods_list) <- new_mods_names
  
  return(poly_mods_list)
}


nonwt.refit <- function(model_list) {
  nonwt_mods_list <- list()
  new_mods_names <- list()
  for (i in 1:length(model_list)) {
    # update each model in the list
    nonwt_model <- update(model_list[[i]], .~., weights = NULL)
    
    # append each new updated model to the list
    nonwt_mods_list <- append(nonwt_mods_list, nonwt_model)
    
    # create a new name for the updated model based on the original model name
    new_name <- paste("plotting", "mod", gsub("final_mod_", "", names(model_list[i])), sep = "_")
    # append each new model name to the list of new model names
    new_mods_names <- append(new_mods_names, new_name)
    
  }
  
  # rename each model in the list of updated plotting models by the new names list
  names(nonwt_mods_list) <- new_mods_names
  
  return(nonwt_mods_list)
}



abund.pred.plots <- function(plotting_mods_list) {
  
  abund_plots_list <- list()
  abund_plot_names <- list()
  for (i in 1:length(plotting_mods_list)) {
    
    if (grepl("rb", names(plotting_mods_list[i]), fixed = TRUE)) {
      if (grepl("crop", names(plotting_mods_list[i]), fixed = TRUE)) {
        preds_rb <- predict_response(plotting_mods_list[[i]], 
                                     terms = "crop_rbsc [all]",
                                     condition = c(num_coverboards = 1),
                                     margin = "empirical",
                                     vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_rb$crop_rbsc <- seq(min(abund_data_2024$crop_rbsc), 
                                   max(abund_data_2024$crop_rbsc), 
                                   length.out = nrow(preds_rb))
      }
      
      else {
        # get model predictions
        preds_rb <- predict_response(plotting_mods_list[[i]], 
                                     terms = "agriculture_rbsc [all]",
                                     condition = c(num_coverboards = 1),
                                     margin = "empirical",
                                     vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_rb$agriculture_rbsc <- seq(min(abund_data_2024$agriculture_rbsc), 
                                           max(abund_data_2024$agriculture_rbsc), 
                                           length.out = nrow(preds_rb))
      }
      # plot using ggplot
      abund_plots_list[[i]] <- local({
        i <- i
        pred_plot <- ggplot(data = abund_data, aes(x = agriculture_rbsc, y = redbelly_count/num_coverboards)) +
          geom_point(shape = 1, colour = "#a3341f") +
          geom_jitter(height = 0.025, width = 0.15, colour = "#a3341f", shape = 1) +
          # xlim(-1, max(abund_data$agriculture_1000sc)) +
          geom_line(data = preds_rb, aes(x, predicted), lwd = 1, colour = "#DE6953") +
          geom_ribbon(data = preds_rb, aes(ymin = predicted - std.error,
                                           ymax = predicted + std.error,
                                           x = x),
                      inherit.aes = FALSE, fill = "#DE6953", alpha = 0.25,
                      show.legend = FALSE, outline.type = "both") +
          labs(tag = "B") +
          theme_classic() +
          theme(axis.title = element_blank(),
                axis.text = element_text(size = 8),
                text = element_text(size = 10),
                axis.line = element_line(linewidth = 0.3))
      })
      
      plot_name <- paste(names(plotting_mods_list[i]), "plot", sep = "_")
      abund_plot_names <- append(abund_plot_names, plot_name)
      # abund_plots_list <- append(abund_plots_list, pred_plot)
    }
    
    else {
      if (grepl("crop", names(plotting_mods_list[i]), fixed = TRUE)) {
        preds_g <- predict_response(plotting_mods_list[[i]], 
                                    terms = "crop_gsc [all]",
                                    condition = c(num_coverboards = 1),
                                    margin = "empirical",
                                    vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_g$crop_gsc <- seq(min(abund_data_2024$crop_gsc), 
                                   max(abund_data_2024$crop_gsc), 
                                   length.out = nrow(preds_g))
      }
      else {
        # get model predictions
        preds_g <- predict_response(plotting_mods_list[[i]], 
                                    terms = "agriculture_gsc [all]",
                                    condition = c(num_coverboards = 1),
                                    margin = "empirical",
                                    vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_g$agriculture_gsc <- seq(min(abund_data_2024$agriculture_gsc), 
                                         max(abund_data_2024$agriculture_gsc), 
                                         length.out = nrow(preds_g))
      }
      
      # plot using ggplot
      abund_plots_list[[i]] <- local({
        i <- i
        pred_plot <- ggplot(data = abund_data, aes(x = agriculture_gsc, y = garter_count/num_coverboards), colour = "#107425") +
          geom_point(shape = 1) +
          geom_jitter(height = 0.025, width = 0.15, colour = "#107425", shape = 1) +
          # xlim(-1, max(abund_data$agriculture_gsc)) +
          geom_line(data = preds_g, aes(x, predicted), lwd = 1, colour = "#107425") +
          geom_ribbon(data = preds_g, aes(ymin = predicted - std.error,
                                          ymax = predicted + std.error,
                                          x = x,
                                          linetype = "blank"),
                      inherit.aes = FALSE, fill = "#107425", alpha = 0.25, show.legend = FALSE) +
          labs(tag = "A") +
          theme_classic() +
          theme(axis.title = element_blank(),
                axis.text = element_text(size = 8),
                text = element_text(size = 10),
                axis.line = element_line(linewidth = 0.3))
      })
      
      plot_name <- paste(names(plotting_mods_list[i]), "plot", sep = "_")
      abund_plot_names <- append(abund_plot_names, plot_name)
      # abund_plots_list[[i]] <- append(abund_plots_list, pred_plot)
   
    }
  }
  names(abund_plots_list) <- abund_plot_names
  
  return(abund_plots_list)
}


svl.pred.plots <- function(plotting_mods_list) {
  
  svl_plots_list <- list()
  svl_plot_names <- list()
  for (i in 1:length(plotting_mods_list)) {
    
    if (grepl("rb", names(plotting_mods_list[i]), fixed = TRUE)) {
      if (grepl("crop", names(plotting_mods_list[i]), fixed = TRUE)) {
        preds_rb <- predict_response(plotting_mods_list[[i]], 
                                     terms = c("crop_rb [all]", "sex"),
                                     margin = "empirical",
                                     vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_rb$crop_rb <- seq(min(svl_data_rb2024$crop_rb), 
                                   max(svl_data_rb2024$crop_rb), 
                                   length.out = nrow(preds_rb))
      }
      
      else {
        # get model predicitons
        preds_rb <- predict_response(plotting_mods_list[[i]],
                                             terms = c("agriculture_rb [all]", "sex"),
                                             margin = "empirical",
                                             vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_rb$agriculture_rb <- seq(min(svl_data_rb$agriculture_rb), 
                                                max(svl_data_rb$agriculture_rb), 
                                                length.out = nrow(preds_rb))
      }
      # plot using ggplot
      svl_plots_list[[i]] <- local({
        i <- i
        pred_plot <- ggplot(data = svl_data_rb, aes(x = agriculture_rb, y = svl_cm, color = sex)) +
          geom_point(shape = 1) +
          geom_jitter(shape = 1) +
          geom_line(data = preds_rb, aes(x, predicted, color = group), lwd = 1) +
          geom_ribbon(data = preds_rb, aes(ymin = predicted - std.error,
                                                   ymax = predicted + std.error,
                                                   x = x,
                                                   fill = group),
                      inherit.aes = FALSE, alpha = 0.25, show.legend = FALSE) +
          scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30)) +
          labs(tag = "B",
               colour = str_wrap("Redbelly snake sex:", width = 10)) +
          scale_colour_manual(values = c("#5a1d11", "#DE6953", "#edada1"),
                              labels = c("Female", "Male", "Unsexed")) +
          scale_fill_manual(values = c("#5a1d11", "#DE6953", "#edada1")) +
          theme_classic() +
          theme(axis.title = element_blank(),
                axis.text = element_text(size = 8),
                text = element_text(size = 10),
                axis.line = element_line(linewidth = 0.3),
                legend.position = "top",
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 7))
      })
      
      plot_name <- paste(names(plotting_mods_list[i]), "plot", sep = "_")
      svl_plot_names <- append(svl_plot_names, plot_name)
    }
    
    else {
      if (grepl("crop", names(plotting_mods_list[i]), fixed = TRUE)) {
        preds_g <- predict_response(plotting_mods_list[[i]], 
                                    terms = c("crop_g [all]", "sex"),
                                    margin = "empirical",
                                    vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_g$crop_g <- seq(min(svl_data_g2024$crop_g), 
                                   max(svl_data_g2024$crop_g), 
                                   length.out = nrow(preds_g))
      }
      else {
        # get predictions from the model
        preds_g <- predict_response(plotting_mods_list[[i]],
                                            terms = c("agriculture_g [all]", "sex"),
                                            margin = "empirical",
                                            vcov = get_varcov(plotting_mods_list[[i]]))
        
        # construct an agriculture column (for setting the geom_ribbon on the x-axis)
        preds_g$agriculture_g <- seq(min(svl_data_g$agriculture_g),
                                               max(svl_data_g$agriculture_g),
                                               length.out = nrow(preds_g))
      }
      
      # plot using ggplot
      svl_plots_list[[i]] <- local({
        i <- i
        pred_plot <- ggplot(data = svl_data_g, aes(x = agriculture_g, y = svl_cm, color = sex)) +
          geom_point(shape = 1) +
          geom_jitter(shape = 1) +
          geom_line(data = preds_g, aes(x, predicted, color = group), lwd = 1) +
          geom_ribbon(data = preds_g, aes(ymin = predicted - std.error,
                                                  ymax = predicted + std.error,
                                                  x = x,
                                                  fill = group),
                      inherit.aes = FALSE, alpha = 0.25, show.legend = FALSE) +
          scale_y_continuous(breaks = c(15, 30, 45, 60, 75)) +
          labs(tag = "A",
               colour = str_wrap("Garter snake sex:", width = 10)) +
          scale_colour_manual(values = c("#070e06", "#107425", "#8ed290"),
                              labels = c("Female", "Male", "Unsexed")) +
          scale_fill_manual(values = c("#070e06", "#107425", "#8ed290"),
                            labels = c("Female", "Male", "Unsexed")) +
          theme_classic() +
          theme(axis.title = element_blank(),
                axis.text = element_text(size = 8),
                text = element_text(size = 10),
                axis.line = element_line(linewidth = 0.3),
                legend.position = "top",
                legend.title = element_text(size = 8),
                legend.text = element_text(size = 7))
      })
      
      plot_name <- paste(names(plotting_mods_list[i]), "plot", sep = "_")
      svl_plot_names <- append(svl_plot_names, plot_name)
      
    }
  }
  names(svl_plots_list) <- svl_plot_names
  
  return(svl_plots_list)
}




overdisp_fun <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1) / 2
  }
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  (rdf <- nrow(model@frame) - model.df)
  rp <- residuals(model)
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE, log.p = TRUE)
  c(chisq = Pearson.chisq, ratio = prat, p = exp(pval))
}

