# This file contains code for Section 4.3 Prediction Modelling for Figure 4.

require(caret)
require(magrittr)
require(viridis)

# The inital prediction modelling should be loaded in from the .R file: 
# "Prediction Modelling part a"

#####################################################
# Creating the prediction modelling for the datasets for no LMS data (weeks 1-6)
# Output not shown in the paper

# Create the datasets
dataset3NE = cbind.data.frame(student_results_3_4, student_background)
dataset5NE = cbind.data.frame(student_results_5, student_background)
dataset6NE = cbind.data.frame(student_results_6, student_background)
dataset3NE_b = cbind.data.frame(student_results_3_4, student_background_oh) 
dataset5NE_b = cbind.data.frame(student_results_5, student_background_oh)
dataset6NE_b = cbind.data.frame(student_results_6, student_background_oh)

# Run the model
No_LMS3 = prediction_function(dataset3NE, dataset3NE_b)  
No_LMS5 = prediction_function(dataset5NE, dataset5NE_b) 
No_LMS6 = prediction_function(dataset6NE, dataset6NE_b) 

# Extract the MAE for each prediction method
Error_no_LMS_bm = c(pred_int$MAE_bm, pred_int$MAE_bm, pred_int$MAE_bm, No_LMS3$MAE_bm,
                    No_LMS3$MAE_bm, No_LMS5$MAE_bm, No_LMS6$MAE_bm)
Error_no_LMS_xg = c(pred_int$MAE_xg, pred_int$MAE_xg, pred_int$MAE_xg, No_LMS3$MAE_xg,
                    No_LMS3$MAE_xg, No_LMS5$MAE_xg, No_LMS6$MAE_xg)
Error_no_LMS_rf = c(pred_int$MAE_rf, pred_int$MAE_rf, pred_int$MAE_rf, No_LMS3$MAE_rf,
                    No_LMS3$MAE_rf, No_LMS5$MAE_rf, No_LMS6$MAE_rf)
Error_no_LMS_pcr = c(pred_int$MAE_pcr, pred_int$MAE_pcr, pred_int$MAE_pcr, No_LMS3$MAE_pcr,
                     No_LMS3$MAE_pcr, No_LMS5$MAE_pcr, No_LMS6$MAE_pcr)
Error_no_LMS_svm = c(pred_int$MAE_svm, pred_int$MAE_svm, pred_int$MAE_svm, No_LMS3$MAE_svm,
                     No_LMS3$MAE_svm, No_LMS5$MAE_svm, No_LMS6$MAE_svm)
Error_no_LMS_nnet = c(pred_int$MAE_nnet, pred_int$MAE_nnet, pred_int$MAE_nnet, No_LMS3$MAE_nnet,
                      No_LMS3$MAE_nnet, No_LMS5$MAE_nnet, No_LMS6$MAE_nnet)
Error_no_LMS_kknn = c(pred_int$MAE_kknn, pred_int$MAE_kknn, pred_int$MAE_kknn, No_LMS3$MAE_kknn,
                      No_LMS3$MAE_kknn, No_LMS5$MAE_kknn, No_LMS6$MAE_kknn)
Error_no_LMS_earth = c(pred_int$MAE_earth, pred_int$MAE_earth, pred_int$MAE_earth, No_LMS3$MAE_earth,
                      No_LMS3$MAE_earth, No_LMS5$MAE_earth, No_LMS6$MAE_earth)

######################################################################
# Creating the prediction modelling for the datasets for Cumulative Variables (weeks 1-6)
# For both weekdays and Sundays
# This output is not shown in the paper

# Notes
# use_of_resources_1w refers to the weekday views of resources for semester 1 week 1 
# use_of_resources_1s refers to the Sunday views of resources for semester 1 Sunday from week 1 
# For these files the number of columns should equal the number of resources/folders

# Cumulative updating for weeks 1-6 (Note no '0' columns should be removed 
# until after obtaining the cumulative dataset)

# Read in resource file for each period as required
use_of_resources_1s = read.csv("Sample_sunday1.csv", header = TRUE) 
use_of_resources_2s = read.csv("Sample_sunday2.csv", header = TRUE)
use_of_resources_3s = read.csv("Sample_sunday3.csv", header = TRUE)
use_of_resources_4s = read.csv("Sample_sunday4.csv", header = TRUE)
use_of_resources_5s = read.csv("Sample_sunday5.csv", header = TRUE)
use_of_resources_6s = read.csv("Sample_sunday6.csv", header = TRUE)
use_of_resources_1w = read.csv("Sample_weekday1.csv", header = TRUE) 
use_of_resources_2w = read.csv("Sample_weekday2.csv", header = TRUE)
use_of_resources_3w = read.csv("Sample_weekday3.csv", header = TRUE)
use_of_resources_4w = read.csv("Sample_weekday4.csv", header = TRUE)
use_of_resources_5w = read.csv("Sample_weekday5.csv", header = TRUE)
use_of_resources_6w = read.csv("Sample_weekday6.csv", header = TRUE)

# Remove "student number" variable
use_of_resources_1s = use_of_resources_1s[,-1]
use_of_resources_2s = use_of_resources_2s[,-1]
use_of_resources_3s = use_of_resources_3s[,-1]
use_of_resources_4s = use_of_resources_4s[,-1]
use_of_resources_5s = use_of_resources_5s[,-1]
use_of_resources_6s = use_of_resources_6s[,-1]
use_of_resources_1w = use_of_resources_1w[,-1]
use_of_resources_2w = use_of_resources_2w[,-1]
use_of_resources_3w = use_of_resources_3w[,-1]
use_of_resources_4w = use_of_resources_4w[,-1]
use_of_resources_5w = use_of_resources_5w[,-1]
use_of_resources_6w = use_of_resources_6w[,-1]

# Cumulative update for weekday views
use_of_resources_up2w = use_of_resources_1w + use_of_resources_2w
use_of_resources_up3w = use_of_resources_up2w + use_of_resources_3w
use_of_resources_up4w = use_of_resources_up3w + use_of_resources_4w
use_of_resources_up5w = use_of_resources_up4w + use_of_resources_5w
use_of_resources_up6w = use_of_resources_up5w + use_of_resources_6w

# Cumulative update for Sunday views
use_of_resources_up2s = use_of_resources_1s + use_of_resources_2s
use_of_resources_up3s = use_of_resources_up2s + use_of_resources_3s
use_of_resources_up4s = use_of_resources_up3s + use_of_resources_4s
use_of_resources_up5s = use_of_resources_up4s + use_of_resources_5s
use_of_resources_up6s = use_of_resources_up5s + use_of_resources_6s

# Remove any columns containing zero - function in Prediction Modelling part a
use_of_resources_update_2w = removecolfunction(use_of_resources_up2w)
use_of_resources_update_3w = removecolfunction(use_of_resources_up3w)
use_of_resources_update_4w = removecolfunction(use_of_resources_up4w)
use_of_resources_update_5w = removecolfunction(use_of_resources_up5w)
use_of_resources_update_6w = removecolfunction(use_of_resources_up6w)
use_of_resources_update_2s = removecolfunction(use_of_resources_up2s)
use_of_resources_update_3s = removecolfunction(use_of_resources_up3s)
use_of_resources_update_4s = removecolfunction(use_of_resources_up4s)
use_of_resources_update_5s = removecolfunction(use_of_resources_up5s)
use_of_resources_update_6s = removecolfunction(use_of_resources_up6s)

# Create the 'Cumulative Variables' datasets
dataset1us = cbind.data.frame(FINAL, student_background, use_of_resources_1w, use_of_resources_1s)
dataset2us = cbind.data.frame(FINAL, student_background, use_of_resources_update_2w, use_of_resources_update_2s)
dataset3us = cbind.data.frame(student_results_3_4, student_background, use_of_resources_update_3w, use_of_resources_update_3s)
dataset4us = cbind.data.frame(student_results_3_4, student_background, use_of_resources_update_4w, use_of_resources_update_4s)
dataset5us = cbind.data.frame(student_results_5, student_background, use_of_resources_update_5w, use_of_resources_update_5s)
dataset6us = cbind.data.frame(student_results_6, student_background, use_of_resources_update_6w, use_of_resources_update_6s)

dataset1us_b = cbind.data.frame(FINAL, student_background_oh, use_of_resources_1w, use_of_resources_1s)
dataset2us_b = cbind.data.frame(FINAL, student_background_oh, use_of_resources_update_2w, use_of_resources_update_2s)
dataset3us_b = cbind.data.frame(student_results_3_4, student_background_oh, use_of_resources_update_3w, use_of_resources_update_3s)
dataset4us_b = cbind.data.frame(student_results_3_4, student_background_oh, use_of_resources_update_4w, use_of_resources_update_4s)
dataset5us_b = cbind.data.frame(student_results_5, student_background_oh, use_of_resources_update_5w, use_of_resources_update_5s)
dataset6us_b = cbind.data.frame(student_results_6, student_background_oh, use_of_resources_update_6w, use_of_resources_update_6s)

# Run the prediction for the 'Cumulative Variables' dataset
pred_up1 = prediction_function(dataset1us, dataset1us_b)           
pred_up2 = prediction_function(dataset2us, dataset2us_b)  
pred_up3 = prediction_function(dataset3us, dataset3us_b)  
pred_up4 = prediction_function(dataset4us, dataset4us_b)  
pred_up5 = prediction_function(dataset5us, dataset5us_b)  
pred_up6 = prediction_function(dataset6us, dataset6us_b)  

# Extract the MAE error for each prediction method
Error_update_bm = c(pred_int$MAE_bm, pred_up1$MAE_bm, pred_up2$MAE_bm, pred_up3$MAE_bm, 
                    pred_up4$MAE_bm, pred_up5$MAE_bm, pred_up6$MAE_bm)
Error_update_rf = c(pred_int$MAE_rf, pred_up1$MAE_rf, pred_up2$MAE_rf, pred_up3$MAE_rf, 
                    pred_up4$MAE_rf, pred_up5$MAE_rf, pred_up6$MAE_rf)
Error_update_pcr = c(pred_int$MAE_pcr, pred_up1$MAE_pcr, pred_up2$MAE_pcr, pred_up3$MAE_pcr, 
                     pred_up4$MAE_pcr, pred_up5$MAE_pcr, pred_up6$MAE_pcr)
Error_update_xg = c(pred_int$MAE_xg, pred_up1$MAE_xg, pred_up2$MAE_xg, pred_up3$MAE_xg, 
                    pred_up4$MAE_xg, pred_up5$MAE_xg, pred_up6$MAE_xg)
Error_update_nnet = c(pred_int$MAE_nnet, pred_up1$MAE_nnet, pred_up2$MAE_nnet, pred_up3$MAE_nnet, 
                     pred_up4$MAE_nnet, pred_up5$MAE_nnet, pred_up6$MAE_nnet)
Error_update_kknn = c(pred_int$MAE_kknn, pred_up1$MAE_kknn, pred_up2$MAE_kknn, pred_up3$MAE_kknn, 
                      pred_up4$MAE_kknn, pred_up5$MAE_kknn, pred_up6$MAE_kknn)
Error_update_svm = c(pred_int$MAE_svm, pred_up1$MAE_svm, pred_up2$MAE_svm, pred_up3$MAE_svm, 
                     pred_up4$MAE_svm, pred_up5$MAE_svm, pred_up6$MAE_svm)
Error_update_earth = c(pred_int$MAE_earth, pred_up1$MAE_earth, pred_up2$MAE_earth, pred_up3$MAE_earth, 
                     pred_up4$MAE_earth, pred_up5$MAE_earth, pred_up6$MAE_earth)

###########################################################################

# Run the Clustering Analysis.R file as required

###########################################
# Figure 4: Plotting the MAE for each modelling method under cluster dataset
# Could switch initial inputs to run for "No LMS Data" or "Cumulative Variables"

MAE_clus = data.frame(Week = 0:6,
                       BART = Error_clus_bm,
                       RF = Error_clus_rf,
                       XGBoost = Error_clus_xg,
                       PCR = Error_clus_pcr,
                       Splines = Error_clus_earth,
                       KNN = Error_clus_kknn,
                       NN = Error_clus_nnet,
                       SVM = Error_clus_svm)

Errors = melt(MAE_clus, id.vars = 'Week', variable.name = 'Prediction Errors')
colnames(Errors) = c("Week","Method","value")
  
ggplot(Errors, aes(Week, value, color = factor(Method, 
                  labels = c("Bayesian Additive Regressive Trees", "Random Forests", 
                   "XGBoost", "Principal Components Regression",
                   "Multivariate Adaptive Regression Splines", "K-Nearest Neighbours",
                  "Neural Networks", "Support Vector Machine")))) + 
    geom_line() + labs(color = "Method") +
    scale_y_continuous(breaks = seq(6, 20, 1)) + expand_limits(y = c(6,14)) +
    scale_x_discrete(breaks = 0:6, limits = c(0:6), labels = c("Intially","1","2","3","4","5","6")) +
    theme_bw() + xlab('Week') + ylab('Mean\nAbsolute\nError') + 
    theme(
      axis.title.x=element_text(angle = 0, color='black', face='bold', size = 10),
      axis.title.y=element_text(angle = 0, color='black', face='bold', size = 8),
      axis.text.x = element_text(hjust = .5, color = 'black'),
      axis.text.y = element_text(color = 'black'),
      legend.title=element_text(size = 9), 
      legend.text=element_text(size = 9),
      legend.position = c(0.2, 0.15),
      legend.key.height = unit(.4, "cm")
         ) +
    scale_color_viridis(discrete = TRUE, option = "D") +
    geom_dl(aes(label = Method), method=list("last.bumpup", rot=20, dl.trans(x = x-1, y = y+0.08)))
    
