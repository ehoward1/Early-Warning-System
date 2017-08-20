# This file contains code for Section 4.2 Clustering Analysis and 
# the prediction code for 'Cluster Variables' Dataset (Section 4.3 Prediction Modelling)
require(mclust)

# Notes
# use_of_resources_1w = semester 1 week 1 dataset for weekdays
# use_of_resources_1s = semester 1 week 1 dataset for Sunday 

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

# Remove 'zero' variables
use_of_resources_1as = removecolfunction(use_of_resources_1s)
use_of_resources_2as = removecolfunction(use_of_resources_2s)
use_of_resources_3as = removecolfunction(use_of_resources_3s)
use_of_resources_4as = removecolfunction(use_of_resources_4s)
use_of_resources_5as = removecolfunction(use_of_resources_5s)
use_of_resources_6as = removecolfunction(use_of_resources_6s)
use_of_resources_1aw = removecolfunction(use_of_resources_1w)
use_of_resources_2aw = removecolfunction(use_of_resources_2w)
use_of_resources_3aw = removecolfunction(use_of_resources_3w)
use_of_resources_4aw = removecolfunction(use_of_resources_4w)
use_of_resources_5aw = removecolfunction(use_of_resources_5w)
use_of_resources_6aw = removecolfunction(use_of_resources_6w)

########################################################################
# Clustering on week 1's dataset 
clust1 = cbind.data.frame(use_of_resources_1as, use_of_resources_1aw) # See paper for variables used
clust1 = scale(clust1) # Advisiable to scale variables before clustering
fit1 = Mclust(clust1, G = c(2:10)) # Performs mclust - use summary(fit) to see model selected 

# Include cluster membership as a variable in our prediction datasets
dataset1u_clust<-cbind.data.frame(FINAL, student_background, use_of_resources_1as,
                                  use_of_resources_1aw, fit1$classification)
# Note in the paper, fit$z (cluster membership variables) is used which can cause errors
# For ease of coding fit$classification is used here which states which cluster a student belongs to
dataset1u_clust_b = cbind.data.frame(FINAL, student_background_oh, use_of_resources_1as,
                                     use_of_resources_1aw, fit1$classification)

# Predict with 'new' cluster datasets
pred_clust1 = prediction_function(dataset1u_clust, dataset1u_clust_b)        

###############################################
# Clustering on week 2's dataset 
clust2 = cbind.data.frame(use_of_resources_1as, use_of_resources_1aw,
                          use_of_resources_2as, use_of_resources_2aw) 
clust2 = scale(clust2) 
fit2 = Mclust(clust2, G = c(2:10)) 
dataset2u_clust = cbind.data.frame(FINAL, student_background, use_of_resources_2as,
                                  use_of_resources_2aw, fit2$classification)
dataset2u_clust_b = cbind.data.frame(FINAL, student_background_oh, use_of_resources_2as,
                                     use_of_resources_2aw, fit2$classification)
pred_clust2 = prediction_function(dataset2u_clust, dataset2u_clust_b)        

###############################################
# Clustering on week 3's dataset 
clust3 = cbind.data.frame(use_of_resources_1as, use_of_resources_1aw,
                          use_of_resources_2as, use_of_resources_2aw,
                          use_of_resources_3as, use_of_resources_3aw)  
clust3 = scale(clust3)  
fit3 = Mclust(clust3, G = c(2:10))  
dataset3u_clust<-cbind.data.frame(student_results_3_4, student_background, use_of_resources_3as,
                                  use_of_resources_3as, fit3$z) 
dataset3u_clust_b = cbind.data.frame(student_results_3_4, student_background_oh, use_of_resources_3as,
                                     use_of_resources_3aw, fit3$z)
pred_clust3 = prediction_function(dataset3u_clust, dataset3u_clust_b)        

###############################################
# Clustering on week 4's dataset 
clust4 = cbind.data.frame(use_of_resources_1as, use_of_resources_1aw,
                          use_of_resources_2as, use_of_resources_2aw,
                          use_of_resources_3as, use_of_resources_3aw,
                          use_of_resources_4as, use_of_resources_4aw) 
clust4 = scale(clust4) 
fit4 = Mclust(clust4, G = c(2:10)) 
dataset4u_clust<-cbind.data.frame(student_results_3_4, student_background, use_of_resources_4as,
                                  use_of_resources_4aw, fit4$classification)
dataset4u_clust_b = cbind.data.frame(student_results_3_4, student_background_oh, use_of_resources_4as,
                                     use_of_resources_4aw, fit4$classification)
pred_clust4 = prediction_function(dataset4u_clust, dataset4u_clust_b)        

###############################################
# CLustering on week 5's dataset 
clust5 = cbind.data.frame(use_of_resources_1aw, use_of_resources_2aw,
                          use_of_resources_3aw, use_of_resources_4aw,
                          use_of_resources_5aw, use_of_resources_1as, 
                          use_of_resources_2as ,use_of_resources_3as,
                          use_of_resources_4as ,use_of_resources_5as)
clust5 = scale(clust5)
fit5 = Mclust(clust5, G = c(2:10))
dataset5u_clust = cbind.data.frame(student_results_5, student_background, 
                                   use_of_resources_update_5s, use_of_resources_update_5w,
                                   fit5$classification)
dataset5u_clust_b = cbind.data.frame(student_results_5, student_background_oh, 
                                     use_of_resources_update_5s, use_of_resources_update_5w,
                                     fit5$classification)
pred_clust5 = prediction_function(dataset5u_clust, dataset5u_clust_b)         

###############################################
# CLustering on week 6's dataset 
clust6 = cbind.data.frame(use_of_resources_1aw, use_of_resources_2aw,
                          use_of_resources_3aw, use_of_resources_4aw,
                          use_of_resources_5aw, use_of_resources_6aw,
                          use_of_resources_1as, use_of_resources_2as,
                          use_of_resources_3as, use_of_resources_4as,
                          use_of_resources_5as, use_of_resources_6as)
clust6 = scale(clust6)
fit6 = Mclust(clust6, G = c(2:10))
dataset6u_clust = cbind.data.frame(student_results_6, student_background, 
                                   use_of_resources_update_6w, use_of_resources_update_6s,
                                   fit6$classification)
dataset6u_clust_b = cbind.data.frame(student_results_6, student_background_oh, 
                                     use_of_resources_update_6w, use_of_resources_update_6s,
                                     fit6$classification)
pred_clust6 = prediction_function(dataset6u_clust, dataset6u_clust_b)         

####################################
# Extract MAE error weeks 1-6 for 'Cluster Variables' Datasets:

Error_clus_bm = c(pred_int$MAE_bm, pred_clust1$MAE_bm, pred_clust2$MAE_bm, pred_clust3$MAE_bm,
                  pred_clust4$MAE_bm, pred_clust5$MAE_bm, pred_clust6$MAE_bm)
Error_clus_pcr = c(pred_int$MAE_pcr, pred_clust1$MAE_pcr, pred_clust2$MAE_pcr, pred_clust3$MAE_pcr,
                   pred_clust4$MAE_pcr, pred_clust5$MAE_pcr, pred_clust6$MAE_pcr)
Error_clus_rf = c(pred_int$MAE_rf, pred_clust1$MAE_rf, pred_clust2$MAE_rf, pred_clust3$MAE_rf,
                  pred_clust4$MAE_rf, pred_clust5$MAE_rf, pred_clust6$MAE_rf)
Error_clus_xg = c(pred_int$MAE_xg, pred_clust1$MAE_xg, pred_clust2$MAE_xg, pred_clust3$MAE_xg,
                  pred_clust4$MAE_xg, pred_clust5$MAE_xg, pred_clust6$MAE_xg)
Error_clus_earth = c(pred_int$MAE_earth, pred_clust1$MAE_earth, pred_clust2$MAE_earth, pred_clust3$MAE_earth,
                     pred_clust4$MAE_earth, pred_clust5$MAE_earth, pred_clust6$MAE_earth)
Error_clus_nnet = c(pred_int$MAE_nnet, pred_clust1$MAE_nnet, pred_clust2$MAE_nnet, pred_clust3$MAE_nnet,
                    pred_clust4$MAE_nnet, pred_clust5$MAE_nnet, pred_clust6$MAE_nnet)
Error_clus_svm = c(pred_int$MAE_svm, pred_clust1$MAE_svm, pred_clust2$MAE_svm, pred_clust3$MAE_svm,
                   pred_clust4$MAE_svm, pred_clust5$MAE_svm, pred_clust6$MAE_svm)
Error_clus_kknn = c(pred_int$MAE_kknn, pred_clust1$MAE_kknn, pred_clust2$MAE_kknn, pred_clust3$MAE_kknn,
                    pred_clust4$MAE_kknn, pred_clust5$MAE_kknn, pred_clust6$MAE_kknn)

################################################
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

##################################################
# Plot Figure 3 from the paper run for week 5

total_views5s = rowSums(use_of_resources_update_5s) # Create Total Sunday views variable
total_views5w = rowSums(use_of_resources_update_5w) # Create Total Weekday views variable
cluster5 = cbind.data.frame(fit5$classification, FINAL, 
                            total_views5s, total_views5w) # Variables for total view for weekdays and Sundays

colnames(cluster5) = c("cluster5",  "Final Result", 
                       "Total Sunday Views","Total Weekday Views")

cluster5[,2:4] = scale(cluster5[,2:4]) # Scale variables for boxplot

# Editing the cluster classifications to include number of students in the cluster
CL<- rep(NA, nrow(cluster5))
  CL[cluster5[,1]=="1"] <- "1 - n=68"  # edit n as appropriate
  CL[cluster5[,1]=="2"] <- "2 - n=26"
  CL[cluster5[,1]=="3"] <- "3 - n=5"
  CL[cluster5[,1]=="4"] <- "4 - n=1"
cluster5[,1]<-CL

# Reformatting dataset to long format
df.m <- melt(cluster5, id.var = "cluster5")

# Plotting Figure 3 from the paper
ggplot(data = df.m, aes(x = variable, y = value)) + 
  geom_boxplot(aes(fill = factor(cluster5))) + coord_flip() +
  theme_bw() + ylab('Standardised Means of Clusters') +
  labs(fill="Cluster Number") + guides(fill = guide_legend(reverse=TRUE)) +
  theme(
    plot.title = element_text(lineheight=0, face = "bold", size = 10),
    axis.text.x = element_text(color = 'black', size = 10),
    axis.text.y = element_text(colour = 'black', size = 10),
    axis.title.x = element_text(angle = 0, color = 'black', face = 'bold', size = 10)
  )  + 
  theme(axis.title.y = element_blank()) + 
  scale_y_continuous(breaks = seq(-3, 3, 1)) +
  scale_fill_viridis(discrete=TRUE,option="D") +
  theme(legend.title = element_text(size=10)) +
  theme(legend.text = element_text( size = 10)) +
  theme(legend.background = element_rect(size=.6))

