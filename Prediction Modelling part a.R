# This file contains code for Section 4.3 Prediction Modelling
# See "Formatting file" for reading in datasets and see fictitious datasets
# There are six ficticious datasets, however code can be repeated for addditional weeks

require(bartMachine)
require(car)
require(caret)
require(directlabels)
require(randomForest)
require(earth)
require(kernlab)
require(kknn)
require(magrittr)
require(nnet)
require(pls)
require(reshape2)
require(viridis)
require(xgboost)

#################################


# Initial datasets for each week
# At this stage the resource files should not have a column relating to student id
datasetint = cbind.data.frame(FINAL, student_background)
dataset1 = cbind.data.frame(FINAL, student_background, use_of_resources_1a)
dataset2 = cbind.data.frame(FINAL, student_background, use_of_resources_1a, use_of_resources_2a)
dataset3 = cbind.data.frame(student_results_3_4, student_background, use_of_resources_1a, use_of_resources_2a, use_of_resources_3a)
dataset4 = cbind.data.frame(student_results_3_4, student_background, use_of_resources_1a, use_of_resources_2a, use_of_resources_3a, use_of_resources_4a)
dataset5 = cbind.data.frame(student_results_5, student_background, use_of_resources_1a, use_of_resources_2a, use_of_resources_3a, use_of_resources_4a, use_of_resources_5a)
dataset6 = cbind.data.frame(student_results_6, student_background, use_of_resources_1a, use_of_resources_2a, use_of_resources_3a, use_of_resources_4a, use_of_resources_5a, use_of_resources_6a)

# Datasets with student information one-hot encoded for XGBoost
datasetint_b<-cbind.data.frame(FINAL, student_background_oh)
dataset1_b<-cbind.data.frame(FINAL, student_background_oh, use_of_resources_1a)
dataset2_b<-cbind.data.frame(FINAL, student_background_oh, use_of_resources_1a, use_of_resources_2a)
dataset3_b<-cbind.data.frame(student_results_3_4, student_background_oh, use_of_resources_1a, use_of_resources_2a, use_of_resources_3a)
dataset4_b<-cbind.data.frame(student_results_3_4, student_background_oh, use_of_resources_1a, use_of_resources_2a,use_of_resources_3a, use_of_resources_4a)
dataset5_b<-cbind.data.frame(student_results_5, student_background_oh, use_of_resources_1a, use_of_resources_2a, use_of_resources_3a, use_of_resources_4a, use_of_resources_5a)
dataset6_b<-cbind.data.frame(student_results_6, student_background_oh, use_of_resources_1a, use_of_resources_2a, use_of_resources_3a, use_of_resources_4a, use_of_resources_5a ,use_of_resources_6a)

# prediction function
prediction_function <- function(dataset, dataset_boost){ # dataset boost is for xgboost 
  
  set.seed(123)
  folds = createFolds(1:nrow(dataset), k = 10, list = FALSE)
  dataset_boost = apply(dataset_boost, 2, as.numeric) # XGBoost runs for numeric data, not integers
  
  # Vectors to store error for other prediction methods
  pred_bm = vector("numeric")
  pred_rf = vector("numeric")
  pred_pcr = vector("numeric")
  pred_xg = vector("numeric")
  pred_kknn = vector("numeric")
  pred_svm = vector("numeric")
  pred_nnet = vector("numeric")
  pred_earth = vector("numeric")
  grades = vector("numeric")
  
  # Loop through the folds for Random Forest, PC regression and XGBoost
  for(i in 1:10){
    
    # Setting up data
    train = dataset[folds!=i,] %>% data.frame %>% na.omit
    train_b = dataset_boost[folds!=i,] %>% data.frame %>% na.omit
    test = dataset[folds==i,] %>% data.frame
    test_b = dataset_boost[folds==i,] %>% data.frame 
    
    # BART
    bm = bartMachine(train[,-1], train[,1], seed = 123, alpha = 0.95, num_burn_in = 400,
                     num_tree = 100, num_rand_samps_in_library = 20000, k = 2, q = 0.9, nu = 3)
    pred_bm = c(pred_bm, predict(bm, test[,-1]))
    
    # Random Forest (RF)
    rf = randomForest(train[,-1], train[,1], ntree = 100)
    pred_rf = c(pred_rf, predict(rf, test[,-1]))
    
    # Principle Components Regression (PCR)
    pcr = pcr(FINAL~., data = train)
    var_exp = compnames(pcr, explvar = TRUE)
    var_e = unlist(strsplit(var_exp, "[ (]")) %>% as.numeric() %>%  setdiff(c(1:150, NA))
    var_total = 0
    
    # Calculating number of variables to include based on variation explained
    for(j in 1:length(var_e))
    {
      var_total = var_total + var_e[j]
      if(var_e[j] < 1 || var_total > 90)
      {
        n_comp = j
        break
      }
    }
    pred_pcr = c(pred_pcr, predict(pcr, test[,-1], ncomp = n_comp))
    
    # Xgboost
    iter = train_b %>% ncol %>% sqrt %>% ceiling
    xg = xgboost(data = as.matrix(train_b[,-1]), label = train_b[,1], eta = 0.5, 
                 nround = iter, max.depth = 4, objective = "reg:linear")
    pred_xg = c(pred_xg, predict(xg, as.matrix(test_b[,-1])))
    
    # K-Nearest Neighbours (KNN)
    kknn = train.kknn(FINAL ~., kmax = 15, distance = 1, data = train)
    pred_kknn = c(pred_kknn, predict(kknn, test[,-1]))
    
    # Neural Network (NN)
    my.grid = expand.grid(.decay = c(0.05, 0.5, 0.75), .size = c(4, 9))
    nnet = train(FINAL~., data = train, linout = 1, 
                 method = "nnet", maxit = 500, tuneGrid = my.grid, trace = FALSE) 
    pred_nnet = c(pred_nnet, predict(nnet, test[,-1]))
    
    # Support Vector Machine (SVM)
    svm = ksvm(FINAL ~., data = train, C = 5)
    pred_svm = c(pred_svm, predict(svm, test[,-1]))
    
    # Multivariate Adaptive Regression Splines
    earth = train(FINAL~., data = train, method = "earth",
                  tuneGrid = data.frame(degree = c(1,2), nprune = 5)) 
    pred_earth = c(pred_earth, predict(earth, test[,-1]))
    
    grades = c(grades, test$FINAL)
  }
  
  # Calculating the error for each method
  error_rf = sum(abs(pred_rf - grades))/nrow(dataset) 
  error_pcr = sum(abs(pred_pcr - grades))/nrow(dataset) 
  error_xg = sum(abs(pred_xg - grades))/nrow(dataset_boost) 
  error_bm = sum(abs(pred_bm - grades))/nrow(dataset) 
  error_earth = sum(abs(pred_earth - grades))/nrow(dataset) 
  error_kknn = sum(abs(pred_kknn - grades))/nrow(dataset) 
  error_nnet = sum(abs(pred_nnet - grades))/nrow(dataset) 
  error_svm = sum(abs(pred_svm - grades))/nrow(dataset) 
  
  # Returning Values
  my_list <- list("MAE_bm" = error_bm, "MAE_rf" = error_rf, "MAE_pcr" = error_pcr, 
                  "MAE_xg" = error_xg, "MAE_kknn" = error_kknn, "MAE_nnet" = error_nnet, 
                  "MAE_svm" = error_svm, "MAE_earth" = error_earth)   
  
  return(my_list)
}

######################################
# Running the prediction code

pred_int = prediction_function(datasetint, datasetint_b)
pred_1 = prediction_function(dataset1, dataset1_b)
pred_2 = prediction_function(dataset2, dataset2_b)
pred_3 = prediction_function(dataset3, dataset3_b)
pred_4 = prediction_function(dataset4, dataset4_b)
pred_5 = prediction_function(dataset5, dataset5_b)
pred_6 = prediction_function(dataset6, dataset6_b)

###################################
#Extracting the MAE values for each method
Error_bm =c(pred_int$MAE_bm, pred_1$MAE_bm, pred_2$MAE_bm, pred_3$MAE_bm, 
            pred_4$MAE_bm, pred_5$MAE_bm, pred_6$MAE_bm)
Error_rf = c(pred_int$MAE_rf, pred_1$MAE_rf, pred_2$MAE_rf, pred_3$MAE_rf, 
             pred_4$MAE_rf, pred_5$MAE_rf, pred_6$MAE_rf)
Error_pcr = c(pred_int$MAE_pcr, pred_1$MAE_pcr, pred_2$MAE_pcr, pred_3$MAE_pcr, 
              pred_4$MAE_pcr, pred_5$MAE_pcr, pred_6$MAE_pcr)
Error_xg = c(pred_int$MAE_xg, pred_1$MAE_xg, pred_2$MAE_xg, pred_3$MAE_xg, 
             pred_4$MAE_xg, pred_5$MAE_xg, pred_6$MAE_xg)
Error_svm =c(pred_int$MAE_svm, pred_1$MAE_svm, pred_2$MAE_svm, pred_3$MAE_svm, 
             pred_4$MAE_svm, pred_5$MAE_svm, pred_6$MAE_svm)
Error_earth = c(pred_int$MAE_earth, pred_1$MAE_earth, pred_2$MAE_earth, pred_3$MAE_earth, 
                pred_4$MAE_earth, pred_5$MAE_earth, pred_6$MAE_earth)
Error_nnet = c(pred_int$MAE_nnet, pred_1$MAE_nnet, pred_2$MAE_nnet, pred_3$MAE_nnet, 
               pred_4$MAE_nnet, pred_5$MAE_nnet, pred_6$MAE_nnet)
Error_kknn = c(pred_int$MAE_kknn, pred_1$MAE_kknn, pred_2$MAE_kknn, pred_3$MAE_kknn, 
               pred_4$MAE_kknn, pred_5$MAE_kknn, pred_6$MAE_kknn)

############################################
# Plotting Figure 3 (for six weeks)
Errors = data.frame(Week = 0:6, # Change for additional week as required
                     BART = Error_bm,
                     RF = Error_rf,
                     XGBoost = Error_xg,
                     PCR = Error_pcr,
                     Splines = Error_earth,
                     KNN = Error_kknn,
                     NN = Error_nnet,
                     SVM = Error_svm)

# write.csv(Errors, "Errors.csv", row.names=FALSE) # Save errors when prediction is completed

Errors = melt(Errors ,id.vars = 'Week', variable.name = 'Prediction Errors') # Reformats data
colnames(Errors) = c("Week", "Method", "value")

ggplot(Errors, aes(Week, value, color = factor(Method, 
               labels = c("Bayesian Additive Regressive Trees", "Random Forests",  # For legend
               "XGBoost", "Principal Components Regression",
               "Multivariate Adaptive Regression Splines", "K-Nearest Neighbours",
               "Neural Networks", "Support Vector Machine")))) +
  geom_line() + labs(color = "Method") +
  scale_y_continuous(breaks=seq(0, 22, 1)) +
  scale_x_discrete(breaks = 0:6, limits = c(0:6) ,labels=c("Intially","1","2","3","4","5","6")) + # x-axis: Change for additional weeks
  theme_bw() + # Makes clear background
  xlab('Week') + ylab('Mean\nAbsolute\nError') + 
  theme( # This section defines text format 
    axis.title.x=element_text(angle = 0, color='black', face='bold', size = 10),
    axis.title.y=element_text(angle = 0, color='black', face='bold', size = 8),
    axis.text.x = element_text(hjust = .5, color = 'black'),
    axis.text.y = element_text(color = 'black'),
    legend.title=element_text(size = 9), 
    legend.text=element_text(size = 9),
    legend.position = c(0.2, 0.11),
    legend.key.height = unit(.35, "cm")
  ) +
  geom_dl(aes(label = Method), method=list("last.bumpup", rot=20, dl.trans(x = x-1, y = y+0.08))) + # add labels to diagram 
  scale_color_viridis(discrete = TRUE, option = "D") # calls package which has colours useful for gray pictures
