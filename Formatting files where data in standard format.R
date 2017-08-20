rm(list=ls()) #clear workspace

# This file imports and removes excessive variables if the data are in the correct layout
# i.e. one variable per column and one row per student where each student's row is the same for
# demographic file, LMS/VLE data and results. 
# If you need to match rows for a student across files please see:
# "formatting files where data in non-standard form"
# Can apply this straight away on fictitious datasets for six weeks of data

# Load LMS files and delete student number column
use_of_resources_1 = read.csv("Sample_week1.csv",header=TRUE) 
use_of_resources_1 = use_of_resources_1[,-1]
use_of_resources_2 = read.csv("Sample_week2.csv",header=TRUE)
use_of_resources_2 = use_of_resources_2[,-1]
use_of_resources_3 = read.csv("Sample_week3.csv",header=TRUE)
use_of_resources_3 = use_of_resources_3[,-1]
use_of_resources_4 = read.csv("Sample_week4.csv",header=TRUE)
use_of_resources_4 = use_of_resources_4[,-1]
use_of_resources_5 = read.csv("Sample_week5.csv",header=TRUE)
use_of_resources_5 = use_of_resources_5[,-1]
use_of_resources_6 = read.csv("Sample_week6.csv",header=TRUE)
use_of_resources_6 = use_of_resources_6[,-1]

# Remove any unnecessary columns - variables that only contain zeroes
# Function to remove columns:
removecolfunction = function(use_of_resourcesgeneral){ 
  
  # Ensure variables are numeric and keep column names
  colnames = colnames(use_of_resourcesgeneral) 
  use_of_resourcesgeneral = matrix(as.numeric(as.character(unlist(use_of_resourcesgeneral))), nrow=nrow(use_of_resourcesgeneral))
  colnames(use_of_resourcesgeneral) = colnames
  
  # Check if the sum of a column is zero; if it is zero remove column
  for(i in  ncol(use_of_resourcesgeneral):1) 
    {
      if(sum(use_of_resourcesgeneral[,i]) == 0)
        {
          use_of_resourcesgeneral = use_of_resourcesgeneral[,-c(i)]
        }
    }
  return(use_of_resourcesgeneral)
}

# Call function to remove 'zero' columns
use_of_resources_1a = removecolfunction(use_of_resources_1)
use_of_resources_2a = removecolfunction(use_of_resources_2)
use_of_resources_3a = removecolfunction(use_of_resources_3)
use_of_resources_4a = removecolfunction(use_of_resources_4)
use_of_resources_5a = removecolfunction(use_of_resources_5)
use_of_resources_6a = removecolfunction(use_of_resources_6)

# Read in background information datafiles
student_background = read.csv("Student_background.csv", header = TRUE)
student_background =student_background[,-1] # Don't need student number as a variable
student_background_oh = read.csv("Student_background_oh.csv", header = TRUE)
student_background_oh =student_background_oh[,-1] # Don't need student number as a variable

# Read in continuous assessment datasfiles and format as appropriate
results = read.csv("sample_results.csv", header = TRUE)
results = results[,-1] # Remove student number as it isn't needed for prediction
FINAL = results[,1]                             # Week 1&2
student_results_3_4 = results[,c(1,2)]          # Week 3&4 - more CA available
student_results_5 = results[,c(1:3)]            # Week 5
student_results_6 = results[,c(1:4)]         # Week 6

# Now LMS data, background information and results should be formatted so that each student's
# data is on the same row number for each file
# For prediction modelling go to "Prediction Modelling part a file"