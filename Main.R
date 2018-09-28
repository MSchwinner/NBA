
# Packages ----------------------------------------------------------------

#install.packages("readxl")
library(readxl)
#install.packages("ggplot2")
library(ggplot2)
# install.packages("caret",
#                  repos = "http://cran.r-project.org",
#                  dependencies = c("Depends","Imports","Suggests"))
library(caret)
#install.packages("corrplot")
library(corrplot)
#install.packages("e1071")
library(e1071)
#install.packages("RANN")
library(RANN)
#install.packages("randomForest")
library(randomForest)
#install.packages("skimr")
library(skimr)
#install.packages("dplyr")
library(dplyr)
#install.packages("plotly")
library(plotly)

# Wrangling ---------------------------------------------------------------

source("Scripts/Wrangling.R")

# Preprocessing -----------------------------------------------------------

source("Scripts/Preprocessing.R")

# Random Forest -----------------------------------------------------------

source("Scripts/RandomForest.R")

# Team Records ------------------------------------------------------------

source("Scripts/Team_Records.R")
