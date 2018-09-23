
# Packages ----------------------------------------------------------------

library(readxl)
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("caret")
library(caret)
library(corrplot)
#install.packages("e1071")
library(e1071)
#install.packages("RANN")
library(RANN)
#install.packages("randomForest")
library(randomForest)
#install.packages("skimr")
library(skimr)

# Wrangling ---------------------------------------------------------------
## ToDo:
#     - Incorporate advanced data from previous seasons

source("Scripts/Wrangling.R")


# Preprocessing -----------------------------------------------------------

source("Scripts/Preprocessing.R")

# Team Records ------------------------------------------------------------

source("Scripts/Team_Records.R")
