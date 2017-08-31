library(data.table)

setwd("D:\\Documents\\kaggle-competition\\House-Prices\\Data")

train <- fread("train.csv", stringsAsFactors = T)

# Check NA count by variable
count_na <- train[, lapply(.SD, function(x) sum(is.na(x)))]
count_na <- melt(count_na, measure.vars = names(count_na)
                 , variable.name = 'Var', value.name = 'NA_Count')
count_na[NA_Count != 0]

# Change NAs to values accordingly to the 'data_description.txt'
train[is.na(Alley), Alley := "No alley access"
     ][is.na(BsmtQual), BsmtQual := "No Basement"
     ][is.na(BsmtCond), BsmtCond := "No Basement"
     ][is.na(BsmtExposure), BsmtExposure := "No Basement"
     ][is.na(BsmtFinType1), BsmtFinType1 := "No Basement"
     ][is.na(BsmtFinType2), BsmtFinType2 := "No Basement"
     ][is.na(FireplaceQu), FireplaceQu := "No Fireplace"
     ][is.na(GarageType), GarageType := "No Garage"
     ][is.na(GarageYrBlt), GarageYrBlt := 0
     ][is.na(GarageFinish), GarageFinish := "No Garage"
     ][is.na(GarageQual), GarageQual := "No Garage"
     ][is.na(GarageCond), GarageCond := "No Garage"
     ][is.na(PoolQC), PoolQC := "No Pool"
     ][is.na(Fence), Fence := "No Fence"
     ][is.na(MiscFeature), MiscFeature := "None"
     ]

# Check NA count by variable after replacing
count_na <- train[, lapply(.SD, function(x) sum(is.na(x)))]
count_na <- melt(count_na, measure.vars = names(count_na)
                 , variable.name = 'Var', value.name = 'NA_Count')
count_na[NA_Count != 0]

# Input missing data with MICE
library(mice)

temp_data <- mice(train, m = 10, method = 'rf', maxit = 50, seed = 42)

# Complete data set
complete_data <- complete(temp_data)
complete_data <- data.table(complete_data)

# Comparing Original and MICE data for 'LotFrontage'
comp_original <- train[, .(LotFrontage)]
comp_original[, FLAG := 'Original']

comp_mice <- complete_data[, .(LotFrontage)]
comp_mice[, FLAG := 'MICE']

compare_data <- rbind(comp_original, comp_mice)

ggplot(compare_data, aes(LotFrontage, fill = FLAG)) +
  geom_histogram(position = "identity", alpha = .3) +
  labs(title = "Compare Data", x = "Linear feet of street", y = "n", color = " ") 


ggplot(compare_data, aes(LotFrontage, fill = FLAG)) +
  geom_density(alpha = .3) +
  labs(title = "Original Data", x = "Linear feet of street", y = "n", color = " ") 

