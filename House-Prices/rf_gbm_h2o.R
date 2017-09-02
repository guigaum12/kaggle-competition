library(h2o)
library(data.table)
setwd("D:\\Documents\\kaggle-competition\\House-Prices\\Data")

# Inicialize H2O cluster
localH2O <- h2o.init(nthreads = 3,max_mem_size = "6g")

# Import data
df <- h2o.importFile("complete_data.csv")
df$MSSubClass <- h2o.asfactor(df$MSSubClass)

# Split Train, Test and Validation
splits <- h2o.splitFrame(df
                         , c(0.8, 0.1)
                         , destination_frames = c("train.hex", "valid.hex", "test.hex")
                         , seed = 42)
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# Distributed Random Forests
rf <- h2o.randomForest(training_frame = train
                       , validation_frame = valid
                       , x = 2:80
                       , y = 81
                       , model_id = "rf_v1"
                       , ntrees = 200
                       , max_depth = 30
                       , sample_rate = 0.7
                       , col_sample_rate_per_tree = 0.7
                       , stopping_rounds = 5
                       , stopping_tolerance = 0.0001
                       , stopping_metric = "RMSLE"
                       , score_each_iteration = T
                       , seed = 42)
summary(rf)

h2o.rmsle(rf) # RMSLE for training data
h2o.rmsle(rf, valid = T) # RMSLE for validation data
h2o.rmsle(h2o.performance(rf, newdata = test)) # RMSLE for testing data

h2o.varimp(rf) # Variable Importance
h2o.varimp_plot(rf, num_of_features = 20) # Variable Importance Plot

# Gradient Boosting Model
gbm <- h2o.gbm(training_frame = train
               , validation_frame = valid
               , x = 2:80
               , y = 81
               , model_id = "gbm_v1"
               , ntrees = 500
               , max_depth = 5
               , learn_rate = 0.01
               , sample_rate = 0.7
               , col_sample_rate = 0.7
               , stopping_rounds = 5
               , stopping_tolerance = 0.0001
               , stopping_metric = "RMSLE"
               , score_each_iteration = T
               , seed = 42)
summary(gbm)

h2o.rmsle(gbm) # RMSLE for training data
h2o.rmsle(gbm, valid = T) # RMSLE for validation data
h2o.rmsle(h2o.performance(gbm, newdata = test)) # RMSLE for testing data

h2o.varimp(gbm) # Variable Importance
h2o.varimp_plot(gbm, num_of_features = 20) # Variable Importance Plot

# Gather the Variable Importance from RF and GB
vi <- rbind(data.table(h2o.varimp(rf), Model = "RF")
            , data.table(h2o.varimp(gbm), Model = "GBM"))
vi[, percentage := round(percentage, digits = 4)]
vi <- dcast(vi, formula = variable ~ Model, value.var = "percentage")

vi[RF >= 0.005 | GBM >= 0.005]

