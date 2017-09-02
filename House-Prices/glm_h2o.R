library(h2o)

# Select Important Variables
  # By Variable Importance in RF and GBM
df_2 <- df[, c(vi[RF >= 0.001 | GBM >= 0.001]$variable, "SalePrice")]
  # By Chi Squared and Pearson's correlation independence test
df_2 <- df[, c(indep_test[p_value < 0.001]$Var, "SalePrice")]

splits_2 <- h2o.splitFrame(df_2
                           , c(0.8, 0.1)
                           , destination_frames = c("train2.hex", "valid2.hex", "test2.hex")
                           , seed = 42)
train2 <- splits_2[[1]]
valid2 <- splits_2[[2]]
test2 <- splits_2[[3]]

# Build GLM
glm <- h2o.glm(training_frame = train2
               , validation_frame = valid2
               , y = h2o.ncol(df_2)
               , model_id = "glm_v1"
               , family = "gamma"
               , alpha = 0
               , lambda_search = T
               , link = "log"
               , remove_collinear_columns = T
               , seed = 42)
summary(glm)

h2o.rmsle(glm) # RMSLE for training data
h2o.rmsle(glm, valid = T) # RMSLE for validation data
h2o.rmsle(h2o.performance(glm, newdata = test2)) # RMSLE for testing data
