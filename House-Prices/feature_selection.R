library(data.table)

setwd("D:\\Documents\\kaggle-competition\\House-Prices\\Data")

# Import Data
dt <- fread("complete_data.csv", stringsAsFactors = T)
dt[, MSSubClass := as.factor(MSSubClass)]

# Divide SalePrice in classes
dt[SalePrice < quantile(SalePrice, probs=c(0.25))
   , SalePriceClass := as.factor("0-25")]
dt[SalePrice >= quantile(SalePrice, probs=c(0.25)) & SalePrice < median(SalePrice)
   , SalePriceClass := as.factor("25-50")]
dt[SalePrice >= median(SalePrice) & SalePrice < quantile(SalePrice, probs=c(0.75))
   , SalePriceClass := as.factor("50-75")]
dt[SalePrice >= quantile(SalePrice, probs=c(0.75))
   , SalePriceClass := as.factor("75-100")]

# Column types
col_type <- data.table(Var = names(dt)
                       , Type = sapply(dt, class))
col_type <- col_type[!(Var %in% c("Id","SalePrice","SalePriceClass"))]

# Calculate Chi Squared and Pearson Correlation test of independence
chi_test <- data.table()
for(i in col_type[Type == "factor"]$Var) {
  chi_test_temp <- data.table(Var = i
                              , p_value = chisq.test(dt[[i]]
                                                     , dt[["SalePriceClass"]])$p.value)
  chi_test <- rbind(chi_test, chi_test_temp)
  rm(chi_test_temp)
}

cor_test <- data.table()
for(i in col_type[Type == "integer"]$Var) {
  cor_test_temp <- data.table(Var = i
                              , p_value = cor.test(dt[[i]]
                                                   , dt[["SalePrice"]])$p.value)
  cor_test <- rbind(cor_test, cor_test_temp)
  rm(cor_test_temp)
}

indep_test <- rbind(chi_test, cor_test)
indep_test[, p_value := round(p_value, digits = 4)]
rm(chi_test, cor_test)
