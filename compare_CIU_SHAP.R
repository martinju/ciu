
#install.packages("shapr")
library(shapr)
require(ggplot2)
require(MASS)
require(caret)
require(ciu)


kfoldcv <- trainControl(method="cv", number=10)
gbm <- train(medv ~ ., Boston, method="gbm", trControl=kfoldcv)
instance <- Boston[370,1:13]
ciu <- ciu.new(gbm, medv~., Boston)
p <- ciu$ggplot.col.ciu(instance); print(p)
ciu$explain(instance,1:14)


#### shapr


# Create custom function of model_type for gbm
model_type.train <- function(x) {
  "regression"
}

# Create custom function of predict_model for gbm
predict_model.train <- function(x, newdata) {
    predict(x, as.data.frame(newdata))
}

# Prepare the data for explanation
set.seed(123)
x_var = colnames(Boston)[!(colnames(Boston)=="medv")]
explainer <- shapr(Boston, gbm, feature_labels = x_var)

# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(Boston[, "medv"])

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with
# bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x = Boston[370,],
  explainer,
  approach = "empirical",
  prediction_zero = p0
)

explanation_indep <- explain(
  x = Boston[370,],
  explainer,
  approach = "empirical",
  type="independence",
  prediction_zero = p0
)


# Plot the resulting explanations for observations 1 and 6, excluding
# the no-covariate effect.
gg = list()
gg[[1]]<- plot(explanation, plot_phi0 = FALSE)+ggtitle("Dependence")
gg[[2]] <- plot(explanation_indep, plot_phi0 = FALSE)+ggtitle("Independence")


library(data.table)
ciu_dt <- as.data.table(p$data)
ciu_dt[,influence:=CI*2*(CU-0.5)]

dt <- as.data.table(t(ciu_dt[,.(influence)]))
names(dt) <- as.character(unlist(ciu_dt[,.(in.names)]))
dt[,type:="CIU"]

res_DT <- rbind(cbind(explanation_indep$dt,type="shapr_indep"),
                cbind(explanation$dt,type="shapr_dep"))
res_DT <- rbind(res_DT[,-1],dt,use.names=T)
features <- names(res_DT)[-14]

rowsums <- res_DT[,rowSums(.SD),.SDcols=features]
res_DT[,(features):=lapply(.SD,function(x)x/rowsums),.SDcols=features]

matplot(t(res_DT[,-14]),type = "l")



rowSums(res_DT[,-14])



library(gridExtra)

grid.arrange(gg[[1]],gg[[2]],p)


