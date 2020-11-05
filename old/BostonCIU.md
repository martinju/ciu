# CIU with Boston Housing Dataset

The Boston Housing Dataset is a classical benchmark set used in Data Analysis and Machine learning, available in the MASS library. 

A gradient boosting model will be used here for learning the mapping from the 13 input variables to the Median value of owner-occupied homes in $1000's. This is a regression task (not a classification task), which means that the best explanation approach might be different. However, for CIU it doesn't really matter whether it is a classification or a regression task.

gbm is not (yet) supported directly by ``ciu``, so we will provide the ``predict``function to use as an argument to ``ciu.new``. This is a generic way for providing support for "any" regression/classification model. 

```r
require(MASS) # Just in case Boston is not already available
require(gbm)

# We don't care about train/test set in this case because it's not about evaluating training performance.
Boston.boost=gbm(medv ~ . ,data = Boston, distribution = "gaussian", n.trees=10000,
                 shrinkage = 0.01, interaction.depth = 4)
```
Gradient Boosting (gbm) is not yet supported by ``ciu`` implementation, so we need to tell ``ciu`` explicitly what it looks like. 

```r
predict.function <- function(model, inputs) { predict(model,inputs,n.trees=10000) }
```
We initialize and create CIU object and get CI and CU for row 370, which has a high median value (50).
```r
source("ContextualImportanceUtility.R") # Check that R's working directory is set correctly.
n.in <- ncol(Boston) - 1
in.mins <- apply(Boston[,1:n.in], 2, min)
in.maxs <- apply(Boston[,1:n.in], 2, max)
c.minmax <- cbind(in.mins, in.maxs)
out.range <- matrix(c(min(Boston$medv), max(Boston$medv)), ncol=2)
ciu <- ciu.new(Boston.boost, in.min.max.limits=c.minmax, abs.min.max=out.range, 
               input.names=names(Boston)[1:n.in], output.names=names(Boston)[n.in+1], 
               predict.function=predict.function)
inst.ind <- 370
CI.CU <- ciu$explain(Boston[inst.ind,], ind.inputs.to.explain=c(1), montecarlo.samples = 100)
CI.CU # Display result
# Gives CI and CU for the first input variable (crim) and looks something like this: 
#            CI       CU    Cmin     Cmax
#medv 0.1562961 0.845962 44.7032 51.73652
```
Then we display the coloured bar plot for the same instance (looks nicer in RStudio than in plain R).

```r
ciu$barplot.CI.CU(Boston[inst.ind,1:n.in])

```
# Alternative way for supporting "any" model
The ``FunctionApproximator`` class is supported automatically by ``ciu``. It essentially means that the model class needs to inherit ``FunctionApproximator`` and implement an ``eval(inputs)``method. 

The following code shows how this can be done for Gradient Boosting. The same principle is applicable for any model. This approach is more flexible than providing a ``predict.function`` argument to ``ciu.new()`` because it is possible to associate information with the object. In this example, the ``n.trees``parameter is stored as an object instance value but it is possible to implement any required processing with this approach. 

```r
# CIU with Gradient Boosting, FunctionApproximator wrapper
gbm.fa.new <- function(gbm, n.trees=1) {
  o.gbm <- gbm
  o.n.trees <- n.trees
  pub <- list(eval = function(inputs) { predict(o.gbm,inputs,n.trees=o.n.trees) })
  class(pub) <- c("gbm.fa",class(function.approximator.new()),class(pub))
  return(pub)
}
gbm.fa <- gbm.fa.new(Boston.boost, 10000)
ciu.gbm.fa <- ciu.new(gbm.fa, in.min.max.limits=c.minmax, abs.min.max=out.range, 
                      input.names=names(Boston)[1:n.in], output.names=names(Boston)[n.in+1])
inst.ind <- 1
CI.CU <- ciu.gbm.fa$explain(Boston[inst.ind,], ind.inputs.to.explain=c(1), montecarlo.samples = 100)
ciu.gbm.fa$barplot.CI.CU(Boston[inst.ind,1:n.in])
```
# Example barplots by CIU

The following figure shows the barplot of three instances: #406 (medv=5, lowest), #6 (medv=28.7, average), #370 (medv=50, highest). 

![CIU barplots for three instances of Boston Housing](/Figures/BostonHousingCIU_barplots.png)

The code for producing this image is the following: 
```r
par(mfrow=c(1,3))
ciu.gbm.fa$barplot.CI.CU(Boston[406,1:n.in])
ciu.gbm.fa$barplot.CI.CU(Boston[6,1:n.in])
ciu.gbm.fa$barplot.CI.CU(Boston[370,1:n.in])
```

Same as pie charts (using pie.CI.CU instead of barplot.CI.CU): 
![CIU pie chart for Boston Housing house 406](/Figures/ciu_gbm_Boston_pie_inst406.png)
![CIU pie chart for Boston Housing house 6](/Figures/ciu_gbm_Boston_pie_inst6.png)
![CIU pie chart for Boston Housing house 370](/Figures/ciu_gbm_Boston_pie_inst370.png)


# Author

[Kary Främling](http://github.com/KaryFramling)

