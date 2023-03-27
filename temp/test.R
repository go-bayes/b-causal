
library(mlim)
library(missRanger)
library(missForest)
library(mice)
library(randomForest)
library(parameters)
require(miceRanger)
require(missRanger)
library(xgboost)

## make na
irisNA <- mlim.na(iris, p = 0.5, stratify = TRUE, seed = 2022)


# mis ranger
m <- missRanger(
  irisNA, formula = . ~ ., pmm.k = 5, num.trees = 1000, seed = 1, verbose = 0
)
print(MRERROR <- mlim.error(m, irisNA, iris))


# Rand Forest
RF <- missForest(irisNA)
RF$ximp

# mlim
MLIM <- mlim(irisNA, m=1, seed = 2022, tuning_time = 180) 
print(MLIMerror <- mlim.error(MLIM, irisNA, iris))

MLIM_0 <- mlim(irisNA, m=1, seed = 0, tuning_time = 180) 
print(MLIMerror <- mlim.error(MLIM, irisNA, iris))

MLIM_00 <- mlim(irisNA, m=1, seed = 123, tuning_time = 180) 

print(MLIMerror <- mlim.error(MLIM_00, irisNA, iris))



# mice
MC <- mice::mice(irisNA, m=5, maxit = 50, method = 'pmm', seed = 500)
print(MCerror <- mlim.error(MC, irisNA, iris))

mc_cart <- mice::mice(irisNA, m=5, maxit = 50, method = 'cart', seed = 500)
print(MCerror_cart <- mlim.error(MC_cart, irisNA, iris))

# mlim
MLIM2 <- mlim(irisNA,  m = 5, seed = 2022) 
print(MLIMerror2 <- mlim.error(MLIM2, irisNA, iris,varwise = TRUE))


# mlim with XGB # takes a very long time
MLIM3 <- mlim(irisNA, algos = c("XGB"),
              m = 5, seed = 2022, tuning_time = 180) 

print(MLIMerror2 <- mlim.error(MLIM3, irisNA, iris,varwise = TRUE))



mlim.summarize(MLIM2)
mlim.summarize(MLIM)
set.seed(2022)


mids <- mlim.mids(MLIM2, irisNA)

fit <- with(data=mids, glm(Sepal.Length ~ Species))
res <- mice::pool(fit)
summary( res )


# original
parameters::model_parameters( glm(Sepal.Length ~ Species,  data = iris) ) 


# case delete (not great)
parameters::model_parameters( glm(Sepal.Length ~ Species,  data = irisNA) ) 


# missRang - not great - terrible
parameters::model_parameters( glm(Sepal.Length ~ Species, data = m) ) 


# randForest - not terrible
parameters::model_parameters( glm(Sepal.Length ~ Species, data = RF$ximp) ) 


# mice -- not terrible
mice_models <- lapply(1:5, function(i) {
  glm(Sepal.Length ~ Species, data = complete(MC, action = i))
})
parameters::pool_parameters(mice_models)


# mice cart -- a little worse

mice_models_cart <- lapply(1:5, function(i) {
  glm(Sepal.Length ~ Species, data = complete(mc_cart, action = i))
})
parameters::pool_parameters(mice_models_cart)


# MLIM single = ok
parameters::model_parameters( glm(Sepal.Length ~ Species,  data = MLIM) ) 

# MLIM single again

parameters::model_parameters( glm(Sepal.Length ~ Species,  data = MLIM_0) ) 

# MLIM single again

parameters::model_parameters( glm(Sepal.Length ~ Species,  data = MLIM_00) ) 


# RF not good
parameters::model_parameters( glm(Sepal.Length ~ Species,  data = RF$ximp) ) 


# MLIM multiple - not the best
models <- with(mids,
  glm(Sepal.Length ~ Species))
parameters::model_parameters(models)


parameters::model_parameters( glm(y ~ x, data = bind) )

# MLIM with XGB





