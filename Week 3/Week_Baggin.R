# Baggin Stands for bootsrap aggregating
# For a complicated mode. athe average of two models can bring a perfect balance of bais and variance in your fit

## Baisc Idea
# Resample cases and calculate predictions and take their average or majority vote. The benefit is the similar bias but reduced variance in predictability 
# Most useful for non-linear functions

## Example Ozone data
library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Trying to predict temperature using ozone as predictor.