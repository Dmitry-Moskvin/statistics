#### Work with X_train
library(caret)
# 1) Scale
preprocessParams <- preProcess(iris[,1:4], method=c("scale"))


# 2) Center
preprocessParams <- preProcess(iris[,1:4], method=c("center"))


# 3) Standartize (N(0,VAR))
preprocessParams <- preProcess(iris[,1:4], method=c("center", "scale"))


# 4) Normalize (range [0,1])
preprocessParams <- preProcess(iris[,1:4], method=c("range"))


# 5) Box-Cox OR Yeo-Johnson Transform (supports raw values that are equal to zero and negative)
preprocessParams <- preProcess(PimaIndiansDiabetes[,7:8], method=c("BoxCox"))
# OR
preprocessParams <- preProcess(PimaIndiansDiabetes[,7:8], method=c("YeoJohnson"))

# 6) Principal Component Analysis. Good for linear models; delete correlated variables
preprocessParams <- preProcess(iris, method=c("center", "scale", "pca"))


# 7) Independent Component Analysis. Useful for algorithms such as naive bayes. 
preprocessParams <- preProcess(PimaIndiansDiabetes[,1:8], method=c("center", "scale", "ica"), n.comp=5)

# 8) Find cor variables
descrCor <-  cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)
highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)

# 9) Find linear dependencies
comboInfo <- findLinearCombos(ltfrDesign)


# 10) One Hot Encoding
library(earth)
dummies <- dummyVars(survived ~ ., data = etitanic)
# OR
library(nnet) # class.ind()



# Example
data(iris)
# summarize data
summary(iris[,1:4])
# summarize transform parameters
print(preprocessParams)
# transform the dataset using the parameters
transformed <- predict(preprocessParams, iris[,1:4])
# summarize the transformed dataset
summary(transformed)
