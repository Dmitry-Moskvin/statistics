# function return coefficients from scale linear model
beta.coef <- function(x){
  model <- lm(scale(x[,1]) ~ scale(x[,2]),x)
  model$coefficients
} 

# check normality of all variables
normality.test  <- function(x){    
  return(sapply(x, shapiro.test)['p.value',])
}


resid.norm <- function(x){
 if(shapiro.test(x$residuals)$p.value < 0.05)
   ggplot(x, aes(x = x$residuals)) + 
    geom_histogram(bins = 30, fill = 'red')
  else
    ggplot(x, aes(x = x$residuals)) + 
    geom_histogram(bins = 30, fill = 'green')
}


high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)),arr.ind=T)))
}



