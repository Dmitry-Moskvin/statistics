#### correlation functions ####
# 1
corr.calc <- function(x){
 result <- c(unlist(cor.test(x = x[,1], y = x[,2])$estimate), 
             cor.test(x = x[,1], y =x[,2])$p.value)}

# 2
filtered.cor <- function(x){
  x <-  x[, which(sapply(x, is.numeric))]
  res <- cor(x)
  diag(res) <- 0
  return(res[which.max(abs(res))])}

# 2.2
filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))    
  cor_mat <- cor(x[, num_var])    
  diag(cor_mat) <- 0    
  return(cor_mat[which.max(abs(cor_mat))])}

#3
smart_cor <- function(x){
  if(shapiro.test(x[,1])$p.value < 0.05 | shapiro.test(x[,2])$p.value < 0.05){
  res <- cor.test(x = x[,1],y = x[,2], method ="spearman")$estimate
  }else{
  res <- cor.test(x = x[,1],y = x[,2], method ="pearson")$estimate
  }
  return(res)
}

#4
regr.calc <- function(d_f){
  if (cor.test(x = d_f[,1], y = d_f[,2])$p.value < 0.05){
    model <- lm(d_f[,1] ~ d_f[,2],d_f)
    d_f$fit <- predict(model)
    return(d_f)
  }
  else
    print("There is no sense in prediction")
}

#4.2

regr.calc <- function(sample_data){    
  cor_result = cor.test(~sample_data[[1]] + sample_data[[2]])    
  if (cor_result$p.value < 0.05){    
    fit_model  <- lm(sample_data[[1]] ~ sample_data[[2]])    
    sample_data$fit  <- fit_model$fitted.values    
    return(sample_data)    
  } else {    
    return('There is no sense in prediction')}
}

#### correlation tests ####
df  <- mtcars

cor.test(x = df$mpg, y = df$hp)
cor.test(~ mpg + hp, df)

fit  <- cor.test(x = df$mpg, y = df$hp)
str(fit)
fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)+
  facet_grid(. ~ am)


df_numeric  <- df[, c(1,3:7)]

pairs(df_numeric)
cor(df_numeric)

fit  <- corr.test(df_numeric)
c(fit$r, fit$p, fit$adjust)