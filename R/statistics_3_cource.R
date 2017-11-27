# Basic of statistics with R examples
# Cource URL: https://stepik.org/course/2152/

#### 1 Module ####
transform_Tukey <- function(x, lambda = 0){
  if(lambda < 0){
    x = -1*(x^lambda)
  } else if(lambda > 0)
  {
    x = x^lambda
  } else
    x = log(x)
  return(x)
}


#В модели log(Y)=b1∗X+b0 коэффициент наклона означает: при единичном изменении переменной X, переменная Y в среднем изменяется на 100∗b1 процентов.
#В модели Y=b1∗log(X)+b0 коэффициент наклона означает:  изменение на 1% по X в среднем приводит к 0.01∗b1 изменению по переменной Y.


# Box-Cox transformation помогает с ненормальностью распределения ошибок. Трансформируется ЗП
# Ynew = Y^(p-1) if p != 0
# Ynew = log(Y) if p = 0


# Гетероскедастичность
# Если мы построим регрессию, где зависимой переменной будет квадрат остатков модели y ~ x, а независимой переменной будет предиктор x, 
# и в этой модели окажется высокий и значимый R квадрат, это означает, что в данных есть гетероскедастичность
# lmtest::bptest
# Проблема решается трансформацией Бокса-Кокса



Hmisc::rcorr(as.matrix(mtcars))
# показатель "вздутия" дисперсии --> car::vif(). Если vif > 10, лучше отбросить переменную, так как "расшатывается" доверительный интервал не только этой переменной, но и др.

#### 1 Module Practice ####
# Practice 1 Module. hetero_test() --> Проверка на гетероскедастичность; Функция должна возвращать значение R квадрат вспомогательной модели.
hetero_test <-  function(test_data){
  names(test_data) <- c("y_first", names(test_data)[-1])
  fit = lm(y_first ~ ., test_data)
  test_data$residuals <- fit$residuals
  fit_2 = lm(residuals^2 ~ ., test_data[,-1])
  summary(fit_2)$r.squared
}

# Practice 1 Module. VIF() --> реализация показателя вздутия дисперсии. Есть в пакете car
VIF <- function(test_data){
  temp = sapply(c(2:dim(test_data)[[2]]), function(i) 1 /(1 - summary(lm(formula = paste(names(test_data)[[i]], " ~ ."), test_data[,-1]))$r.squared))
  names(temp) = names(test_data[,-1])
  return(temp)
}

VIF(mtcars) # try

# Practice 1 Module.. smart_model() --> итерационное исключение из модели переменных с vif > 10
smart_model <-  function(test_data){
  names(test_data) = c("dependent_variable", names(test_data)[2:ncol(test_data)])
  while (sum(VIF(test_data) > 10) > 0) {
    test_data = test_data[,-(which.max(VIF(test_data)) + 1)]
    if(dim(test_data)[[2]] == 2) break
   } 
  return(lm(dependent_variable ~ .,test_data)$coefficients)
}
smart_model(mtcars) # try

# Practice 1 Module. transform_x() --> поиск лучшей степени для трансформации переменной. В датафрейме есть лишь Y,X.
transform_x <-  function(test_data, seq_find = seq(-2,2,0.1)){ # need transform_Tukey()
  temp = abs(sapply(seq_find, function(x) cor(test_data[,1], transform_Tukey(test_data[,2], x))))
  best_lambda =  seq_find[which.max(temp)]
  return(transform_Tukey(test_data[,2], best_lambda))
}

#### 2 Module ####
# Синтаксис для смешанных регрессионных моделей в пакете 'lme4'
library(mlmRev)
library(lme4)
library(ggplot2)
# lmer(DV ~ IV + (1 + IV | RV), data = my_data)
# lmer(DV ~ FE + (1 + FE | RE), data = my_data) 
# FE - fix effect
# RE - random effect


data("Exam")
str(Exam)
help(Exam)

ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point()

ggplot(data = Exam, aes(x = standLRT, y = normexam, col = school)) + 
  geom_point()


# Один главный эффект
Model1 <- lm(normexam ~ standLRT, data=Exam)
summary(Model1)

Exam$Model1_pred <- predict(Model1)

ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point() + 
  geom_line(data = Exam, aes(x = standLRT, y = Model1_pred), col = 'blue', size = 1)

Model1 <- lmer(normexam ~ standLRT, data=Exam)
# mistake because of no random effects


# Главный эффект + случайный свободный член
Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2) # coefficients --> restrictred max likelihood
# Если t-value >~2, то коэф. значим

Exam$Model2_pred <- predict(Model2)

ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model2_pred, col = school))


# Главный эффект + случайный свободный член + случайный угловой коэффициент
Model3 <- lmer(normexam ~ standLRT + (1 + standLRT|school), data=Exam)
summary(Model3)

Exam$Model3_pred <- predict(Model3)

ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model3_pred, col = school))



# Главный эффект + случайный угловой коэффициент
Model4 <- lmer(normexam ~ standLRT + (0 + standLRT|school), data=Exam)
summary(Model4)

Exam$Model4_pred <- predict(Model4)
ggplot(data = Exam, aes(x = standLRT, y = normexam)) + 
  geom_point(alpha = 0.2) + 
  geom_line(data = Exam, aes(x = standLRT, y = Model4_pred, col = school))



# Нескоррелированные случайные эффекты
Model5 <- lmer(normexam ~ standLRT + (1|school) + (0 + standLRT|school), data=Exam)
summary(Model5)


# Сравнение моделей

Model2 <- lmer(normexam ~ standLRT + (1|school), REML = FALSE, data=Exam) 
summary(Model2)

Model0 <- lmer(normexam ~ 1 + (1|school), REML = FALSE, data = Exam)
summary(Model0)

anova(Model0, Model2)

# p-значения
library(lmerTest)

Model2 <- lmer(normexam ~ standLRT + (1|school), data=Exam)
summary(Model2)

# Обобщённые смешанные модели
Exam$school_type <- ifelse(Exam$type == 'Mxd', 1, 0)

Model5 <- glmer(school_type ~ normexam + (1|school), family = "binomial", data = Exam)
summary(Model5)


# Предсказания на новых датасетах
predict(Model2, Exam)

new_Exam <- Exam[sample(1:nrow(Exam), 100), ]
new_Exam$school <- sample(101:200)

predict(Model2, new_Exam, allow.new.levels = T)

# Исследование случайных эффектов
fixef(Model3)
ranef(Model3)
#### 3 Module ####
# 1 task
x = sample(c(1:10),100,replace = T0)
median_cl_boot <- function(x){
  B = 1000
  n = length(x)
  real_median = median(x)
  boot.samples = matrix(sample(x, size = B*n,replace = T),B,n)
  boot.statistics = apply(boot.samples, 1, median)
  se = sd(boot.statistics)
  interval = real_median + c(-1,1)*2*se
  return(interval)
}
# 2 task
test = data.frame(sample(c(0,1), 100, T), sample(c(0,1), 100, T))
names(test) = c("y", "x")

slope_cl_boot <- function(x, B = 1000){
  n = nrow(x)
  real_slope = lm(y ~ x, data = x)$coefficients[2]
  boot.statistics = replicate(B, lm(y ~ x, data = x[sample(n, replace=T),])$coefficients[2] - real_slope)
  interval = real_slope + quantile(cors, c(.025,.975))
  return(interval)
}