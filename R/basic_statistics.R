set.seed(17)
test = factor(sample(c(1,0), 100, replace = TRUE), levels = c(0, 1), labels = c("T-","T+"))
human = factor(sample(c(1,0), 100, replace = TRUE), levels = c(0, 1), labels = c("H-","H+"))
theory = addmargins(table(test, human))                    

#       human
# test  H-  H+  Sum
# T-    29  27   56
# T+    26  18   44
# Sum   55  45  100

# Чувствительность P(T+|H+)
# ошибка первого рода (1 - Se)
Se = 18/45
# Специфичность P(T-|H-)
# ошибка второго рода (1 - Sp)
Sp = 29/55
# Отношение правдоподобия
LR_plus = Se / (1 - Sp)
LR_minus = (1- Se) / Sp
odds_ratio = LR_plus / LR_minus # Хорошо, когда больше 3!
# Апостериорная вероятность
PV_plus = 18 / 44
PV_minus = 29 / 56

# OR with library
test <-prettyR::calculate.xtab(test, human,
                     varnames=c("Test","Human",
                                "T+","T-",
                                "H+","H-"))


# ROC-AUC curve по оси x показывает (1 — specificity), то есть ошибку второго рода, а по оси y sensitivity.
# Нужно максимизировать и sensitivity, и specificity. Площадь под кривой должна быть больше 0.5
# area under the curve calculation
# ROC-curve library(ROCR)


###################
mad(x) # median absolute deviation
MASS::hist.FD(x) # good histogram with automatic binwidth
t <- MASS::hist.FD(c(1,3,4,5,2,34,5,5)); t$mids[t$counts==max(t$counts)] # get moda

w_mean <- sum(w * x)/sum(w) # weight_mean
w_sd <- ((sum(w * x * x)/sum(w)) - w_mean^2)^0.5 # weight standard deviation
Hmisc::wtd.mean, Hmisc::wtd.var, Hmisc::wtd.rank
library(Weighted.Desc.Stat) # best one