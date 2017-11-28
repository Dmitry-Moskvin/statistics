#1
smart_test <-  function(x){
  if(min(table(x))>=5)as.numeric(chisq.test(table(x))[c("statistic", "parameter", "p.value")]) else fisher.test(table(x))$p.value
}


#2
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data.csv", stringsAsFactors = F)

most_significant <- function(x){
  tmp <- sapply(x, function(y){chisq.test(table(y))$p.value})
  names(which(tmp == min(tmp)))
}

most_significant(test_data)


#4
test_data <- data.frame(V1 = c(16, 21, 18), 
                        V2 = c(17, 7, 16), 
                        V3 = c(25, 23, 27), 
                        V4 = c(20, 22, 18), 
                        V5 = c(16, 17, 19))

get_important_cases <- function(x){
  x$important_cases <- factor(ifelse(apply(x, 1,
                                           function(y) sum(y > sapply(x, mean))) <=dim(x)[[2]]/2, "No","Yes"),
                              levels = c("No","Yes"))
  return(x)
}

get_important_cases(test_data)

#5
stat_mode <- function(x){
  sort(unique(x))[table(x) == max(table(x))]
}

test <- c(13,13,13,18,12,12,19,5,15,13,7,7,19,2,2,17,5,19,13)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


#6
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_drugs.csv")

max_resid <- function(test_data){
  tmp <- as.data.frame(chisq.test(table(test_data))$stdres)
  tmp <- tmp[which(tmp$Freq == max(tmp$Freq)),c(1:2)]
  c(as.vector(tmp[['Drugs']]), as.vector(tmp[['Result']]))
}


# 10
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_luggage_2.csv")

get_features <- function(test_data){
  fit <- anova(glm(is_prohibited ~ .,
            data = test_data,
            family = "binomial"),
        test = "Chisq")
  NM <- names(test_data)[fit$`Pr(>Chi)` < 0.05]
  if(sum(fit$`Pr(>Chi)` < 0.05, na.rm = T) == 0) 
    print("Prediction makes no sense") else
      NM[!is.na(NM)]
}


# 11
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

most_suspicious <- function(test_data, data_for_predict){
  fit <- glm(is_prohibited ~ .,
             data = test_data,
             family = "binomial")
  pas <- predict(object = fit,newdata = data_for_predict,  type = "response")
  pas <- which(pas == max(pas))
  data_for_predict$passangers[pas]
}


# 13
test_data <- read.csv("https://stepic.org/media/attachments/course/524/s_anova_test.csv")

smart_anova <- function(test_data){
  require(data.table)
  test_data <- as.data.table(test_data)
  if(
  sum(sapply(levels(test_data$y), function(xy) shapiro.test(test_data[y == xy,x])$p.value) > 0.05) == 3 || # нормальность распределения
  bartlett.test(x = test_data$x, g = test_data$y)$p.value > 0.05 # гомогенность дисперсии
  ) c("ANOVA" = summary(aov(x ~ y, test_data))[[1]]$'Pr(>F)'[1]) else # сравнение выборок
    c("KW" = kruskal.test(x = test_data$x, g = test_data$y)$p.value) # если не выполняются условия нормальности и гомогенности
}


# 14
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_for_norm.csv")

normality_by <- function(test_data){
  require(data.table)
  test_data <- as.data.table(test_data)
  test_data[, .(p_value = .SD[,shapiro.test(x)$p.value]), by = list(y, z)]
}


# 16
# fit <- factanal(swiss, factors = 2, rotation = "varimax") # факторный анализ для нахождения скрытых переменных и выявления структуры данных
# print(fit)
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")


is_multicol <- function(test_data){
  find_col <- as.data.frame(cor(test_data, use = "pair"))
  diag(find_col) <- 0
  if(any(find_col == 1 | find_col == -1))
    {find_col <- apply(find_col == 1| find_col == -1,2, sum)
    names(find_col[which(find_col == 1)])} else
      print("There is no collinearity in the data")
}
