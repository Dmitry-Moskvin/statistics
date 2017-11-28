# basic testing of data
aov_result  <- apply(iris[, 1:4], 2, function(x) aov(x ~ iris$Species))
norm_test   <- apply(iris[, 1:4], 2, function(x) shapiro.test(x))
norm_test_p <- apply(iris[, 1:4], 2, function(x) shapiro.test(x)$p.value)



# other apply-family functions

# by; tapply
tapply(mtcars$mpg, mtcars$am, function(x) mean(x))
aggregate(mpg ~ am, mtcars, function(x) mean(x))
by(iris[1:4], iris$Species, function(x) sapply(x, function(col) shapiro.test(col)$p.value))
aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)

# vapply, 
vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)
mapply(rep, c(1, 2, 3, 4), c(1, 2, 3, 4))

rep(1, 3)
x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)


# function find outliers
iris_num <- iris[, 1:4]

outliers_count <- function(x){
  otliers <- x[abs(x - mean(x)) > 2 * sd(x)]
  if (length(otliers) > 0){
    return(otliers)
  } else {
    return("There are no otliers")
  }
}

iris_outliers <- apply(iris_num, 2, outliers_count)
str(iris_outliers)


# function return negative values
test_data <- as.data.frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9),
                                V2 = c(NA, -10.2, -10.1, -9.3, -12.2),
                                V3 = c(NA, NA, -9.3, -10.9, -9.8)))

get_negative_values <- function(test_data){    
  negative_col <- apply(test_data, 2, function(x) any(x[!is.na(x)] < 0))    
  return(apply(test_data[negative_col], 2, function(x) x[!is.na(x) & x <0]))
}

get_negative_values(test_data)


# function replace na to means
na_rm <- function(x){
  apply(x, 2, function(y) {y[is.na(y)]<- mean(y, na.rm=T); y})
}



#### 1 ####
# Функция пробегает по списку names и проверяет, есть ли в тестовом 
# датафрейме соответствующие значения, если есть, то отбирает их
test_data <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"),
                                expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))
names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
my_names(test_data, names)

#for (i in names) {   print(test_data[which(grepl(i, test_data$name)),]) } 
my_names<- function(test_data, names){
  test_data[sapply(names, function(x) which(grepl(x, test_data$name))),]
}



#### 2 ####
# 1 этап: отобрать предикторы, распределение которых нормально; V1 - зависимая переменная
# 2 этап: вывести вектор из коэффециентов линейной регрессии, если предикторов, 
# распределенных нормально, нет, то выводим сообщение "There are no normal variables in the data"
test_data <- read.csv("https://stepik.org/media/attachments/course/724/test.csv")

smart_lm <- function(x){    
  check_norm <- sapply(x[-1], function(var) shapiro.test(var)$p.value > 0.05)    
  if (any(check_norm)){    
    x = x[, c(1, (which(check_norm) + 1))]    
    coef <- lm(x[[1]] ~ ., x[-1])$coef    
    return(coef)    
  } else{    
    return('There are no normal variables in the data')}}

smart_lm(swiss)
smart_lm(test_data)



#### 3 ####
# t - test, который сравнивает выборочное среднее с предполагаемым средним в генеральной совокупности
# x - Числовое значение среднего в генеральной совокупности
# Функция должна применять одновыборочный t - test к каждой числовой переменной в данных,
# и сравнивать среднее значение этой переменной с указанным значением среднего в генеральной совокупности.
# Функция должна возвращать список, где каждый элемент это вектор, состоящий из t - значения, числа степеней свобод (df) и значения p - value.
one_sample_t <- function(test_data, general_mean){
  test_data <- test_data[sapply(test_data, is.numeric)]
  sapply(test_data,function(x) list(unlist(t.test(x, mu = general_mean)[1:3])))
}

one_sample_t(iris[, 1:4], 4)



#### 4 ####
# Необходимо вытащить только по конкретному элементу из каждого списка списков, в данном случае p-value
normality_tests <- lapply(iris[, 1:4], shapiro.test)

get_p_value <- function(normality_tests_p){
  lapply(normality_tests_p, function(x) return(x$p.value))
}

# Скобка в кавычках - это индексация, а аргумент после неё - индекс
get_element <- function(test_data){    
  sapply(test_data, '[', 2)
}
