#### dplyr library ####
library(dplyr)
library(ggplot2)

# data_frame
my_data <- data_frame(x = rnorm(10000), y = rnorm(10000), f = factor(rep(1:2, 5000)))
my.data <- data.frame(x = rnorm(10000), y = rnorm(10000), f = factor(rep(1:2, 5000)))

diamonds <- as_data_frame(diamonds)
glimpse(diamonds)

my_data_2 <- data_frame(x = rnorm(10), y = abs(x))
my.data.2 <- data.frame(x = rnorm(10), y = abs(x))

# select columns
select(diamonds, 1, 2, 3)
diamonds[c("cut", "price", "color")]
select(diamonds, contains("t"))


# slice rows
slice(diamonds, c(1, 4, 5))
diamonds[c(1, 4, 5)]


# filter observations
filter(diamonds, carat > 0.3 | color == "J")
diamonds[diamonds$carat > 0.3 & diamonds$color == "J", ]
subset(diamonds, carat > 0.3 & color == "J")


# arrange and mutate
arrange(diamonds, desc(price))
diamonds[order(diamonds$price, diamonds$depth), ]

mutate(diamonds, 
       sqrt_price = sqrt(price), 
       log_carat = log(carat))

mutate(mtcars,
       am = factor(am),
       vs = factor(vs))

# pipelines

my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter( mpg > 14, hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename("Miles per gallon" = mpg, "Gross horsepower" = hp)


# mutate_each
d <- as_data_frame(matrix(rnorm(30), ncol = 5))
mutate_each(d, funs(ifelse(. < 0, 0, .)))

# group_by 
diamonds <- as_data_frame(diamonds)
gr_diamonds <-  group_by(diamonds, cut)

sample_n(diamonds, 2)
slice(diamonds, 1)

sample_n(gr_diamonds, 2)
slice(gr_diamonds, 1)


# group_by and summarise
?summarise

summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y))


gr_diamonds <-  group_by(diamonds, cut, color)

summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y))

summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y), 
          great_price = sum(price > 5000))


gr_mtcars <- group_by(mtcars, am, vs)
my_means <- summarise_all(gr_mtcars, funs(mean))



high_price <- as_data_frame(diamonds)
high_price %>%
  group_by(color) %>%
  select(color, price) %>%
  arrange(desc(price)) %>%
  slice(1:10)




# function that transform all variables to factor
all_to_factor <- function(x){
  mutate_each(x, funs(as.factor(.)))
}



# center %>% log of variables
# 1) функция должна выполнить предобработку числовых переменных
# 2) логарифмирование переменных
test_data <- as.data.frame(list(V1 = c(0.4, 1.2, -0.5, -0.3, -0.3),
                                V2 = c(-0.2, 1.6, 0.2, -1.1, 0.5),
                                V3 = c("B", "A", "B", "A", "A"),
                                V4 = c(1.3, 0.8, -1, -1.6, -0.5)))

log_transform <- function(x){
  mutate_each(x,funs(if(is.numeric(.)) log(((.- min(.))/(max(.)- min(.)))+1) else .))
}

log_transform(test_data)



#### Памятка dplyr для незнакомых переменных ####
#summarise_(mtcars, ~shapiro.test(mpg)$p.value)

library(lazyeval)
num_var <- names(which(sapply(mini_mtcars, is.numeric)))
# interp позволяет подставить на место желаемой 
# переменной строку с нужным нам названием колонки в данных. 
mutate_(mini_mtcars, new_var = interp(~(var - mean(var)) / sd(var), var = as.name(num_var)))

# Example
var_for_group <- c("am", "vs")
var_for_filter <- "hp"
var_for_arrange <- "mpg"
var_for_mutate <- "qsec"
var_for_summirise <- "cyl"

group_by_(mtcars, .dots = var_for_group) %>% 
  filter_(interp(~var > 100, var = as.name(var_for_filter))) %>% 
  arrange_(var_for_arrange) %>% 
  mutate_(new_var = interp(~ifelse(var > mean(var), 1, 0), 
                           var = as.name(var_for_mutate))) %>% 
  summarise_(max = interp(~max(var), var = as.name(var_for_summirise)))



# Функция получает на вход dataframe с тремя переменными:
# salary - значение заработной платы
# gender - фактор с двумя градациями (male, female)
# country - фактор с двумя градациями (England, France).

# Функция должна возвращать dataframe с описательными статистиками
# и количеством наблюдений,среднее значение, стандартное отклонение,
# медиана, первый квартиль, третий квартиль, число пропущенных значений.

test_data <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv")
descriptive_stats(test_data)

descriptive_stats <- function(x){
  gr_x<- group_by(x, gender, country)
  summarise(gr_x,
            numbers = n(),
            mean = mean(salary, na.rm = T),
            sd = sd(salary, na.rm = T),
            median = median(salary, na.rm = T),
            first_quartile = quantile(salary, na.rm = T)[2],
            third_quartile  = quantile(salary, na.rm = T)[4],
            na_values = sum(is.na(salary))
  )
}
