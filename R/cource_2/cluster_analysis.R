#### Cluster Analysis ####
# 1
install.packages("cluster")
library("cluster")
KM <- kmeans(iris[1:4], 3, iter.max = 1000, algorithm = "Hartigan-Wong")
clusplot(iris[1:4], KM$cluster, color = TRUE, 
         shade = TRUE, labels=2,
         main = 'Cluster Analysis for Iris')
# or 2
install.packages("factoextra")
library(ggplot2)
library(factoextra)
theme_set(theme_minimal())
d <- iris[, c("Sepal.Length", "Petal.Width")]
fit <- kmeans(d, 3)
fviz_cluster(fit, d)
# 3
library(ggplot2)
d <- iris[, c("Sepal.Length", "Petal.Width")]

fit <- kmeans(d, 3)


# Hierarchical clusterisation
library(ggplot2) 
library(ggrepel) # для симпатичной подписи точек на графике

x <- rnorm(10)
y <- rnorm(10)
test_data <- data.frame(x, y)
test_data$labels <- 1:10

ggplot(test_data, aes(x, y, label = labels))+
  geom_point()+
  geom_text_repel()

d = dist(test_data)
fit <- hclust(d, method = "single")
plot(fit, labels = test_data$labels)
rect.hclust(fit, 2) # укажите желаемое число кластеров, сейчас стоит 2
d$clusters <- factor(fit$cluster)

ggplot(d, aes(Sepal.Length, Petal.Width, col = clusters))+
  geom_point(size = 2)+
  theme_bw()

# Example
library(ape)
set.seed(222)
tr <- rtree(20, tip.label = c("B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U")) 
#левое дерево
plot.phylo(tr) 
#правое дерево 
plot.phylo(tr, use.edge.length=FALSE)


# Analysis of factors
fit <- factanal(swiss, factors = 2, rotation = "varimax")
print(fit)


# Practice
library(dplyr)
##### 1 #####
# функция, которая получает на вход dataframe  с произвольным числом количественных переменных
# и число кластеров, которое необходимо выделить при помощи иерархической кластеризации.
# Функция должна в исходный набор данных добавлять новую переменную фактор - cluster.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
smart_hclust(test_data,2)

smart_hclust <- function(df, clustersN){
 df$cluster <-  as.factor(cutree(hclust(dist(df)), clustersN))
 return(df)
}

##### 2 #####
# Функция должна вернуть названия переменных, по которым были обнаружен значимые различия между выделенными кластерами(p < 0.05).
# Мы добавляем в исходные данные новую группирующую переменную — номер кластера,
#и сравниваем получившиеся группы между собой по количественным переменным при помощи дисперсионного анализа.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
get_difference(test_data, 2)

get_difference <- function(test_data , n_cluster ){
  test_data$cluster <- as.factor(cutree(hclust(dist(test_data)), n_cluster))
  for_loop <- c(1:(length(colnames(test_data))-1))
  vector_of_pV <- sapply(for_loop, function(x)
    {summary(aov(test_data[,x] ~ cluster, test_data))[[1]]$'Pr(>F)'[1]})
 names(vector_of_pV) <- colnames(test_data[,-(length(colnames(test_data)))])
 ifelse(vector_of_pV<0.05, names(vector_of_pV),NA)
}

##### 3 #####
# Функция должна выполнять анализ главных компонент и
# добавлять в исходные данные две новые колонки со значениями первой и второй главной компоненты.
test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")
get_pc(test_data)

get_pc <- function(test_data){
  cbind(test_data,round(prcomp(test_data)$x[,1:2], 1))
}
# Функция должна рассчитать, какое минимальное число главных компонент объясняет больше 90 % изменчивости
#в исходных данных,и добавлять значения этих компонент в исходный dataframe в виде новых переменных.

get_pca2 <- function(df){
 stats <- t(summary(prcomp(test_data))$importance)[,3]
 cbind(test_data,round(prcomp(test_data)$x[,1:( sum(stats < 0.9) + 1)], 1))
}

##### 4 #####
# Функция возвращает имена переменных,
# между которыми есть линейная зависимость или cобщение "There is no collinearity in the data".
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_2.csv")

is_multicol <- function(df){
  find_col <- as.data.frame(cor(df, use = "pair"))
  diag(find_col) <- 0
  find_col <- apply(find_col == 1,2, sum)
  ifelse(find_col == 1,names(find_col),"")
}
