#### Categorical data ####
df <- read.csv("data/1_cource_stepik/grants.csv")
str(df)


df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))


# 1d Table 
t1 <- table(df$status)
dim(t1)


# 2d Table
t2 <- table(df$status, df$field)
t2 <- table(status = df$status, field = df$field)
dim(t2)

prop.table(t2)

prop.table(t2, 1)
prop.table(t2, 2)


# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
dim(t3)


# plots
barplot(t1)
barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)


# Binomial Test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)


# Chi-Square
chisq.test(t1)
chisq.test(t2)

chi <- chisq.test(t1)
chi$exp
chi$obs


# Fisher's Exact Test
fisher.test(t2)
