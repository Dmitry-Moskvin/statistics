ggplot(diamonds, aes(carat))+ 
  geom_ (alpha = .1) + 
  facet_grid(cut ~ color)

mtcars <- mutate(mtcars, 
                 am = factor(am, labels = c("A", "M")), 
                 vs = factor(vs, labels = c("V", "S")))
                 
ggplot(mtcars, aes(hp, mpg))+
  geom_point(aes(col = factor(cyl))) + 
  facet_grid(vs ~ am) + 
  geom_smooth(method = "lm")


ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_wrap( ~ cut, ncol = 1)

ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ .)

ggplot(diamonds, aes(carat, price))+
  geom_smooth() +   
  facet_grid(color ~ .)
  


ggplot(mtcars, aes(mpg)) +
  geom_dotplot()+
  facet_grid( am ~ vs)



ggplot(iris, aes(Sepal.Length)) +
  geom_density() +
  facet_wrap(~ Species, nrow = 1)




ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~ Species, nrow = 1)


ggplot(myMovieData, aes(Type, Budget)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_grid(.~ Year)

