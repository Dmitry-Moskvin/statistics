# Функция должна возвращать dataframe c новой переменной  y_full.
# Сохраните в нее переменную y, в которой пропущенные значения
# заполнены предсказанными значениями построенной модели.

test_data <- as.data.frame(list(x_1 = c(6, 13, 5, 12, 8, 12, 9, 5, 14, 10),
                                 x_2 = c(34, 30, 37, 31, 38, 36, 35, 22, 31, 43),
                                 y = c(13, NA, 13, NA, NA, 14, 21, NA, 11, 9)
                                 )
                            )

fill_na <- function(my_df){    
  fit <- lm(y ~ x_1+x_2, my_df)    
  my_df$y_full = ifelse(is.na(my_df$y), predict(fit, my_df), my_df$y)    
  return(my_df)
}

fill_na(test_data)