library(data.table)
library(ggplot2)
library(plotly)
# install.packages("plotly")
# https://stepik.org/lesson/Динамическая-визуализация-с-plotly-28857/step/4?unit=undefined
##################################
purchases <- fread("purchases.csv")

price.hist <- ggplot(purchases) + 
  geom_histogram(aes(totalcents), fill="white", color="black") +
  scale_x_log10("Item price", labels = function(x) {
    format(x / 100, scientific = F, big.mark = " ")
  }) +
  ylab("Times purchased")

interactive.price.hist <- ggplotly(price.hist)

interactive.price.hist
##################################
data(mtcars)
plot_ly(data = mtcars, x = mpg, y = disp, text = rownames(mtcars), group = as.factor(cyl), mode="markers+text", type="scatterplot")
##################################
smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), economics, span = 0.2))
plot_ly(economics, x = date, y = uempmed, type = "scatter", showlegend = FALSE, mode="lines+markers") %>%
  add_trace(x = date, y = smoothed.vector)

# OR

smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), economics, span = 0.2))
my.plot <- ggplot(economics) + 
  geom_line(aes(x = date, y = uempmed), color = "red") + 
  geom_line(aes(x = date, y = smoothed.vector), color = "blue")
ggplotly(my.plot)

##################################
# https://plot.ly/r/reference/
purchases <- fread("purchases.csv")
purchases[, log.tc := log(purchases$totalcents)]
purchases[, normal.approx.prob := dnorm(log.tc, mean(log.tc), sd(log.tc))]

ggplotly(ggplot(purchases, aes(log.tc)) + 
           geom_histogram(aes(y=..count.. / max(..count..)), fill="white", color="blue", binwidth = 0.2) + 
           geom_line(aes(log.tc, normal.approx.prob / max(normal.approx.prob)), color="red"))

##################################
p <- plot_ly(economics,
             type = "scatter",        # all "scatter" attributes: https://plot.ly/r/reference/#scatter
             x = ~date,               # more about scatter's "x": /r/reference/#scatter-x
             y = ~uempmed,            # more about scatter's "y": /r/reference/#scatter-y
             name = "unemployment",   # more about scatter's "name": /r/reference/#scatter-name
             marker = list(           # marker is a named list, valid keys: /r/reference/#scatter-marker
               color="#264E86"        # more about marker's "color" attribute: /r/reference/#scatter-marker-color
             )) %>%
  
  add_trace(x = ~date,                                         # scatter's "x": /r/reference/#scatter-x
            y = ~fitted((loess(uempmed ~ as.numeric(date)))),  # scatter's "y": /r/reference/#scatter-y
            mode = 'lines',                                    # scatter's "y": /r/reference/#scatter-mode
            line = list(                                       # line is a named list, valid keys: /r/reference/#scatter-line
              color = "#5E88FC",                               # line's "color": /r/reference/#scatter-line-color
              dash = "dashed"                                  # line's "dash" property: /r/reference/#scatter-line-dash
            )
  ) %>%
  
  layout(                        # all of layout's properties: /r/reference/#layout
    title = "Unemployment", # layout's title: /r/reference/#layout-title
    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
      title = "Time",      # xaxis's title: /r/reference/#layout-xaxis-title
      showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
      title = "uidx")     # yaxis's title: /r/reference/#layout-yaxis-title
  )
##################################
plot_ly(z = volcano, type="surface")
##################################
mesh <- data.table(
  x = rnorm(40),
  y = rnorm(40),
  z = rnorm(40)
)
plot_ly(mesh, type="mesh3d", x = ~x, y = ~y, z = ~z, alphahull = 0)
##################################
# Рисуем чайник
teapot <- fread("teapot.csv")

make.fancy.teapot <- function(teapot.coords) {
  i.s <- seq(from = 0, to = (length(teapot.coords$x) -3), by = 3)
  j.s <- seq(from = 1, to = (length(teapot.coords$x) -2), by = 3)
  k.s <- seq(from = 2, to = length(teapot.coords$x -1), by = 3)
  plot_ly(teapot.coords, x = ~x, y = ~y, z = ~z, i = ~i.s, j = ~j.s, k = ~k.s, type="mesh3d")
}
