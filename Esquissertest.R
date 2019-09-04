library(esquisse)
data(mtcars)
esquisser(mtcars)
library(ggplot2)

ggplot(data = mtcars) +
  aes(x = hp, y = mpg, size = cyl) +
  geom_point(color = '#0c4c8a') +
  theme_minimal()
View(mtcars)
