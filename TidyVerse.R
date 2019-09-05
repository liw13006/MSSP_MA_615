library(tidyverse)
mpg
diamonds
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,color = drv))+
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
# ex.2
# the difference between the following two plots is that the second plot only stats that there exist cars which has the specified drv and cyl
# but the 1st plot shows how displ and hwy is distributed among the cars has the same cyl and drv
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy))+
  facet_grid(cyl ~ drv)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl))
# ex.3
# the . means ignore the grid accordingly. if '.' appears before '~' means that there is no row facets grid.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# ex.4
# advantages: faceting gives more information within each group of datas while aesthetic provides more direct information of how different groups of data are distributed relatively to other groups.
# which is better will ultimately depends on what kind of question one tries to answer through the dataset.

# ex.5
# ncol & nrow controls the layout of the facet wrapping. facet_grid's layout is controlled by how many distinct values the faceting variables has.

# ex.6
# maybe because most of the screens are horizontally elongated?

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

ggplot(data = filter(mpg, cyl > 5)) + 
  geom_point(mapping = aes(x = displ, y = hwy))  

filter(data = diamond, carat > 3)
