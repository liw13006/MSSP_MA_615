
library(tmap)

data("World")



tm_shape(World) + tm_polygons("HPI")



tm_shape(World) + tm_polygons("inequality")

tmap_mode("view")


tm_shape(World) + tm_polygons("inequality")

tmap_mode("plot")


data(World, metro, rivers, land)

tmap_mode("view")

tm_shape(metro) + 
  tm_symbols(col="red", size = "pop1950", scale = .5) +
  tm_legend(show=FALSE)

tm_shape(rivers) + tm_lines(col="blue") 



tm_basemap("Stamen.Watercolor") + 
tm_shape(rivers) + tm_lines(col="blue") 


tmap_mode("plot")
## tmap mode set to plotting
tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
  tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA") +
  tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020", scale = .5) +
  tm_legend(show = FALSE)


