library(leaflet)
library(htmltools)
library(tidyverse)
library(geosphere)
library(shiny)
library(shinydashboard)
library(DT)
library(magrittr)
useful.col <- c("#288dc1", "#799900", "#e89719", "#d0746f",  "#56004e","#94c6e0", "#817777", "#5d87a1", "#429080", "#c2a204", "#b45340", "#4a7628", "#4c5f6c", "#b4cc95", "#dc8633", "#477718", "#e6b17f", "#7a9a01", "#755372", "#477227", "#c19943", "#d89d8a", "#c8cfd3", "#977f96", "#ffffff", "#bccc80")

##--------------
##Useful function for plot
data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}
##--------------
##3. Set default colour:
source("./ggtheme/theme.R")
#bs_poly <- geojsonio::geojson_read(x = "./data/Boston.geojson",what = "sp")

sf <- read_csv("./data/sf.csv")
boston <- read_csv("./data/bs.csv")

#centers <- data.frame(gCentroid(bs_poly, byid = TRUE))
#centers <- dplyr::bind_cols(centers,data.frame(bs_poly$Name))

## Clean data
sf_clean <- sf%>%dplyr::filter(!is.na(longitude),longitude<-110,latitude<37.85,city %in% c("San Francisco"))
sf.split <- split(sf_clean,sf_clean$genre)


bs_clean <- boston%>%dplyr::filter(city %in% c("Boston"))
bs.split <- split(bs_clean,bs_clean$genre)



## get cluster analysis:

## boston
pacman::p_load(mclust)

coor_bs_cn <- bs.split$Chinese%>%dplyr::select(longitude,latitude)%>%as.matrix.data.frame()
boston_gm_cn <- Mclust(data = coor_bs_cn,modelNames = "VVV",G=5)



coor_bs_it <- bs.split$Italian%>%
  dplyr::select(longitude,latitude)%>%
  as.matrix.data.frame()

boston_gm_it <- Mclust(data = coor_bs_it,modelNames = "VVV",G = 11)



bs.split$Chinese$cluster <-  boston_gm_cn$classification
bs.split$Italian$cluster <-  boston_gm_it$classification


## plot Chinese only
pal_cluster <- colorFactor(domain = factor(bs.split$Italian$cluster),palette = c("#288dc1", "#799900", "#e89719", "#d0746f",  "#56004e","#94c6e0", "#817777", "#5d87a1", "#429080", "#c2a204", "#b45340"))


## calculate geo distance within cluster in Boston
## cn 1st

cn_mean_bs <- data.frame(t(boston_gm_cn$parameters$mean))%>%dplyr::mutate(cluster = 1:5)
colnames(cn_mean_bs) <- c("mean_long","mean_lat","cluster")
bs.split$Chinese <- dplyr::left_join(bs.split$Chinese,cn_mean_bs,by="cluster")



bs.split$Chinese%<>%mutate(dist = distHaversine(p1 = cbind(bs.split$Chinese$longitude,bs.split$Chinese$latitude),p2 = cbind(bs.split$Chinese$mean_long,bs.split$Chinese$mean_lat)))

mean_dist_cn_bs <- bs.split$Chinese%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = mean)
colnames(mean_dist_cn_bs) <- c("cluster","mean_dist","mean_rating")
sd_dist_cn_bs <- bs.split$Chinese%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = sd)
colnames(sd_dist_cn_bs) <- c("cluster","sd_dist","sd_rating")
cn_cluster_bs <- dplyr::left_join(cn_mean_bs,dplyr::left_join(mean_dist_cn_bs,sd_dist_cn_bs,"cluster"),"cluster")



## it

it_mean_bs <- data.frame(t(boston_gm_it$parameters$mean))%>%dplyr::mutate(cluster = 1:ncol(boston_gm_it$parameters$mean))
colnames(it_mean_bs) <- c("mean_long","mean_lat","cluster")
bs.split$Italian <- dplyr::left_join(bs.split$Italian,it_mean_bs,by="cluster")

bs.split$Italian%<>%mutate(dist = distHaversine(p1 = cbind(bs.split$Italian$longitude,bs.split$Italian$latitude),p2 = cbind(bs.split$Italian$mean_long,bs.split$Italian$mean_lat)))

mean_dist_it_bs <- bs.split$Italian%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = mean)
colnames(mean_dist_it_bs) <- c("cluster","mean_dist","mean_rating")
sd_dist_it_bs <- bs.split$Italian%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = sd)
colnames(sd_dist_it_bs) <- c("cluster","sd_dist","sd_rating")
it_cluster_bs <- dplyr::left_join(it_mean_bs,dplyr::left_join(mean_dist_it_bs,sd_dist_it_bs,"cluster"),"cluster")




## check dis to cluster centroid vs rating




popup_restaurant_bs <- bs.split$Chinese%>%
  dplyr::select(name,url,review_count,rating,price,cluster)%>%
  dplyr::mutate(pop = paste0("<b><a href='",url,
                              "'>",name,"</a></b><br/>",
                              "Rating: <b>",as.character(rating),
                              "</b>. Price: <b>",as.character(price),
                              "</b>. <br/>", "In Cluster: <b>",as.character(cluster),"</b>."))%>%
  dplyr::select(pop)%>%dplyr::pull()
popup_cluster_cn_bs <- cn_cluster_bs%>%
  dplyr::transmute(pop = paste0("Cluster Number:<b>", 
                                as.character(cluster),
                                "</b><br/>Average Distance to cluster centroid: <b>",
                                as.character(round(mean_dist)),
                                "</b> m<br/>Average rating within Cluster: <b>",
                                as.character(round(mean_rating,2)),"</b>/5.00") )%>%
  dplyr::pull()
popup_cluster_it_bs <- it_cluster_bs%>%
  dplyr::transmute(pop = paste0("Cluster Number:<b>", 
                                as.character(cluster),
                                "</b><br/>Average Distance to cluster centroid: <b>",
                                as.character(round(mean_dist)),
                                "</b> m<br/>Average rating within Cluster: <b>",
                                as.character(round(mean_rating,2)),"</b>/5.00") )%>%
  dplyr::pull()

bs_map_test <- leaflet()%>%
  addProviderTiles(providers$CartoDB.Positron)

names(bs.split) %>%
  purrr::walk( function(df) {
    bs_map_test <<- bs_map_test %>%
      addCircleMarkers(data=bs.split[[df]],
                       lng=~longitude, lat=~latitude,
                       label=~name,
                       color = ~pal_cluster(cluster),
                       radius =4,
                       popup=popup_restaurant_bs,
                       group = df,
                       #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto',
                                                   interactive = TRUE,
                                                   sticky = TRUE))
  })

bs_map_test%<>%
  addCircles(data = cn_cluster_bs,
             lng = ~mean_long,
             lat = ~mean_lat,
             radius = ~log(mean_dist)*70,
             color = ~pal_cluster(cluster),
             group = "Chinese Cluster",
             stroke = TRUE,
             fillOpacity = 0,
             dashArray = "10",
             label = lapply(popup_cluster_cn_bs,HTML),
             labelOptions = labelOptions(noHide = F,
                                         direction = 'auto'))%>%
  addCircles(data = it_cluster_bs,
             lng = ~mean_long,
             lat = ~mean_lat,
             radius = ~log(mean_dist)*70,
             label = lapply(popup_cluster_it_bs,HTML),
             color = ~pal_cluster(cluster),
             group = "Italian Cluster",
             stroke = TRUE,
             dashArray = "10",
             fillOpacity = 0,
             labelOptions = labelOptions(noHide = F,direction = 'auto') )

bs_map_test%<>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 12)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLayersControl(
    overlayGroups = c("Chinese Cluster","Italian Cluster"),
    baseGroups = c("None",names(bs.split)),
    options = layersControlOptions(collapsed = FALSE)
  )%>%hideGroup(c("Chinese Cluster","Italian Cluster"))


#bs_map_test

## get cluster analysis on sf data

coor_sf_cn <- sf.split$Chinese%>%dplyr::select(longitude,latitude)%>%as.matrix.data.frame()
sf_gm_cn <- Mclust(data = coor_sf_cn,modelNames = "VVV")



coor_sf_it <- sf.split$Italian%>%
  dplyr::select(longitude,latitude)%>%
  as.matrix.data.frame()

sf_gm_it <- Mclust(data = coor_sf_it,modelNames = "VVV",G = 13)



sf.split$Chinese$cluster <-  sf_gm_cn$classification
sf.split$Italian$cluster <-  sf_gm_it$classification
## calculate geo distance
## 

cn_mean_sf <- data.frame(t(sf_gm_cn$parameters$mean))%>%dplyr::mutate(cluster = 1:ncol(sf_gm_cn$parameters$mean))
colnames(cn_mean_sf) <- c("mean_long","mean_lat","cluster")
sf.split$Chinese <- dplyr::left_join(sf.split$Chinese,cn_mean_sf,by="cluster")



sf.split$Chinese%<>%mutate(dist = distHaversine(p1 = cbind(sf.split$Chinese$longitude,sf.split$Chinese$latitude),p2 = cbind(sf.split$Chinese$mean_long,sf.split$Chinese$mean_lat)))

mean_dist_cn_sf <- sf.split$Chinese%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = mean)
colnames(mean_dist_cn_sf) <- c("cluster","mean_dist","mean_rating")
sd_dist_cn_sf <- sf.split$Chinese%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = sd)
colnames(sd_dist_cn_sf) <- c("cluster","sd_dist","sd_rating")
cn_cluster_sf <- dplyr::left_join(cn_mean_sf,dplyr::left_join(mean_dist_cn_sf,sd_dist_cn_sf,"cluster"),"cluster")



## it

it_mean_sf <- data.frame(t(sf_gm_it$parameters$mean))%>%dplyr::mutate(cluster = 1:ncol(sf_gm_it$parameters$mean))
colnames(it_mean_sf) <- c("mean_long","mean_lat","cluster")
sf.split$Italian <- dplyr::left_join(sf.split$Italian,it_mean_sf,by="cluster")

sf.split$Italian%<>%mutate(dist = distHaversine(p1 = cbind(sf.split$Italian$longitude,sf.split$Italian$latitude),p2 = cbind(sf.split$Italian$mean_long,sf.split$Italian$mean_lat)))

mean_dist_it_sf <- sf.split$Italian%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = mean)
colnames(mean_dist_it_sf) <- c("cluster","mean_dist","mean_rating")
sd_dist_it_sf <- sf.split$Italian%>%group_by(cluster)%>%summarise_at(vars(dist,rating),.funs = sd)
colnames(sd_dist_it_sf) <- c("cluster","sd_dist","sd_rating")
it_cluster_sf <- dplyr::left_join(it_mean_sf,dplyr::left_join(mean_dist_it_sf,sd_dist_it_sf,"cluster"),"cluster")



## check dis to cluster centroid vs rating

## CN
sf_rating_cn <- ggplot(sf.split$Chinese)+aes(x = cluster, y = rating,color = factor(cluster),fill = factor(cluster))+geom_violin(alpha = .2,show.legend = FALSE)+ggtitle("Distribution of ratings by clusters",subtitle = "San Francisco's Chinese Restaurant")+theme(plot.title = element_text(face = "bold",size = 18),axis.text.x = element_blank(),axis.title.x = element_blank())
sf_dist_cn <- ggplot(sf.split$Chinese)+aes(x = cluster, y = dist,color = factor(cluster),fill = factor(cluster))+geom_violin(alpha = .2,show.legend = FALSE)+ggtitle("Distribution of distance to centroid by clusters",subtitle = "San Francisco's Chinese Restaurant")+theme(plot.title = element_text(face = "bold",size = 18),axis.text.x = element_blank(),axis.title.x = element_blank())
## IT
sf_rating_it <- ggplot(sf.split$Italian)+aes(x = cluster, y = rating,color = factor(cluster),fill = factor(cluster))+geom_violin(alpha = .2,show.legend = FALSE)+ggtitle("Distribution of ratings by clusters",subtitle = "San Francisco's Italian Restaurant")+theme(plot.title = element_text(face = "bold",size = 18),axis.text.x = element_blank(),axis.title.x = element_blank())
sf_dist_it <- ggplot(sf.split$Italian)+aes(x = cluster, y = dist,color = factor(cluster),fill = factor(cluster))+geom_violin(alpha = .2,show.legend = FALSE)+ggtitle("Distribution of distance to centroid by clusters",subtitle = "San Francisco's Italian Restaurant")+theme(plot.title = element_text(face = "bold",size = 18),axis.text.x = element_blank(),axis.title.x = element_blank())

## leaflet
pal_cluster_sf <- colorFactor(domain = factor(sf.split$Italian$cluster),c("#288dc1", "#799900", "#e89719", "#d0746f", "#94c6e0", "#56004e", "#817777", "#5d87a1", "#429080", "#c2a204", "#b45340", "#4a7628", "#4c5f6c"))

popup_restaurant_sf <- sf.split$Chinese%>%
  dplyr::select(name,url,review_count,rating,price,cluster)%>%
  dplyr::mutate(pop = paste0("<b><a href='",url,
                             "'>",name,"</a></b><br/>",
                             "Rating: <b>",as.character(rating),
                             "</b>. Price: <b>",as.character(price),
                             "</b>. <br/>", "In Cluster: <b>",as.character(cluster),"</b>."))%>%
  dplyr::select(pop)%>%dplyr::pull()
popup_cluster_cn_sf <- cn_cluster_sf%>%
  dplyr::transmute(pop = paste0("Cluster Number:<b>", 
                                as.character(cluster),
                                "</b><br/>Average Distance to cluster centroid: <b>",
                                as.character(round(mean_dist)),
                                "</b> m<br/>Average rating within Cluster: <b>",
                                as.character(round(mean_rating,2)),"</b>/5.00") )%>%
  dplyr::pull()
popup_cluster_it_sf <- it_cluster_sf%>%
  dplyr::transmute(pop = paste0("Cluster Number:<b>", 
                                as.character(cluster),
                                "</b><br/>Average Distance to cluster centroid: <b>",
                                as.character(round(mean_dist)),
                                "</b> m<br/>Average rating within Cluster: <b>",
                                as.character(round(mean_rating,2)),"</b>/5.00") )%>%
  dplyr::pull()

sf_map_test <- leaflet()%>%
  addProviderTiles(providers$CartoDB.Positron)

names(sf.split) %>%
  purrr::walk( function(df) {
    sf_map_test <<- sf_map_test %>%
      addCircleMarkers(data=sf.split[[df]],
                       lng=~longitude, lat=~latitude,
                       label=~name,
                       color = ~pal_cluster_sf(cluster),
                       radius =4,
                       popup=popup_restaurant_sf,
                       group = df,
                       #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto',
                                                   interactive = TRUE,
                                                   sticky = TRUE))
  })

sf_map_test%<>%
  addCircles(data = cn_cluster_sf,
             lng = ~mean_long,
             lat = ~mean_lat,
             radius = ~log(mean_dist)*70,
             color = ~pal_cluster_sf(cluster),
             group = "Chinese Cluster",
             stroke = TRUE,
             fillOpacity = 0,
             dashArray = "10",
             label = lapply(popup_cluster_cn_sf,HTML),
             labelOptions = labelOptions(noHide = F,
                                         direction = 'auto'))%>%
  addCircles(data = it_cluster_sf,
             lng = ~mean_long,
             lat = ~mean_lat,
             radius = ~log(mean_dist)*70,
             color = ~pal_cluster_sf(cluster),
             group = "Italian Cluster",
             stroke = TRUE,
             dashArray = "10",
             fillOpacity = 0,
             label = lapply(popup_cluster_it_sf,HTML),
             labelOptions = labelOptions(noHide = F,
                                         direction = 'auto'))

sf_map_test%<>%
  setView(lng = -122.4194, lat = 37.7749, zoom = 12)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLayersControl(
    overlayGroups = c("Chinese Cluster","Italian Cluster"),
    baseGroups = c("None",names(sf.split)),
    options = layersControlOptions(collapsed = FALSE)
  )%>%hideGroup(c("Chinese Cluster","Italian Cluster"))
#sf_map_test




bs.split$Chinese <- dplyr::left_join(bs.split$Chinese,cn_cluster_bs,by = c("cluster","mean_long","mean_lat"))%>%dplyr::mutate(dist_scale = dist/sd_dist)


bs.split$Italian <- dplyr::left_join(bs.split$Italian,it_cluster_bs,by = c("cluster","mean_long","mean_lat"))%>%dplyr::mutate(dist_scale = dist/sd_dist)

bs_scale_dist_vs_rating_cn <- ggplot(bs.split$Chinese)+
  aes(y = dist_scale,x = factor(rating),fill = factor(rating))+
  geom_violin(alpha = .3)+
  stat_summary(fun.data = data_summary)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.8,alpha = .3)+
  labs(x = "Ratings",
       y = "Scaled distances",
       title = "Scaled Distance vs. Yelp Ratings", 
       subtitle = "Boston Chinese Restaurant")+
  theme(legend.position = "",plot.title = element_text(face = "bold",size = 18))

bs_scale_dist_vs_rating_it <- ggplot(bs.split$Italian)+
  aes(y = dist_scale,x = factor(rating),fill = factor(rating))+
  geom_violin(alpha = .3)+
  stat_summary(fun.data = data_summary)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.6,alpha = .3)+
  labs(x = "Ratings",
       y = "Scaled distances",
       title = "Scaled Distance vs. Yelp Ratings", 
       subtitle = "Boston Italian Restaurant")+
  theme(legend.position = "",plot.title = element_text(face = "bold",size = 18))


sf.split$Chinese <- dplyr::left_join(sf.split$Chinese,cn_cluster_sf,by = c("cluster","mean_long","mean_lat"))%>%dplyr::mutate(dist_scale = dist/sd_dist)


sf.split$Italian <- dplyr::left_join(sf.split$Italian,it_cluster_sf,by = c("cluster","mean_long","mean_lat"))%>%dplyr::mutate(dist_scale = dist/sd_dist)

sf_scale_dist_vs_rating_cn <- ggplot(sf.split$Chinese)+
  aes(y = dist_scale,x = factor(rating),fill = factor(rating))+
  geom_violin(alpha = .3)+
  stat_summary(fun.data = data_summary)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5,alpha = .3)+
  labs(x = "Ratings",
       y = "Scaled distances",
       title = "Scaled Distance vs. Yelp Ratings", 
       subtitle = "San Francisco Chinese Restaurant")+
  theme(legend.position = "",plot.title = element_text(face = "bold",size = 18))

sf_scale_dist_vs_rating_it <- ggplot(sf.split$Italian)+
  aes(y = dist_scale,x = factor(rating),fill = factor(rating))+
  geom_violin(alpha = .3)+
  stat_summary(fun.data = data_summary)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5,alpha = .3)+
  labs(x = "Ratings",
       y = "Scaled distances",
       title = "Scaled Distance vs. Yelp Ratings", 
       subtitle = "San Francisco Italian Restaurant")+
  theme(legend.position = "",plot.title = element_text(face = "bold",size = 18))
## Boston
## CN
bs_rating_cn <- ggplot(bs.split$Chinese)+
  aes(x = factor(cluster), 
      y = rating,
      fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of ratings by clusters",
          subtitle = "Boston's Chinese Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")

bs_dist_cn <- ggplot(bs.split$Chinese)+
  aes(x = factor(cluster), 
      y = dist_scale,
      fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of Scaled distance to centroid by clusters",
          subtitle = "Boston's Chinese Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")

## IT
bs_rating_it <- ggplot(bs.split$Italian)+
  aes(x = factor(cluster), 
      y = rating,
      fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of ratings by clusters",
          subtitle = "Boston's Italian Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")

bs_dist_it <- ggplot(bs.split$Italian)+
  aes(x = factor(cluster), y = dist_scale,fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of Scaled distance to centroid by clusters",
          subtitle = "Boston's Italian Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")

## San Francisco
## CN
sf_rating_cn <- ggplot(sf.split$Chinese)+
  aes(x = factor(cluster),
      y = rating,
      fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of ratings by clusters",
          subtitle = "San Francisco's Chinese Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")

sf_dist_cn <- ggplot(sf.split$Chinese)+
  aes(x = factor(cluster),
      y = dist_scale,
      fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of Scaled Distance to Centroid by Clusters",
          subtitle = "San Francisco's Chinese Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")
## IT
sf_rating_it <- ggplot(sf.split$Italian)+
  aes(x = factor(cluster), 
      y = rating,
      fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of ratings by clusters",
          subtitle = "San Francisco's Italian Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")
sf_dist_it <- ggplot(sf.split$Italian)+
  aes(x = factor(cluster), 
      y = dist_scale,
      fill = factor(cluster))+
  geom_violin(alpha = .2)+
  stat_summary(fun.data = data_summary)+
  ggtitle("Distribution of Scaled Distance to Centroid by Clusters",
          subtitle = "San Francisco's Italian Restaurant")+
  theme(plot.title = element_text(face = "bold",size = 18),
        axis.title.x = element_blank(),
        legend.position = "")

## additional maps for raw data
## Set palette for genre
pal_genre = colorFactor(domain = names(bs.split),palette = c("#288dc1", "#799900"))
## get unclustered map for boston
map_bs_unclus <- leaflet()%>%addProviderTiles(providers$CartoDB.Positron)
names(bs.split) %>%
  purrr::walk( function(df) {
    map_bs_unclus <<- map_bs_unclus %>%
      addCircleMarkers(data=bs.split[[df]],
                       lng=~longitude, lat=~latitude,
                       label=~name,
                       color = ~pal_genre(genre),
                       radius =4,
                       popup=popup_restaurant_bs,
                       group = df,
                       #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto',
                                                   interactive = TRUE,
                                                   sticky = TRUE))
  })
map_bs_unclus%<>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 12)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLayersControl(
    overlayGroups = names(bs.split),
    options = layersControlOptions(collapsed = FALSE)
  )%>%hideGroup(names(bs.split))

## Get unclustered map for San Francisco
map_sf_unclus <- leaflet()%>%addProviderTiles(providers$CartoDB.Positron)
names(sf.split) %>%
  purrr::walk( function(df) {
    map_sf_unclus <<- map_sf_unclus %>%
      addCircleMarkers(data=sf.split[[df]],
                       lng=~longitude, lat=~latitude,
                       label=~name,
                       color = ~pal_genre(genre),
                       radius =4,
                       popup=popup_restaurant_sf,
                       group = df,
                       #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto',
                                                   interactive = TRUE,
                                                   sticky = TRUE))
  })
map_sf_unclus%<>%
  setView(lng = -122.4194, lat = 37.7749, zoom = 12)%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addLayersControl(
    overlayGroups = names(bs.split),
    options = layersControlOptions(collapsed = FALSE)
  )%>%hideGroup(names(bs.split))

hist_bs_rating <- ggplot(bs_clean)+
  aes(x = rating,fill = factor(genre))+
  geom_histogram(bins = 30,position = "dodge")+
  labs(x = "Yelp Rating",
       y = "Count",
       title = "Histogram of Yelp Rating",
       subtitle = "City of Boston",
       fill = "Genre")+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold",size = 18))
hist_sf_rating <- ggplot(sf_clean)+
  aes(x = rating,fill = factor(genre))+
  geom_histogram(bins = 30,position = "dodge")+
  labs(x = "Yelp Rating",
       y = "Count",
       title = "Histogram of Yelp Rating",
       subtitle = "City of San Francisco",
       fill = "Genre")+
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold",size = 18))

hist_bs_rat_dist <- ggplot(bs_clean)+
  aes(x = factor(rating),y = distance,fill = factor(genre))+
  geom_violin(alpha = .3)+
  stat_summary(fun.data = data_summary)+
  facet_grid(rows = ~genre)+
  labs(x = "Yelp Rating",
       y = "Distance to City Center",
       title = "Yelp Rating vs Distance to City Center",
       subtitle = "City of Boston",
       fill = "Genre")+
  theme(legend.position = "",
        plot.title = element_text(face = "bold",size = 18))

hist_sf_rat_dist <- ggplot(sf_clean)+
  aes(x = factor(rating),y = distance,fill = factor(genre))+
  geom_violin(alpha = .3)+
  stat_summary(fun.data = data_summary)+
  facet_grid(rows = ~genre)+
  labs(x = "Yelp Rating",
       y = "Distance to City Center",
       title = "Yelp Rating vs Distance to City Center",
       subtitle = "City of San Francisco",
       fill = "Genre")+
  theme(legend.position = "",
        plot.title = element_text(face = "bold",size = 18))
rm(boston,it_mean_bs,it_mean_sf,mean_dist_cn_bs,mean_dist_cn_sf,mean_dist_it_bs,mean_dist_it_sf,sd_dist_cn_bs,sd_dist_cn_sf,sd_dist_it_bs,sd_dist_it_sf,sf)