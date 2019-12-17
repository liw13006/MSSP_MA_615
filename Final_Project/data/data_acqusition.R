pacman::p_load(yelpr,tidyverse,magrittr)
key <- readLines("Yelp_api_key.txt",warn = FALSE)
##------------------------------------------------------------------------------
## Genre Used:
## * Chinese
## * Korean
## * Japanese
## * Mediterranean
## * Italian
## * Greek
## * Spanish

## Build a function to acquire business automatically
get_business <- function(api_key, location = "boston",term = "Chinese"){
  ## get business end info from given criteria
  business <- business_search(api_key = key,
                              location = location,
                              categories = "Restaurant",
                              term = term,
                              limit = 50)
  ## if total business number is less than 50, then just return the searched
  ## business. If number is greater than 50, then we need to use multiple
  ## query by setting proper offsets

  business_df <- get_df(business = business)
  
  if(business$total>50){ 
    loop_i <- get_offset_i(business)
    for(i in 1:loop_i){
      temp_df <- business_search(api_key = key,
                                location = location,
                                categories = "Restaurant",
                                 term = term,
                                limit = 50,
                                offset = i*50)
      temp_df <- get_df(business = temp_df)
      business_df <- dplyr::bind_rows(business_df,temp_df)}
  }
  return(business_df)
}

## supplimental function, get index for offset loop
get_offset_i <- function(business){
  temp <-  business$total/50
  temp_int <- round(temp)
  rem_add <- if_else(temp-temp_int>0,true = 1,false = 0)
  i <-  temp_int+rem_add-1
  i <- min(i,19)
  return(i)
}

## Data Wrangling Process
get_df <- function(business){
  business_coor <- business$businesses$coordinates
  
  business_loc <- business$businesses$location%>%
    dplyr::select(-display_address)
  
  business_df <- business$businesses%>%
    dplyr::select(-categories,-transactions,-coordinates,-location)
  
  business_df <- bind_cols(business_coor,business_loc,business_df)
  return(business_df)
}

## Use the above implementation to get citywise restaurant data based on genre
get_city_df <- function(genre,location = "boston"){
  for(i in 1:length(genre)){
    if(i==1) df <- get_business(api_key = key,
                                location = location,
                                term = genre[i])%>%
        dplyr::mutate(genre = genre[i])
    
    else df <- dplyr::bind_rows(df,
                                    get_business(api_key = key,location = location,term = genre[i])%>%
                                      dplyr::mutate(genre = genre[i]))
  }
  return(df)
}

##------------------------------------------------------------------------------
##
## Get actual Data:
## 1. specify genre
genre <- c("Chinese",
           "Korean",
           "Japanese",
           "Mediterranean",
           "Italian",
           "Greek",
           "Spanish")

genre <- c("Chinese",
           "Italian")
## get boston and san francisco data

boston <- get_city_df(genre = genre,location = "boston")

sf <- get_city_df(genre = genre,location = "san francisco")

boston_id <- boston$id
sf_if <- sf$id
test <- yelpr::business_search_review(api_key = key,business_id = boston_id[1])

boston %<>% dplyr::filter(!is.na(latitude))
sf%<>% dplyr::filter(!is.na(latitude))

write_csv(boston,"./data/bs.csv")
write_csv(sf,"./data/sf.csv")
