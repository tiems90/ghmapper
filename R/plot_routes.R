#' Plot all generated routes
#'
#' This function take as an input a list of dataframes 
#' containing the data of the points along a route, and 
#' produce a map in leaflet that visualises these as lines. 
#' 
#'
#' @param Routes Input List, containing dataframes with two columns: "longitude" and "latitude" and points ordered correctly. 
#' @return A leafleft map that will show the routes as indicated.  
#' @export

plot_routes <- function(Routes) {
  df <- tibble()
  for (i in 1:length(Routes)) {
    df_aux <- data_frame(id = i,
                         long = Routes[[i]]$longitude,
                         lat = Routes[[i]]$latitude)
    df <- rbind(df, df_aux) 
  }
  
  lines <- points_to_line(data = df,
                          long = "long",
                          lat = "lat",
                          id_field = "id")
  proj4string(lines) <- CRS("+init=epsg:4326")
  
  num_lines <- length(lines)
  #Create colour palete
  pal <- colorNumeric("magma", 1:num_lines)
  
  #Plot basemap
  map <- leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron)
  #Plot Routes
  for (i in 1:num_lines) {
    map <- map %>% addPolylines(data = lines[i],
                                weight = 3,
                                color = pal(i),
                                group = paste0("Route ",i)
    )
  }
  map <- map %>% addLayersControl(
    overlayGroups = paste0("Route ",1:num_lines),
    options = layersControlOptions(collapsed = FALSE)
  )
  map
}
