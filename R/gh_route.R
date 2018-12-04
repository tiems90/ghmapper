#' Send Routing request to Graph Hopper
#'
#' This function will send a request for alternative routes, and return a list
#' containing the data of the points along the route. 
#' 
#'
#' @param Start Start location, as a string, formatted as "lat,long"
#' @param End End location, as a string, formatted as "lat,long"
#' @param min_plateau_factor As per Graphhopper documentation
#' @param max_paths As per Graphhopper documentation
#' @param max_share_factor As per Graphhopper documentation
#' @return A list containing dataframes with lat & long for every point along the route. 
#' @export
#' @importFrom magrittr "%>%"
#' 

gh_route <- function(Start, End, min_plateau_factor = 0.2, max_paths = 5, max_share_factor = 0.6) {
  if (getOption("Graphhopper_key") %>% length != 0) {
    key <- getOption("Graphhopper_key")
  } else {
    key <- readline(prompt="Graphhopper key not found. Please type in your API key: ") %>% 
      trimws
    options("Graphhopper_key" = key)
  }
  
  url <- "https://graphhopper.com/api/1/route?locale=en-gb&vehicle=car&weighting=fastest&elevation=false&algorithm=alternative_route&ch.disable=true&use_miles=false&layer=Omniscale&points_encoded=false"
  url <- paste0(url, "&alternative_route.max_paths=", max_paths)
  url <- paste0(url, "&alternative_route.min_plateau_factor=", min_plateau_factor)
  url <- paste0(url, "&alternative_route.max_share_factor=", max_share_factor)
  url <- paste0(url, "&point=", gsub(" ", "", Start), "&point=", gsub(" ", "", End))
  url <- paste0(url, "&key=", key)
  
  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  routes <- extract_paths(content(resp, "text"))
  routes
}

