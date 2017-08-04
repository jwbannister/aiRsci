# AGOL_functions.R 
# Functions related to getting data from the Formation ArcGIS
# server located at https://maps.formationclient.com/apollo/rest/services

#' Query Formation ArcGIS server.
#'
#' \code{query_AOGL} performs a query on the specified group and layer
#' in the Formation Environmental ArcGIS server through the REST API. The query 
#' structure is such that all the information present in the layer is returned.
#' If the token is expired, it is automatically regenerated. 
#'
#' @param group Text string. The relevant ArcGIS Online group.
#' @param server Text string. The desired server in the group.
#' @param layer Integer. The number of the desired layer in the server. (Layer 
#' numbers begin with 0)
#' @return A data frame object containing the queried data.
#' @examples
#' query_AGOL("SFWCT", "20150528_SFWCT_Wetness_Collection", 0)
query_AGOL <- function(group, server, layer){
    jsn1 <- query_AGOL_json(group, server, layer)
    df_out <- parse_AGOL_json(jsn1)
  df_out
}

query_AGOL_json <- function(group, server, layer){
    tok <- system(paste0("curl --data $AGOL_CREDENTIALS ", 
                  "https://maps.formationclient.com/apollo/tokens/gettoken"), 
                intern=T)
  site <- paste0("https://maps.formationclient.com/apollo/rest/services/", 
                 group, "/", server, "/FeatureServer/", layer, "?f=pjson",
                 "&token=", tok)
  df_out <- jsonlite::fromJSON(site)
  df_out
}

parse_AGOL_json <- function(jsn){
  df1 <- jsonlite::flatten(jsn$features)
  colnames(df1) <- gsub("attributes.", "", colnames(df1))
  colnames(df1) <- gsub("geometry.", "", colnames(df1))
  colnames(df1) <- tolower(colnames(df1))
  colnames(df1) <- gsub("_", ".", colnames(df1))
  df1
}  

convert_ESRI_date <- function(vec){
  vec <- vec / 1000
  vec <- sapply(vec, 
                function (x) as.character(as.POSIXct(x, origin="1970-01-01")))
  vec <- as.POSIXct(vec, format="%Y-%m-%d %H:%M:%S")
  vec
} 
