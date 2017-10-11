#' Get Owens Lake areas polygons from database
pull_onlake_polygons <- function(){
    query <- paste0("SELECT dca.dust_control_area_id AS objectid, dca.dca_name, ", 
                    "dca.bacm_type, dca.phase, ",
                    "ST_X(ST_TRANSFORM((ST_DUMPPOINTS(dca.geom)).geom, 26911)) AS x, ",
                    "ST_Y(ST_TRANSFORM((ST_DUMPPOINTS(dca.geom)).geom, 26911)) AS y ",
                    "FROM info.dust_control_areas dca;")
    df1 <- query_db("owenslake", query)
}
pull_sfwcrft_polygons <- function(){
    query <- paste0("SELECT sf.gid AS objectid, sf.dca, sf.treatment, sf.phase, ", 
                    "ST_X((ST_DUMPPOINTS(sf.geom)).geom) AS x, ",
                    "ST_Y((ST_DUMPPOINTS(sf.geom)).geom) AS y ",
                    "FROM info.sfwcrft sf ")
    df1 <- query_db("owenslake", query)
}
pull_offlake_polygons <- function(){
    query <- paste0("SELECT lb.lakebed_area_id AS objectid, lb.area_name, ", 
                    "ST_X(ST_TRANSFORM((ST_DUMPPOINTS(lb.geom)).geom, 26911)) ",
                    "AS x, ",
                    "ST_Y(ST_TRANSFORM((ST_DUMPPOINTS(lb.geom)).geom, 26911)) ",
                    "AS y ",
                    "FROM info.lakebed_areas lb ", 
                    "LEFT JOIN info.dust_control_areas dcas ", 
                    "ON lb.area_name=dcas.dca_name ",
                    "WHERE dcas.dca_name IS NULL;")
    df1 <- query_db("owenslake", query)
    df2 <- df1 %>% filter(grepl("Off Lake", area_name) | 
                          area_name=='Keeler Dunes') 
}
pull_all_polygons <- function(){
    query <- paste0("SELECT lb.lakebed_area_id AS objectid, lb.area_name, ", 
                    "ST_X(ST_TRANSFORM((ST_DUMPPOINTS(lb.geom)).geom, 26911)) ",
                    "AS x, ",
                    "ST_Y(ST_TRANSFORM((ST_DUMPPOINTS(lb.geom)).geom, 26911)) ",
                    "AS y ",
                    "FROM info.lakebed_areas lb;") 
    df1 <- query_db("owenslake", query)
}

#' Get Owens Lake DCA labels from database
pull_dca_labels <- function(){
    query <- paste0("SELECT area_name AS label, ",
                    "ST_X(ST_CENTROID(ST_TRANSFORM(geom::geometry, 26911))) AS x, ",
                    "ST_Y(ST_CENTROID(ST_TRANSFORM(geom::geometry, 26911))) AS y ",
                    "FROM info.lakebed_areas;")
    df1 <- query_db("owenslake", query)
}
pull_onlake_labels <- function(){
    query <- paste0("SELECT dca_name AS label, bacm_type, ",
                    "ST_X(ST_CENTROID(ST_TRANSFORM(geom::geometry, 26911))) AS x, ",
                    "ST_Y(ST_CENTROID(ST_TRANSFORM(geom::geometry, 26911))) AS y ",
                    "FROM info.dust_control_areas;")
    df1 <- query_db("owenslake", query)
}
pull_sfwcrft_labels <- function(){
    query <- paste0("SELECT dca, treatment, phase, ",
                    "ST_X(ST_CENTROID(geom::geometry)) AS x, ",
                    "ST_Y(ST_CENTROID(geom::geometry)) AS y ",
                    "FROM info.sfwcrft sf ")
    df1 <- query_db("owenslake", query)
}

#' Get Owens Lake shoreline polygon from database
pull_shoreline_polygon <- function(){
    query <- paste0("SELECT shr.source AS area_name, ", 
                    "ST_X(ST_TRANSFORM((ST_DUMPPOINTS(shr.geom)).geom, 26911)) AS x, ",
                    "ST_Y(ST_TRANSFORM((ST_DUMPPOINTS(shr.geom)).geom, 26911)) AS y ",
                    "FROM info.shoreline shr;")
    df1 <- query_db("owenslake", query)
}

#' Get polygon data from shapefile
#' 
#' @param dsn String. Path to shapefile directory.
#' @param layer String. Name of shapefile.
#' @param proj_string String. CRS projection string in "proj4string" format.
#' @return Data frame with treatment area polygon data.
shape_data <- function(dsn, layer, proj_string){
  dsn <- path.expand(dsn)
  areas <- rgdal::readOGR(dsn=dsn, layer=layer, verbose=FALSE)
  areas <- sp::spTransform(areas, proj_string)
  dat <- areas@data 
  labpnts <- lapply(c(1:length(areas@polygons)), 
                    function(x) areas@polygons[[x]]@labpt)
  polypnts <- lapply(c(1:length(areas@polygons)), 
                     function(x) areas@polygons[x][[1]]@Polygons[[1]]@coords)
  area_data <- cbind(dat, I(labpnts), I(polypnts)) 
  colnames(area_data) <- tolower(colnames(area_data))
  area_data
}

#' Get polygon plot points from shapefile
#'
#' Shapefile must have first attribute be a unique identifier for the area.
#' 
#' @param dsn String. Path to shapefile directory.
#' @param layer String. Name of shapefile.
#' @param proj_string String. CRS projection string in "proj4string" format.
extract_polygons <- function(dsn, layer, proj_string){
    dsn <- path.expand(dsn)
    areas <- rgdal::readOGR(dsn=dsn, layer=layer, verbose=FALSE)
    areas <- sp::spTransform(areas, proj_string)
    polypnts <- data.frame(x=c(), y=c(), dca=c(), polyid=c())
    polyid <- 1
    for (i in 1:length(areas@polygons)){
        dca <- areas@data[[1]][i] 
        for (j in 1:length(areas@polygons[[i]]@Polygons)){
            pnts <- as.data.frame(areas@polygons[[i]]@Polygons[[j]]@coords)
            names(pnts) <- c('x', 'y')
            pnts$dca <- dca
            pnts$polyid <- polyid
            polyid <- polyid + 1
            polypnts <- rbind(polypnts, pnts)
        }
    }
    polypnts
}

#' Build data frame from multiple lists contained in a data frame.
#' 
#' @param df_in Data frame. 
#' @param list_ind Integer. Column index of lists to process.
#' @param id_ind Integer. Column index of object id to be associated with all 
#' elements of corresponding list.
#' @return Data frame.
lists2df <- function(df_in, list_ind, id_ind){
  df_out <- data.frame(x=numeric(), y=numeric(), objectid=integer())
  for (i in 1:nrow(df_in)){
    df1 <- data.frame(matrix(df_in[, list_ind][[i]], ncol=2))
    df1$objectid <- rep(df_in[i, id_ind], nrow(df1))
    colnames(df1)[1:2] <- c("x", "y")
    df_out <- rbind(df_out, df1)
  }
  df_out
}

point_in_dca <- function(vec_in, poly_df){
    for (j in unique(poly_df$objectid)){
      polycheck <- sp::point.in.polygon(vec_in[1], vec_in[2],
                                    dplyr::filter(poly_df, objectid==j)$x, 
                                    dplyr::filter(poly_df, objectid==j)$y)
      if (polycheck==1) return(filter(poly_df, objectid==j)$dca_name[1])
    }
    return("Uncontrolled")
}

