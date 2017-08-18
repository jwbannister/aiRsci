#' Get coordinate ranges for square plot around polygon.
#' 
#' @param polys. Data frame. Points defining polygons of interest.
#' @param external_points Data frame. *x* and *y* coordinates of points external 
#' to polygons to be included in plot range. 
#' @return A list with the x and y ranges for a sqaure plot around the areas of 
#' interest.
get_plot_range <- function(polys, external_points=NULL){
  p.temp <- ggplot(polys, aes(x=x, y=y)) + geom_path() +
    geom_point(data=external_points)
  info <- ggplot_build(p.temp)
  plot_xrange <- info[[2]]$panel_ranges[[1]]$x.range
  plot_yrange <- info[[2]]$panel_ranges[[1]]$y.range
  maxspan <- max(c(plot_xrange[2] - plot_xrange[1], 
                   plot_yrange[2] - plot_yrange[1]))
  midpoint <- c(mean(plot_xrange), mean(plot_yrange))
  xrange <- c(midpoint[1] - (maxspan/2), 
              midpoint[1] + (maxspan/2))
  yrange <- c(midpoint[2] - (maxspan/2), 
              midpoint[2] + (maxspan/2))
  plot.range <- list(x=xrange, y=yrange)
  plot.range
}

wd_2_geomspoke <- function(deg){
    geomspoke_translate <- list(q0=function(x) (x * (pi/180) - pi/2), 
                                q90=function(x) (x *(pi/180) + pi/2),
                                q1=function(x) (-x *(pi/180) - pi/2), 
                                q2=function(x) (-x *(pi/180) + 3*pi/2)) 
    angle_classify <- function(x){
        if (x==0 | x==180) return("q0")
        if (x==90 | x==270) return("q90")
        if (x>0 & x<180) return("q1")
        if (x>180 & x<360) return("q2")
    }
    if (deg==360) deg <- 0
    cls <- angle_classify(deg)
    rad <- geomspoke_translate[[cls]](deg)
    rad
}

photo_background <- function(xmin, xmax, ymin, ymax, zone, src="google"){
    bounds_utm <- sp::SpatialPoints(cbind(c(xmin, xmax), c(ymin, ymax)), 
                                proj4string=sp::CRS(paste0("+proj=utm +zone=", 
                                                           zone)))
    bounds_latlon <- sp::spTransform(bounds_utm, sp::CRS("+proj=longlat"))
    p1 <- ggmap::get_map(location=bounds_latlon@bbox, 
                 maptype=c("satellite"), source=src)
    map_bbox <- attr(p1, 'bb') 
    bounds_ras <- raster::extent(as.numeric(map_bbox[c(2, 4, 1, 3)]))
    ras <- raster::raster(bounds_ras, nrow= nrow(p1), ncol = ncol(p1))
    rgb_cols <- setNames(as.data.frame(t(col2rgb(p1))), c('red','green','blue'))
    red <- ras
    raster::values(red) <- rgb_cols[['red']]
    green <- ras
    raster::values(green) <- rgb_cols[['green']]
    blue <- ras
    raster::values(blue) <- rgb_cols[['blue']]
    stack_latlon <- raster::stack(red,green,blue)
    raster::crs(stack_latlon) <- "+proj=longlat"
    stack_utm <- raster::projectRaster(stack_latlon, crs=paste0("+proj=utm +zone=", 
                                                               zone))
    df1 <- data.frame(raster::rasterToPoints(stack_utm))
    for (i in 3:5){
        df1[ , i][df1[ , i]>255] <- 255
        df1[ , i][df1[ , i]<0] <- 0
    }
    p2 <- ggplot(data=df1) + coord_equal() + theme_bw() +
        geom_tile(aes(x=x, y=y, fill=rgb(layer.1,layer.2,layer.3, 
                                         maxColorValue = 255)), alpha=0.75) + 
        scale_fill_identity() + 
        scale_x_continuous(breaks=range(df1$x)*c(1.01, 0.99), 
                           labels=range(df1$x), expand = c(0,0)) +
        scale_y_continuous(breaks=range(df1$y)*c(0.99, 1.01), 
                           labels=range(df1$y), expand = c(0,0)) +
        theme(panel.grid=element_blank(), 
              axis.title=element_blank(), 
              axis.text=element_blank(), 
              axis.ticks=element_blank(), 
              plot.title=element_text(hjust=0.5), 
              panel.background=element_rect(fill='darkgrey'))
    p2
}


