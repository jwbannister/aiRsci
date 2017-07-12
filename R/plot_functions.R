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
    cls <- angle_classify(deg)
    rad <- geomspoke_translate[[cls]](deg)
    rad
}

