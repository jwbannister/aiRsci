degree_to_cardinal <- function(deg){
    val <- as.integer((deg/22.5) + .5)
    cardinal_index <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", 
                        "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
    cardinal_index[(val %% 16) + 1]
}

mround <- function(x, base){
  base * round(x/base)
}
