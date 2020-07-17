StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        
                        compute_group = function(data, scales) {

                          x.offset <- swarmx(x = rep(0, length(data$y)), y = data$y,
                                             cex = 1, side = 0L, priority = "ascending")$x
                          data$x <- data$x + x.offset
                          data
                          
                        },
                        
                        required_aes = c("x", "y")
)

stat_beeswarm <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
