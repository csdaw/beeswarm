StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        setup_params = function(data, params) {
                          params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = TRUE,
                                                                group_has_equal = TRUE,
                                                                main_is_optional = TRUE)
                          params
                        },
                        
                        extra_params = c("na.rm", "orientation"),
                        
                        compute_group = function(data, scales, flipped_aes = FALSE,
                                                 spacing = 1, side = 0L, priority = "ascending") {
                          
                          data <- flip_data(data, flipped_aes)
                          
                          x.offset <- swarmx(x = rep(0, length(data$y)), y = data$y,
                                             cex = spacing, side = side, priority = priority)$x
                          data$x <- data$x + x.offset
                          
                          flip_data(data, flipped_aes)
                        },
                        
                        required_aes = c("x", "y")
)

stat_beeswarm <- function(mapping = NULL, data = NULL,
                          position = "identity", ...,
                          spacing = 1, side = 0L,
                          priority = c("ascending", "descending", "density", "random", "none"),
                          na.rm = FALSE, orientation = NA, show.legend = NA, 
                          inherit.aes = TRUE) {
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      spacing = spacing,
      side = side,
      priority = priority,
      ...
    )
  )
}
