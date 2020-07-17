StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        compute_group = function(data, scales) {
                          data
                          
                        },
                        
                        required_aes = c("x", "y")
)

stat_beeswarm <- function(mapping = NULL, data = NULL, geom = "point",
                          position = "identity", na.rm = FALSE, show.legend = NA, 
                          inherit.aes = TRUE, ...) {
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = GeomBeeswarm, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomBeeswarm <- ggproto("GeomBeeswarm", Geom,
                     required_aes = c("x", "y"),
                     non_missing_aes = c("size", "shape", "colour"),
                     default_aes = aes(
                       shape = 19, colour = "black", size = 1.5, fill = NA,
                       alpha = NA, stroke = 0.5
                     ),
                     
                     draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
                       if (is.character(data$shape)) {
                         data$shape <- translate_shape_string(data$shape)
                       }
                       
                       coords <- coord$transform(data, panel_params)
                       
                       normal.points <- pointsGrob(
                         coords$x, coords$y,
                         pch = coords$shape,
                         gp = gpar(
                           col = alpha(coords$colour, coords$alpha),
                           fill = alpha(coords$fill, coords$alpha),
                           # Stroke is added around the outside of the point
                           fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                           lwd = coords$stroke * .stroke / 2
                         )
                       )
                       
                       my.point <- pointsGrob(
                         0.5, 0.5,
                         pch = coords$shape,
                         gp = gpar(
                           col = alpha(coords$colour, coords$alpha),
                           fill = alpha(coords$fill, coords$alpha),
                           # Stroke is added around the outside of the point
                           fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                           lwd = coords$stroke * .stroke / 2
                         )
                       )
                       
                      my.text <- grid::textGrob('o',gp=grid::gpar(cex=1, col = "red"))
                       
                       
                       ## Combine grobs
                       ggplot2:::ggname(
                         "geom_beeswarm",
                         grobTree(normal.points, my.point, my.text)
                       )
                     },
                     
                     draw_key = draw_key_point
)