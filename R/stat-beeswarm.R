StatBeeswarm <- ggproto("StatBeeswarm", Stat,
                        setup_params = function(data, params) {
                          params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = TRUE,
                                                                group_has_equal = TRUE,
                                                                main_is_optional = TRUE)
                          # define n.groups 
                          params$n.groups <- length(unique(data$group))
                          
                          # get range of data and extend a little
                          if (params$flipped_aes) {
                            params$dlim <- grDevices::extendrange(data$x, f = 0.01)
                          } else {
                            # define dlim
                            params$dlim <- grDevices::extendrange(data$y, f = 0.01)
                          }
                          params
                        },
                        
                        extra_params = c("na.rm", "orientation"),
                        
                        compute_group = function(data, scales, flipped_aes = FALSE, n.groups, dlim,
                                                 method = "swarm",
                                                 spacing = 1, breaks = NULL, side = 0L, priority = "ascending",
                                                 corral = "none", corral.width = 0.889) {
                          
                          data <- flip_data(data, flipped_aes)
                          
                          if (method == "swarm") {
                            x.offset <- swarmx(x = rep(0, length(data$y)), y = data$y,
                                               cex = spacing, side = side, priority = priority)$x
                          } else {
                            ## non-swarm methods

                            # define size.multiplier
                            # sizeMultiplier <- par('cex') * cex * spacing
                            size.multiplier <- spacing
                            
                            # define size.d an size.g
                            size.d <- yinch(0.08, warn.log = FALSE) * size.multiplier
                            size.g <- xinch(0.08, warn.log = FALSE) * size.multiplier
                            
                            # hex method specific step
                            if (method == "hex") size.d <- size.d * sqrt(3) / 2
                            
                            ## first determine positions along the y axis
                            if(is.null(breaks))
                              breaks <- seq(dlim[1], dlim[2] + size.d, by = size.d)

                            if(length(breaks) == 1 && is.na(breaks[1])) {
                              d.index <- data$y
                              d.pos <- data$y
                            } else {
                              mids <- (head(breaks, -1) + tail(breaks, -1)) / 2

                              d.index <- sapply(data$y, cut, breaks = breaks, labels = FALSE)

                              d.pos <- sapply(d.index, function(a) mids[a])  
                              data$y <- d.pos
                            }
                            
                            
                            ## now determine positions along the x axis
                            x.index <- determine_pos(d.index, method, side)
                            
                            x.offset <- x.index * size.g
                          }
                          
                          

                          if (corral != "none") {
                            corral.low <- (side - 1) * corral.width / 2
                            corral.high <- (side + 1) * corral.width / 2
                            
                            if (corral == "gutter") {
                              x.offset <- sapply(x.offset, function(zz) pmin(corral.high, pmax(corral.low, zz)))
                            }
                            if (corral == "wrap") {
                              if (side == -1L) {
                                # special case with side=-1: reverse the corral to avoid artefacts at zero
                                x.offset <- sapply(x.offset, function(zz) corral.high - ((corral.high - zz) %% corral.width))
                              } else {
                                x.offset <- sapply(x.offset, function(zz) ((zz - corral.low) %% corral.width) + corral.low)
                              }
                            }
                            if (corral == 'random') {
                              x.offset <- sapply(
                                x.offset, 
                                function(zz) ifelse(
                                  zz > corral.high | zz < corral.low, 
                                  yes = runif(length(zz), corral.low, corral.high), 
                                  no = zz
                                )
                              )
                            }
                            if (corral == 'omit') {
                              x.offset <- sapply(
                                x.offset, 
                                function(zz) ifelse(
                                  zz > corral.high | zz < corral.low, 
                                  yes = NA, 
                                  no = zz
                                )
                              )
                            }
                          }
                          
                          data$x <- data$x + x.offset
                          
                          flip_data(data, flipped_aes)
                        },
                        
                        required_aes = c("x", "y")
)

stat_beeswarm <- function(mapping = NULL, data = NULL,
                          position = "identity", ...,
                          method = "swarm",
                          spacing = 1, breaks = NULL, side = 0L,
                          priority = "ascending",
                          corral = "none",
                          corral.width = 0.889,
                          na.rm = FALSE, orientation = NA, show.legend = NA, 
                          inherit.aes = TRUE) {
  stopifnot(method %in% c("swarm", "centre", "center", "hex", "square"))
  if (method == "center") method <- "centre"
  stopifnot(priority %in% c("ascending", "descending", "density", "random", "none"))
  stopifnot(corral %in% c("none", "gutter", "wrap", "random", "omit"))
  stopifnot(is.numeric(corral.width))
  stopifnot(
    length(corral.width) == 1 & corral.width > 0
    )
  
  layer(
    stat = StatBeeswarm, data = data, mapping = mapping, geom = "point", 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      method = method,
      spacing = spacing,
      breaks = breaks,
      side = side,
      priority = priority,
      corral = corral,
      corral.width = corral.width,
      ...
    )
  )
}

determine_pos <- function(v, method, side) {
  if(length(na.omit(v)) == 0) 
    return(v)

  v.s <- lapply(split(v, v), seq_along)

  if(method %in% c("centre", "square") && side == -1)
    v.s <- lapply(v.s, function(a) a - max(a))
  else if(method %in% c("centre", "square") && side == 1)
    v.s <- lapply(v.s, function(a) a - 1)
  else if(method == "centre")
    v.s <- lapply(v.s, function(a) a - mean(a))
  else if(method == "square")
    v.s <- lapply(v.s, function(a) a - floor(mean(a)))
  else if(method == "hex") {
    odd.row <- (as.numeric(names(v.s)) %% 2) == 1
    if(side == 0) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - floor(mean(a)) - 0.25)
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - ceiling(mean(a)) + 0.25)
    } else if(side == -1) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - max(a))
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - max(a) - 0.5)
    } else if(side ==  1) {
      v.s[ odd.row] <- lapply(v.s[ odd.row], function(a) a - 1)
      v.s[!odd.row] <- lapply(v.s[!odd.row], function(a) a - 0.5)
    }
  }
  unsplit(v.s, v)
} 
