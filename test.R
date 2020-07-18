## Generate some random data
set.seed(123)
distro <- list(runif = runif(100, min = -3, max = 3), 
               rnorm = rnorm(100))

## beeswarm
library(beeswarm)
beeswarm::beeswarm(distro, 
                   col = 2:3, pch = 16,
                   method = "center",
                   main = "title")


library(dplyr)
library(ggplot2)

distro2 <- data.frame(
  variable = c(rep("runif", 100), rep("rnorm", 100)),
  value = c(distro$runif, distro$rnorm)
)

distro3 <- as.data.frame(rev(distro)) %>% 
  tidyr::pivot_longer(everything(),values_to = "value", names_to = "variable", )

set.seed(123)
ggplot2::ggplot(distro3, aes(x = rev(variable), y = value)) + 
  stat_beeswarm(method = "centre") + 
  scale_y_continuous(limits = c(-3, 3)) + 
  theme(
    plot.margin = unit(c(0.11, 0.07, 0.11, 0.07), "npc")
  )

ggplot2::ggplot(distro3, aes(y = variable, x = value)) + 
  stat_beeswarm(corral.width = 1) + 
  scale_x_continuous(limits = c(-3, 3)) + 
  theme(
    plot.margin = unit(c(0.12, 0.07, 0.12, 0.07), "npc")
  )
