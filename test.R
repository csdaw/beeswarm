## Generate some random data
set.seed(123)
distro <- list(runif = runif(100, min = -3, max = 3), 
               rnorm = rnorm(100))

## beeswarm
library(beeswarm)
beeswarm::beeswarm(distro, 
                   col = 2:3, pch = 16,
                   method = "swarm", 
                   main = "title")


library(dplyr)
library(ggplot2)

distro2 <- as.data.frame(distro) %>% 
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value")

ggplot2::ggplot(distro2, aes(x = variable, y = value)) + 
  stat_beeswarm()
