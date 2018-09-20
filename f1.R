library(tidyverse)

ggplot(diamonds, aes(cut, carat, fill = cut)) + 
  geom_jitter(alpha = .2) + 
  geom_violin(draw_quantiles = .5, color = 'white') 
