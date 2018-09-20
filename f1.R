library(tidyverse)

ggplot(diamonds, aes(cut, carat, fill = cut)) + 
  geom_jitter(alpha = .2) + 
  geom_violin(draw_quantiles = .5, color = 'white') 

diamonds %>% 
  group_by(cut) %>% 
  summarise(n = n(),
            me_price = median(price),
            min_price = min(price),
            max_price = max(price),
            me_carat = median(carat),
            min_carat = min(carat),
            max_carat = max(carat)) %>% 
  knitr::kable()
