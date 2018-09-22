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


# Density ridgeline plots
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

library(ggridges)

ggplot(diamonds, aes(price, color)) +
  geom_density_ridges()

ggplot(diamonds, aes(price, clarity)) +
  geom_density_ridges()

ggplot(diamonds, aes(price, clarity)) +
  geom_density_ridges() +
  theme_ridges()

ggplot(diamonds, aes(price, clarity, fill = clarity)) +
  geom_density_ridges(scale = 3) +
  scale_fill_brewer(palette = 4) +
  theme_ridges() + theme(legend.position = 'none')

ggplot(diamonds, aes(price, clarity, fill = clarity)) +
  geom_density_ridges(scale = 3) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = 4) +
  theme_ridges() + theme(legend.position = 'none')

ggplot(diamonds, aes(x = price, y = cut, fill = cut, height = ..density..)) +
  geom_density_ridges(scale = 4, stat = "density") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = 4) +
  theme_ridges() + theme(legend.position = "none")

# Varying fill colors along the x axis

ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C")

ggplot(diamonds, aes(x = price, y = cut, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = .01, alpha = .2) +
  scale_fill_viridis_c() +
  theme_ridges() + theme(legend.position = "none")
  
# Quantile lines and coloring by quantiles or probabilities

ggplot(diamonds, aes(price, cut)) +
  stat_density_ridges(quantile_lines = TRUE)

ggplot(diamonds, aes(price, cut)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2)

ggplot(diamonds, aes(price, cut, fill = factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles")


ggplot(iris, aes(x=Sepal.Length, y=Species, fill=factor(..quantile..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles")

ggplot(iris, aes(x=Sepal.Length, y=Species, fill=0.5 - abs(0.5-..ecdf..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)

ggplot(diamonds, aes(x=price, y=cut, fill=0.5 - abs(0.5-..ecdf..))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1)

# Jittering points

ggplot(sample_n(diamonds, 200), aes(x=price, y=cut)) +
  geom_density_ridges(jittered_points = TRUE)

# simulate a rug

ggplot(sample_n(diamonds, 500), aes(x=price, y=cut)) +
  geom_density_ridges(jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, alpha = 0.7)


# Cyclical scales

ggplot(diamonds, aes(x = price, y = cut, fill = cut)) + 
  geom_density_ridges(scale = 4) + 
  scale_fill_cyclical(values = c("blue", "green"))
