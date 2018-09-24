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



#

url <- "https://cdn.rawgit.com/kjhealy/viz-organdata/master/organdonation.csv"
organs <- read_csv(url)
glimpse(organs)

options(scipen = 999)

gapminder <- gapminder::gapminder
glimpse(gapminder)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp))
p + geom_point()

p + geom_point() + 
  scale_x_continuous(trans = 'log10')

gapminder %>% filter(year == 1952) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) +
  geom_point() +
  scale_x_continuous(trans = 'log10')

gapminder %>% filter(year == 1982) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) +
  geom_point() + 
  scale_x_log10()

gapminder %>% filter(year == 1982) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  scale_x_continuous(trans = 'log10')

count(gapminder, year)

gapminder %>% filter(year == 1982) %>% 
  ggplot(aes(gdpPercap, lifeExp, color = continent)) +
  geom_point() + 
  scale_x_continuous(trans = 'log10', labels = scales::dollar)

gapminder %>%  
  ggplot(aes(gdpPercap, lifeExp, color = continent, fill =  continent)) +
  geom_point() + 
  geom_smooth(method = 'loess') +
  scale_x_continuous(trans = 'log10', labels = scales::dollar)

gapminder %>%  
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(aes(color = continent)) + 
  geom_smooth(method = 'loess') +
  scale_x_continuous(trans = 'log10', labels = scales::dollar)

gapminder %>% filter(year == 1982) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(aes(color = log(pop))) + 
  scale_x_continuous(trans = 'log10', labels = scales::dollar)

ggplot(gapminder, aes(year, lifeExp)) + 
  geom_line(aes(group = country)) + 
  facet_wrap(~ continent, ncol = 1)

ggplot(gapminder, aes(year, gdpPercap)) + 
  geom_line(aes(group = country)) + 
  facet_wrap(~ continent, nrow = 1) +
  theme(axis.text.x = element_text(angle = 60))

ggplot(gapminder, aes(year, lifeExp)) + 
  geom_line(aes(group = country)) + 
  facet_wrap(~ continent, nrow = 1) +
  theme(axis.text.x = element_text(angle = 60))

ggplot(gapminder, aes(year, gdpPercap)) + 
  geom_line(aes(group = country), color = 'grey75') + 
  geom_smooth(method = 'gam', se = FALSE, color = 'darkblue') +
  scale_y_continuous(trans = 'log10', labels = scales::dollar) +
  facet_wrap(~ continent, nrow = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = 'GDP per capita on Five Continents',
       subtitle = '1950-2000',
       caption = 'Source: Gapminder',
       x = 'Year', 
       y = 'GDP per capita')


# stat_ & geom_

ggplot(diamonds, aes(cut)) +
  geom_bar()

ggplot(diamonds, aes(cut)) +
  geom_bar(aes(y = ..prop.., group = 1))

ggplot(diamonds, aes(color, fill = color)) +
  geom_bar() + guides(fill = FALSE) +
  scale_fill_viridis_d()

set.seed(123)
df <- data_frame(year = 1990:2010, 
                 EU = round(runif(21, 68, 75)),
                 US = round(runif(21, 67, 73)))
df <- df %>% mutate(diff = EU - US, 
                    hi_lo = factor(ifelse(diff >= 0, 1, 0))) 

df
summary(df)
table(df$hi_lo)

ggplot(df, aes(x = year, y = diff, fill = hi_lo)) + 
  geom_col() + guides(fill = FALSE) + theme_minimal() +
  scale_x_continuous(breaks = 1990:2010) +
  theme(axis.text.x = element_text(angle = 60))

set.seed(4393)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsmall, aes(x, y))
# If you map an aesthetic to a categorical variable, you will get a
# set of contours for each value of that variable
d + geom_density_2d(aes(colour = cut))

# If we turn contouring off, we can use use geoms like tiles:
d + stat_density_2d(geom = "raster", aes(fill = stat(density)), contour = FALSE)
# Or points:
d + stat_density_2d(geom = "point", aes(size = stat(density)), n = 20, contour = FALSE)


ggplot(diamonds, aes(price, carat)) + geom_density_2d() + 
  scale_x_log10() + scale_y_log10()

ggplot(diamonds, aes(price, carat)) + geom_point() +
  scale_x_log10() + scale_y_log10()
summary(diamonds$carat)

diamonds %>% mutate(carat_round = round(carat)) %>% 
  ggplot(aes(x = price, y = factor(carat_round))) +
  geom_density_ridges(scale = 1) +
  theme_ridges()

diamonds %>% mutate(carat_round = round(carat)) %>% 
ggplot(aes(x = price, y = factor(carat_round))) +
  stat_density_ridges(quantile_lines = TRUE)

diamonds %>% mutate(carat_round = round(carat)) %>% 
ggplot(aes(x = price, y = factor(carat_round), fill = factor(carat_round))) +
  geom_density_ridges(scale = 3, quantile_lines = TRUE) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_fill_brewer(palette = 4) +
  theme_ridges() + theme(legend.position = 'none')

ggplot(diamonds, aes(color, fill = clarity)) + 
  geom_bar()

ggplot(diamonds, aes(color, fill = cut)) + 
  geom_bar(position = 'dodge2')

ggplot(diamonds, aes(color, fill = cut)) + 
  geom_bar(position = 'dodge2') + 
  coord_flip() +
  scale_fill_brewer(palette = 5) +
  facet_wrap(~ clarity, nrow = 1, scales = 'free_x') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60))
  
