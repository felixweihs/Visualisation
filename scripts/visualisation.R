library(tidyverse)
library("readxl")
troops <- read_csv("data/troops.csv")
fps <- read_excel("data/fps.xlsx")
fps <- rename(fps, excitation = 2, emission = 3)
gapminder <-  read_csv("data/gapminder.csv")

gapminder_three <- gapminder %>% filter(year == 1952 | year == 1977 | year == 2007)
gapminder_SouthAfrica <- gapminder %>% filter(year == 1977)

# 
figure <- ggplot(
  data = gapminder_three, 
  mapping = aes(x = gdpPercap, y = lifeExp, size = pop, colour = year)
) + scale_x_log10() + geom_point() + scale_colour_continuous(type = "viridis")

figure + annotate("text", x = 20000, y = 40, label = "No data here")

#function list for copying
# + facet_wrap( ~continent)
# +	scale_colour_brewer()
# + scale_x_log10() 
# + annotate("text", x = 20000, y = 40, label = "No data here")
# + geom_point()
# + geom_line()
# + scale_colour_manual(values =c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00"))

#Challenge4
challenge4 <- ggplot(
  data = gapminder_1977, 
  mapping = aes(x = country, y = pop, size = lifeExp, colour = gdpPercap)
) + geom_point()

#Challenge5
challenge5 <- ggplot(data = gapminder_1977) +
  geom_point(mapping = aes(x = pop, y = lifeExp, size = lifeExp, colour = continent))

#Challenge6
ggplot(
  data = gapminder_1977, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop)
) +
  geom_point(shape = "triangle", alpha = 0.8) +
  scale_x_log10()

#Challenge7
gapminder_lifeExp <- gapminder %>% filter(contains(country == "A"))
ggplot(
  data = gapminder_lifeExp, 
  mapping = aes(x = year, y = lifeExp, group = country, colour = continent)
) + geom_line()

#Challenge8
gapminder_lifeExp <- gapminder
ggplot(
  data = gapminder_lifeExp, 
  mapping = aes(x = year, y = lifeExp, group = country, colour = continent)
) + geom_line() + geom_point(colour = "black")

ggplot(
  data = gapminder_lifeExp, 
  mapping = aes(x = year, y = lifeExp, group = country)
) + geom_line(aes(colour= continent)) + geom_point()

#Challenge9 pre
ggplot(data = gapminder, 
       mapping = aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.3, aes(colour = continent), size = 2) + 
  scale_x_log10() +
  geom_smooth(method = "lm", size = 0.1) +
  scale_colour_manual(values = c("red", "green", "purple", "blue", "black"))

#Challenge9
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(colour = "purple", size = 1.9) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)

#Challenge10
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent, shape = continent)) +
  geom_point() + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)

#Challenge11
ggplot(
  data = gapminder, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point(shape = 18, size = 2) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5) +
  scale_colour_brewer(palette = "Dark2")

#Challenge12 pre
gapminder_A <- gapminder %>% filter(str_starts(country, "A"))
ggplot(
  data = gapminder_A, 
  mapping = aes(x = year, y = lifeExp, group = country, colour = country)
) + geom_line() + geom_point(colour = "black") + facet_wrap(~ country) + geom_smooth(method = "lm")


gapminder_rich <-  filter(gapminder, gdpPercap > 40000)
gapminder_poor <-  filter(gapminder, gdpPercap < 300)
ggplot(
  data = gapminder, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop, label = country)
) + geom_point() + 
  scale_x_log10() +
  geom_text(data = gapminder_rich, size = 3, colour = "black") +
  facet_wrap(~ year)

#Challenge12
ggplot(
  data = gapminder, 
  mapping = aes(x = pop,fill = continent)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  facet_wrap(~year)

#Output figure and clean-up labels and themes
rough_plot <- ggplot(
  data = gapminder, 
  mapping = aes(x = gdpPercap, y = lifeExp, colour = continent, size = pop, label = country)
) + geom_point() + 
  scale_x_log10() +
  geom_text(data = gapminder_rich, size = 3, colour = "black") +
  geom_text(data = gapminder_poor, size = 3, colour = "black") +
  facet_wrap(~ year)

rough_plot + scale_colour_brewer(palette = "Set1")

rough_plot +
  labs(title = "Figure 1", 
       x = "GDP/Capita", 
       y = "Life Expectancy",
       colour = "Continent",
       caption = "Data source: Gapminder") +
  theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 20),
  axis.title = element_text(size = 20),
  axis.line = element_line(colour = "black", size = 1)
)

rough_plot + theme_linedraw()

#Save plots
ggsave("results/my_first_plot2.jpg", plot = rough_plot, width = 12, height = 12, units = "cm")


#Multipanel figures with cowplot
install.packages("cowplot")
library(cowplot)

plot1 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point()
plot2 <- ggplot(gapminder, aes(x = continent, y = lifeExp)) + geom_boxplot()
plot3 <- ggplot(gapminder, aes(x = gdpPercap, y = pop)) + geom_point()
plot4 <- ggplot(gapminder, aes(x = lifeExp, y = pop)) + geom_point()

plot_grid(plot1, plot2, plot3, plot4)
