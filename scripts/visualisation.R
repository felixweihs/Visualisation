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
gapminder_lifeExp <- gapminder %>% filter(contains(country == "A"))
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
  geom_point(colour = "black", size = 1.9) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)

#Challenge10
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point(shape = 18) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5)

#Challenge11
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, colour = continent)) +
  geom_point(shape = 18, size = 2) + 
  scale_x_log10() + 
  geom_smooth(method = "lm", size = 1.5) +
  scale_colour_brewer(palette = "Dark2")






