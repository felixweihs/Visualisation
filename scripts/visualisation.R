library(tidyverse)
library("readxl")
troops <- read_csv("data/troops.csv")
fps <- read_excel("data/fps.xlsx")
fps <- rename(fps, excitation = 2, emission = 3)
gapminder <-  read_csv("data/gapminder.csv")



gapminder_1977 <- gapminder %>% filter(year == "1977")

# 
ggplot(
  data = gapminder_1977, 
  mapping = aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)
) 
+ scale_x_log10() 
+ geom_point() 

#function list for copying
+ facet_wrap( ~continent)
+	scale_colour_brewer()