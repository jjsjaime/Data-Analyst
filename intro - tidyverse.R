#------------------------------#
#          TIDY-VERSE          #
#------------------------------#

install.packages("tidyverse")
library(tidyverse)
tidyverse_logo()
tidyverse_update()
install.packages("tidyr")

# datasets que vem junto
library(datasets) # Load the datasets package
library(gapminder) #Load the gapminder package
attach(iris)
str(iris)
table(iris$Species)
iris %>% filter(Species=="virginica")

str(gapminder)
gapminder %>% arrange(desc(gdpPercap))

# combinar multiplos verbos do dplyr
table(gapminder$year)
gapminder %>% filter(year=="2007") %>% arrange(desc(gdpPercap))


gapminder %>% filter(year=="2007") %>% mutate(GDP=gdpPercap*pop) %>%
  arrange(desc(gdpPercap))

# summarize - da uma medida resumo 
iris %>%  filter(Species=="virginica") %>% summarize(medianSL=median(Sepal.Length))

iris %>%
  filter(Species=="virginica") %>%  summarize(medianSL=median(Sepal.Length),
            maxSL=max(Sepal.Length))


# group by
# Find median and max sepal length of eachspecies
iris %>% 
group_by(Species) %>% summarize(medianSL=median(Sepal.Length), 
          maxSL=max(Sepal.Length))

# gdp per capita maximo de cada continente

gapminder %>% group_by(continent) %>% summarize(maxgdp=max(gdpPercap), 
                                  meangdp=mean(gdpPercap))

# ggplot
iris_small <- iris %>%  filter(Sepal.Length > 5)

ggplot(iris_small, aes(x=Petal.Length, y=Petal.Width)) + 
geom_point()

gapminderexp <- gapminder %>%  filter(lifeExp > 75)

ggplot(gapminderexp, aes(x=pop, y=gdpPercap, color = year)) + 
  geom_point() + facet_wrap(~continent)

# grafico de linha
by_year <- gapminder %>%  group_by(year) %>%
  summarize(medianGdpPerCap=median(gdpPercap))

ggplot(by_year, aes(x=year,y=medianGdpPerCap))+
  geom_line() + expand_limits(y=0)

# box plot
ggplot(iris_small, aes(x=Species, y=Sepal.Width)) +
  geom_boxplot()

ggplot(gapminderexp, aes(x=continent, y=pop)) +
  geom_boxplot()

#----------------------#
# solucoes do datacamp #
#----------------------#

# pipe (%>%) significa pegar o que está antes disso,
# e alimenta-lo (preenche-lo) no proximo passo (com o que vem depois)
library(gapminder)
library(dplyr)

# Filter, mutate, and arrange the gapminder dataset
gapminder %>% filter(year == 2007) %>%
  mutate(lifeExpMonths = lifeExp*12) %>%
  arrange(desc(lifeExpMonths))

#--------------
# visualizacao

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()


# colocando cores e tamanho

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) +
  geom_point() +
  scale_x_log10()

# usando o facet para dividir entre os continentes
# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) + geom_point() +
  scale_x_log10() + facet_wrap(~ continent)

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) + 
  geom_point() + scale_x_log10() + facet_wrap(~year)

# PIPE = ctrl + shft + M

# Summarize to find the median life expectancy
gapminder %>% summarize(medianLifeExp = median(lifeExp))

# visualizando dados agrupados
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) + 
  geom_point() + expand_limits(y = 0) 
# expand_limits serve para que o y comece do zero


#life expectancy per continent in 2007
by_continent_2007 <- gapminder %>% filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(medianLifeExp = median(lifeExp), medianGdpPercap = median(gdpPercap))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, 
                              color = continent)) + geom_point() + expand_limits(y = 0)

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) + geom_line() +
  expand_limits(y = 0)

# Filter for observations in the Oceania continent in 1952
europa <- gapminder %>% filter(continent == "Europe", year == 1952)

# Create a bar plot of gdpPercap by country
ggplot(europa, aes(x = country, y = gdpPercap)) + geom_col()

# histograma
gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Create a histogram of population (pop_by_mil)
ggplot(gapminder_1952, aes(x = pop_by_mil)) + geom_histogram(bins = 50)

