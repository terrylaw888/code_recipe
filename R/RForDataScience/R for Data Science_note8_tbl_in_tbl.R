## This memo teaches the concept of list-columns (tibbles with in a tibble)

library(nycflights13)
library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(modelr)

library(gapminder)
??nest
library(tidyr)
library(purrr)

install.packages("broom")
library(broom)

gapminder

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)



nz <- filter(gapminder, country == "New Zealand")
nz %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear trend + ")

nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining pattern")


## CREATE nested dataframe

by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest() ## A function from tidyr

by_country

by_country$data[[1]]
by_country[[3]][[1]]


country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

by_country <- by_country %>% 
  mutate(model = map(data, country_model))

by_country


by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

resids <- unnest(by_country, resids)
resids

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)

resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) +
  facet_wrap(~continent)

glance(nz_mod)


by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) ## unnest default will keep the list-columns for dataframe with only one-single row

glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

## Then we can look at the models that don't fit well

glance %>%
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)


bad_fit <- filter(glance, r.squared < 0.25)
gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()

# 
# Generally there are three parts of an effective list-column pipeline:
#   1. You create the list-column using one of nest(), summarize() + list(), or mutate() + a map function, as described in “Creat‐ ing List-Columns” on page 411.
# 2. You create other intermediate list-columns by transforming existing list columns with map(), map2(), or pmap(). For exam‐ ple, in the previous case study, we created a list-column of mod‐ els by transforming a list-column of data frames.
# 3. You simplify the list-column back down to a data frame or atomic vector, as described in “Simplifying List-Columns” on page 416.

# example
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>%
  group_by(cyl) %>%
  summarize(p = list(probs), q = list(quantile(mpg, probs))) %>%
  unnest()

mtcars %>%
  group_by(cyl) %>%
  summarize_each(funs(list))

## simplify ==> use map_xxx() or unnest()

df <- tribble(
  ~x,
  letters[1:5], 1:3, runif(5)
)

df <- df %>%
  mutate(type = map_chr(x, typeof),
         length = map_int(x, length))
df
