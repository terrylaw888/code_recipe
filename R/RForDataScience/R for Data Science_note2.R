## Part 2 Data wrangling 
#library(tidyverse)
detach("package:tibble", unload=TRUE)
library(dplyr)
library(tibble)
as_tibble(iris)

tb <- tibble::tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)
tb
?tibble

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)

df$x
df[["x"]]
df %>% .$x
df %>% .[["x"]]
class(mtcars)
class(as.data.frame(as_tibble(mtcars)))


df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

df <- tibble(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

v <- "x"

df[[v]]


## Read in data from file
library(readr)

## parsing

parse_number(
  "123'456'789",
  locale = locale(grouping_mark = "'")
)

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))

guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

# Generate the correct format string to parse each of the following dates and times:
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)") 
d5 <- "12/30/14"
t1 <- "1705"
t2 <- "11:15:10.12 PM"
parse_date(d1, "%B %d, %Y")
parse_date(d2, "%Y-%b-%d")
parse_date(d3, "%d-%b-%Y")
parse_date(d4, "%B %d (%Y)")
parse_date(d5, "%m/%d/%y")
parse_time(t1, "%H%M")

library(hms)
parse_time(t2)

## writing to file
write_excel_csv(df, "/Users/ctlaw/Documents/MyDoc/Doc/Learn/book_code/RForDataScience/df.csv")


## tidyr

library(tidyr)
stocks <- tibble(
  year =c(2015,2015,2015,2015,2016,2016,2016), qtr =c( 1, 2, 3, 4, 2, 3, 4), return=c(1.88,0.59,0.35, NA,0.92,0.17,2.66)
)
stocks %>% complete(year, qtr)

who

who <- who %>% gather(
  new_sp_m014:newrel_f65, key = "key",
  value = "cases",
  na.rm = TRUE
)

who %>% count(key)

library(stringr)

who2 <- who %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3

who4 <- who3 %>% separate(sexage, c("sex", "age"), sep = 1)
who4
who_clean <- who4 %>% select(-iso2,-iso3,-new)
who_clean


### relational datatables

library(nycflights13)
airlines
airports
weather

flights2 <- flights %>% group_by(dest) %>% summarise(avg_delay = mean(arr_delay, na.rm = TRUE))
flights2

library(ggplot2)
install.packages("maps")
library(maps)

airports %>%
  right_join(flights2, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat, color = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
?semi_join

?borders
