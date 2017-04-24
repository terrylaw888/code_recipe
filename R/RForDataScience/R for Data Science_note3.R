## stringr

library(stringr)
x <- c("a", "b", "c")
y <- str_c("W",x, collapse = ",")
?str_wrap
example("str_wrap")
install.packages("stringr")
x <- c("apple","banana","pear")
str_view(x, "an")
install.packages("htmlwidgets")

# double escape for explicit dot
str_view(c("abc", "a.c", "bef"), "a\\.c")

# start ^, end $

#backreference
str_view(fruit, "(..)\\1", match = TRUE)

#start and end having same char
x <- c("ppap", "fuckf", "fuck", "happy")
str_view(x, "^(.).*\\1$")

seq_along(x)


noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>%
  str_extract(noun)

has_noun %>%
  str_match(noun)


### Working with factors
library(dplyr)
library(tibble)
install.packages("forcats")
sessionInfo()
library(forcats)
library(ggplot2)

gss_cat
gss_cat %>% count(race)

gss_cat %>% ggplot(mapping = aes(x=race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

gss_cat
gss_cat %>% count(relig)


gss_cat %>% ggplot(mapping = aes(x=rincome)) +
  geom_bar() +
  coord_flip()

relig <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE), tvhours = mean(tvhours, na.rm = TRUE), n=n()
  )
ggplot(relig, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat",
                              "Other" = "No answer",
                              "Other" = "Don't know",
                              "Other" = "Other party"
  )) %>% count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>% count(relig, sort = TRUE) %>%
  print(n = Inf)



