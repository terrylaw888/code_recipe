library(dplyr)
library(tibble)
library(ggplot2)

install.packages("hexbin")

diamonds

## A subset of data, log transform for linear relationship

diamonds_2 <- diamonds %>%
  filter(carat < 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))
  

diamonds_2 %>% ggplot(aes(lcarat, lprice)) +
  geom_hex(bins =50)

## implement the intuition as a model => lprice ~ lcarat, remove the obvious confounding

mod_diamond <- lm(lprice ~ lcarat, data = diamonds_2)

grid <- diamonds_2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)
grid

ggplot(diamonds_2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

## Now, we can focus on the residual
diamonds2 <- diamonds_2 %>%
  add_residuals(mod_diamond, "lresid")
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

## A model with multiple predictors, without interactions
mod_diamond2 <- lm(
  lprice ~ lcarat + color + cut + clarity,
  data = diamonds2
)

## data_grid(x_focus, .model = fitted_model) <== gives you the "typical" value for other predictors, very useful for visualizaiton
grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamond2) %>%
  add_predictions(mod_diamond2)
grid

ggplot(grid, aes(cut, pred)) +
  geom_point()



#####
#####
##### Example 2 NYCFlight

library(nycflights13)
library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)
library(modelr)

daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n())

daily <- daily %>% 
  mutate(wday = wday(date, label = TRUE))
daily

ggplot(daily, aes(date, n)) +
  geom_line()

ggplot(daily, aes(wday, n)) +
  geom_boxplot()

mod <- lm(n ~ wday, data = daily)

grid <- daily %>% 
  data_grid(wday) %>% 
  add_predictions(mod, "n")
grid

ggplot(daily, aes(x=wday, y=n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red")

daily <- daily %>% 
  add_residuals(mod)
daily %>% ggplot(aes(x=date, y=resid)) +
  geom_ref_line(h=0) +
  geom_line()
  
daily %>% ggplot(aes(x=date, y=resid, color = wday)) +
  geom_ref_line(h=0) +
  geom_line()

daily %>%
  filter(resid < -100)

daily %>%
  filter(resid > 100)

daily %>%
  ggplot(aes(date, resid)) + geom_ref_line(h = 0) + geom_line(color = "grey50") + geom_smooth(se = FALSE, span = 0.20) # span is a parameter for loess


daily %>%
  filter(wday == "Sat") %>% ## only look at SAT
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month", date_labels = "%b"
  )

## create a function to determine term
term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
  )
}

daily <- daily %>% 
  mutate(term = term(date))
daily

daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(
    NULL,
    date_breaks = "1 month", date_labels = "%b"
  )

daily %>%
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()
## many outliners in FALL


mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)

daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75) +
  geom_smooth(span = 0.2, se = FALSE)


grid <- daily %>%
  data_grid(wday, term) %>%
  add_predictions(mod2, "n")

## A generic technique => overlay the model on the original data points
ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") +
  facet_wrap(~ term)


mod3 <- MASS::rlm(n ~ wday * term, data = daily)
daily %>%
  add_residuals(mod3, "resid") %>%
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") +
  geom_line()

library(splines)
mod4 <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)
daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod4) %>%
  ggplot(aes(date, pred, color = wday)) +
  geom_line() +
  geom_point()


ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color = class)) + 
  geom_smooth(se = FALSE) + theme(legend.position = "bottom") + 
  labs(title = "hwy decreases with displ") +
  guides(
  color = guide_legend(
    nrow = 1,
    override.aes = list(size = 4)
  ) )


ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()


ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(
    values = c(Republican = "red", Democratic = "blue")
  )

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))
ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
