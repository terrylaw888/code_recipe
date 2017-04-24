## How to fit a poisson distribution with MLE by MASS

set.seed(101) ## for reproducibility
x.pois<-rpois(100, 20)
hist(x.pois, breaks=100,freq=FALSE)
lines(density(x.pois, bw=0.8), col="red")
library(MASS)
(my.mle<-fitdistr(x.pois, densfun="gamma"))
summary(my.mle)
c(my.mle$estimate - my.mle$sd*2, my.mle$estimate + my.mle$sd*2)

BIC(my.mle)
?BIC
?fitdistr

### R for Data Science Notebook

install.packages("tidyverse")
library(tidyverse)
install.packages(c("nycflights13", "gapminder", "Lahman"))


?dput
mcar <- dput(mtcars)

### Ch2 ggplot2

library(tidyverse)
mpg
ggplot(data=mpg) +
geom_point(mapping = aes(x=displ, y=hwy))

###
# ggplot(data = <DATA>) +
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>,
#     position = <POSITION>
#   )+ <COORDINATE_FUNCTION> + <FACET_FUNCTION>
# 
# ##

str(mtcars)
?mpg

ggplot(data=mpg) +
  geom_point(mapping = aes(x=class, y=drv))

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = class))

str(mpg)
mpg[c(1,2,6,7,10,11)]
mpg[-c(1,2,6,7,10,11)]

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = displ))

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape = displ))
# cannot map continuous var to shape

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size = displ))

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy), shape = 12, stroke = 1)
?geom_point
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 5, stroke = 1)

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color = displ < 5))

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

?facet_grid

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ displ)
## facet on continuous var: show every value as one level => don't do this

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

## mapping to local layer vs mapping to global ggplot2 command
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class)) +
  geom_smooth(
    data = filter(mpg, class == "subcompact"),
    se = FALSE )

## Q: how to plot a boxplot for a single variable?
ggplot(data=mpg, aes(class, displ)) +
  geom_boxplot()
?geom_boxplot

ggplot(data=mpg, aes(displ)) +
  geom_histogram()

ggplot(data=mpg, aes(displ, hwy)) +
  geom_area()
?geom_area

ggplot(
  data = mpg,
  mapping = aes(x = displ, y = hwy, color = drv)
)+
  geom_point(show.legend = TRUE) + geom_smooth(se = FALSE, show.legend = TRUE)

#1
ggplot(data = mpg, mapping = aes(x= displ, y=hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)
#2
ggplot(data = mpg, mapping = aes(x= displ, y=hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(line=drv), se = FALSE)
#3
ggplot(data = mpg, mapping = aes(x= displ, y=hwy, color=drv)) +
  geom_point() +
  geom_smooth(se = FALSE)
#4
ggplot(data = mpg, mapping = aes(x= displ, y=hwy)) +
  geom_point(aes(color=drv)) +
  geom_smooth(se = FALSE)
#5
ggplot(data = mpg, mapping = aes(x= displ, y=hwy)) +
  geom_point(aes(color=drv)) +
  geom_smooth(aes(linetype = drv), se = FALSE)
#6
ggplot(data = mpg, mapping = aes(x= displ, y=hwy)) +
  geom_point(aes(fill = drv), shape = 21, color = "white", stroke = 1)


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

demo <- tribble(
  ~a,      ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)
ggplot(data = demo) +
  geom_bar(
    mapping = aes(x = a, y = b), stat = "identity"
  )

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop.., group = 1)
  )

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = diamonds) +
  stat_count(
    mapping = aes(x = cut))

## For detatils of each stat function, check ggplot2 cheatsheet

ggplot(data = diamonds, mapping = aes(x = cut, y = depth)) +
  geom_pointrange(stat = "summary", fun.ymin = min, fun.ymax = max, fun.y = median)

?geom_pointrange
?stat_summary
?mean_se
?stat_smooth

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = clarity))


## different position adjustments
ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
)+
  geom_bar(alpha = 1/5, position = "identity")
ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
)+
  geom_bar(fill = NA, position = "identity")

ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
)+
  geom_bar(position = "fill")

ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
)+
  geom_bar(position = "dodge")

ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy),
    position = "jitter"
  )

ggplot(data = mpg) +
  geom_jitter(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_count(mapping = aes(x = displ, y = hwy))

## Barchart to Piechart???? how?
ggplot(data = diamonds,
  mapping = aes(x = "", fill = cut)) + 
  geom_bar(width=1) +
  coord_polar(theta = "y")

?labs

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  coord_fixed()
## coord fix: fix the ratio between width and height

#alt + shift + k

### Ch 3 dplyr
library(nycflights13)
?flights
class(flights)
library(tidyverse)
??tibble
(jan1 <- flights %>% filter(month == 1, day == 1))

flights %>% filter(arr_delay >= 2)
flights %>% filter(dest == "HOU" | dest == "IAH")
?between()
NA ^ 0
NA * 0
?one_of()
(x <- 1:10)
lead(x)
lag(x)

not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay)
  )
delays
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarize(
    delay = mean(arr_delay, na.rm = TRUE),
    n=n() )

ggplot(data = delays, mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

delays %>% filter(n > 25) %>% ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha = 1/10)

batting <- as_tibble(Lahman::Batting)
batting
?tbl
?as_tibble
class(batting)
?as.tbl
?tibble
str(batting)

batters <- batting %>%
  group_by(playerID) %>%
  summarize(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE) )

batters %>%
  filter(ab > 100) %>%
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() +
  geom_smooth(se = FALSE)

flights %>% filter(is.na(arr_delay) & !is.na(dep_delay))
flights %>% filter(!is.na(arr_delay) & is.na(dep_delay))


flights %>% group_by(carrier, dest) %>% summarize(count = n(), avg_delay = mean(arr_delay, na.rm = TRUE))
?n

### Find the number of flights for each plane before it first lated for > 60mins i.e. time till failure

initial <- flights %>% 
  select(tailnum, everything()) %>% 
  arrange(tailnum, year, month, day) %>%  
  group_by(tailnum) %>% 
  mutate(first = cummax(dep_delay)) %>% 
  filter(first <= 60) %>%  
  summarise(n())


## Ch 5 EDA
## variation and covariation

diamonds %>% ggplot(mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

diamonds %>% count(cut_width(carat, 0.5))

diamonds %>%  ggplot(mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

diamonds %>%  ggplot(mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)


diamonds %>%  ggplot(mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01) +
  coord_cartesian(xlim = c(2,4), ylim = c(0, 10))

diamonds %>% ggplot() +
  geom_freqpoly(mapping = aes(x = x),color = "blue", binwidth = 0.5) +
  geom_freqpoly(mapping = aes(x = y),color = "red", binwidth = 0.5) +
  geom_freqpoly(mapping = aes(x = z),color = "green", binwidth = 0.5) +
  coord_cartesian(xlim = c(0,10))

diamonds %>%  ggplot(mapping = aes(x=price)) +
  geom_histogram(binwidth = 100)

diamonds %>%  filter(carat == 0.99 | carat == 1) %>% group_by(carat) %>% summarise(n())

diamonds %>%  ggplot(mapping = aes(x = carat)) +
  geom_histogram() +
  ylim(c(0,10))

## standardized freqpoly plot
diamonds %>%  ggplot(mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut))

diamonds %>%  ggplot(mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

## boxplot
diamonds %>%  ggplot(mapping = aes(x = cut, y = price)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median), y=hwy
    ) )+
  coord_flip()

diamonds %>% ggplot(mapping = aes(x=carat, y=price)) +
  geom_point(aes(color = cut), alpha = 1/10) +
  geom_smooth(aes(color = cut), se = FALSE)

diamonds %>%  ggplot(mapping = aes(x = carat, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 0.5)

diamonds %>% ggplot(mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1) +
  facet_grid(~cut)

## categorical vs categorical
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>%
  count(color, cut)

diamonds %>% group_by(color, cut) %>% summarise(n())
ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.3)))

## remove the impact of carat on price, then investigate the impact of cut on price
library(modelr)

?add_residuals

fit <- lm(log(price)~log(carat), data = diamonds)
diamonds_adj <- diamonds %>% add_residuals(fit)
diamonds_adj

## a pattern in the residual
ggplot(data = diamonds_adj) +
  geom_point(mapping = aes(x = carat, y = resid))

diamonds_adj %>% ggplot(aes(x = cut, y = resid)) +
  geom_boxplot()
diamonds %>% ggplot(aes(x = cut, y = price)) +
  geom_boxplot()
