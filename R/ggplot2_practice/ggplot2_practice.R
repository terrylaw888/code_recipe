library(dplyr)
library(tibble)
library(ggplot2)

library(modelr)


x <- rnorm(100,10,15)
data <- tibble(x=x, y=x*2+3+rnorm(100,0,30))
data
qplot(x,y, data = data, geom=c("smooth", "point"))


## When want to refer to variables by string, use " " and aes_string()
ggplot(mtcars, aes_string(x = "wt", y = "mpg")) +
  geom_point(size = 2, shape = 23)

ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_line(data = head(mtcars), color = "red")


## stat_qq, stat_ecdf

ggplot(data = mtcars, aes(sample = mpg)) +
  stat_qq()

ggplot(data = mtcars, aes(x = mpg)) +
  stat_ecdf()

ggplot(data = mtcars, aes(x = mpg)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(mpg)), color = "red", linetype = "dashed", size = 1)

### histogram
set.seed(1234)
wdata <- data.frame(sex = factor(rep(c("F", "M"), each=200)), weight = c(rnorm(200, 55), rnorm(200, 58)))

a <- ggplot(wdata, aes(x = weight))

a + geom_histogram(aes(y = ..density.., color = sex), fill = "grey") +  ## default position = "stack"
  geom_vline(aes(xintercept = median(weight)), color = "red", linetype = "dashed", size = 1)

a + geom_histogram(aes(y = ..density.., color = sex), fill = "grey", position = "identity") +
  geom_vline(aes(xintercept = median(weight)), color = "red", linetype = "dashed", size = 1)
