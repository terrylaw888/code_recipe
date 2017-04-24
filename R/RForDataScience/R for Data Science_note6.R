## Visualization of models

library(modelr)
sim1

library(magrittr)
library(ggplot2)
library(dplyr)
library(tibble)

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)
ggplot(sim1, aes(x, y)) +
  geom_abline(
    aes(intercept = a1, slope = a2),
    data = models, alpha = 1/4 )+
  geom_point()

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)


measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

## helper function
sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models


ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(models, rank(dist) <= 10)
  )


ggplot(models, aes(a1, a2)) +
  geom_point(
    data = filter(models, rank(dist) <= 10),
    size = 4, color = "red" )+
  geom_point(aes(colour = -dist))


grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(
    data = filter(grid, rank(dist) <= 10),
    size = 4, colour = "red" )+
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

## Newton-Raphson
best <- optim(c(0, 0), measure_distance, data = sim1) 
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])


## lm()

sim1_mod <- lm(y ~ x, data = sim1) 
coef(sim1_mod)


sim1a <- tibble(
  x = rep(1:10, each = 3), y=x*1.5+6+rt(length(x),df=2)
)

sim1a_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1a)
}


grid_1a <- expand.grid(
  a1 = seq(-20, 20, length = 50),
  a2 = seq(-3, 3, length = 50)
) %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1a_dist))

ggplot(sim1a, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid_1a, rank(dist) <= 1)
  )


sim1

## modelr::data_grid()

grid_resid <- sim1 %>% 
  data_grid(x)
grid_resid

grid_resid <- grid_resid %>% 
  add_predictions(sim1_mod)
grid_resid

sim1 %>% ggplot(aes(x=x)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y = pred), data = grid_resid, color = "red")

sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()


## use loess() instead

sim2_mod <- loess(y ~ x, data = sim1)
grid_resid <- sim1 %>% 
  data_grid(x)
grid_resid

grid_resid <-  grid_resid %>% 
  add_predictions(sim2_mod)

sim1 %>% ggplot(aes(x=x)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y = pred), data = grid_resid, color = "red")

sim1 %>% ggplot(aes(x=x)) +
  geom_point(aes(y=y)) +
  geom_smooth(aes(y = y), color = "blue")

## multiple predictors and multiple model => gather_
mod1<-lm(y~x1+x2,data=sim3) 
mod2<-lm(y~x1*x2,data=sim3)

grid <- sim3 %>%
  data_grid(x1, x2) %>% ##x1 is discrete
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)
ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

## for continuous variables, data_grid() should be created for only some discrete values using seq_range(x1, n=?)

mod1<-lm(y~x1+x2,data=sim4) 
mod2<-lm(y~x1*x2,data=sim4)
grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)
grid

seq_range(c(0.0123, 0.923423), n = 5)
#> [1] 0.0123 0.2401 0.4679 0.6956 0.9234 
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE) 
#> [1] 0.0 0.2 0.4 0.6 0.8 1.0


## fitting natural spline ns()

library(splines)
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50), y = 4 * sin(x) + rnorm(length(x))
)
ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)
  

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6,  .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)


## Other model class
## library(mgcv)

install.packages("mgcv")
library(mgcv)
?gam

mod6 <- gam(y ~ s(x), data = sim5)


