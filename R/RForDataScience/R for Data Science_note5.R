## R Programming
## library(purrr)

library(purrr)

mtcars

output <- vector("double", length = length(mtcars))
for (i in seq_along(mtcars)) {
  output[i] <- mean(mtcars[,i])
}

output
mean(mtcars[[1]])

out2 <- vector("character", length = length(flights))
for (i in seq_along(flights)) {
  out2[i] <- typeof(flights[[i]])
}
out2

out3 <- vector("integer", length = length(iris))
for (i in seq_along(iris)) {
  out3[i] <- length(unique(iris[[i]]))
}

length(unique(iris[,1]))


z<-list(x=1:3,y=4:5) 
map_int(z, length)

## create one-side formula with purrr
## create 3 models, one for a particular level of $cyl
models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))
models

models %>%
  map(summary) #%>%
  #map_dbl(~.$r.squared)

## loop over multiple parameters

params <- tribble( ~mean, ~sd, ~n, 
                   5, 1,1, 
                   10, 5, 3,
                   -3, 10, 5 )
params %>%
  pmap(rnorm)


sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>%
  mutate(sim = invoke_map(f, params, n = 10))


## walk() is the side-effect version of map()

plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- stringr::str_c(names(plots), ".pdf")
pwalk(list(paths, plots), ggsave, path = tempdir())


## common function: remove non-categorical var, or the opposite
iris %>%
  keep(is.factor) %>%
  str()

iris %>% 
  discard(is.factor) %>% 
  str()


