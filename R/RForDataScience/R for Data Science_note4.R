## time and date

library(lubridate)
library(nycflights13)

now()
today()

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(
      year, month, day, sched_dep_time
    ),
    sched_arr_time = make_datetime_100(
      year, month, day, sched_arr_time
    ) ) %>%
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>%
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt %>%
  filter(dep_time < ymd(20130102)) %>% 
  ggplot(aes(dep_time)) +
  geom_freqpoly(binwidth = 1800) # 1800 seconds = 30 mins

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)") 
d5 <- "12/30/14" # Dec 30, 2014

mdy(d1)
ymd(d2)
dmy(d3)
mdy(d4)
mdy(d5)

## An interesting pattern observed

flights_dt %>%
  mutate(minute = minute(dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE),
    n=n())%>% ggplot(aes(minute, avg_delay)) +
  geom_line()


sched_dep <- flights_dt %>%
  mutate(minute = minute(sched_dep_time)) %>%
  group_by(minute) %>%
  summarize(
    avg_delay = mean(arr_delay, na.rm = TRUE), n=n())
ggplot(sched_dep, aes(minute, avg_delay)) +
  geom_line()


ggplot(sched_dep, aes(minute, n)) +
  geom_line()

## plot flights per week: using round down / rounding to week
flights_dt %>%
  count(week = floor_date(dep_time, "week"))

flights_dt %>%
  count(week = floor_date(dep_time, "week")) %>%
  ggplot(aes(week,n)) +
  geom_line()

ymd("2015-02-01") %>% update(mday = 30)


flights_dt %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%   ### All make it into one day to see the distribution of time within one day!!!
  ggplot(aes(dep_hour)) +
  geom_freqpoly(binwidth = 1800) # 30 mins

## duration, period, interval

next_year <- today() + years(1) 
(today() %--% next_year) / ddays(1)
## interval
(today() %--% next_year)
(today() %--% next_year) %/% days(1)

## Q: how to cut the time into interval groups? e.g. 10 mins interval
## floor_date
?floor_date

flights_dt %>%
  count(mth = round_date(dep_time, "month")) %>%
  ggplot(aes(mth, n)) +
  geom_line()
