library(nycflights13)
library(tidyverse)
library(styler)
library(tidyverse)

f <- flights

filter(flights, month == 1, day == 1)
colnames(flights)

select_e1 <- select(
  flights,
  starts_with("dep_"), starts_with("arr_")
)

select_e2 <- select(flights, year2 = "year", year1 = "year")

vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select_e3 <- select(flights, one_of(vars))

select_e4 <- select(flights, contains(ignore.case = TRUE, "TIME"))

flights_sml <- select(
  flights,
  year:day,
  ends_with("delay"),
  distance,
  air_time
)
a <- mutate(flights_sml, gain = arr_delay - dep_delay,
            speed = distance / air_time * 60,
            gain_per_hour=gain/speed
            )

a1 <- transmute(flights_sml, gain = arr_delay - dep_delay,
            speed = distance / air_time * 60,
            gain_per_hour=gain/speed
)

a2 <- transmute(flights, flights$dep_time,
                hour=dep_time%/%100,
                minute=dep_time%%100
                )
Mutate_e1 <- mutate(flights,
                    dep_time_mins=dep_time%/%100*60+dep_time%%100,
                    arr_time_mins=arr_time%/%100*60+arr_time%%100,
                    sched_dep_time_mins=sched_dep_time%/%100*60+sched_dep_time%%100
                    )
Mutate_e2 <- mutate(Mutate_e1 , diff_time = air_time- (arr_time_mins-dep_time_mins))

e2 <- mutate(Mutate_e2,air_time, dep_time,dep_time,dep_time_mins,arr_time_mins,diff_time=arr_time_mins-arr_time_mins)
e3 <- select(f, dep_time, sched_dep_time, dep_delay)

Mutate_e4 <-arrange(flights,rank(flights$arr_delay, ties.method = max)) %>% top_n(10)
mutate_e5 <- 1:3+1:10

s1 <- flights %>% summarise(delay=mean(flights$dep_delay, na.rm = TRUE))

by_day <- flights %>% group_by(year, month, day) %>% 
  summarise(delay=mean(dep_delay, na.rm = TRUE))

by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,count=n(), dist=mean(distance, na.rm = TRUE), delay=mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count>20, dest!="HNL")

ggplot(data = delay, mapping = aes(x = dist, y = delay))+
  geom_point(aes(size=count), alpha=1/3) + 
  geom_smooth(se=FALSE)

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

delay <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay=mean(arr_delay, na.rm = TRUE),
    n=n()
  )

delay %>% 
  filter(n>25) %>% 
ggplot(mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth=10)


delay %>% 
  filter(n>25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) +
  geom_point(alpha=1/10)

not_cancelled %>% count(dest)
not_cancelled %>% count(tailnum, wt=distance)
not_cancelled %>% group_by(year, month, day) %>% 
  summarise(n_early=sum(dep_time<500))


Group_e1 <- not_cancelled %>% group_by(flight) %>% 
  summarise(n=n(), 
            fifteen_early=IQR(arr_delay)
            ) 

e1 <- not_cancelled %>% filter(flight==88)
quantile()

Group_e2 <- not_cancelled %>% group_by(flight) %>% summarise(MinDelay=min(arr_delay)) 
Group_e2 <- Group_e2 %>% filter(MinDelay==10)
mpg <- mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = mpg$displ, y = mpg$hwy))

ggplot(data = mpg)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = mpg$drv, y = mpg$class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = mpg$displ, y = mpg$hwy, shape=class))

ggplot(data = mpg) +  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

colnames(mpg)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = mpg$drv, y = mpg$class), color = "blue", stroke=2)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = cty))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, colour = cty, size = cty))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(dvr ~ cyl)

ggplot(data = mpg) +  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) +  geom_point(mapping = aes(x = displ, y = hwy)) +  facet_grid(drv ~ .)

ggplot(data = mpg) +  geom_point(mapping = aes(x = drv, y = cyl)) +  facet_grid(drv ~ cyl)
ggplot(data = mpg) +  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = mpg) +  geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(. ~ cyl)

ggplot(data = mpg) +  geom_point(mapping = aes(x = displ, y = hwy)) +  facet_wrap(.~ class, ncol = 2)

colnames(mpg)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=drv)) +
  geom_smooth(mapping = aes(x = displ, y = hwy,color=drv, linetype=drv))


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x=cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x=cut, y = depth))

ggplot(data = diamonds) +
  geom_col(
    mapping = aes(x=cut, y = depth))

p1 <- ggplot(data = diamonds) +
  geom_smooth(mapping = aes(x=carat, y = price))
p1

ggplot(data = diamonds) +  
  geom_bar(mapping = aes(x = cut, y = ..prop.., group=1))

ggplot(data = diamonds) +  
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop.., group=color))


ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, color=cut))

staced <-  ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill=clarity))


ggplot(data = diamonds, 
      mapping = aes(x = cut, fill = clarity)
      ) +
      geom_bar(position = "dodge")

ggplot(data = diamonds, 
       mapping = aes(x = cut, color = clarity)
) +
  geom_bar(fill=NA, position = "identity")
  
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +  
  geom_count()

ggplot(data = mpg, mapping = aes(x = drv, y = hwy, colour=class)) +  
  geom_boxplot(position = "dodge")

nz <- map_data("nz")

ggplot(nz, mapping = aes(long, lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map()
  #coord_quickmap()coord_map()

staced + 
  coord_polar()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +  
  geom_point() +
  geom_abline() + 
  coord_fixed()


############ Cap3 ##########
tibble()