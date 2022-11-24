library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
#laod in incarceration data into dataframe
incarceration_data <- read.csv("../incarceration-trends/incarceration_trends.csv")

#view incarceration data frame
View(incarceration_data)

#filter data to only show years after 2013
incarceration_data_2013plus <- incarceration_data %>% 
  filter(year >= 2013)

#new data frame that shows black_jail_pop, white_jail_pop, total_jail_pop
black_vs_white_jail_pop <- incarceration_data_2013plus %>% 
  summarize(year, state, county_name, black_jail_pop, white_jail_pop, total_jail_pop)

#max black jail population in a given year
max_black_pop <- black_vs_white_jail_pop %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>% 
  select(black_jail_pop)

#max white jail population in a given year
max_white_pop <- black_vs_white_jail_pop %>% 
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>% 
  select(white_jail_pop)

#new columns that show black and white jail proportion of total jail population
black_vs_white_jail_pop <- black_vs_white_jail_pop %>% 
  mutate(black_proportion = black_jail_pop / total_jail_pop, white_proportion = white_jail_pop / total_jail_pop)

#filter data frame to only present 2018 data in Bullock County 
black_vs_white_jail_pop_2018_Bullock <- black_vs_white_jail_pop %>% 
  filter(year == 2018, county_name == "Bullock County")
View(black_vs_white_jail_pop_2018_Bullock)

#black jail population proportion of total jail proportion of 2018 in Bullock
black_jail_population_2018_Bullock <- black_vs_white_jail_pop_2018_Bullock$black_proportion


#average white jail population proportion of total jail proportion of 2018
white_jail_population_2018_Bullock <- black_vs_white_jail_pop_2018_Bullock$white_proportion

View(black_vs_white_jail_pop)
#new data frame that shows black_pop, white_pop, and total_pop
black_vs_white_pop <- incarceration_data %>% 
  summarise(year, state, county_name, black_pop_15to64, white_pop_15to64, total_pop_15to64)

#new columns that show black proportion and white proportion of total population
black_vs_white_pop <- black_vs_white_pop %>% 
  mutate(black_proportion = black_pop_15to64 / total_pop_15to64, white_proportion = white_pop_15to64 / total_pop_15to64)
 
black_vs_white_pop <- black_vs_white_pop %>% 
  filter (black_pop_15to64 > white_pop_15to64)
#view new data frame
View(black_vs_white_pop)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here

plot_jail_pop_for_us <- function (df) {
  ggplot(data = get_year_jail_pop(df)) +
    geom_col(mapping = aes(x = year, y = total_jail_pop))
}
View(plot_jail_pop_for_us(incarceration_data))

#----------------------------------------------------------------------------#
# This function aggregates the incarceration data to present the 
#numbers of total jail populations from years 1970-2018
get_year_jail_pop <- function() {
  yearly <- incarceration_data %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(year, total_jail_pop)
  
  yearly <- aggregate(total_jail_pop~., yearly, FUN = sum)
return(yearly)   
}

yearly_pop <- get_year_jail_pop()
View(yearly_pop)
# This function creates a bar graph that represents the total 
#jail popualtion from each year
plot_jail_pop_for_us <- function()  {
  us_plot <- ggplot(yearly_pop, aes(x = year, y = total_jail_pop)) + geom_bar(state = "identity")
  return(us_plot)   
} 
yearly_pop_plot <- plot_jail_pop_for_us()
View(yearly_pop_plot)

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here 
# See Canvas
#----------------------------------------------------------------------------#

#states <- states_total_jail_pop$state

#This function produces a data frame with total jail population per year from 
#the states that are passed to the function in a vector
get_jail_pop_by_states <- function(states) {
  by_states <- incarceration_data %>% 
    select(year, state, total_jail_pop)
    
  choose_state <- by_states %>% 
    filter(state == states)
  return(choose_state)
}
states_jail_pop <- get_jail_pop_by_states()
View(get_jail_pop_by_states(c("CA", "WA")))

#This function...
plot_jail_pop_by_states <- function(states) {
  states_plot <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, color = state))
  + stat_summary(fun = "mean", geom = "line") + xlab("Year")
  + ylab("Total Jail Population") + ggtitle("Increase of Jail Population in United States 
                                            (1970-2018)")
  return(states_plot)
}

plot_jail_pop_by_states("CA", "WA")
## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here 
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


