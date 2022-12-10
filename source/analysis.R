library(tidyverse)
library(dplyr)

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
#laod in incarceration data into dataframe
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute
                               /incarceration-trends/master/incarceration_trends.csv")

#view incarceration data frame
View(incarceration_data)

#filter data to only show years after 2013
incarceration_data_2013plus <- incarceration_data %>% 
  filter(year >= 2013)

#new data frame that shows black_jail_pop, white_jail_pop, total_jail_pop
incarceration_data_2013plus <- incarceration_data_2013plus %>% 
  summarize(year, state, county_name, black_jail_pop, white_jail_pop, total_jail_pop)

View(incarceration_data_2013plus)
#max black jail population in a given year
max_black_pop <- incarceration_data_2013plus %>% 
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

get_year_jail_pop <- function() {
  yearly <- incarceration_data %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(year, total_jail_pop)
  
  yearly <- aggregate(total_jail_pop~., yearly, FUN = sum)
return(yearly)   
}


View(get_year_jail_pop())
# This function creates a bar graph that represents the total 
#jail popualtion from each year
plot_jail_pop_for_us <- function()  {
  yearly_jail_pop <- get_year_jail_pop()
  total_pop_plot <- ggplot(yearly_jail_pop, aes(x = year, y = total_jail_pop)) +
    geom_bar(stat = "identity") + xlab("Year") + 
    ylab("Total Jail Population") + 
    ggtitle("Increase of Jail Popualtion in the U.S. (1970-2018)")
  return (total_pop_plot)
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here 
# See Canvas
#----------------------------------------------------------------------------#

#This function produces a data frame with total jail population per year from 
#the states that are passed to the function in a vector
get_jail_pop_by_states <- function(states) {
  by_states <- incarceration_data %>% 
    select(year, state, total_jail_pop)
    
  choose_state <- by_states %>% 
    filter(state %in% states)
  return(choose_state)
}

#This function creates a line graph of the rate of change of jail population in the 
#given states
plot_jail_pop_by_states <- function(states) {
  states_plot <- ggplot(get_jail_pop_by_states(states), aes(x = year, y = total_jail_pop, color = state)) + 
    stat_summary(fun = "mean", geom = "line") + labs(title = "Increase in jail Population in the US (1970-2018)", 
                                                     x = "Year", y = "Total Jail Population")
  return(states_plot)
}

plot_jail_pop_by_states(c("CA", "WA"))
## Section 5  ---- 
#----------------------------------------------------------------------------#
#black avg vs white avg jail population 

female_adult_jail_pop_by_states <- function(states) {
  per_state <- incarceration_data %>% 
    select(year, state, female_adult_jail_pop)
  
  choosen_states <- per_state %>% 
    filter(state %in% states)
  return(choosen_states)
}

plot_female_adult_jail_pop_by_states <- function(states) {
  by_state_plot <- ggplot(female_adult_jail_pop_by_states(states), aes(x = year, y = female_adult_jail_pop, color = state)) + 
    stat_summary(fun = "mean", geom = "line") + labs(title = "Female jail Population in the US (1970-2018)", 
                                                     x = "Year", y = "Female Jail Population")
  return(by_state_plot)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here 
# See Canvas

latin_jail_pop_states <- function(){
  by_state <- incarceration_data %>% 
    filter(year == "2010") %>% 
    group_by(state) %>% 
    summarise(state, latinx_jail_pop)
  by_state <- aggregate(latinx_jail_pop~., by_state, FUN = sum)
  return (by_state)
}

state_info <- unique(latin_jail_pop_states())

state_names <- c("alaska", "alabama", "arkansas", "arizona", "california", "colorado", "district of columbia",
                                "florida", "georgia", "iowa", "idaho", "illinois", "indiana", "kansas", "kentucky", "louisiana",
                                "massachusetts", "maryland", "maine", "michigan", "minnesota", "missouri", "mississippi", "montana",
                                "north carolina", "north dakota", "nebraska", "new hampshire", "new jersey", "new mexico",
                                "nevada", "new york", "ohio", "oklahoma", "oregon", "pennsylvania", "south carolina",
                                "south dakota", "tennessee", "texas", "utah", "virginia", "washington", "wisconsin", "west virginia",
                                "wyoming")


latin_jail_pop_states <- subset(latin_jail_pop_states, select = -c(state))
latin_jail_pop_states$state = state_names
#plot for map
state_form <- map_data("state") %>% 
  rename(state = region) %>% 
  left_join(state_names, by = "state")


map_plot <- ggplot(state_form) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", size = .2) + coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Turquoise") + 
  labs(fill = "Latinx Jail Population", title = "Latinx Jail Popualtion by State",
       subtitle = "2010")

map_plot
#----------------------------------------------------------------------------#

## Load data frame ---- 


