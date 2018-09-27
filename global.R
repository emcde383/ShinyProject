library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(shinyjs)
library(dplyr)
library(zipcode)
library(readxl)
library(maps)
library(rsconnect)
library(ggplot2)
library(stringr)

#set defaults for ui widgets
d_syear <- 2012
d_fyear <- 2017

agg_data <- read.csv("./data/output/agg_data.csv", header = TRUE)
sample_agg_data <- read.csv("./data/output/sample_agg_data.csv", header = TRUE)
pop_2010 <- read.csv("./data/pop_2010.csv", header = TRUE)
tot_agg_data <- read.csv("./data/output/tot_agg_data.csv", header = TRUE)
state_link <- read.csv("./data/state_abbreviation.csv", header = TRUE)
loc_data <- read.csv("./data/output/loc_data.csv", header = TRUE)
data(zipcode)
zip_to_zctab <- read.csv("./data/zip_to_zcta_2017.csv", header = TRUE)
states <- read.csv("./data/output/states.csv", header = TRUE)
spatial_comp_data <- read.csv("./data/output/spatial_comp_data.csv", header = TRUE)
spatial_prod_data <- read.csv("./data/output/spatial_prod_data.csv", header = TRUE)

#clean state population data for scaling
popdata <- pop_2010 %>% 
  mutate(state_name = as.character(State)) %>% 
  select(state_name, pop_2010, region) %>% 
  left_join(state_link, by = "state_name") %>% 
  filter(is.na(state) == FALSE) %>% 
  mutate(state = as.character(state)) %>% 
  select(state_name, state, pop_2010, region)

#make link for linking charts to ui input
state_to_name <- popdata %>%
  select(state, state_name, region)

#clean zip code data for merges
zdata <- zipcode %>% 
  mutate(zipcode = zip)

#clean location data
ca_data <- states[states$region == "california", ] #fix

colnames(zip_to_zctab) <- tolower(colnames(zip_to_zctab))
zlink <- zip_to_zctab %>% 
  select(zipcode = zip_code, zcta_state = state, zcta) %>% 
  mutate(zipcode = as.character(zipcode))

#compute top compnames
top_comp_geog <- sample_agg_data %>% #f
  group_by(company) %>% 
  summarise(count = n()) %>% 
  top_n(10) %>% 
  select(company) %>% 
  mutate(top_company = 1, company = as.character(company))