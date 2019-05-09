# Load required packages
library(HOT)
library(tidyverse)

# This function takes a TravelSurvey object, a given input dataframe of associated info stratified by locations, and a given location to return
getSummary <- function(ts, info, loc){
  
  # Check that location column of info corresponds to unique locations in ts, sort to match ordering
  info <- info[match(ts@location$location, info$location),]
  
  # Check location as valid input
  if( !(loc %in% info$location) ){ stop("Error with location argument: ", loc) }
  
  # Calculate HOT metrics (filter ts, then calculate)
  location <- ts@location %>% dplyr::filter(location == loc)
  house <- ts@house %>% dplyr::filter(location == loc)
  ts <- new("TravelSurvey", person = ts@person, trip = ts@trip, house = house, location = location)
  
  part.df <- getParticipation(ts)
  prop.df <- getProportion(ts)
  intensity.df <- getIntensity(ts) %>% select(location, mean)
  
  HOT.df <- full_join(part.df, full_join(prop.df, intensity.df, by = "location"), by = "location")
  
  # Merge HOT df with demographics info
  summary.df <- full_join(HOT.df, info, by = "location")
  
  # Return comprehensive dataframe
  return(summary.df)
}