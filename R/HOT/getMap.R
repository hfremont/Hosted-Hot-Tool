# This function takes the output of CRA.ts and an author, then outputs an interactive map of London by CRA
getCRAMap <- function(craTS, scen = "Zero", incomeLevel = "Low", authors = "Arem"){
  
  # Load London GIS
  London.gis.df <- readRDS(file = "~/HOT/inst/LondonGIS.rds")
  
  # Filter CRA output by author parameter, if given (default of Arem)
  craAuthor <- craTS %>% dplyr::filter(author == authors)
  
  # If author is Lear, filter by income
  if( authors == "Lear" ){ craAuthor <- craAuthor %>% dplyr::filter(income == incomeLevel) }
  
  # Join filtered CRA output and GIS dataframe
  craMap <- left_join(London.gis.df, craAuthor, by = "location")
  
  # Configure display by scenario
  if( scen == "Full" ){ choice <- "rho.s.1" ; aesScenario <- aes(fill = rho.s.1) }
  else if( scen == "Zero" ){ choice <- "rho.s.2" ; aesScenario <- aes(fill = rho.s.2) }
  
  # Create ggplot
  craMap <- craMap %>%
    ggplot() +
    aes(x = long, y = lat, group = location) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = paste("Map of Greater London Boroughs: ", scen, " Participation--", authors ), fill = "Population Attributable Fraction") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
    aesScenario # Scenario-based aes
  
  if(authors == "Lear"){ text <- paste("Income Level: ", incomeLevel)
  }else{ text <- "" }
  # Make plot interactive
  return(ggplotly(craMap, tooltip = c("location", choice)) %>% 
           config(displayModeBar = F) %>% # hide toolbar
           layout(annotations = list(x = 0 , y = 0.0, text = text, showarrow = F, xref='paper', yref='paper'))
  )
}



# This function outputs an interactive map of London by participation
getParticipationMap <- function(ts){
  
  # Load London GIS
  London.gis.df <- readRDS(file = "~/HOT/inst/LondonGIS.rds")
  
  # Get participation by borough
  part.df <- getParticipation(ts)
  
  # Join the location and the participation dataframes.
  participationMap <- left_join(London.gis.df, part.df, by = "location")
  
  # Plot participation
  participationMap <- participationMap %>%
    ggplot() +
    aes(x = long, y = lat, group = location, fill = participation) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Map of Greater London Boroughs by Participation",  fill = "Participation") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) + coord_fixed()
  
  # Make plot interactive
  return(ggplotly(participationMap, tooltip = c("location", "participation")) %>% config(displayModeBar = F))
}


# This function outputs an interactive map of London by frequency
getFrequencyMap <- function(ts){
  
  # Load London GIS
  London.gis.df <- readRDS(file = "~/HOT/inst/LondonGIS.rds")
  
  # Get frequency by borough
  freq.df <- getFrequency(ts)
  
  # Join the location and the frequency dataframes.
  frequencyMap <- left_join(London.gis.df, freq.df, by = "location")
  
  # Plot frequency map
  frequencyMap <- frequencyMap %>%
    ggplot() +
    aes(x = long, y = lat, group = location, fill = frequency) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Map of Greater London Boroughs by Frequency",  fill = "Frequency") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) 
  
  # Make plot interactive
  return(ggplotly(frequencyMap, tooltip = c("location", "frequency")) %>% config(displayModeBar = F))
}




# This function outputs an interactive map of London by intensity. If mean intensity is calculatable, this is the metric viewed. Else, standard deviation.
getIntensityMap <- function(ts, mean = TRUE){
  
  # Load London GIS
  London.gis.df <- readRDS(file = "~/HOT/inst/LondonGIS.rds")
  
  # Get intensity by borough
  intense.df <- getIntensity(ts)
  
  # Join the location and the frequency dataframes
  intensityMap <- left_join(London.gis.df, intense.df, by = "location")
  
  # If mean intensity exists, plot the mean and set the cursor parameter. 
  # Else, plot the sd and make the cursor parameter sd.
  if( mean ){ intensity <- aes(fill = mean) ; cursorParam <- "mean" }
  else{ intensity <- aes(fill = sd) ; cursorParam <- "sd" }
  
  # Plot intensity map
  intensityMap <- intensityMap %>%
    ggplot() +
    aes(x = long, y = lat, group = location) +
    geom_polygon() +
    geom_path(color = "white", size = 0.2) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Map of Greater London Boroughs by Intensity",  fill = "Population Attributable Fraction") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) +
    intensity # Heatmap aes specified by mean param
  
  # Make plot interactive
  return(ggplotly(intensityMap, tooltip = c("location", cursorParam)) %>% config(displayModeBar = F))
}

