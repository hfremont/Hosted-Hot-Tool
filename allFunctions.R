# This file contains miscellaneous functions for use in app.R
# These functions may contain outsourced code chunks so as to reduce clutter in the Shiny app code structure



# This function returns a string description of the HOT Shiny app
getAppDescription <- function(){
  description <- "Physical activity is one of the greatest modifiable risk behaviors
                  for many chronic, non-communicable diseases such as cancer and heart disease.
                  Due to the associated health benefits to increased physical activity,
                  policies that promote such behavior will improve public health. This
                  analysis, performed by the Initiative for Health-Oriented Transportation (HOT) team,
                  examines the health benefit gained from increase in active transportation
                  (walking and cycling) as it translates to an increase in physical activity.
                  The health benefit is estimated as the change in the annual mortality rate associated
                  with a change in active transportation. We may apply the HOT model to any location
                  where one finds the requisite data and can wrangle them into the specified form (found on the next page.)"
  
  return(description)
}

getMetricDescription <- function(){
  description <- paste("All options return a \"heat\" map of the selected statistic.",
                       "CRA: Comparative Risk Assessment contrasts current levels of physical activity (participation) with a hypothetical scenarios: full participation and zero particpation.",
                       "Particpation: gives proportion of population that is active at a weekly minimum by borough.",
                       "Frequency: returns the likelyhood that an average person in a given borough is active on any day of the week.",
                       "Intensity: calculates the mean and standard deviation of physical activity among active travelers.",
                       "Duration: returns average trip length (in minutes) by mode (walking or cycling).",
                       "Trips: shows average number trips per day by mode.", sep = "\n")
  
  return(description)
}


# This function returns a HOT map for use in Shiny
getShinyMap <- function(inputSurvey = NULL,
                        inputCustom = NULL,
                        inputMetric = NULL,
                        inputMode = NULL,
                        inputScenario = NULL,
                        inputAuthor = NULL,
                        inputIncome = NULL){
  
  # Load TS based on user selection
  switch(inputSurvey,
         "London" = { ts <- readRDS(file = paste0("./data/inst/LTDS.ts.rds")) },
         "France" = { ts <- readRDS(file = paste0("./data/inst/ENTD.fr.ts.rds")) },
         "USA" = {}, # update later once USA integrated
         "Custom" = { ts <- inputCustom })
  
  # Load GIS
  switch(inputSurvey,
         "London" = { GIS.df <- readRDS(file = paste0("./data/inst/LondonGIS.rds")) },
         "France" = { GIS.df <- readRDS(file = paste0("./data/inst/FranceGIS.rds")) },
         "USA" = {}, # update later once USA integrated
         "Custom" = {}) # update later once custom/GIS developed
  
  # Translate input into proper strings for processing
  if( inputMetric %in% c("Duration", "Trips") ){ mode <- tolower(inputMode) }else{ mode <- NULL} # need lowercase mode if provided
  if( inputMetric != "CRA" ){
    metric <- tolower(inputMetric) # need lowercase string for non-CRA metrics
    scen <- NULL # need scen set to null for non-CRA metrics
  }else{ 
    metric <- inputMetric
    switch(inputScenario, 
           "Zero" = { scen <- "rho.s.1" },
           "Full" = { scen <- "rho.s.2" },
           "Custom" = {})} # update later once custom scenario integrated
  
  # CRA treated separately
  if( metric == "CRA"){
    
    # Load CRA.ts results, if available. If unavailable, generate.
    switch(inputSurvey,
           "London" = { filename <- paste0("./data/inst/craTSLondon.rds") },
           "France" = { filename <- paste0("./data/inst/craTSFrance.rds") },
           "USA" = {}, # update later once USA integrated
           "Custom" = { filename <- inputCustom })
    
    # Add progress bar message "Analyzing..." to CRA load process
    withProgress(message = 'Analyzing...', value = 2, {
      if( file.exists(filename) ){ craTS <- readRDS(file = filename) }else{ craTS <- CRA.ts(ts) ; saveRDS(craTS, file = filename) }
    })
    
    # Filter CRA.ts results by author
    data.df <- craTS %>% dplyr::filter(author == inputAuthor)
    
    # If author is Lear, filter results by income
    if( inputAuthor == "Lear" ){ data.df <- data.df %>% dplyr::filter(income == inputIncome) }
    
    # Join results with GIS
    map.df <- left_join(GIS.df, data.df, by = "location")
    
    # Generate map with progress bar message "Mapping.."
    withProgress(message = 'Mapping...', value = 3, {
      getMap(map.df, metric, mode = mode, scenario = scen, author = inputAuthor, income = inputIncome, interactive = FALSE)
    })
  }
  
  # Everything besides CRA, but written such that this works for CRA in future
  else{ 
    
    # Create temp file directory (if does not already exist) -- reinstalling HOT package will remove the directory without error
    dir.create("./data/inst/temp_files", showWarnings = FALSE)
    
    # Check if local results file exists -- if so, load. If not, compute, load, and store locally.
    filename <- paste0( "./data/inst/temp_files/", metric, mode, inputSurvey, ".rds")
    
    # Add progress bar message "Analyzing..." to metric data load process
    withProgress(message = 'Analyzing...', value = 4, {
      if( file.exists(filename) ){ data.df <- readRDS(file = filename) }
      else{ data.df <- getHOT(ts, metric, mode = mode, scenario = scen, author = inputAuthor, income = inputIncome) ; saveRDS(data.df, file = filename) }
    })
    # Join results with GIS
    map.df <- left_join(GIS.df, data.df, by = "location")
    
    # Generate map with progress bar message "Mapping.."
    withProgress(message = 'Mapping...', value = 5, {
      getMap(map.df, metric, mode = mode, scenario = scen, author = inputAuthor, income = inputIncome, interactive = FALSE)
    })
  }
}


# This function clears all generated files from the HOT Shiny app
cleanup <- function(){
  
  # Clear locally generated files
  filenames <- list.files(paste0("./data/inst/temp_files/"), full.names = TRUE)
  foo <- file.remove(filenames)
  
  # # Retaining CRA-related cleanup code for future development
  # if( file.exists(paste0(getHOTdirectory(), "/craTSLondon.rds")) ){ file.remove(paste0(getHOTdirectory(), "/craTSLondon.rds")) }
  # if( file.exists(paste0(getHOTdirectory(), "/craTSFrance.rds")) ){ file.remove(paste0(getHOTdirectory(), "/craTSFrance.rds")) }
}