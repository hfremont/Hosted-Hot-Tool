# This file details functions to generate example TravelSurvey objects and corresponding GIS files for use with the HOT R Package


# Load required packages
library("HOT")
library("rgdal")
library("rgeos")
library("maptools")
library("foreign")


# This function loads GIS files for London as well as the LTDS Travel Survey object, cleaning the GIS files to match and storing a local version of the final product
getLondon <- function() {
  
  # If local rds file for LTDS TS object does not already exist, generate the file
  if( !file.exists("~/HOT/inst/LTDS.ts.rds") ){ newLTDS() }
  
  # Load TS from local rds file
  LTDS <- readRDS(file = "~/HOT/inst/LTDS.ts.rds")
  
  # Load London GIS, make sure in line with LTDS location slot
  London.gis <- readOGR(dsn = "~/CUSSH/data/london/London-GIS/", layer = "London_Borough_Excluding_MHW", verbose = FALSE)
  London.gis@data$id <- rownames(London.gis@data)
  London.gis.points <- fortify(London.gis, region = "id")
  London.gis.df <- plyr::join(London.gis.points, London.gis@data, by = "id")
  levels(London.gis.df$NAME) <- ifelse(levels(London.gis.df$NAME) == "Hammersmith and Fulham", "Hammersmith & Fulham", levels(London.gis.df$NAME))
  
  London.gis.df <- London.gis.df %>%
    mutate( location = factor(NAME, levels = levels(LTDS@location$location)) ) %>%
    select(long, lat, order, hole, piece, id, group, location, GSS_CODE, HECTARES, NONLD_AREA, ONS_INNER)
  
  # Store local version to HOT/inst directory
  saveRDS(London.gis.df, file = "~/HOT/inst/LondonGIS.rds")
}


# This function loads GIS files for France as well as the ENTD Travel Survey object, cleaning the GIS files to match and storing a local version of the final product
getFrance <- function(){
  
  # If local rds file for ENTD TS object does not already exist, generate the file
  if( !file.exists("~/HOT/inst/ENTD.fr.ts.rds") ){ newENTD() }
  
  # Load TS from local rds file
  ENTD <- readRDS("~/HOT/inst/ENTD.fr.ts.rds")
  
  # Load FR GIS file
  fmap <- raster::getData("GADM", path = "~/HOT/inst", country = "FRA", level = 2)
  
  # Clear local file that was downloaded
  if( file.exists("~/HOT/inst/gadm36_FRA_2_sp.rds") ){ file.remove("~/HOT/inst/gadm36_FRA_2_sp.rds") }
  
  # Edit GIS object to align with ts object ENTD's location slot
  fmap <- subset(fmap, NAME_2 != "Alpes-de-Haute-Provence") # This départment is not in ENTD (not sure why..)
  fmap$NAME_2 <- factor(fmap$NAME_2, levels = sort(unique(fmap$NAME_2)))
  
  # Check that 95 départments align
  if( !identical(sort(unique(fmap$NAME_2)), sort(ENTD@location$location)) ){ stop("GIS and ENTD départments do not align.") }
  
  # Convert spatial polygon object to dataframe for merging with HOT metrics and plotting with ggplot
  fmap@data$id <- rownames(fmap@data)
  fmap.points <- fortify(fmap, region = "id")
  fmap.gis.df <- plyr::join(fmap.points, fmap@data, by = "id")
  
  # Create location column and remove unused columns
  fmap.gis.df <- fmap.gis.df %>%
    mutate( location = factor(NAME_2, levels = levels(ENTD@location$location)) ) %>%
    select(long, lat, order, hole, piece, id, group, location)
  
  # Store local version to HOT/inst directory
  saveRDS(fmap.gis.df, file = "~/HOT/inst/FranceGIS.rds")
}


# This function generates a local rds file for full LTDS TravelSurvey object
newLTDS <- function(){
  
  # Code pulled and modified from ~/CUSSH/R/LTDS.Rmd
  
  LTDS0515 <- readRDS(file = "~/CUSSH/R/data/LTDS0515.rds")
  LTDS1516 <- readRDS(file = "~/CUSSH/R/data/LTDS1516.rds")
  LTDS1617 <- readRDS(file = "~/CUSSH/R/data/LTDS1617.rds")
  LTDS.part <- read.csv("~/Box Sync/HOT/data/London-Participation-All.csv")
  
  
  lapply(LTDS0515,nrow) %>% unlist() %>% sort() %>% tail(10)
  lapply(LTDS1516,nrow) %>% unlist() %>% sort() %>% tail(10)
  lapply(LTDS1617,nrow) %>% unlist() %>% sort() %>% tail(10)
  
  
  HOUSE0515 <- LTDS0515$Household
  HOUSE1516 <- LTDS1516$Household
  HOUSE1617 <- LTDS1617$Household
  
  
  foo <- read.csv(file = "~/CUSSH/data/london/LTDS/LTDS Travel Data/hhaboro.csv")
  hhaboro.vec <- foo$hhaboro
  names(hhaboro.vec) <- foo$code
  
  
  HOUSE0515 <- within(HOUSE0515,{
    houseID <- as.character(hhid)
    location <- as.factor(hhaboro.vec[hhaboro])
    year <- as.character(hyearid)
  })
  
  HOUSE1516 <- within(HOUSE1516,{
    houseID <- as.character(hhid)
    location <- as.factor(hhaboro.vec[hhaboro])
    year <- as.character(hyearid)
  })
  
  HOUSE1617 <- within(HOUSE1617,{
    houseID <- as.character(hhid)
    location <- as.factor(hhaboro.vec[hhaboro])
    year <- as.character(hyearid)
  })
  
  HOUSE0515 <- HOUSE0515 %>% select(houseID, location, year)
  HOUSE1516 <- HOUSE1516 %>% select(houseID, location, year)
  HOUSE1617 <- HOUSE1617 %>% select(houseID, location, year)
  
  
  PERSON0515 <- LTDS0515$Person
  PERSON1516 <- LTDS1516$Person
  PERSON1617 <- LTDS1617$Person
  
  PERSON0515 <- within(PERSON0515,{
    subjectID <- as.character(ppid)
    houseID <- as.character(phid)
    sex <- factor(ifelse(psex %in% c(-2,-1), NA, psex), levels = c(1,2), labels = c("M","F"))
    age.numeric <- as.numeric(ifelse(page %in% c(-2,-1), NA, page))
    age <- factor(ifelse(!is.na(age.numeric), ifelse(age.numeric <= 18, "child", ifelse(age.numeric <= 65, "adult", "senior")),NA), levels = c("child","adult","senior"))
  })
  
  PERSON1516 <- within(PERSON1516,{
    subjectID <- as.character(ppid)
    houseID <- as.character(phid)
    sex <- factor(ifelse(psex %in% c(-2,-1), NA, psex), levels = c(1,2), labels = c("M","F"))
    age.numeric <- as.numeric(ifelse(page %in% c(-2,-1), NA, page))
    age <- factor(ifelse(!is.na(age.numeric), ifelse(age.numeric <= 18, "child", ifelse(age.numeric <= 65, "adult", "senior")),NA), levels = c("child","adult","senior"))
  })
  
  PERSON1617 <- within(PERSON1617,{
    subjectID <- as.character(ppid)
    houseID <- as.character(phid)
    sex <- factor(ifelse(psex %in% c(-2,-1), NA, psex), levels = c(1,2), labels = c("M","F"))
    age.numeric <- as.numeric(ifelse(page %in% c(-2,-1), NA, page))
    age <- factor(ifelse(!is.na(age.numeric), ifelse(age.numeric <= 18, "child", ifelse(age.numeric <= 65, "adult", "senior")),NA), levels = c("child","adult","senior"))
  })
  
  PERSON0515 <- PERSON0515 %>% select(houseID, subjectID, sex, age)
  PERSON1516 <- PERSON1516 %>% select(houseID, subjectID, sex, age)
  PERSON1617 <- PERSON1617 %>% select(houseID, subjectID, sex, age)
  
  
  STAGE0515 <- LTDS0515$Stage
  STAGE1516 <- LTDS1516$Stage
  STAGE1617 <- LTDS1617$Stage
  
  STAGE0515 <- within(STAGE0515,{
    subjectID <- as.character(spid)
    houseID <- as.character(shid)
    mode <- factor(ifelse(smode %in% c(-1,-2), NA, ifelse(smode == 1, "walk", ifelse(smode == 2, "cycle", "other"))),levels = c("walk","cycle","other"))
    duration <- as.numeric(sdurn)
  })
  
  STAGE1516 <- within(STAGE1516,{
    subjectID <- as.character(spid)
    houseID <- as.character(shid)
    mode <- factor(ifelse(smode %in% c(-1,-2), NA, ifelse(smode == 1, "walk", ifelse(smode == 2, "cycle", "other"))),levels = c("walk","cycle","other"))
    duration <- as.numeric(sdurn)
  })
  
  STAGE1617 <- within(STAGE1617,{
    subjectID <- as.character(spid)
    houseID <- as.character(shid)
    mode <- factor(ifelse(smode %in% c(-1,-2), NA, ifelse(smode == 1, "walk", ifelse(smode == 2, "cycle", "other"))),levels = c("walk","cycle","other"))
    duration <- as.numeric(sdurn)
  })
  
  STAGE0515 <- STAGE0515 %>% 
    within(., duration <- ifelse(duration < 0, NA, duration)) %>%
    select(houseID, subjectID, duration, mode)
  STAGE1516 <- STAGE1516 %>% 
    within(., duration <- ifelse(duration < 0, NA, duration)) %>%
    select(houseID, subjectID, duration, mode)
  STAGE1617 <- STAGE1617 %>% 
    within(., duration <- ifelse(duration < 0, NA, duration)) %>%
    select(houseID, subjectID, duration, mode)
  
  
  LTDS.part <- within(LTDS.part,{
    location <- as.factor(LTDS.part$location)
    participation <- as.numeric(LTDS.part$weekly)/100
  })
  
  LTDS.part <- LTDS.part %>% select(location, participation)
  
  
  LTDS <- new("TravelSurvey",
              house = rbind(HOUSE0515,HOUSE1516,HOUSE1617),
              person = rbind(PERSON0515, PERSON1516, PERSON1617),
              trip = rbind(STAGE0515, STAGE1516, STAGE1617),
              location = LTDS.part)
  saveRDS(LTDS, file = "~/HOT/inst/LTDS.ts.rds")
}


# This function generates a local rds file for full ENTD TravelSurvey object
newENTD <- function(){
  
  # Initialise 'person' TS slot
  person <- read.dta("~/CUSSH/data/rennes/Stata/q_individu.dta")
  person <- person %>% select(houseID = ident_men, subjectID = ident_ind, sex = sexe, age = age)
  
  person <- within(person,{
    houseID <- as.character(houseID)
    subjectID <- as.character(subjectID)
    sex <- factor(sex, levels = c(1,2), labels = c("M","F"))
    age <- factor(ifelse(!is.na(age), ifelse(age <= 18, "child", ifelse(age <= 65, "adult", "senior")),NA), levels = c("child","adult","senior"))
  })
  
  # Initialise 'house' TS slot
  house0 <- read.dta("~/CUSSH/data/rennes/Stata/q_tcm_menage.dta")
  
  dept <- read.csv(file = "~/CUSSH/data/rennes/departmentList.csv", stringsAsFactors = FALSE)
  deptVec <- dept$deptName
  names(deptVec) <- dept$deptNumber
  
  house <- house0 %>% select(houseID = ident_men, location = dep) %>% mutate(year = "2008") %>% # house0$dep only has 95 unique values for départment -- there should be 96
    within(.,{
      houseID <- as.character(houseID)
      location <- factor(deptVec[location])
    })
  levels(house$location) <- ifelse(levels(house$location) == "Côte-dOr", "Côte-d'Or",
                                   ifelse(levels(house$location) == "Côtes-dArmor", "Côtes-d'Armor", levels(house$location)))
  
  # Initialise 'trip' TS slot
  trip <- read.dta("~/CUSSH/data/rennes/Stata/k_deploc.dta")
  
  trip <- trip %>% select(houseID = ident_men, subjectID = ident_ind, duration = v2_duree, mode = v2_mtp) %>%
    within(.,{
      subjectID <- as.character(subjectID)
      houseID <- as.character(houseID)
      mode <- factor(ifelse(mode %in% c("1.1"), "walk", ifelse(mode %in% c("2.2"), "cycle", "other")), levels = c("walk","cycle","other"))
    })
  
  # Initialise 'location' TS slot
  tempTS <- new("TravelSurvey", person = person, house = house, trip = trip, location = data.frame(location = levels(house$location), participation = 1)) # pi1 = 1 assumption not used, since we need this TS to calculate p1
  p1.df <- getPrevalence(tempTS) # with assumption Freq = 1, then pi1 = p1
  location <- data.frame(location = p1.df$location, participation = p1.df$p1)
  
  # Create TS object and store to local rds file
  ENTD <- new("TravelSurvey", person = person, house = house, trip = trip, location = location)
  saveRDS(ENTD, file = "~/HOT/inst/ENTD.fr.ts.rds")
}