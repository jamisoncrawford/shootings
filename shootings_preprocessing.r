# VISUALIZING WORST MASS SHOOTINGS ACROSS THE U.S.

    # Date: 2019-08-07
    # R Version: 3.6.0
    # RStudio Version: 1.2.1335
    # Operating System: Windows 10
    # Source: Follman, Mark et al. (2019). 
        # "US Mass Shootings, 1982-2019: Data From Mother Jones' Investigation". 
        # Mother Jones. Retrieved on 07 August 2019 from
        # https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/

# Install & Load Packages

if(!require(readr)){install.packages("readr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(stringr)){install.packages("stringr")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(lubridate)){install.packages("lubridate")}

library(readr)
library(dplyr)
library(stringr)
library(leaflet)
library(lubridate)

# Read In Data (Source: Mother Jones, )

url <- paste0("https://docs.google.com/spreadsheets/d/e",
              "/2PACX-1vQBEbQoWMn_P81DuwmlQC0_jr2sJDzkk",
              "C0mvF6WLcM53ZYXi8RMfUlunvP1B5W0jRrJvH-wc",
              "-WGjDB1/pub?gid=0&single=true&output=csv")

mass <- read_csv(url)

rm(url)

# Dimension Reduction, Class Coersion, & Cleaning

mass <- mass %>%
  select(case:total_victims, sources, weapon_type:weapon_details, 
         race:gender, latitude:longitude) %>%
  mutate(date = mdy(date),
         case = str_to_title(case),
         fatalities = as.integer(fatalities),
         injured = as.integer(injured),
         total_victims = as.integer(total_victims))

# Converting State Abbreviations

mass$city <- str_split(mass$location, pattern = ", ", simplify = TRUE)[, 1]
mass$state <- str_split(mass$location, pattern = ", ", simplify = TRUE)[, 2]

mass <- left_join(mass, tibble(state = state.abb,
                               state_full = state.name), 
                  by = "state")

index <- which(!is.na(mass$state_full))

mass[index, "state"] <- mass[index, "state_full"]

mass <- mass %>%
  mutate(location = paste(city, state, sep = ", ")) %>%
  select(-city:-state_full)

# Convert URLs to Hyperlinks

mass <- mass %>%
  mutate(sources = str_replace_all(sources, pattern = "; .*$", ""),
         link = paste0("<a href='", sources, "'>", case, "</a>"))

# Title & Sentence Case

mass$weapon_type <- str_to_title(mass$weapon_type)
mass$weapon_details <- str_to_title(mass$weapon_details)

mass$race <- str_to_title(mass$race)
mass$race <- str_replace_all(mass$race, pattern = "^-$", "Unavailable")

mass$gender <- str_replace_all(mass$gender, pattern = "^M$", "Male")
mass$gender <- str_replace_all(mass$gender, pattern = "^F$", "Female")

names(mass) <- str_replace_all(names(mass), "_", " ")
names(mass) <- str_to_title(names(mass))  

# Leaflet Map

mass %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng = mass$Longitude,
                   lat = mass$Latitude,
                   weight = 1.25,
                   radius = mass$Fatalities,
                   color = "tomato", 
                   opacity = 1, 
                   fillOpacity = 0.5,
                   popup = paste(mass$Link, 
                                 "<br>", mass$Location,
                                 "<br><br>Injuries: ", mass$Injured,
                                 "<br>Fatalities: ", mass$Fatalities,
                                 "<br>Total Victims: ", mass$`Total Victims`,
                                 "<br><br>Weapon(s): ", mass$`Weapon Type`,
                                 "<br><br>Weapon Details: ", mass$`Weapon Details`, 
                                 "<br><br>", mass$Summary))

# Table

appn <- mass %>%
  select(Case:Date, `Total Victims`) %>%
  arrange(desc(`Total Victims`))

                   