library(tidyverse)
library(lubridate)

#importing the csvs
sqf_2006 <-read_csv("https://www1.nyc.gov/assets/nypd/downloads/zip/analysis_and_planning/stop-question-frisk/sqf-2006-csv.zip", col_types = cols(.default = col_character()))
sqf_2007 <-read_csv("Desktop/college/soc2272/2007.csv", col_types = cols(.default = col_character()))
sqf_2008 <-read_csv("Desktop/college/soc2272/2008.csv", col_types = cols(.default = col_character()))
sqf_2009 <-read_csv("Desktop/college/soc2272/2009.csv", col_types = cols(.default = col_character()))
sqf_2010 <-read_csv("Desktop/college/soc2272/2010.csv", col_types = cols(.default = col_character()))
sqf_2011 <-read_csv("Desktop/college/soc2272/2011.csv", col_types = cols(.default = col_character()))
sqf_2012 <-read_csv("Desktop/college/soc2272/2012.csv", col_types = cols(.default = col_character()))

#create list of all data frames
list_sqf = list(sqf_2006, sqf_2007, sqf_2008, sqf_2009, sqf_2010, sqf_2011, sqf_2012)

#combine the data frames
sqf_all <- bind_rows(list_sqf)

#needs: ID, "year", "datestop", "timestop", "race", "female", "age", "beat", "sector", "xcoord", "ycoord"
#restricting the variables to the above specs
sqf_all <- transmute(sqf_all,
                     ID = row_number(),
                     date = as.Date((date %>% na_if("1900-12-31")), format = "%Y-%m-%d"),
                     year = as.integer(format(date, "%Y")),
                     time = ((as.POSIXct(datestop, format = "%Y-%m-%d")) + hours(timestop)),
                     race = recode(race,
                                   'W' = 'White',
                                   'B' = 'Non-Hispanic Black',
                                   'P' = 'White Hispanic',
                                   'Q' = 'Black Hispanic',
                                   'A' = 'Asian',
                                   'I' = 'Other',
                                   'Z' = 'Other',
                                   .default = NA_character_
                     ),
                     female = recode(sex, 'M' = FALSE, 'F' = TRUE, .default = NA),
                     age = age,
                     police_force = ifelse((pf_hands == 'Y' | pf_wall == 'Y' | pf_grnd == 'Y' | pf_drwep == 'Y' | pf_ptwep == 'Y' | pf_baton == 'Y' | pf_hcuff == 'Y' | pf_pepsp == 'Y' | pf_other == 'Y'), "TRUE", "FALSE"),
                     precinct = sector,
                     xcoord = xcoord,
                     ycoord = ycoord
)

#filter to only rows with non-hispanic black
sqf_all <- filter(sqf_all, race == 'Non-Hispanic Black')