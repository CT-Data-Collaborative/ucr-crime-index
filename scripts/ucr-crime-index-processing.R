library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for UCR Crime Index
# Created by Jenna Daly
# On 08/16/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = ".csv") 

UCR_data <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(raw_data)) {
  current_file <- read.csv(paste0(path_to_raw, "/", raw_data[i]), stringsAsFactors = FALSE, header=T, check.names = FALSE)
  get_year <- as.numeric(unique(unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")))
  current_file$Year <- get_year
  fileCols <- names(current_file)
  ## build list of columns to select out of data
  #Town
  town = fileCols[grepl("Agency", fileCols, fixed = T) | grepl("Department", fileCols, fixed = T) | grepl("Jurisdiction$", fileCols)]
  population = "Population"
  #Filter to only include columns named "Offenses" in conjunction with other filters below
  offenses = grepl("Offenses", fileCols, fixed = T)
  ## specific Crime columns
  #Violent Crimes
  assault = fileCols[offenses & grepl("AgAsslt", fileCols, fixed = T)]
  murder = "Murder Offenses"
  rape = fileCols[offenses & grepl("Rape", fileCols, fixed = T)]
  robbery = c(
    "Robbery Firearm Offenses",
    "Robbery Hands_Fists_Feet_etc. Offenses",
    "Robbery Knife_Cutting_Instrument Offenses",
    "Robbery Other_Dangerous_Weapon Offenses"
  )
  #Property Crimes
  burglary = fileCols[offenses & grepl("Burglary", fileCols, fixed = T) & grepl("Entry", fileCols, fixed = T)]
  larceny = fileCols[offenses & grepl("Larceny", fileCols, fixed = T) & grepl("$", fileCols, fixed = T)]
  mvtheft = fileCols[offenses & grepl("MVTheft", fileCols, fixed = T)]
  #Arson
  arson = fileCols[offenses & grepl("Arson", fileCols, fixed = T)]
  ## Finally, read data, selecting just the columns we want
  names(current_file)[names(current_file) == town] <- "Town"
  current_file <- current_file %>% select(Town, population, assault, murder, rape, robbery, burglary, larceny, mvtheft, arson, Year)
  UCR_data <- rbind(UCR_data, current_file)
}

#Removing values for county (where name is both a county and a town)
years <- c("2010", "2011", "2012", "2013") #Years where indicators are counties not towns
towns <- c("Hartford", "Windham", "Tolland", "New London", "New Haven", "Litchfield", "Fairfield")
UCR_data <- UCR_data[!(UCR_data$Year %in% years & UCR_data$Town %in% towns),]

#Convert to long format
UCR_data_long <- gather(UCR_data, `Crime Type`, Value, `AgAsslt Firearm Offenses`:`Arson Other Offenses`, factor_key=FALSE)

#Rename CT
UCR_data_long$Town[UCR_data_long$Town == "CT"] <- "Connecticut"
UCR_data_long$Town[UCR_data_long$Town == "Connecticut Total"] <- "Connecticut"

#Remove PD and CSP from names
UCR_data_long$Town <- gsub(" CSP", "", UCR_data_long$Town)
UCR_data_long$Town <- gsub(" PD", "", UCR_data_long$Town)

#Merge Groton names and Putnam names
UCR_data_long$Town[which(grepl("Groton", UCR_data_long$Town))] <- "Groton"
UCR_data_long$Town[which(grepl("Putnam", UCR_data_long$Town))] <- "Putnam"

#Aggregate across towns (Groton and Putnam)
UCR_data_long <- UCR_data_long %>% 
  group_by(Town, Year, `Crime Type`) %>% 
  summarise(Value = sum(Value), 
            Population = sum(Population))

#Merge in FIPS to remove non-towns
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

UCR_data_long_fips <- merge(UCR_data_long, fips, by = "Town", all.y=T)

#Create CT total for 2014 and 2015
CT_totals <- UCR_data_long_fips[UCR_data_long_fips$Year %in% c("2015", "2016"),]

CT_totals <- CT_totals %>% 
  group_by(Year, `Crime Type`) %>% 
  summarise(Value = sum(Value), 
            Population = sum(Population))

CT_totals$Town <- "Connecticut"
CT_totals$FIPS <- "09"
CT_totals <- as.data.frame(CT_totals)

UCR_data_long_fips <- rbind(UCR_data_long_fips, CT_totals)

# Aggregate data by crime
# First, turn Crimes into just their broader categories (Murder, Rape, etc)
UCR_data_long_fips$`Crime Type` <- sub(" .*", "", UCR_data_long_fips$`Crime Type`)

# Then sum values by town, year and crime type
UCR_data_long_fips <- UCR_data_long_fips %>% 
  group_by(Town, Year, `Crime Type`, FIPS, Population) %>% 
  summarise(Value=sum(Value))

# Re-code "AgAsslt", "MVTheft"
UCR_data_long_fips$`Crime Type`[UCR_data_long_fips$`Crime Type`== "AgAsslt"] <- "Aggravated Assault"
UCR_data_long_fips$`Crime Type`[UCR_data_long_fips$`Crime Type`== "MVTheft"] <- "Motor Vehicle Theft"

#Note Arson is not included in either Violent, Property, or Total Crimes. It is not used in the calculation of the traditional crime index. 
# Aggregate Total Violent Crimes by Town
violentTotals <- UCR_data_long_fips[UCR_data_long_fips$`Crime Type` %in% c("Aggravated Assault", "Murder", "Rape", "Robbery"),]

violentTotals <- violentTotals %>%
  group_by(Town, Year) %>% 
  mutate(Value = sum(Value)) 

violentTotals$`Crime Type` <- "Total Violent Crime"

violentTotals <- unique(violentTotals)

#Bind violent crimes back with original df
UCR_data_long_fips <- rbind(UCR_data_long_fips, violentTotals)

# Aggregate Total Property Crimes by Town
propertyTotals <- UCR_data_long_fips[UCR_data_long_fips$`Crime Type` %in% c("Burglary", "Larceny", "Motor Vehicle Theft"),]

propertyTotals <- propertyTotals %>%
  group_by(Town, Year) %>% 
  mutate(Value = sum(Value)) 

propertyTotals$`Crime Type` <- "Total Property Crime"

propertyTotals <- unique(propertyTotals)

#Bind violent crimes back with original df
UCR_data_long_fips <- rbind(UCR_data_long_fips, propertyTotals)

# Aggregate Crime Index Totals by Town
indexTotals <- UCR_data_long_fips[UCR_data_long_fips$`Crime Type` %in% c("Total Violent Crime", "Total Property Crime"),]

indexTotals <- indexTotals %>%
  group_by(Town, Year) %>% 
  mutate(Value = sum(Value)) 

indexTotals$`Crime Type` <- "Total Crime"

indexTotals <- unique(indexTotals)

#Bind violent crimes back with original df
UCR_data_long_fips <- rbind(UCR_data_long_fips, indexTotals)

UCR_data_long_fips$`Measure Type` <- "Number"

# Calculate rates
rates <- UCR_data_long_fips

rates$Value = round(100000 * (rates$Value / rates$Population), 1)

rates$`Measure Type` <- "Rate (per 100,000)"

# Bind into dataset
UCR_data_long_fips <- rbind(UCR_data_long_fips, rates)

#remove population, add variable columns
UCR_data_long_fips$Population <- NULL
UCR_data_long_fips$Variable = "Crime Index"

#Set Crime Type to factor for sorting
UCR_data_long_fips$`Crime Type` <- factor(UCR_data_long_fips$`Crime Type`, 
                                          levels = c("Total Crime",
                                                     "Total Violent Crime",
                                                     "Total Property Crime",
                                                     "Aggravated Assault",
                                                     "Murder",
                                                     "Rape",
                                                     "Robbery",
                                                     "Burglary", 
                                                     "Larceny",
                                                     "Motor Vehicle Theft",
                                                     "Arson"))

#Order and sort columns
UCR_data_long_fips <- UCR_data_long_fips %>% 
  select(Town, FIPS, Year, `Crime Type`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Crime Type`, `Measure Type`)

# write to file
write.table(
  UCR_data_long_fips,
  file.path(getwd(), "data", "ucr-crime-index_2016.csv"),
  sep = ",",
  row.names = F
)
