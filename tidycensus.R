# Tidycensus Workshop
# Fall 2019
# Jennifer Huck
# UVA Library

# This R Script will ...

## Load packages----------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(censusapi)

## API call---------------------------------------------------------------------

# census_api_key("YOUR API KEY GOES HERE")
# when install = TRUE, you don't have to keep using this line in your scripts
census_api_key("a33ce039311c73244aadb7f001693bbcf6c856a1", install = TRUE)

## Review documentation---------------------------------------------------------
?get_decennial
?get_acs # defaults: year: 2017; survey: 5-year; moe_level: 90
?load_variables #dataset: One of "sf1", "sf3", "acs1", "acs3", "acs5", 
  #"acs1/profile", "acs3/profile, "acs5/profile", "acs1/subject", "acs3/subject",
  # or "acs5/subject".

## Load Variables---------------------------------------------------------------
v17 <- load_variables(year = 2017, dataset = "acs5", cache = TRUE)
View(v17)

v17_profile <- load_variables(2017, "acs5/profile", cache = TRUE)
View(v17_profile)

## Look for tables--------------------------------------------------------------
# Now you need some table IDs
# Use the search bar with the magnifying glass in the spreadsheet viewer or
# Use a secondary source like Census Reporter or data.census.gov or

# Try grep
v17_profile[grep(x = v17_profile$label, "Median household"), c("name", "label")]

v17[grep(x = v17$label, "Median household"), c("name", "label")] %>% 
  print(n = 100)

# Or filter the variables with dplyr's filter and stringr's str_detect
B17010 <- filter(v17, str_detect(name, "B17010"))
View(B17010)

poverty <- filter(v17, str_detect(label, fixed("poverty",
                                               ignore_case = TRUE)))
View(poverty)


## Decennial Census-------------------------------------------------------------

# Pull Decennial Census population counts by state
state_pop <- get_decennial(geography = "state", 
                           variables = "P001001") # default is 2010
state_pop #evaluate, notice number of rows

# Save random rows from a table, more obviously helpful when you have a lot of observations
# Test your code on your sample
state_pop_sample_n <- sample_n(state_pop, 10) #retrieve 10 random rows
state_pop_sample_frac <- sample_frac(state_pop, .1) #retrieve 10% of rows

# Use ggplot to visualize and reorder
state_pop %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()


## ACS--------------------------------------------------------------------------

# Pull income estimates by state with ACS 2013-2017 5-year survey

state_income <- get_acs(geography = "state",
                        variables = "B19013_001") 
head(state_income)


## Your turn #1-----------------------------------------------------------------

# Pull population counts by state for the year 2000. Find and replace "___" to make code run.

state_pop_00 <- get_decennial(geography = "state", 
                              variables = "P001001",
                              ___ = ___) 
head(state_pop_00)


## Lookup geography codes with tigris package-----------------------------------

# Look up up FIPS codes for specific geographies
lookup_code("Virginia", "Charlottesville") 

# List all counties in Virginia, three ways - they all work the same
list_counties("Virginia") # use state name
list_counties("VA") # use postal code
list_counties("51") # use FIPS code

## Geographic subsets-----------------------------------------------------------

# Pull the 2013-2017 ACS 5 year estimates for family poverty in Charlottesville

# by census tract
fp_cv_tract_tc <- get_acs(geography = "tract", #geography is tract
                      variables = c(family_poverty = "B17010_002"), #pass a named vector in the variables argument
                      state = "51", # you are required to include a state to pull tracts
                      county = "540") # you can optionally include a county to pull tracts
head(fp_cv_tract_tc)

# by block group
fp_cv_bg <- get_acs(geography = "block group", #geography is block group
                          variables = c(family_poverty = "B17010_002"),
                          state = "51",
                          county ="540")

# Pull multiple counties to create the Cville MSA: Charlottesville, Albemarle, 
# Fluvanna, Greene, Nelson
fp_msa_tract <- get_acs(geography = "tract", 
                           variables = c(family_poverty = "B17010_002"), 
                           state = "51", 
                           county = c("540","003","065","079","125"), #concatenate county codes
                           geometry = T) # returns an sf tibble with simple feature geometry

# Median household income
hhi_msa_tract <- get_acs(geography = "tract", 
                        variables = c(family_poverty = "DP03_0062E"), 
                        state = "51", 
                        county = c("540","003","065","079","125"))
# Check for missing estimates
filter(hhi_msa_tract, is.na(estimate))

## Visualizations---------------------------------------------------------------

# Create a map with ggplot with our MSA geometry
fp_msa_tract %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() +
  ggtitle("Family Poverty - Charlottesville MSA, 2017")

# Visualize with a plot
fp_cv_tract %>%
  mutate(NAME = gsub(", Charlottesville city, Virginia", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Poverty by Census Tract In Cville",
       subtitle = "2013-2017 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

## Your turn #1-----------------------------------------------------------------

fam_pov_alb_16 <- get_acs(geography = "tract", 
                       variables = c(family_poverty = "B17010_002"), #pass a named vector in the variables argument
                       state = "VA",
                       county = "Albemarle",
                       year = 2016)
head(fam_pov_alb_16)

fam_pov_alb_17 <- get_acs(geography = "tract", 
                          variables = c(family_poverty = "B17010_002"), #pass a named vector in the variables argument
                          state = "VA",
                          county = "Albemarle",
                          year = 2017)
head(fam_pov_alb_17)

pov_cville <- get_acs(geography = "tract", 
                          variables = c(poverty = "B17001_002"), 
                          state = "51",
                          county = "540")
head(pov_cville)
View(pov_cville)


## Your turn #2-----------------------------------------------------------------

# Find the 1990 Decennial Census table ID for total population.  
# Hint: in 2000 and 2010 it was "P001001".
# Use your new table ID to make the next few lines run. 

v90 <- load_variables(___, ___, cache = TRUE)
View(v90)

state_pop_90 <- get_decennial(geography = "state", 
                              ___ = ___,
                              year = 1990) 
head(state_pop_90)


## CensusAPI package------------------------------------------------------------

# different package means we need to install the API again 
# if you are on a shared computer, use censusapi::getcensus key argument instead

# add key to .Renviron, only need to do once
Sys.setenv(CENSUS_KEY="a33ce039311c73244aadb7f001693bbcf6c856a1")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


# View of a list of every available endpoint
# Includes useful metadata such as 'name'
apis <- listCensusApis()
View(apis)

# Get unisured rates by income group using the Small Area Health Insurance Estimates API
# which provides detailed annual and county-level estimates of health insurance rates

# retrieve variable options
sahie_vars <- listCensusMetadata(name = "timeseries/healthins/sahie", 
                                 type = "variables")
View(sahie_vars)

# retrieve geography options
listCensusMetadata(name = "timeseries/healthins/sahie", 
                   type = "geography")
# 3 geographic levels: US, County, State

# Let's call these variables:
# IPRCAT: Income Poverty Ratio Category
# IPR_DESC: Income Poverty Ratio Category Description
# PCTUI_PT: Percent Uninsured in Demographic Group for Selected Income Range, Estimate
# NAME: Name of the geography returned (e.g. state or county name)

# view uninsured rate by income group at the national level for 2017
getCensus(name = "timeseries/healthins/sahie",
          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
          region = "us:*", 
          time = 2017)
# FYI - Income <= 138% of Poverty is eligible for Medicaid

# view uninsured rate by income group at state-level for Virginia in 2017
getCensus(name = "timeseries/healthins/sahie",
                          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                          region = "state:51", 
                          time = 2017)

# view Charlottesville
getCensus(name = "timeseries/healthins/sahie",
          vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
          region = "county:540", 
          regionin = "state:51", 
          time = 2017)

# save uninsured rate by income group at county-level for Virginia in 2017
sahie_counties <- getCensus(name = "timeseries/healthins/sahie",
                            vars = c("NAME", "IPRCAT", "IPR_DESC", "PCTUI_PT"), 
                            region = "county:*", 
                            regionin = "state:51", 
                            time = 2017)
head(sahie_counties, n=12L)

# using tidyverse tools, create a ratio variable (<= 138% of Poverty / All Incomes), 
# arrange by largest ratio of low-income to all-income uninsured
sahie_counties_ratio <- sahie_counties %>% 
  filter(IPRCAT == 3 | IPRCAT == 0) %>% 
  select(-IPRCAT) %>% 
  spread(key = IPR_DESC, value = PCTUI_PT, convert = TRUE) %>% 
  rename(poverty_138 = "<= 138% of Poverty", all_incomes = "All Incomes") %>% 
  mutate(ratio = poverty_138 / all_incomes) %>% 
  arrange(desc(ratio))

## Recreate Family Poverty estimates using censusapi, compare to tidycensus-----

# note available geographies
acs5_geo <- listCensusMetadata(name = "acs/acs5", 
                   type = "geography",
                   vintage = 2017)

# use 'group' argument to call what is often referred to as a table (e.g. in American FactFinder)
acs5_B17010 <- listCensusMetadata(name = "acs/acs5", 
                            vintage = 2017,
                            type = "variables",
                            group = "B17010")
View(acs5_B17010)

fp_cv_tract_ca <- getCensus(name = "acs/acs5",
                        vintage = 2017, 
                        vars = c("NAME", "B17010_002E", "B17010_002EA", "B17010_002M"), 
                        region = "tract:*", 
                        regionin = "state:51+county:540")
head(fp_cv_tract_ca)

# compare to tidycensus version, ~line 107
# fp_cv_tract_tc <- get_acs(geography = "tract",
#                           variables = c(family_poverty = "B17010_002"),
#                           state = "51", 
#                           county = "540")
head(fp_cv_tract_tc)
