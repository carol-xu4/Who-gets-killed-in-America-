## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ipumsr)

setwd("C:/Users/CarolXu/Who gets killed in America - NVSS and NIBRS")

## Read ACS 2014-2024 (IPUMS USA; 1-year ACS samples, 50 states + DC) -------
ddi_acs = read_ipums_ddi("NVSS-data/input/usa_00034.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
  rename_with(tolower) %>%
  select(year, serial, hhwt, statefip, gq, pernum,
         perwt, sex, age, marst, race, raced, hispan, hispand,
         bpl, bpld, citizen, occ2010) %>%
  mutate(year = as.integer(year),
         statefip = as.integer(statefip),
         bpl = as.integer(bpl),
         age = as.integer(age))

## Nativity: US-born (50 states, DC, territories) vs. foreign-born ----------
us_states_dc = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY")

us_territories = c("PR", "VI", "GU", "AS", "MP")

acs = acs %>%
  mutate(nativity = case_when(
    bpl %in% c(1:56, 90, 99, 100, 105, 110, 115, 120) ~ "native",
    bpl %in% 150:950                                  ~ "foreign",
    .default = NA_character_))

## Foreign birthplace group: Canada, Mexico, Cuba, rest of world ------------
# (native left NA here; only used for the foreign-born denominators by subgroup)
acs = acs %>%
  mutate(birthplace_group = case_when(
    bpl == 150 ~ "Canada",
    bpl == 200 ~ "Mexico",
    bpl == 250 ~ "Cuba",
    bpl %in% setdiff(150:950, c(150, 200, 250)) ~ "Rest of world",
    .default = NA_character_))

## Age groups, to match NVSS age_recode12 (ACS has no "age not stated") -----
acs = acs %>%
  mutate(age_group = case_when(
    age <  1              ~ "01",
    age >= 1  & age <= 4  ~ "02",
    age >= 5  & age <= 14 ~ "03",
    age >= 15 & age <= 24 ~ "04",
    age >= 25 & age <= 34 ~ "05",
    age >= 35 & age <= 44 ~ "06",
    age >= 45 & age <= 54 ~ "07",
    age >= 55 & age <= 64 ~ "08",
    age >= 65 & age <= 74 ~ "09",
    age >= 75 & age <= 84 ~ "10",
    age >= 85             ~ "11"))

## Race/ethnicity, to match NVSS race_recode40 groupings --------------------
# (own "more than one race" bucket here, not force-collapsed like the NIBRS recode)
acs = acs %>%
  mutate(
    race_num   = as.integer(race),
    hispan_num = as.integer(hispan),
    race_group = case_when(
      race_num == 1            ~ "White",
      race_num == 2            ~ "Black",
      race_num == 3            ~ "AIAN",
      race_num %in% c(4, 5, 6) ~ "Asian or Pacific Islander",
      race_num %in% c(8, 9)    ~ "More than one race",
      .default = NA_character_),
    ethnicity_group = case_when(
      hispan_num == 0     ~ "Non-Hispanic",
      hispan_num %in% 1:4 ~ "Hispanic",
      .default = NA_character_))

## State FIPS -> USPS abbreviation, for joining against NVSS state_res_fips -
fips_to_usps = c(
  "01" = "AL", "02" = "AK", "04" = "AZ", "05" = "AR", "06" = "CA",
  "08" = "CO", "09" = "CT", "10" = "DE", "11" = "DC", "12" = "FL",
  "13" = "GA", "15" = "HI", "16" = "ID", "17" = "IL", "18" = "IN",
  "19" = "IA", "20" = "KS", "21" = "KY", "22" = "LA", "23" = "ME",
  "24" = "MD", "25" = "MA", "26" = "MI", "27" = "MN", "28" = "MS",
  "29" = "MO", "30" = "MT", "31" = "NE", "32" = "NV", "33" = "NH",
  "34" = "NJ", "35" = "NM", "36" = "NY", "37" = "NC", "38" = "ND",
  "39" = "OH", "40" = "OK", "41" = "OR", "42" = "PA", "44" = "RI",
  "45" = "SC", "46" = "SD", "47" = "TN", "48" = "TX", "49" = "UT",
  "50" = "VT", "51" = "VA", "53" = "WA", "54" = "WV", "55" = "WI",
  "56" = "WY")

acs = acs %>%
  mutate(state_usps = fips_to_usps[sprintf("%02d", statefip)])

## Checks --------------------------------------------------------------------
# missingness among in-scope (50 states + DC) records
acs %>% filter(!is.na(state_usps)) %>%
  summarise(across(c(nativity, age_group), ~ sum(is.na(.x)))) %>% print()

# population by year and nativity
acs %>% filter(!is.na(nativity)) %>%
  group_by(year, nativity) %>%
  summarise(pop_millions = round(sum(perwt) / 1e6, 1), .groups = "drop") %>%
  print(n = Inf)

saveRDS(acs, "data/output/acs_nvss.rds")