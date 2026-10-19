## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ipumsr)

setwd("C:/Users/CarolXu/Who gets killed in America - NVSS and NIBRS")

## Read ACS 2021-2024 (IPUMS USA; 1-year ACS samples, 50 states + DC) -------
ddi_acs = read_ipums_ddi("NIBRS-data/input/usa_00031.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>%
  rename_with(tolower) %>%
  select(year, perwt, sex, age, race, hispan,
         racamind, racasian, racblk, racpacis, racwht, statefip) %>%
  zap_labels() %>%                                   
  filter(year >= 2021) %>%
  mutate(statefip = as.integer(statefip))

## Age groups ----------------------------------------------------------------
age_levels_5yr = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+")

acs = acs %>%
  mutate(age_group_5yr = case_when(
    age >= 80 ~ "80+",
    TRUE ~ paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4)
  ) %>% factor(levels = age_levels_5yr))

## Recode to NIBRS categories ------------------------------------------------
# NIBRS records one race per person, with no "some other race" or multiracial category. 
# ACS respondents are assigned one race as follows:
#   1. Multiracial respondents: hierarchical assignment, minority race first
#      (Black > AIAN > Asian > NHPI > White), following OMB Bulletin 00-02
#      guidance to allocate minority-and-White responses to the minority race.
#   2. "Some other race" only (no White/Black/AIAN/Asian/NHPI box checked):
#      assigned White. These respondents are overwhelmingly Hispanic, and
#      police in NIBRS typically record Hispanic persons as White.
# Ethnicity (Hispanic/Not Hispanic) is recorded separately, as in NIBRS.

acs = acs %>%
  mutate(
    sex_nibrs = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female"),
    sor_only  = racblk != 2 & racamind != 2 & racasian != 2 &
                racpacis != 2 & racwht != 2,
    race_nibrs = case_when(
      racblk   == 2 ~ "Black or African American",
      racamind == 2 ~ "American Indian or Alaska Native",
      racasian == 2 ~ "Asian",
      racpacis == 2 ~ "Native Hawaiian or Other Pacific Islander",
      racwht   == 2 ~ "White",
      sor_only      ~ "White"
    ),
    ethnicity_nibrs = case_when(
      hispan == 0     ~ "Not Hispanic/Latino",
      hispan %in% 1:4 ~ "Hispanic/Latino"
    ),
    race_ethnicity_nibrs = factor(
      if_else(is.na(race_nibrs) | is.na(ethnicity_nibrs), NA_character_,
              paste0(race_nibrs, ", ", ethnicity_nibrs)),
      levels = c(
        "White, Not Hispanic/Latino", "White, Hispanic/Latino",
        "Black or African American, Not Hispanic/Latino", "Black or African American, Hispanic/Latino",
        "American Indian or Alaska Native, Not Hispanic/Latino", "American Indian or Alaska Native, Hispanic/Latino",
        "Asian, Not Hispanic/Latino", "Asian, Hispanic/Latino",
        "Native Hawaiian or Other Pacific Islander, Not Hispanic/Latino",
        "Native Hawaiian or Other Pacific Islander, Hispanic/Latino"))
  )

## Checks ----------------------------------------------------------------------
# share of population reassigned from "some other race only" to White
acs %>%
  group_by(year) %>%
  summarise(pop_millions = sum(perwt) / 1e6,
            sor_only_millions = sum(perwt[sor_only]) / 1e6,
            pct_sor_only = round(100 * sum(perwt[sor_only]) / sum(perwt), 1)) %>%
  print()

# no missing race/ethnicity/sex remaining??
acs %>% summarise(across(c(race_nibrs, ethnicity_nibrs, sex_nibrs, race_ethnicity_nibrs),
                         ~ sum(is.na(.x)))) %>% print()

# population by NIBRS race categores, year
acs %>% group_by(year, race_nibrs) %>%
  summarise(pop_millions = round(sum(perwt) / 1e6, 1), .groups = "drop") %>%
  print(n = Inf)

## Grouped table: weighted population, NIBRS categories ------------------------
acs_table_nibrs = acs %>%
  group_by(year, race_ethnicity = race_ethnicity_nibrs, age_group_5yr,
           sex = sex_nibrs, statefip) %>%
  summarise(n = n(), weighted = sum(perwt), .groups = "drop") %>%
  arrange(year, race_ethnicity, age_group_5yr, sex)

print(acs_table_nibrs, n = 100)

write_csv(acs_table_nibrs, "results/acs_race_ethnicity_age_sex_nibrs.csv")

saveRDS(acs, "NIBRS-data/output/acs_nibrs.rds")
