## Preliminaries -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::count)
conflicted::conflicts_prefer(dplyr::lag)

setwd("C:/Users/CarolXu/Who gets killed in America - NVSS and NIBRS")

# read in extracted NVSS and ACS
homicides = read_csv("NVSS-data/output/homicides_2014_2024.csv")

names(homicides)

acs = readRDS("NVSS-data/output/acs_nvss.rds")  

# homicides per year
homicides_year = homicides %>%
    group_by(data_year) %>%
    summarise(n = n())

homicides_year

## Reference lists -------------------------------------------------------------------------
us_states_dc = c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY")

us_territories = c("PR", "VI", "GU", "AS", "MP")

standard_weights = c(
    "01" = 0.013818,  # Under 1 year
    "02" = 0.055317,  # 1-4 years
    "03" = 0.145565,  # 5-14 years
    "04" = 0.138646,  # 15-24 years
    "05" = 0.135573,  # 25-34 years
    "06" = 0.162613,  # 35-44 years
    "07" = 0.134834,  # 45-54 years
    "08" = 0.087247,  # 55-64 years
    "09" = 0.066037,  # 65-74 years
    "10" = 0.044842,  # 75-84 years
    "11" = 0.015508)  # 85 years and over

age_group_labels = c(
    "01" = "Under 1 year",
    "02" = "1-4 years",
    "03" = "5-14 years",
    "04" = "15-24 years",
    "05" = "25-34 years",
    "06" = "35-44 years",
    "07" = "45-54 years",
    "08" = "55-64 years",
    "09" = "65-74 years",
    "10" = "75-84 years",
    "11" = "85 years and over")

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

colors_2 = c("native" = "#3043B4", "foreign" = "#C97703")

colors_5 = c(
    "Foreign total" = "#7C756D",
    "Canada"        = "#00847E",
    "Cuba"          = "#A6192E",
    "Mexico"        = "#D4A017",
    "Rest of world" = "#7B2D8B")

linetypes_5 = c(
    "Foreign total" = "dashed",
    "Canada"        = "solid",
    "Cuba"          = "solid",
    "Mexico"        = "solid",
    "Rest of world" = "solid")

theme_cato = theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    axis.title = element_text(size = 16, color = "gray40"),
    axis.text = element_text(size = 14, color = "gray40"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.4),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

# fig. 3 total homicides by nativity
homicides = homicides %>%
    mutate(nativity = case_when(
        state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
        state_country_birth_recode %in% c("CC", "MX", "CU", "YY")       ~ "foreign",
        .default = NA_character_))

homicides_by_nativity = homicides %>% 
    group_by(data_year, nativity) %>%
    summarize(n = n())

print(homicides_by_nativity, n = Inf)

write_csv(homicides_by_nativity, "results/homicides_by_nativity_year.csv")

ggplot(homicides_by_nativity %>% filter(!is.na(nativity)),
  aes(x = data_year, y = n, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 25000)) +
  labs(
    title = "Homicide Deaths by Nativity, 2014-2024",
    subtitle = "UCOD: assault (homicide), ICD-10 X85-Y09, Y87.1 \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS Detailed Mortality - Limited Geography files, restricted use") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.3_total_homicides_by_nativity_2014_2024.png", width = 15, height = 10)

# fig. 4 homicide rates by nativity
homicides_by_nativity = homicides %>%
    mutate(
        data_year = as.integer(data_year),
        nativity = case_when(
            state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
            state_country_birth_recode %in% c("CC", "MX", "CU", "YY")       ~ "foreign",
            .default = NA_character_)) %>%
    filter(!is.na(nativity)) %>%
    group_by(data_year, nativity) %>%
    summarize(n = n(), .groups = "drop")

acs_pop_by_nativity = acs %>%
    mutate(
        year = as.integer(year),
        bpl = as.integer(bpl),
        nativity = case_when(
            bpl %in% c(1:56, 90, 99, 100, 105, 110, 115, 120) ~ "native",
            bpl %in% 150:950                                  ~ "foreign",
            .default = NA_character_)) %>%
    filter(!is.na(nativity)) %>%
    group_by(year, nativity) %>%
    summarize(pop = sum(perwt), .groups = "drop")

homicide_rates_by_nativity = homicides_by_nativity %>%
    left_join(acs_pop_by_nativity, by = c("data_year" = "year", "nativity")) %>%
    mutate(rate = n / pop * 100000)

print(homicide_rates_by_nativity, n = Inf)

write_csv(homicide_rates_by_nativity, "results/homicide_rates_by_nativity_2014_2024.csv")

ggplot(homicide_rates_by_nativity,aes(x = data_year, y = rate, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "Homicide Rate by Nativity, 2014-2024",
    subtitle = "Homicide deaths per 100,000 population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.4_homicide_rate_by_nativity_2014_2024.png", width = 15, height = 10)

# age-adjusted rates
table(homicides$age_recode12, homicides$data_year)

acs = acs %>%
    mutate(age = as.integer(age),
        age_group = case_when(
            age <  1                ~ "01",   
            age >= 1  & age <= 4    ~ "02",  
            age >= 5  & age <= 14   ~ "03",   
            age >= 15 & age <= 24   ~ "04", 
            age >= 25 & age <= 34   ~ "05", 
            age >= 35 & age <= 44   ~ "06", 
            age >= 45 & age <= 54   ~ "07", 
            age >= 55 & age <= 64   ~ "08",
            age >= 65 & age <= 74   ~ "09",   
            age >= 75 & age <= 84   ~ "10", 
            age >= 85               ~ "11"))  

homicides_age_nativity = homicides %>%
    filter(!is.na(nativity), age_recode12 != "12") %>%
    group_by(data_year, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_age_nativity = acs %>%
    filter(!is.na(nativity), !is.na(age_group)) %>%
    group_by(year, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

age_rates = homicides_age_nativity %>%
    left_join(
        acs_age_nativity,
        by = c("data_year" = "year", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight)

print(age_rates, n = Inf)

age_adjusted_rates = age_rates %>%
    group_by(data_year, nativity) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

print(age_adjusted_rates, n = Inf)

write_csv(age_adjusted_rates, "results/age_adjusted_homicide_rates_nativity_year.csv")

# table a6. crude and age-adjusted homicide rates by nativity, 2014-2024
nativity_rates_table = homicide_rates_by_nativity %>%
    select(data_year, nativity, n, pop, crude_rate = rate) %>%
    left_join(age_adjusted_rates, by = c("data_year", "nativity")) %>%
    filter(!is.na(nativity)) %>%
    mutate(
        crude_rate = round(crude_rate, 2),
        age_adjusted_rate = round(age_adjusted_rate, 2)) %>%
    arrange(data_year, nativity)

print(nativity_rates_table, n = Inf)

write_csv(nativity_rates_table, "results/table_a6_crude_and_age_adjusted_homicide_rates_by_nativity_2014_2024.csv")

# fig. 5 age-adjusted homicide rates by nativity
ggplot(age_adjusted_rates,aes(x = data_year, y = age_adjusted_rate, color = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
  labs(
    title = "Age-Adjusted Homicide Rate by Nativity, 2014-2024",
    subtitle = "Age-adjusted to the year 2000 U.S. standard population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.5_age_adjusted_homicide_rate_by_nativity_2014_2024.png", width = 15, height = 10)

# fig. 6 homicide rates by age and nativity (age-adjusted)
age_specific_rates_pooled = homicides %>%
    filter(!is.na(nativity), age_recode12 != "12") %>%
    group_by(nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop") %>%
    left_join(
        acs %>%
            filter(!is.na(nativity), !is.na(age_group)) %>%
            group_by(nativity, age_group) %>%
            summarize(pop = sum(perwt), .groups = "drop"),
        by = c("nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate = n / pop * 100000,
        age_group_label = age_group_labels[age_recode12])

age_specific_rates_wide = age_specific_rates_pooled %>%
    select(age_recode12, age_group_label, nativity, rate) %>%
    pivot_wider(names_from = nativity, values_from = rate) %>%
    arrange(age_recode12)

print(age_specific_rates_wide, n = Inf)

write_csv(age_specific_rates_wide, "results/age_specific_homicide_rates_native_vs_foreign_pooled_2014_2024.csv")

age_specific_rates_pooled = age_specific_rates_pooled %>%
    mutate(age_group_label = factor(age_group_label, levels = age_group_labels))

ggplot(age_specific_rates_pooled, aes(x = age_group_label, y = rate, color = nativity, group = nativity)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_2) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, NA), breaks = seq(0, 14, by = 2)) +
  labs(
    title = "Homicide Rate by Age and Nativity, 2014-2024",
    subtitle = "Pooled homicide deaths per 100,000 population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 16, color = "gray40", angle = 30, hjust = 1),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.6_homicide_rate_by_age_and_nativity_pooled_2014_2024.png", width = 15, height = 10)

# fig. 7 homicide rates by foreign birthplace
# canada 2020 censored (suppressed manually post-export; not handled in code)
birthplace = homicides %>%
    mutate(
        data_year = as.integer(data_year),
        birthplace_group = case_when(
            state_country_birth_recode %in% c(us_states_dc, us_territories) ~ "native",
            state_country_birth_recode == "CC" ~ "Canada",
            state_country_birth_recode == "MX" ~ "Mexico",
            state_country_birth_recode == "CU" ~ "Cuba",
            state_country_birth_recode == "YY" ~ "Rest of world",
            .default = NA_character_)) %>%
    filter(!is.na(birthplace_group)) %>%
    group_by(data_year, birthplace_group) %>%
    summarize(n = n(), .groups = "drop")

foreign_total = birthplace %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    group_by(data_year) %>%
    summarize(n = sum(n)) %>%
    mutate(birthplace_group = "Foreign total")

foreign_numerator = birthplace %>%
    filter(birthplace_group %in% c("Canada", "Mexico", "Cuba", "Rest of world")) %>%
    bind_rows(foreign_total)

acs_birthplace = acs %>%
    mutate(
        year = as.integer(year),
        bpl = as.integer(bpl),
        birthplace_group = case_when(
            bpl == 150 ~ "Canada",
            bpl == 200 ~ "Mexico",
            bpl == 250 ~ "Cuba",
            bpl %in% setdiff(150:950, c(150, 200, 250)) ~ "Rest of world",
            .default = NA_character_)) %>%
    filter(!is.na(birthplace_group)) %>%
    group_by(year, birthplace_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

acs_foreign_total = acs_birthplace %>%
    group_by(year) %>%
    summarize(pop = sum(pop)) %>%
    mutate(birthplace_group = "Foreign total")

foreign_denominator = acs_birthplace %>%
    bind_rows(acs_foreign_total) %>%
    rename(data_year = year)

foreign_rates = foreign_numerator %>%
    left_join(foreign_denominator, by = c("data_year", "birthplace_group")) %>%
    mutate(
        rate = n / pop * 100000,
        rate = if_else(n < 10, NA_real_, rate),
        birthplace_group = factor(birthplace_group,
            levels = c("Foreign total", "Canada", "Cuba", "Mexico", "Rest of world")))

print(foreign_rates, n = Inf)

write_csv(foreign_rates, "results/homicide_rates_by_foreign_birthplace_2014_2024.csv")

ggplot(foreign_rates,aes(x = data_year, y = rate, color = birthplace_group, linetype = birthplace_group)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_5) +
  scale_linetype_manual(values = linetypes_5) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0.02, 0)) +
  scale_y_continuous(expand = c(0.02, 0), limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  labs(
    title = "Homicide Rate by Foreign Birthplace, 2014-2024",
    subtitle = "Homicide deaths per 100,000 population \n Resident deaths, 50 states and DC",
    x = NULL,
    y = NULL,
    color = NULL,
    linetype = NULL,
    caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 25, color = "gray40"),
    axis.text.y = element_text(size = 25, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.7_homicide_rate_by_foreign_birthplace_2014_2024.png", width = 15, height = 10)

# race recode
homicides_race_ethnicity_age = homicides %>%
    filter(!is.na(nativity), age_recode12 != "12") %>%
    mutate(
        hispanic_origin_num = as.integer(hispanic_origin),
        race_group = case_when(
            race_recode40 == "01" ~ "White",
            race_recode40 == "02" ~ "Black",
            race_recode40 == "03" ~ "AIAN",
            race_recode40 %in% c("04","05","06","07","08","09","10","11","12","13","14") ~ "Asian or Pacific Islander",
            race_recode40 >= "15" ~ "More than one race",
            .default = NA_character_),
        ethnicity_group = case_when(
            hispanic_origin_num %in% 100:199 ~ "Non-Hispanic",
            hispanic_origin_num %in% 200:299 ~ "Hispanic",
            .default = NA_character_)) %>%
    filter(!is.na(race_group), !is.na(ethnicity_group)) %>%
    group_by(race_group, ethnicity_group, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_race_ethnicity_age = acs %>%
    mutate(
        race_num = as.integer(race),
        hispan_num = as.integer(hispan),
        race_group = case_when(
            race_num == 1 ~ "White",
            race_num == 2 ~ "Black",
            race_num == 3 ~ "AIAN",
            race_num %in% c(4, 5, 6) ~ "Asian or Pacific Islander",
            race_num %in% c(8, 9) ~ "More than one race",
            .default = NA_character_),
        ethnicity_group = case_when(
            hispan_num == 0 ~ "Non-Hispanic",
            hispan_num %in% 1:4 ~ "Hispanic",
            .default = NA_character_)) %>%
    filter(!is.na(nativity), !is.na(age_group), !is.na(race_group), !is.na(ethnicity_group)) %>%
    group_by(race_group, ethnicity_group, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

race_ethnicity_rates = homicides_race_ethnicity_age %>%
    left_join(acs_race_ethnicity_age,
              by = c("race_group", "ethnicity_group", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight) %>%
    group_by(race_group, ethnicity_group, nativity) %>%
    summarize(n = sum(n), pop = sum(pop), age_adjusted_rate = sum(weighted_rate), .groups = "drop") %>%
    mutate(
        crude_rate = n / pop * 100000,
        category = paste(race_group, ethnicity_group,
                          if_else(nativity == "native", "Native-born", "Foreign-born"), sep = ", ")) %>%
    select(category, race_group, ethnicity_group, nativity, n, pop, crude_rate, age_adjusted_rate) %>%
    arrange(race_group, ethnicity_group, nativity)

print(race_ethnicity_rates, n = Inf)

write_csv(race_ethnicity_rates, "results/homicide_rates_by_race_ethnicity_nativity_pooled_2014_2024.csv")

# fig. 8 same race, different risks
dumbbell_data = race_ethnicity_rates %>%
    mutate(group_label = paste(race_group, ethnicity_group, sep = ", ")) %>%
    select(group_label, nativity, age_adjusted_rate) %>%
    pivot_wider(names_from = nativity, values_from = age_adjusted_rate) %>%
    filter(!is.na(native), !is.na(foreign)) %>%
    mutate(
        gap = native - foreign,
        group_label = fct_reorder(group_label, gap))

ggplot(dumbbell_data) +
    geom_segment(aes(x = foreign, xend = native, y = group_label, yend = group_label),
                 color = "gray70", linewidth = 1) +
    geom_point(aes(x = native, y = group_label, color = "native"), size = 4) +
    geom_point(aes(x = foreign, y = group_label, color = "foreign"), size = 4) +
    scale_color_manual(values = colors_2, breaks = c("native", "foreign"),
                        labels = c("Native-born", "Foreign-born")) +
    labs(
        title = "Same Race, Different Risks",
        subtitle = "Age-adjusted homicide rate per 100,000 population, by race, ethnicity, and nativity \n Pooled 2014-2024, resident deaths, 50 states and DC",
        x = NULL,
        y = NULL,
        color = NULL,
        caption = "Source: NCHS restricted-use mortality files; ACS via IPUMS") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 20),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 16, color = "gray40"),
    axis.text.y = element_text(size = 16, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/fig.8_same_race_different_risks_dumbbell.png", width = 15, height = 10)

# fig. 9 immigrant share vs. native-born homicide rate by state
stopifnot(abs(sum(standard_weights) - 1) < 1e-6)

# age-adjusted homicide rate by state
age_adjusted_by_state = function(nat, yr = 2024) {
  deaths = homicides %>%
    filter(nativity == nat, data_year == yr, age_recode12 != "12") %>%
    count(state_res_fips, age_recode12, name = "n")

  acs %>%
    filter(nativity == nat, year == yr, !is.na(age_group)) %>%
    mutate(state = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop") %>%
    # start from population so age groups with zero deaths are kept
    left_join(deaths, by = c("state" = "state_res_fips", "age_group" = "age_recode12")) %>%
    mutate(
      n = coalesce(n, 0L),
      weighted_rate = n / pop * 1e5 * standard_weights[as.character(age_group)]) %>%
    group_by(state) %>%
    summarize(
      n = sum(n),
      pop = sum(pop),
      age_adjusted_rate = sum(weighted_rate),
      .groups = "drop") %>%
    mutate(
      crude_rate = n / pop * 1e5,
      suppressed = n < 10,
      across(c(crude_rate, age_adjusted_rate), ~ if_else(suppressed, NA_real_, .x)))
}

native_rate_2024  = age_adjusted_by_state("native")
foreign_rate_2024 = age_adjusted_by_state("foreign")   

# foreign-born share of TOTAL state population
immig_share_2024 = acs %>%
  filter(year == 2024) %>%
  mutate(state = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
  group_by(state) %>%
  summarize(
    immig_pop = sum(perwt[nativity == "foreign"]),
    total_pop = sum(perwt),
    .groups = "drop") %>%
  mutate(immig_share = immig_pop / total_pop * 100)

plot_data_2024 = native_rate_2024 %>%
  left_join(immig_share_2024, by = "state")

r = with(plot_data_2024, cor(immig_share, age_adjusted_rate, use = "complete.obs"))

fig_share = ggplot(plot_data_2024, aes(x = immig_share, y = age_adjusted_rate)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray60", linewidth = 0.6) +
  geom_point(color = colors_2[1], size = 2.4) +
  ggrepel::geom_text_repel(aes(label = state), size = 3) +
  labs(
    title = "Immigrant Share vs. Native-Born Homicide Rate by State",
    subtitle = sprintf("2024, age-adjusted rate per 100,000 native-born population (r = %.2f)", r),
    x = "Immigrants as % of state population",
    y = "Age-adjusted native-born homicide rate per 100,000",
    caption = "Sources: NCHS restricted-use mortality files; American Community Survey. Rates age-adjusted to the 2000 US standard population.") +
  theme_cato

ggsave("results/fig.9_immigrant_share_vs_native_homicide_rates_2024.png",
       fig_share, width = 14, height = 9)

# table a6. age-adjusted us-born and foreign-born homicide rates by state, 2024
# rates based on 1-9 observations suppressed
homicides_age_state_nativity_2024 = homicides %>%
    filter(!is.na(nativity), data_year == 2024, age_recode12 != "12") %>%
    group_by(state_res_fips, nativity, age_recode12) %>%
    summarize(n = n(), .groups = "drop")

acs_age_state_nativity_2024 = acs %>%
    filter(!is.na(nativity), year == 2024, !is.na(age_group)) %>%
    mutate(state_usps = fips_to_usps[sprintf("%02d", as.integer(statefip))]) %>%
    group_by(state_usps, nativity, age_group) %>%
    summarize(pop = sum(perwt), .groups = "drop")

age_specific_state_nativity_2024 = homicides_age_state_nativity_2024 %>%
    left_join(
        acs_age_state_nativity_2024,
        by = c("state_res_fips" = "state_usps", "nativity", "age_recode12" = "age_group")) %>%
    mutate(
        rate_i = n / pop * 100000,
        weight = standard_weights[age_recode12],
        weighted_rate = rate_i * weight)

totals_state_nativity_2024 = age_specific_state_nativity_2024 %>%
    group_by(state_res_fips, nativity) %>%
    summarize(n = sum(n), pop = sum(pop), .groups = "drop") %>%
    mutate(crude_rate = n / pop * 100000)

age_adjusted_state_nativity_2024 = age_specific_state_nativity_2024 %>%
    group_by(state_res_fips, nativity) %>%
    summarize(age_adjusted_rate = sum(weighted_rate), .groups = "drop")

state_rates_by_nativity_2024 = totals_state_nativity_2024 %>%
    left_join(age_adjusted_state_nativity_2024, by = c("state_res_fips", "nativity")) %>%
    mutate(
        suppressed = n > 0 & n < 10,                                   # NCHS suppression: counts 1-9 not reportable
        n = if_else(suppressed, NA_integer_, n),
        crude_rate = if_else(suppressed, NA_real_, crude_rate),
        age_adjusted_rate = if_else(suppressed, NA_real_, age_adjusted_rate)) %>%
    select(state_res_fips, nativity, n, pop, crude_rate, age_adjusted_rate)

print(state_rates_by_nativity_2024, n = Inf)

write_csv(state_rates_by_nativity_2024, "results/age_adjusted_homicide_rates_by_state_nativity_2024.csv")

state_rate_table_2024 = state_rates_by_nativity_2024 %>%
    select(state_res_fips, nativity, age_adjusted_rate) %>%
    pivot_wider(names_from = nativity, values_from = age_adjusted_rate,
                names_glue = "{nativity}_rate") %>%
    rename(state = state_res_fips) %>%
    mutate(
        native_rate = round(native_rate, 2),
        foreign_rate = round(foreign_rate, 2)) %>%
    arrange(state)

print(state_rate_table_2024, n = Inf)

write_csv(state_rate_table_2024, "results/state_rate_table_native_foreign_2024.csv")
