#######################################################################
# victimization and offending rates by race and ethnicity

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats, scales)
count = dplyr::count

setwd("C:/Users/CarolXu/Who gets killed in America - NVSS and NIBRS")

# read in cleaned homicide extracts + ACS
incidents = readRDS("NIBRS-data/output/incidents_murder_2021_2024.rds") 

victims = readRDS("NIBRS-data/output/victims_murder_2021_2024.rds")

offenders = readRDS("NIBRS-data/output/offenders_murder_2021_2024.rds")

acs = readRDS("NIBRS-data/output/acs_nibrs.rds")

#######################################################################

# TABLE A2. Victimization and offending rates by race and ethnicity, with unknowns
# victims by race and ethnicity
victims_race_eth = victims %>%
  mutate(
    race = replace_na(v_race, "Unknown"),
    ethnicity = case_when(
      v_hisp == "Hispanic" ~ "Hispanic/Latino",
      v_hisp == "Not Hispanic" ~ "Not Hispanic/Latino",
      TRUE ~ "Unknown"
    ),
    race_ethnicity = paste0(race, ", ", ethnicity)
  ) %>%
  count(year, race_ethnicity, name = "n_victims")

acs_race_eth = acs %>%
  mutate(race_ethnicity = as.character(race_ethnicity_nibrs)) %>%
  group_by(year, race_ethnicity) %>%
  summarise(acs_pop = sum(perwt, na.rm = TRUE), .groups = "drop")

victims_rates = victims_race_eth %>%
  full_join(acs_race_eth, by = c("year", "race_ethnicity")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    victim_rate_per_100k = round(n_victims / acs_pop * 100000, 2)
  ) %>%
  arrange(year, race_ethnicity)

victims_totals = victims_rates %>%
  group_by(year) %>%
  summarise(
    race_ethnicity = "Total",
    n_victims = sum(n_victims),
    acs_pop = sum(acs_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(victim_rate_per_100k = round(n_victims / acs_pop * 100000, 2))

victims_rates = bind_rows(victims_rates, victims_totals) %>%
  arrange(year, race_ethnicity == "Total", race_ethnicity)

# offenders by race and ethnicity
offenders_race_eth = offenders %>%
  mutate(
    race = replace_na(o_race, "Unknown"),
    ethnicity = case_when(
      o_hisp == "Hispanic" ~ "Hispanic/Latino",
      o_hisp == "Not Hispanic" ~ "Not Hispanic/Latino",
      TRUE ~ "Unknown"
    ),
    race_ethnicity = if_else(known, paste0(race, ", ", ethnicity), "No offender information")
  ) %>%
  count(year, race_ethnicity, name = "n_offenders")

offenders_rates = offenders_race_eth %>%
  full_join(acs_race_eth, by = c("year", "race_ethnicity")) %>%
  mutate(
    n_offenders = replace_na(n_offenders, 0),
    offender_rate_per_100k = round(n_offenders / acs_pop * 100000, 2)
  ) %>%
  arrange(year, race_ethnicity)

offenders_totals = offenders_rates %>%
  group_by(year) %>%
  summarise(
    race_ethnicity = "Total",
    n_offenders = sum(n_offenders),
    acs_pop = sum(acs_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(offender_rate_per_100k = round(n_offenders / acs_pop * 100000, 2))

offenders_rates = bind_rows(offenders_rates, offenders_totals) %>%
  arrange(year, race_ethnicity == "Total", race_ethnicity)

# pool 2021-2024
pooled_base = victims_rates %>%
  filter(race_ethnicity != "Total") %>%
  select(year, race_ethnicity, n_victims, acs_pop) %>%
  full_join(
    offenders_rates %>%
      filter(race_ethnicity != "Total") %>%
      select(year, race_ethnicity, n_offenders),
    by = c("year", "race_ethnicity")
  ) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    # main-text grouping: unknown ethnicity (known race) assigned non-Hispanic
    race_ethnicity_main = if_else(
      !str_starts(race_ethnicity, "Unknown") & str_ends(race_ethnicity, ", Unknown"),
      str_replace(race_ethnicity, ", Unknown$", ", Not Hispanic/Latino"),
      race_ethnicity))

make_pooled = function(df, group_var) {
  pooled = df %>%
    group_by(group = .data[[group_var]]) %>%
    summarise(
      n_victims = sum(n_victims),
      n_offenders = sum(n_offenders),
      acs_pop = sum(acs_pop, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(acs_pop = na_if(acs_pop, 0))

  totals = pooled %>%
    summarise(
      group = "Total",
      n_victims = sum(n_victims),
      n_offenders = sum(n_offenders),
      acs_pop = sum(acs_pop, na.rm = TRUE)
    )

  bind_rows(pooled, totals) %>%
    mutate(
      victim_rate_per_100k = n_victims / acs_pop * 100000,
      offender_rate_per_100k = n_offenders / acs_pop * 100000,
      offender_to_victim_ratio = round(offender_rate_per_100k / victim_rate_per_100k, 2),
      victim_rate_per_100k = round(victim_rate_per_100k, 2),
      offender_rate_per_100k = round(offender_rate_per_100k, 2)
    ) %>%
    rename(!!group_var := group) %>%
    arrange(.data[[group_var]] == "Total", .data[[group_var]]) %>%
    as_tibble()
}

race_ethnicity_merged_unused = make_pooled(pooled_base, "race_ethnicity_main")

table_a2 = make_pooled(pooled_base, "race_ethnicity")

print(table_a2, n = Inf, width = Inf)

write_csv(table_a2, "results/table_a2_victims_offenders_race_ethnicity_2021_2024.csv")

# smell check FBI expanded homicide table 6, 2026
# single victim-offender pairs by race/ethnicity
svso_2024 = victims %>%
  filter(year == 2024) %>%
  add_count(incident_key, name = "n_victims") %>%
  filter(n_victims == 1) %>%
  inner_join(
    offenders %>%
      filter(year == 2024) %>%
      add_count(incident_key, name = "n_offenders") %>%
      filter(n_offenders == 1, known) %>%
      select(incident_key, o_race),
    by = "incident_key"
  ) %>%
  mutate(
    victim_race = case_when(
      v_race == "White" ~ "White",
      v_race == "Black or African American" ~ "Black",
      TRUE ~ "Other/Unknown"
    ),
    offender_race = case_when(
      o_race == "White" ~ "White",
      o_race == "Black or African American" ~ "Black",
      !is.na(o_race) ~ "Other",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(victim_race %in% c("White", "Black")) %>%
  count(victim_race, offender_race) %>%
  pivot_wider(names_from = offender_race, values_from = n, values_fill = 0) %>%
  mutate(Total = White + Black + Other + Unknown) %>%
  select(victim_race, Total, White, Black, Other, Unknown)

print(svso_2024, n = Inf, width = Inf)

# incidents with no offender info?

victims_per_incident = victims %>%
  count(year, incident_key, name = "n_victims")

offenders_per_incident = offenders %>%
  group_by(year, incident_key) %>%
  summarise(n_offenders = sum(known), .groups = "drop")

incident_counts = incidents %>%
  select(year, incident_key) %>%
  left_join(victims_per_incident, by = c("year", "incident_key")) %>%
  left_join(offenders_per_incident, by = c("year", "incident_key")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victims_cat = case_when(n_victims >= 4 ~ "4+", TRUE ~ as.character(n_victims)),
    offenders_cat = case_when(
      n_offenders == 0 ~ "No offender information",
      n_offenders >= 4 ~ "4+",
      TRUE ~ as.character(n_offenders)))

incidents_by_vo = incident_counts %>%
  count(victims_cat, offenders_cat) %>%
  pivot_wider(names_from = offenders_cat, values_from = n, values_fill = 0) %>%
  select(victims_cat, `1`, `2`, `3`, `4+`, `No offender information`) %>%
  mutate(Total = rowSums(across(-victims_cat)))

print(incidents_by_vo, n = Inf, width = Inf)

write_csv(incidents_by_vo, "results/incidents_by_n_victims_offenders_2021_2024.csv")

## TABLE A3a abd A3b. Homicide victim-offender pairs by race / by ethnicity
# one row per victim x linked known offender
links = victims %>%
  select(year, incident_key, victim_id, v_race, v_hisp, starts_with("off_link_")) %>%
  pivot_longer(starts_with("off_link_"), names_to = NULL, values_to = "off_seq") %>%
  filter(!is.na(off_seq), off_seq != 0) %>%
  distinct()

pairs = links %>%
  inner_join(
    offenders %>%
      filter(known) %>%
      select(incident_key, off_seq, o_race, o_hisp),
    by = c("incident_key", "off_seq")
  )

race_group = function(x) {
  case_when(
    x == "White" ~ "White",
    x == "Black or African American" ~ "Black",
    x == "Asian" ~ "Asian",
    x %in% c("American Indian or Alaska Native", "Native Hawaiian or Other Pacific Islander") ~ "Other",
    TRUE ~ "Unknown"
  )
}

eth_group = function(x) {
  case_when(
    x == "Hispanic" ~ "Hispanic or Latino",
    x == "Not Hispanic" ~ "Not Hispanic or Latino",
    TRUE ~ "Unknown"
  )
}

pairs = pairs %>%
  mutate(
    v_race_group = race_group(v_race),
    o_race_group = race_group(o_race),
    v_eth_group = eth_group(v_hisp),
    o_eth_group = eth_group(o_hisp)
  )

race_levels = c("White", "Black", "Asian", "Other", "Unknown")

table_a3a = pairs %>%
  count(v_race_group, o_race_group) %>%
  pivot_wider(names_from = o_race_group, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(any_of(race_levels)))) %>%
  mutate(v_race_group = factor(v_race_group, levels = race_levels)) %>%
  arrange(v_race_group) %>%
  mutate(v_race_group = recode(as.character(v_race_group), "Black" = "Black or African American")) %>%
  select(`Race of victim` = v_race_group, Total, any_of(race_levels))

print(table_a3a, width = Inf)
write_csv(table_a3a, "results/table_a3a_race_victim_by_offender_2021_2024.csv")

table_a3a %>%
  filter(`Race of victim` != "Unknown") %>%
  mutate(
    same_race = case_when(
      `Race of victim` == "White" ~ White,
      `Race of victim` == "Black or African American" ~ Black,
      `Race of victim` == "Asian" ~ Asian,
      `Race of victim` == "Other" ~ Other
    ),
    known_offenders = Total - Unknown
  ) %>%
  summarise(
    same_race = sum(same_race),
    known_offenders = sum(known_offenders),
    pct_same_race = round(100 * same_race / known_offenders, 1)
  )

eth_levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Unknown")

table_a3b = pairs %>%
  count(v_eth_group, o_eth_group) %>%
  pivot_wider(names_from = o_eth_group, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(any_of(eth_levels)))) %>%
  mutate(v_eth_group = factor(v_eth_group, levels = eth_levels)) %>%
  arrange(v_eth_group) %>%
  select(`Ethnicity of victim` = v_eth_group, Total, any_of(eth_levels))

print(table_a3b, width = Inf)
write_csv(table_a3b, "results/table_a3b_ethnicity_victim_by_offender_2021_2024.csv")

table_a3b %>%
  filter(`Ethnicity of victim` != "Unknown") %>%
  mutate(
    same_ethnicity = case_when(
      `Ethnicity of victim` == "Hispanic or Latino" ~ `Hispanic or Latino`,
      `Ethnicity of victim` == "Not Hispanic or Latino" ~ `Not Hispanic or Latino`
    ),
    known_offenders = Total - Unknown
  ) %>%
  summarise(
    same_ethnicity = sum(same_ethnicity),
    known_offenders = sum(known_offenders),
    pct_same_ethnicity = round(100 * same_ethnicity / known_offenders, 1)
  )

race_ethnicity_combo = function(race, hisp) case_when(
  hisp == "Hispanic" ~ "Hispanic (any race)",
  hisp == "Not Hispanic" & race == "White" ~ "White, non-Hispanic",
  hisp == "Not Hispanic" & race == "Black or African American" ~ "Black, non-Hispanic",
  hisp == "Not Hispanic" & race == "Asian" ~ "Asian, non-Hispanic",
  hisp == "Not Hispanic" & race %in% c("American Indian or Alaska Native",
                                        "Native Hawaiian or Other Pacific Islander") ~ "Other, non-Hispanic",
  TRUE ~ "Unknown"
)

pairs = pairs %>%
  mutate(
    v_race_ethnicity = race_ethnicity_combo(v_race, v_hisp),
    same_race = if_else(v_race_group != "Unknown" & o_race_group != "Unknown",
                        v_race_group == o_race_group, NA),
    same_ethnicity = if_else(v_eth_group != "Unknown" & o_eth_group != "Unknown",
                             v_eth_group == o_eth_group, NA)
  )

combo_levels = c("White, non-Hispanic", "Black, non-Hispanic", "Asian, non-Hispanic",
                 "Other, non-Hispanic", "Hispanic (any race)", "Unknown")

table_1 = pairs %>%
  mutate(v_race_ethnicity = factor(v_race_ethnicity, levels = combo_levels)) %>%
  group_by(v_race_ethnicity) %>%
  summarise(
    total = n(),
    same_race_n = sum(same_race, na.rm = TRUE),
    total_race_known = sum(!is.na(same_race)),
    same_ethnicity_n = sum(same_ethnicity, na.rm = TRUE),
    total_eth_known = sum(!is.na(same_ethnicity)),
    .groups = "drop"
  ) %>%
  bind_rows(
    pairs %>%
      summarise(
        v_race_ethnicity = "Total known",
        total = n(),
        same_race_n = sum(same_race, na.rm = TRUE),
        total_race_known = sum(!is.na(same_race)),
        same_ethnicity_n = sum(same_ethnicity, na.rm = TRUE),
        total_eth_known = sum(!is.na(same_ethnicity))
      )
  ) %>%
  mutate(
    pct_same_race = round(100 * same_race_n / total_race_known, 1),
    pct_same_ethnicity = round(100 * same_ethnicity_n / total_eth_known, 1)
  ) %>%
  select(`Race/ethnicity of victim` = v_race_ethnicity,
         Total = total,
         `Same race (%)` = pct_same_race,
         `Same ethnicity (%)` = pct_same_ethnicity)

print(table_1, n = Inf, width = Inf)
write_csv(table_1, "results/table_1_victim_offender_same_race_ethnicity_2021_2024.csv")


## FIGURE 2. Victim-offender relationship + table A4

relationship_group = function(x) {
  case_when(
    is.na(x) ~ "Unknown relationship",
    x == "Relationship Unknown" ~ "Unknown relationship",
    x == "Victim was Offender" ~ "Victim was also an offender",
    x %in% c(
      "Victim was Spouse", "Victim was Common-Law Spouse",
      "Victim was Boyfriend/Girlfriend", "Victim was Ex-Spouse",
      "Victim was Ex-relationship (Ex-boyfriend/ex-girlfriend)"
    ) ~ "Intimate partner",
    x %in% c(
      "Victim was Parent", "Victim was Child", "Victim was Sibling",
      "Victim was Grandparent", "Victim was Grandchild", "Victim was In-Law",
      "Victim was Stepparent", "Victim was Stepchild", "Victim was Stepsibling",
      "Victim was Child of Boyfriend/Girlfriend",
      "Victim was Other Family Member"
    ) ~ "Family",
    x %in% c(
      "Victim was Acquaintance", "Victim was Friend", "Victim was Neighbor",
      "Victim was Employee", "Victim was Employer",
      "Cohabitant (non-intimate relationship)",
      "Victim was Babysittee (the baby)",
      "Victim was Babysitee/Child in the Care of a Babysitter",
      "Victim was Otherwise Known"
    ) ~ "Acquaintance or otherwise known",
    x == "Victim was Stranger" ~ "Stranger",
    TRUE ~ "Unknown relationship"
  )
}

relationships = victims %>%
  select(year, incident_key, victim_id, starts_with("off_link_"), starts_with("rel_")) %>%
  pivot_longer(
    cols = c(starts_with("off_link_"), starts_with("rel_")),
    names_to = c(".value", "slot"),
    names_pattern = "(off_link|rel)_(\\d+)"
  ) %>%
  filter(!is.na(off_link), off_link != 0) %>%
  inner_join(
    offenders %>% filter(known) %>% select(incident_key, off_seq),
    by = c("incident_key", "off_link" = "off_seq")
  ) %>%
  mutate(relationship_group = relationship_group(rel))

relationships %>% count(rel, relationship_group) %>% arrange(relationship_group) %>% print(n = Inf)

# TABLE A4. Murder victims by relationship to offender
rel_hierarchy_all = c("Intimate partner", "Family", "Acquaintance or otherwise known",
                      "Victim was also an offender", "Stranger", "Unknown relationship")

table_a4 = relationships %>%
  mutate(rel_rank = match(relationship_group, rel_hierarchy_all)) %>%
  group_by(victim_id) %>%
  summarise(relationship_group = rel_hierarchy_all[min(rel_rank)], .groups = "drop") %>%
  count(relationship_group, name = "victims") %>%
  mutate(
    relationship_group = factor(relationship_group, levels = rel_hierarchy_all),
    pct_of_victims = round(100 * victims / sum(victims), 1),
    pct_of_known_relationship = if_else(
      relationship_group == "Unknown relationship", NA_real_,
      round(100 * victims / sum(victims[relationship_group != "Unknown relationship"]), 1)
    )
  ) %>%
  arrange(relationship_group)

print(table_a4, width = Inf)

write_csv(table_a4, "results/table_a4_relationship_victims_2021_2024.csv")

# FIGURE 2 data -- known relationships only, re-based shares
rel_hierarchy = c("Intimate partner", "Family", "Acquaintance or otherwise known",
                  "Victim was also an offender", "Stranger")

relationship_victims = relationships %>%
  filter(relationship_group != "Unknown relationship") %>%
  mutate(rel_rank = match(relationship_group, rel_hierarchy)) %>%
  group_by(victim_id) %>%
  summarise(relationship_group = rel_hierarchy[min(rel_rank)], .groups = "drop") %>%
  count(relationship_group, name = "victims") %>%
  mutate(
    relationship_group = factor(relationship_group, levels = rel_hierarchy),
    pct_of_victims = round(100 * victims / sum(victims), 1)
  ) %>%
  arrange(relationship_group)

print(relationship_victims)
write_csv(relationship_victims, "results/figure_2_relationship_victims_2021_2024.csv")

# share of victims (with a known offender) whose relationship is unknown for every offender
victims_relationship_status = relationships %>%
  group_by(victim_id) %>%
  summarise(any_known = any(relationship_group != "Unknown relationship"), .groups = "drop")

pct_unknown_victims = round(100 * mean(!victims_relationship_status$any_known))
n_known_victims = sum(victims_relationship_status$any_known)

pct_unknown_victims
n_known_victims

# FIGURE 2 
relationship_plot = relationship_victims %>%
  mutate(
    relationship_group = recode(as.character(relationship_group),
                                "Acquaintance or otherwise known" = "Acquaintance or\notherwise known",
                                "Victim was also an offender" = "Victim was also\nan offender"),
    relationship_group = fct_reorder(relationship_group, pct_of_victims)
  )

ggplot(relationship_plot, aes(x = pct_of_victims, y = relationship_group)) +
  geom_col(fill = "#3043B4", width = 0.7) +
  geom_text(aes(label = paste0(round(pct_of_victims, 1), "%")),
            hjust = -0.15, size = 6, color = "gray20") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Most homicide victims were killed by someone they knew",
    subtitle = paste0("Relationship of victim to offender, share of victims with known relationship, 2021-2024\n",
                      "Relationship was unknown for ", pct_unknown_victims, "% of victims with a known offender"),
    x = NULL, y = NULL,
    caption = paste0(
      "Source: Bureau of Justice Statistics, NIBRS Extract Files, 2021-2024, via ICPSR. Murder and nonnegligent manslaughter (UCR 091).\n",
      "Victims with more than one offender are assigned their closest relationship (intimate partner, family, acquaintance, then stranger).\nVictims with no known offender or unknown relationship are excluded."    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
    axis.text.y = element_text(size = 15, color = "gray20"),
    axis.text.x = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("results/figure_2_relationship_victims_2021_2024.png", width = 11, height = 6.5)

# FIGURE 2 v2 - pie chart
relationship_pie_data = relationship_victims %>%
  mutate(group = if_else(
    relationship_group %in% c("Intimate partner", "Family", "Acquaintance or otherwise known"),
    "Knew offender", "Other")) %>%
  group_by(group) %>%
  summarise(victims = sum(victims), .groups = "drop") %>%
  mutate(
    group = factor(group, levels = c("Knew offender", "Other")),
    pct_of_victims = round(100 * victims / sum(victims), 1)) %>%
  arrange(group) %>%
  mutate(
    ymax = cumsum(victims),
    ymin = lag(ymax, default = 0),
    ymid = (ymin + ymax) / 2)

subgroup_lookup = relationship_victims %>%
  select(relationship_group, pct_of_victims) %>%
  deframe()

subgroup_labels = tibble(
  group = factor(c("Knew offender", "Other"), levels = c("Knew offender", "Other")),
  label = c(
    paste0(
      "Intimate partner: ", subgroup_lookup[["Intimate partner"]], "%\n",
      "Family: ", subgroup_lookup[["Family"]], "%\n",
      "Acquaintance or otherwise known: ", subgroup_lookup[["Acquaintance or otherwise known"]], "%"),
    paste0(
      "Stranger: ", subgroup_lookup[["Stranger"]], "%\n",
      "Victim was also an offender: ", subgroup_lookup[["Victim was also an offender"]], "%"))) %>%
  left_join(relationship_pie_data %>% select(group, ymid), by = "group")

colors_relationship = c("Knew offender" = "#3043B4", "Other" = "#C97703")

ggplot(relationship_pie_data, aes(x = 1, y = victims, fill = group)) +
  geom_col(width = 2, color = "white", linewidth = 1, position = position_stack(reverse = TRUE)) +
  coord_polar(theta = "y") +
  xlim(0, 3.2) +
  scale_fill_manual(values = colors_relationship) +
  geom_text(aes(x = 1, y = ymid, label = paste0(pct_of_victims, "%")),
            size = 9, color = "white", fontface = "bold") +
  geom_text(data = subgroup_labels, aes(x = 2.6, y = ymid, label = label),
            size = 5, color = "gray30", lineheight = 1.1, inherit.aes = FALSE) +
  labs(
    title = "Most homicide victims were killed by someone they knew",
    subtitle = paste0("Relationship of victim to offender, share of victims with known relationship, 2021-2024\n",
                      "Relationship was unknown for ", pct_unknown_victims, "% of victims with a known offender"),
    fill = NULL,
    caption = paste0(
      "Source: Bureau of Justice Statistics, NIBRS Extract Files, 2021-2024, via ICPSR. Murder and nonnegligent manslaughter (UCR 091).\n",
      "Victims with more than one offender are assigned their closest relationship (intimate partner, family, acquaintance, then stranger).\nVictims with no known offender or unknown relationship are excluded.")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 14),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/figure_2v3_relationship_pie_with_subgroup_labels_2021_2024.png", width = 10, height = 9)

colors_relationship_all = c(
  "Intimate partner"                = "#1B2560",
  "Family"                          = "#3043B4",
  "Acquaintance or otherwise known" = "#7B8AD6",
  "Victim was also an offender"     = "#E3A94D",
  "Stranger"                        = "#C97703")

ggplot(relationship_victims, aes(x = "", y = victims, fill = relationship_group)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = colors_relationship_all) +
  geom_text(aes(label = paste0(pct_of_victims, "%")),
            position = position_stack(vjust = 0.5), size = 5, color = "white", fontface = "bold") +
  labs(
    title = "Most homicide victims were killed by someone they knew",
    subtitle = paste0("Relationship of victim to offender, share of victims with known relationship, 2021-2024\n",
                      "Relationship was unknown for ", pct_unknown_victims, "% of victims with a known offender"),
    fill = NULL,
    caption = paste0(
      "Source: Bureau of Justice Statistics, NIBRS Extract Files, 2021-2024, via ICPSR. Murder and nonnegligent manslaughter (UCR 091).\n",
      "Victims with more than one offender are assigned their closest relationship (intimate partner, family, acquaintance, then stranger).\nVictims with no known offender or unknown relationship are excluded.")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "right",
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/figure_2v4_relationship_pie_all_categories_2021_2024.png", width = 10, height = 9)

## FIGURE 1. Groups with higher murder victimization rates have higher offending rates
age_keep = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")

age_group = function(age) {
  case_when(
    is.na(age) ~ "Unknown",
    age >= 80 ~ "80+",
    TRUE ~ paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4)
  )
}

victims_cells = victims %>%
  mutate(
    race = replace_na(v_race, "Unknown"),
    ethnicity = case_when(
      v_hisp == "Hispanic" ~ "Hispanic/Latino",
      v_hisp == "Not Hispanic" ~ "Not Hispanic/Latino",
      race != "Unknown" ~ "Not Hispanic/Latino",   # known race, unknown ethnicity -> non-Hispanic
      TRUE ~ "Unknown"
    ),
    race_ethnicity = paste0(race, ", ", ethnicity),
    age_group_5yr = age_group(v_age),
    sex = replace_na(v_sex, "Unknown")
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, name = "n_victims")

offenders_cells = offenders %>%
  filter(known) %>%
  mutate(
    race = replace_na(o_race, "Unknown"),
    ethnicity = case_when(
      o_hisp == "Hispanic" ~ "Hispanic/Latino",
      o_hisp == "Not Hispanic" ~ "Not Hispanic/Latino",
      race != "Unknown" ~ "Not Hispanic/Latino",
      TRUE ~ "Unknown"
    ),
    race_ethnicity = paste0(race, ", ", ethnicity),
    age_group_5yr = age_group(o_age),
    sex = replace_na(o_sex, "Unknown")
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, name = "n_offenders")

acs_cells = acs %>%
  mutate(
    race_ethnicity = as.character(race_ethnicity_nibrs),
    age_group_5yr = as.character(age_group_5yr),
    sex = sex_nibrs
  ) %>%
  group_by(year, race_ethnicity, age_group_5yr, sex) %>%
  summarise(acs_pop = sum(perwt, na.rm = TRUE), .groups = "drop")

cells = victims_cells %>%
  full_join(offenders_cells, by = c("year", "race_ethnicity", "age_group_5yr", "sex")) %>%
  full_join(acs_cells, by = c("year", "race_ethnicity", "age_group_5yr", "sex")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / acs_pop * 100000,
    offender_rate_per_100k = n_offenders / acs_pop * 100000
  ) %>%
  as_tibble()

write_csv(cells, "results/cells_year_race_ethnicity_age_sex.csv")

# how many victims/offenders fall in cells with no ACS match
cells %>%
  summarise(
    victims_unknown_cells = sum(n_victims[is.na(acs_pop)]),
    offenders_unknown_cells = sum(n_offenders[is.na(acs_pop)]),
    pct_victims_unknown = round(100 * victims_unknown_cells / sum(n_victims), 1),
    pct_offenders_unknown = round(100 * offenders_unknown_cells / sum(n_offenders), 1)
  ) %>% print()

# regression data: ages 15-54, cells with an ACS population
reg_data = cells %>%
  filter(!is.na(acs_pop), age_group_5yr %in% age_keep) %>%
  mutate(year = factor(year))

nrow(reg_data)

scatter_data = reg_data
r_value = round(cor(scatter_data$victim_rate_per_100k, scatter_data$offender_rate_per_100k), 2)

ggplot(scatter_data, aes(x = victim_rate_per_100k, y = offender_rate_per_100k)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray60") +
  geom_point(aes(color = year, alpha = acs_pop), size = 3) +
  scale_color_manual(values = c("2021" = "#3043B4", "2022" = "#E3A72F",
                                "2023" = "#2CA58D", "2024" = "#B5446E")) +
    scale_alpha_continuous(range = c(0.15, 1), trans = "log10", guide = "none") + 
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 4))) +
  annotate("text", x = 0, y = max(scatter_data$offender_rate_per_100k),
           label = paste0("Correlation = ", r_value), hjust = 0, size = 5, color = "gray30") +
  labs(
    title = "Groups with higher murder victimization rates have higher offending rates",
    subtitle = "Murder victimization and offending rates per 100,000 by race, ethnicity, age, and sex, ages 15-54, 2021-2024",
    x = "Victimization rate per 100,000",
    y = "Offending rate per 100,000",
    color = "Year",
    caption = paste0(
      "Source: Bureau of Justice Statistics, NIBRS Extract Files, 2021-2024, via ICPSR; American Community Survey, 2021-2024, via IPUMS.\n",
      "Murder and nonnegligent manslaughter (UCR 091). Each point is a race, ethnicity, age, and sex group in one year; darker points have larger populations.\n",
      " Victims and offenders with known race but unknown ethnicity are counted as non-Hispanic. Those with unknown race are excluded."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0, margin = margin(b = 12)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    axis.title = element_text(size = 14, color = "gray20"),
    axis.text = element_text(size = 12, color = "gray40"),
    legend.position = "top",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("results/figure_1_scatter_offending_vs_victimization_2021_2024.png", width = 13, height = 9, dpi = 300)
