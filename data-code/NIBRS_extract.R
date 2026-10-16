## HOMICIDES EXTRACT ##

# extract murder/nonnegligent manslaughter (UCR offense code 91) from NIBRS extract files
# incidents: any incident with 091 offense 
# victims: individual and law enforcement officer victims whose own offense codes include 091   
# offenders: all offender records in those incidents, including unknown offender

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, matrixStats)

setwd("C:/Users/CarolXu/Who gets killed in America - NVSS and NIBRS")

INPUT_ROOT = "NIBRS-data/input"
OUT_DIR    = "NIBRS-data/output"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

STUDIES = c("2021" = "38807", "2022" = "38925", "2023" = "39270", "2024" = "39868")

## Columns to keep from each raw file

VIC_OFFENSE_COLS = paste0("V40", sprintf("%02d", 7:16))   # V4007-V4016: victim's offense codes
OFFNUM_COLS      = paste0("V40", seq(31, 49, by = 2))     # V4031...V4049: linked offender numbers
REL_COLS         = paste0("V40", seq(32, 50, by = 2))     # V4032...V4050: relationship to offender

VIC_KEEP = c("ORI", "INCNUM", "FIPS_STATE", "BH008",
             "V4006",                                     # victim sequence number
             VIC_OFFENSE_COLS,
             "V4017",                                     # type of victim
             "V4018", "V4019", "V4020", "V4021",          # age, sex, race, ethnicity
             "V4023",                                     # circumstance
             OFFNUM_COLS, REL_COLS)

INC_KEEP = c("ORI", "INCNUM", "FIPS_STATE", "BH008", "INCDATE",
             "V20061", "V20062", "V20063")                # incident offense codes

OFF_KEEP = c("ORI", "INCNUM", "FIPS_STATE",
             "V5006",                                     # offender sequence number
             "V5007", "V5008", "V5009", "V5011")          # age, sex, race, ethnicity


# Loads one raw .rda, keeps needed columns and murder-related rows only, and returns the small subset

load_subset = function(path, keep_cols, segment, year, keys = character(0)) {
  env = new.env()
  nm  = load(path, envir = env)
  nm  = nm[vapply(nm, function(n) is.data.frame(get(n, envir = env)), logical(1))][1]
  df  = get(nm, envir = env)
  rm(list = ls(env), envir = env); rm(env)
  df  = df[, intersect(keep_cols, names(df)), drop = FALSE]
  gc()

  is_murder = function(x) {
    x = trimws(as.character(x))
    grepl("^\\(0*91\\)", x) | x %in% c("91", "091")
  }

  n_all = nrow(df)
  key   = paste(year, df$ORI, trimws(as.character(df$INCNUM)), sep = "_")

  if (segment == "victims") {
    cols = grep("^V40(0[7-9]|1[0-6])$", names(df), value = TRUE)
    keep = Reduce(`|`, lapply(cols, function(cn) is_murder(df[[cn]])))
  } else if (segment == "incidents") {
    cols = intersect(c("V20061", "V20062", "V20063"), names(df))
    keep = Reduce(`|`, lapply(cols, function(cn) is_murder(df[[cn]]))) | key %in% keys
  } else {                                   # offenders
    keep = key %in% keys
  }

  out = df[keep, , drop = FALSE]
  out$incident_key = key[keep]
  list(data = out, n_all = n_all)
}

run_subset = function(path, keep_cols, segment, year, keys = character(0)) {
  message("  ", segment, ": loading ", basename(path), " in background R process ...")
  res = callr::r(load_subset,
                 args = list(path = path, keep_cols = keep_cols, segment = segment,
                             year = year, keys = keys),
                 show = FALSE)
  message("    ", format(res$n_all, big.mark = ","), " rows in file -> ",
          format(nrow(res$data), big.mark = ","), " kept")
  res
}

## Cleaning helpers 

# Strip "(2) " style prefixes; blank -> NA
lab = function(x) {
  x = str_trim(str_remove(as.character(x), "^\\([^)]*\\)\\s*"))
  na_if(x, "")
}

clean_race = function(x) {
  r = lab(x)
  if_else(is.na(r) | str_detect(r, regex("unknown", ignore_case = TRUE)), NA_character_, r)
}

clean_hisp = function(x) case_when(
  str_detect(as.character(x), "^\\(1\\)") ~ "Hispanic",
  str_detect(as.character(x), "^\\(0\\)") ~ "Not Hispanic",
  TRUE ~ NA_character_)

clean_sex = function(x) {
  s = lab(x)
  if_else(s %in% c("Male", "Female"), s, NA_character_)
}

# Victim age: blank = unknown; 0.1/0.2/0.5 = under 1 year -> 0; 99 = 99+
clean_v_age = function(x) {
  a = suppressWarnings(as.numeric(as.character(x)))
  if_else(!is.na(a) & a < 1, 0, a)
}

# Offender age: 0 = unknown
clean_o_age = function(x) {
  a = suppressWarnings(as.numeric(as.character(x)))
  if_else(!is.na(a) & a == 0, NA_real_, a)
}

# Combined race/ethnicity: Hispanic of any race; non-Hispanic by race
race_eth = function(race, hisp) case_when(
  hisp == "Hispanic" ~ "Hispanic",
  hisp == "Not Hispanic" & race == "White" ~ "White, non-Hispanic",
  hisp == "Not Hispanic" & race == "Black or African American" ~ "Black, non-Hispanic",
  hisp == "Not Hispanic" & race == "Asian" ~ "Asian, non-Hispanic",
  hisp == "Not Hispanic" & race == "American Indian or Alaska Native" ~ "AIAN, non-Hispanic",
  hisp == "Not Hispanic" & race == "Native Hawaiian or Other Pacific Islander" ~ "NHPI, non-Hispanic",
  TRUE ~ NA_character_)

## One year 

extract_year = function(year, study) {
  message("\n=== ", year, " (ICPSR ", study, ") ===")
  dir_in = file.path(INPUT_ROOT, paste0("ICPSR_", study))
  f_inc  = file.path(dir_in, "DS0003", paste0(study, "-0003-Data.rda"))
  f_vic  = file.path(dir_in, "DS0004", paste0(study, "-0004-Data.rda"))
  f_off  = file.path(dir_in, "DS0006", paste0(study, "-0006-Data.rda"))
  stopifnot(file.exists(f_inc), file.exists(f_vic), file.exists(f_off))

  ## Victims (all victims with a 091 offense) 
  vic_res = run_subset(f_vic, VIC_KEEP, "victims", year)
  vic     = vic_res$data
  keys_from_victims = unique(vic$incident_key)

  # individuals (1) and law enforcement officers (5): both are persons with demographics   
  is_ind = grepl("^\\(0*[15]\\)", as.character(vic$V4017)) | as.character(vic$V4017) %in% c("1", "5") 

  link_cols  = intersect(OFFNUM_COLS, names(vic))
  rel_cols   = intersect(REL_COLS, names(vic))
  link_names = setNames(link_cols, paste0("off_link_", seq_along(link_cols)))
  rel_names  = setNames(rel_cols,  paste0("rel_",      seq_along(rel_cols)))

  victims_out = vic[is_ind, , drop = FALSE] %>%
    mutate(
      year         = as.integer(year),
      state_fips   = as.integer(as.character(FIPS_STATE)),
      state_abbr   = str_trim(as.character(BH008)),
      victim_seq   = as.integer(as.character(V4006)),
      victim_id    = paste(incident_key, victim_seq, sep = "_"),
      v_age        = clean_v_age(V4018),
      v_sex        = clean_sex(V4019),
      v_race       = clean_race(V4020),
      v_hisp       = clean_hisp(V4021),
      v_race_eth   = race_eth(v_race, v_hisp),
      circumstance = lab(V4023)
    ) %>%
    select(year, incident_key, state_fips, state_abbr, victim_seq, victim_id,
           v_age, v_sex, v_race, v_hisp, v_race_eth, circumstance,
           all_of(link_names), all_of(rel_names)) %>%
    mutate(across(starts_with("off_link_"), ~ suppressWarnings(as.integer(as.character(.x)))),
           across(starts_with("rel_"), lab))

  ## Incidents (091 in offense slots, or any 091 victim) --
  inc_res = run_subset(f_inc, INC_KEEP, "incidents", year, keys = keys_from_victims)

  incidents_out = inc_res$data %>%
    transmute(
      year       = as.integer(year),
      incident_key,
      ori        = as.character(ORI),
      state_fips = as.integer(as.character(FIPS_STATE)),
      state_abbr = str_trim(as.character(BH008)),
      inc_date   = as.character(INCDATE)
    ) %>%
    distinct(incident_key, .keep_all = TRUE)

  victims_out = victims_out %>% filter(incident_key %in% incidents_out$incident_key)

  ## Offenders (all offender records in murder incidents) --
  off_res = run_subset(f_off, OFF_KEEP, "offenders", year, keys = incidents_out$incident_key)

  offenders_out = off_res$data %>%
    transmute(
      year        = as.integer(year),
      incident_key,
      state_fips  = as.integer(as.character(FIPS_STATE)),
      off_seq     = as.integer(as.character(V5006)),
      offender_id = paste(incident_key, off_seq, sep = "_"),
      known       = !is.na(off_seq) & off_seq != 0,
      o_age       = clean_o_age(V5007),
      o_sex       = clean_sex(V5008),
      o_race      = clean_race(V5009),
      o_hisp      = clean_hisp(V5011),
      o_race_eth  = race_eth(o_race, o_hisp)
    )

  summary = tibble(
    year                    = as.integer(year),
    incidents_all           = inc_res$n_all,
    murder_incidents        = nrow(incidents_out),
    victim_records_all      = vic_res$n_all,
    murder_victims          = nrow(victims_out),
    offender_records_all    = off_res$n_all,
    offender_records_murder = nrow(offenders_out),
    known_offenders         = sum(offenders_out$known)
  )
  print(summary)

  list(incidents = incidents_out, victims = victims_out,
       offenders = offenders_out, summary = summary)
}

## Run each year (saved as it finishes; skipped if already done)

for (yr in names(STUDIES)) {
  f_year = file.path(OUT_DIR, paste0("year_", yr, ".rds"))
  if (file.exists(f_year)) {
    message("\n", yr, " already extracted, skipping (delete ", f_year, " to redo)")
    next
  }
  res_year = extract_year(yr, STUDIES[[yr]])
  saveRDS(res_year, f_year)
  rm(res_year); gc()
}

## Stack all years 

res = map(names(STUDIES), ~ readRDS(file.path(OUT_DIR, paste0("year_", .x, ".rds"))))

incidents = map_dfr(res, "incidents")
victims   = map_dfr(res, "victims")
offenders = map_dfr(res, "offenders")
summary   = map_dfr(res, "summary")
rm(res); gc()

saveRDS(incidents, file.path(OUT_DIR, "incidents_murder_2021_2024.rds"))
saveRDS(victims,   file.path(OUT_DIR, "victims_murder_2021_2024.rds"))
saveRDS(offenders, file.path(OUT_DIR, "offenders_murder_2021_2024.rds"))
write_csv(incidents, file.path(OUT_DIR, "incidents_murder_2021_2024.csv"), na = "")
write_csv(victims,   file.path(OUT_DIR, "victims_murder_2021_2024.csv"),   na = "")
write_csv(offenders, file.path(OUT_DIR, "offenders_murder_2021_2024.csv"), na = "")
write_csv(summary,   file.path(OUT_DIR, "extract_summary.csv"))

print(summary, width = Inf)
