library(purrr)
library(stringr)
library(forcats)
library(tidyr)
library(dplyr)
library(tibble)

# Taken from the tables (pp. 10-12) in the report:
#    NSW Arbovirus Surveillance and Mosquito Monitoring 2022-2023
#    Weekly Update: Week ending 25 February 2023
#    (Report Number 19)
#    https://www.health.nsw.gov.au/environment/pests/vector/Publications/nswasp-weekly-report-2023-02-25.pdf
raw_counts <- list(
  c("Albury", "lhml", "....", "lhml"),
  c("Armidale", "lll.", "....", "mll."),
  c("Balranald", ".lll", "....", ".lll"),
  c("Bourke", "....", "....", "...."),
  c("Cootamundra", ".l.l", "....", ".l.l"),
  c("Corowa", ".hhl", "....", ".hhl"),
  c("Deniliquin", ".hhl", "....", ".hhl"),
  c("Forbes", ".hhl", "....", ".hhl"),
  c("Goulburn", "...l", "....", "...l"),
  c("Griffith", "vvvh", "....", "vvvh"),
  c("Grong Grong", ".h.h", "....", ".h.h"),
  c("Leeton", ".h.h", "....", ".h.h"),
  c("Macquarie Marshes", "lhll", "....", "lhmm"),
  c("Mathoura", "..h.", "....", "..h."),
  c("Menindee", "vvmh", "....", "vvmh"),
  c("Moama", "..h.", "....", "..h."),
  c("Moree", "mm.l", "....", "hh.l"),
  c("Murrumbidgee", "hhlh", "....", "hhlh"),
  c("Narrabi", "ll..", "....", "ll.."),
  c("Narrendera", ".hlh", "....", ".hlh"),
  c("Temora", ".hhl", "....", ".hhl"),
  c("Wagga Wagga", ".mhl", "....", ".mhm"),
  c("Walgett", ".lll", "....", ".lll"),
  c("West Wyalong", ".ll.", "....", ".ll."),
  c("Wilcannia", "hlmm", "....", "hlmm"),
  c("Yass", ".lll", "....", ".lll"),
  c("Young", ".lll", "....", ".llm"),

  c("Ballina", ".lll", ".lll", ".mhh"),
  c("Bega", ".lll", ".lll", ".lhh"),
  c("Byron Bay", "llll", "llll", "llll"),
  c("Coffs Harbour", "...l", "...l", "...l"),
  c("Gosford", "lll.", "lll.", "mmm."),
  c("Kempsey", ".lll", ".lll", ".lll"),
  c("Kiama", ".ll.", ".ll.", ".ll."),
  c("Lake Cathie", "llll", "llll", "mlhm"),
  c("Lismore", "....", "....", "...."),
  c("Milbank", ".lll", ".lll", ".lll"),
  c("Mullumbimby", ".ll.", ".ll.", ".ll."),
  c("Murwillumbah", ".lll", ".lll", ".lll"),
  c("Numbucca", "....", "....", "...."),
  c("Newcastle", "llll", "llll", "hmhh"),
  c("Port Macquarie", "llll", "llll", "llll"),
  c("Shoalhaven", ".lll", ".lll", ".lll"),
  c("Tweed Heads", ".lll", ".lll", ".llm"),
  c("Wauchope", "llll", "llll", "llll"),
  c("Wollongong", "....", "....", "...."),
  c("Wyong", "llll", "llll", "lmmm"),

  c("Bankstown", "llll", "llll", "lllm"),
  c("Blacktown", "llll", "llll", "llll"),
  c("Camden", ".lll", ".lll", ".lll"),
  c("Canada Bay", "llll", "lhll", "lhml"),
  c("Earlwood", "llll", "llll", "llll"),
  c("Georges River", "llll", "llll", "mlll"),
  c("Hawkesbury", ".lll", ".lll", ".lll"),
  c("Hills Shire", "lll.", "lll.", "mll."),
  c("Liverpool", "llll", "llll", "mhmm"),
  c("Northern Beaches", "llll", "llll", "llll"),
  c("Parramatta", "llll", "mhhl", "hhhh"),
  c("Penrith", "llll", "llll", "llml"),
  c("Sydney Olympic Park", "llml", "llll", "mhmh")
)

# taken from Wikipedia as of 15 Jan 2024, in WGS 84 CRS
sites <- tribble(
  ~location, ~type, ~lat, ~long,
  "Albury", "inland", "-36 04 50", "146 54 57",
  "Armidale", "inland", "-30 30 0", "151 39 0",
  "Ballina", "coastal", "-28 51 49", "153 31 58",
  "Balranald", "inland", "-34 37 0", "143 34 0",
  "Bankstown", "sydney", "-33 55 05", "151 02 06",
  "Bega", "coastal", "-36 40 0", "149 50 0",
  "Blacktown", "sydney", "-33 46 16", "150 54 23",
  "Bourke", "inland", "-30 6 0", "145 56 0",
  "Byron Bay", "coastal", "-28 38 35", "153 36 54",
  "Camden", "sydney", "-34 03 16", "150 41 45",
  "Canada Bay", "sydney", "-33 51 24", "151 07 04",
  "Coffs Harbour", "coastal", "-30 18 08", "153 07 08",
  "Cootamundra", "inland", "-34 38 30", "148 01 30",
  "Corowa", "inland", "-35 59 0", "146 23 0",
  "Deniliquin", "inland", "-35 32 0", "144 58 0",
  "Earlwood", "sydney", "-33 55 13", "151 07 37",
  "Forbes", "inland", "-33 23 0", "148 01 0",
  "Georges River", "sydney", "-34 0 36", "151 7 48",
  "Gosford", "coastal", "-33 25 37", "151 20 31",
  "Goulburn", "inland", "-34 45 17", "149 37 7",
  "Griffith", "inland", "-34 17 24", "146 2 24",
  "Grong Grong", "inland", "-34 43 0", "146 47 0",
  "Hawkesbury", "sydney", "-33 33 54", "151 18 0",
  "Hills Shire", "sydney", "-33 46 0", "151 0 0",
  "Kempsey", "coastal", "-31 5 0", "152 50 0",
  "Kiama", "coastal", "-34 40 15", "150 51 15",
  "Lake Cathie", "coastal", "-31 33 7", "152 51 18",
  "Leeton", "inland", "-34 34 0", "146 24 0",
  "Lismore", "coastal", "-28 49 0", "153 17 0",
  "Liverpool", "sydney", "-33 55 15", "150 55 23",
  "Macquarie Marshes", "inland", "-30 55 55", "147 37 44",
  "Mathoura", "inland", "-35 49 0", "144 54 0",
  "Menindee", "inland", "-32 23 33", "142 25 05",
  "Milbank", "coastal", "-30 45 0", "152 53 0",
  "Moama", "inland", "-36 5 0", "144 45 0",
  "Moree", "inland", "-29 27 57", "149 50 02",
  "Mullumbimby", "coastal", "-28 33 0", "153 30 0",
  "Murrumbidgee", "inland", "-34 48 0", "145 53 0",
  "Murwillumbah", "coastal", "-28 19 39", "153 23 45",
  "Narrabi", "inland", "-30 19 0", "149 46 0",
  "Narrendera", "inland", "-34 45 0", "146 33 0",
  "Newcastle", "coastal", "-32 55 0", "151 45 0",
  "Northern Beaches", "sydney", "-33 45 0", "151 17 0",
  "Numbucca", "coastal", "-30 43 0", "152 55 0",
  "Parramatta", "sydney", "-33 49 0", "151 0 0",
  "Penrith", "sydney", "-33 45 04", "150 41 39",
  "Port Macquarie", "coastal", "-31 26 0", "152 54 0",
  "Shoalhaven", "coastal", "-34 51 0", "150 44 0",
  "Sydney Olympic Park", "sydney", "-33 50 51", "151 03 54",
  "Temora", "inland", "-34 26 0", "147 32 0",
  "Tweed Heads", "coastal", "-28 11 0", "153 33 0",
  "Wagga Wagga", "inland", "-35 7 8", "147 22 8",
  "Walgett", "inland", "-30 01 0", "148 07 0",
  "Wauchope", "coastal", "-31 27 0", "152 44 0",
  "West Wyalong", "inland", "-33 55 0", "147 13 0",
  "Wilcannia", "inland", "-31 33 25", "143 22 45",
  "Wollongong", "coastal", "-34 25 38", "150 53 38",
  "Wyong", "coastal", "-33 16 55", "151 25 5",
  "Yass", "inland", "-34 49 0", "148 54 0",
  "Young", "inland", "-34 18 0", "148 18 0"
) |>
  mutate(
    across(c(lat, long), coords_to_decimal),
    type = factor(type),
  )

coords_to_decimal <- function(coords) {
  parts <- coords |>
    str_split_fixed(" ", 3) |>
    as.integer() |>
    matrix(ncol = 3)
  sign_coords <- sign(parts[, 1])
  sign_coords * (abs(parts[, 1]) + parts[, 2] / 60 + parts[, 3] / 3600)
}

expand_levels <- function(raw) {
  str_split_1(raw, "") |>
    factor(
      levels = c("l", "m", "h", "v", "x", "."),
      labels = c("low", "medium", "high", "very_high", "extreme", "NA"),
      ordered = TRUE
    ) |>
    fct_na_level_to_value("NA")
}

counts_dfr <- function(location, cx_annul, ae_vigilax, total) {
  data.frame(
    location = location,
    week_ending = as.Date(paste0("2023-01-", c("07", "14", "21", "28"))),
    cx_annul = expand_levels(cx_annul),
    ae_vigilax = expand_levels(ae_vigilax),
    total = expand_levels(total)
  )
}

mozzies_nsw2301 <- raw_counts |>
  map_dfr(function(x) counts_dfr(x[[1]], x[[2]], x[[3]], x[[4]])) |>
  pivot_longer(
    cols = c(cx_annul, ae_vigilax, total),
    names_to = "species",
    values_to = "count"
  ) |>
  mutate(species = factor(
    species,
    levels = c("cx_annul", "ae_vigilax", "total"),
    labels = c("Culex annulirostris", "Aedes vigilax", "total")
  )) |>
  left_join(sites, by = join_by(location))

usethis::use_data(mozzies_nsw2301, overwrite = TRUE)
