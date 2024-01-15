#' Mosquito counts from NSW Arbovirus Surveillance program
#'
#' This dataset is derived from the
#' [NSW Arbovirus Surveillance and Mosquito Monitoring program](https://www.health.nsw.gov.au/Infectious/mosquito-borne/Pages/surveillance.aspx).
#' The program monitors mosquito-borne diseases in the state of New South Wales,
#' Australia. A number of mosquito traps are managed by the program during the
#' spring to autumn months when mosquitoes are active.
#'
#' Each week traps are collected and the mosquito species are identified and
#' counted. This is analysed alongside climate conditions, and arbovirus
#' detections in the traps to inform public health management of human disease
#' risk from arboviruses in NSW. This dataset includes the mosquito abundance
#' tables for January 2023. Additional context and analysis can be found in the
#' original report published by NSW Health.
#'
#' The trap locations are classified as inland or coastal (since the species
#' found will depend on the environmental conditions). A separate group of sites
#' are labelled as being in the Sydney region (i.e. with the highest human
#' population density).
#'
#' The counts are binned with the following definition:
#' \describe{
#'   \item{\code{NA}}{No observation}
#'   \item{low}{< 50}
#'   \item{medium}{50 - 100}
#'   \item{high}{101 - 1,000}
#'   \item{very high}{1,001 - 10,000}
#'   \item{extreme}{> 10,000}
#' }
#'
#' @format Data frame with the following fields:
#' \describe{
#'   \item{location}{Location of the mosquito trap}
#'   \item{week_ending}{Date of the end of the week of observation}
#'   \item{species}{Mosquito species counted, or "total" for the total count}
#'   \item{count}{Binned mosquito abundance}
#'   \item{type}{Category of the site}
#'   \item{lat}{Latitude of trap in WGS 84 coordinates}
#'   \item{long}{Longitude of trap in WGS 84 coordinates}
#' }
#'
#' @source
#'   Surveillance and Risk Unit, Environmental Health Branch,
#'   Health Protection NSW, NSW Health.
#'   "NSW Arbovirus Surveillance and Mosquito Monitoring 2022-2023; Weekly Update: Week ending 25 February 2023 (Report Number 19)"
#'   \url{https://www.health.nsw.gov.au/environment/pests/vector/Publications/nswasp-weekly-report-2023-02-25.pdf},
#'   accessed 15 January 2024.
#'
#'   The original dataset is published under the
#'   [Creative Commons Attribution 4.0](https://creativecommons.org/licenses/by/4.0/)
#'   licence © State of New South Wales NSW Ministry of Health 2023.
"mozzies_nsw2301"
