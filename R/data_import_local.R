# setup_environment.R
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
library(renv)

# Proceed without manual confirmation
renv::restore(clean = TRUE, prompt = FALSE)

options(conflicts.policy = list(warn = FALSE))

################################################################################

# Core Shiny and UI packages
library(shiny)
library(bslib)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)           
library(shinyjs)
library(shinycssloaders)

# library(rstudioapi)

# Visualization
library(ggplot2)
library(scales)
library(RColorBrewer)
library(geofacet)
library(devtools)
library(ggthemr)
ggthemr("fresh")
   
# Themes and UI elements
library(htmltools)
library(bsicons)

# Parallel processing
library(doParallel)
library(foreach)
library(iterators)
library(parallel)
library(future)
library(future.apply)

# Optimizing
library(data.table)

# Data handling and manipulation
library(arrow)
library(tidyr)
suppressMessages(library(DT))
library(dplyr)

library(conflicted)

# Options
options("sp_evolution_status" = 2)

conflicted::conflicts_prefer(base::as.matrix, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::left_join, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::distinct, .quiet = TRUE)
conflicted::conflicts_prefer(data.table::`:=`, .quiet = TRUE)

################################################################################
################################################################################

geo_names_codes <<- readRDS("data/geo_names_codes.Rds")
print("geo_names_codes loaded properly in the server!")

prim_state_options <<- readRDS("data/state_options.Rds")
print("prim_state_options loaded properly in the server!")

msas_options <<- read_parquet("data/msas_names_codes.parquet")$AREA_TITLE
occ_codes_and_titles <<- read_parquet("data/occ_codes_and_titles.parquet")
occ_code_options <<- occ_codes_and_titles$OCC_CODE
occ_title_options <<- sort(occ_codes_and_titles$Title)

existing_annual_entry <<- read_parquet("data/existing_annual_entry.parquet")
existing_annual_exit <<- read_parquet("data/existing_annual_exit.parquet")

existing_trend_entry <<- read_parquet("data/existing_trend_entry.parquet")
existing_trend_exit <<- read_parquet("data/existing_trend_exit.parquet")

mobile_occupations <<- readRDS("data/mobile_occupations.Rds")

proposed_annual_exit <<- read_parquet("data/proposed_annual_exit.parquet")
proposed_trend_exit <<- read_parquet("data/proposed_trend_exit.parquet")

national_wage_dist_hourly_2021 <<- read_parquet("./data/national_wage_dist_hourly_2021.parquet")
national_wage_dist_annual_2021 <<- read_parquet("./data/national_wage_dist_annual_2021.parquet")
national_wage_dist_hourly_2022 <<- read_parquet("./data/national_wage_dist_hourly_2022.parquet")
national_wage_dist_annual_2022 <<- read_parquet("./data/national_wage_dist_annual_2022.parquet")


msa_wage_dist_hourly_2021 <<- read_parquet("./data/msa_wage_dist_hourly_2021.parquet")
msa_wage_dist_annual_2021 <<- read_parquet("./data/msa_wage_dist_annual_2021.parquet")
state_wage_dist_hourly_2021 <<- read_parquet("./data/state_wage_dist_hourly_2021.parquet")
state_wage_dist_annual_2021 <<- read_parquet("./data/state_wage_dist_annual_2021.parquet")

msa_wage_dist_hourly_2022 <<- read_parquet("./data/msa_wage_dist_hourly_2022.parquet")
msa_wage_dist_annual_2022 <<- read_parquet("./data/msa_wage_dist_annual_2022.parquet")
state_wage_dist_hourly_2022 <<- read_parquet("./data/state_wage_dist_hourly_2022.parquet")
state_wage_dist_annual_2022 <<- read_parquet("./data/state_wage_dist_annual_2022.parquet")


softskills <<- read_parquet("data/onet_softskills.parquet")

wage_interval_hourly <<- 1
wage_lower_hourly <<- 8
wage_upper_hourly <<- 115

wage_interval_annual <<- 2500
wage_lower_annual <<- 17000
wage_upper_annual <<- 230000


###########################
