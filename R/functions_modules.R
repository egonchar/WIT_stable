##########################
##########################

# DATA PROCESSING FUNCTIONS

##########################

## POST-LOGIN HELPER FUNCTIONS

validateInput <- function(input) {
  (!is.null(input$existing_occupations_code) ||
     !is.null(input$existing_occupations_title) ||
     !is.null(input$file_upload)) &&
    (!is.null(input$area_of_interest_msa) || 
       !is_null(input$area_of_interest_state))
}

showModalIfNeeded <- function(input) {
  if(!is.null(input$existing_occupations_code) & !is.null(input$existing_occupations_title) & is.null(input$file_upload)) {
    showModal(modalDialog(
      title = "Input required",
      "Please upload a proposed occupation file.",
      easyClose = TRUE
    ))
  }
}

updateCheckboxGroups <- function(input, session) {
  if(input$existing_or_proposed == 'existing') {
    updateCheckboxGroupInput(session, "checkGroup_proposed_template", selected = NULL)
    updateCheckboxGroupInput(session, "checkGroup_proposed_no_template", selected = NULL)
  } else if(input$existing_or_proposed == 'proposed') {
    updateCheckboxGroupInput(session, "checkGroup_existing", selected = NULL)
    if(input$template_needed == 'yes') {
      updateCheckboxGroupInput(session, "checkGroup_proposed_no_template", selected = NULL)
    } else {
      updateCheckboxGroupInput(session, "checkGroup_proposed_template", selected = NULL)
    }
  }
}


logIPAddress <- function(IP) {
  cat("Log IP: \n", capture.output(str(IP()), split=TRUE), "\n")
}

getSelectedTraits <- function(input) {
  if(input$existing_or_proposed == 'existing') {
    input$checkGroup_existing
  } else if(input$existing_or_proposed == 'proposed') {
    if(input$template_needed == 'yes') {
      input$checkGroup_proposed_template
    } else {
      input$checkGroup_proposed_no_template
    }
  } else {
    NULL
  }
}

###########################

titleWithPopover <- function(title, popover_title, popover_body) {
  htmltools::span(
    class = "d-flex justify-content-between align-items-center",
    title,
    shiny::icon(
      name = "circle-info",
      style = "cursor: pointer; color: #45a3a4;",
      `data-bs-toggle` = "popover",
      `data-bs-trigger` = "hover focus",  # Added focus here
      `data-bs-html` = "true",            # Ensure the popover interprets HTML
      title = popover_title,
      `data-bs-content` = popover_body
    )
  )
}


############

# Usage:
# This function generates a download handler for Shiny applications. The handler will generate 
# a CSV file named "template_<current date>.csv", which contains occupation data based on user's 
# selected traits from the `input$checkGroup`. The data will be read from corresponding parquet files, 
# then processed, and eventually written to a CSV file.
#
# The function assumes that certain `input` values are present in the Shiny environment:
# - input$checkGroup: A list of selected traits by the user (e.g., "w", "a", "s", "k").
# - input$num_proposed_occupations: An integer indicating how many proposed occupations are to be generated.
#
# output$downloadTemplate <- generateTemplateDownloadHandler()

generateTemplateDownloadHandler <- function(input, selected_traits, session) {
  downloadHandler(
    filename = function() {
      paste("template_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Check the selected occupation traits
      print(selected_traits)
      # List to store data frames
      dfs_list <- list()
      
      # Loop through all possible input values
      for (input_value in selected_traits) {
        print(input_value)
        # Determine the template file path based on the input value
        if (input_value == "w") {
          file_path <- "./data/templates/occupationW_template.parquet"
        } else if (input_value == "a") {
          file_path <- "./data/templates/occupationA_template.parquet"
        } else if (input_value == "s") {
          file_path <- "./data/templates/occupationS_template.parquet"
        } else if (input_value == "k") {
          file_path <- "./data/templates/occupationK_template.parquet"
        } else {
          stop("Invalid input value.")
        }
        # Read the parquet file and add to list
        dfs_list[[length(dfs_list)+1]] <- arrow::read_parquet(file_path)
      }
      
      # Append the data frames together for a single occupation
      combined_df <- do.call(rbind, dfs_list)
      
      # Repeat the rows based on num_proposed_occupations
      final_df <- combined_df[rep(1:nrow(combined_df), input$num_proposed_occupations),]
      
      # Update OCC_CODE column
      final_df$OCC_CODE <- rep(sprintf("00-%04d", 1:input$num_proposed_occupations), each = nrow(combined_df))
      
      # Update Title column
      final_df$Title <- rep(sprintf("Test Occupation %d", 1:input$num_proposed_occupations), each = nrow(combined_df))
      
      # Rename columns for the final dataframe
      colnames(final_df) <- gsub("OCC_CODE", "Proposed Occupation Code", colnames(final_df))
      colnames(final_df) <- gsub("Title", "Proposed Occupation Title", colnames(final_df))
      colnames(final_df) <- gsub("elementID", "Skill ID", colnames(final_df))
      colnames(final_df) <- gsub("elementName", "Skill Name", colnames(final_df))
      colnames(final_df) <- gsub("surveyValue_IM", "Importance", colnames(final_df))
      colnames(final_df) <- gsub("surveyValue_LV", "Level", colnames(final_df))
      colnames(final_df) <- gsub("standardError_IM", "Standard Error (Importance)", colnames(final_df))
      colnames(final_df) <- gsub("standardError_LV", "Standard Error (Level)", colnames(final_df))
      colnames(final_df) <- gsub("ONET_dataType", "Occupation Trait", colnames(final_df))
      
      # Write the final data frame to CSV with NAs as blanks
      write.csv(final_df, file, row.names = FALSE, na = "")
    },
    contentType = "csv" # Explicitly set contentType to ensure correct handling
  )
}

############

extractInputs <- function(input, occ_codes_and_titles, wage_lower_hourly, wage_lower_annual, wage_upper_hourly, wage_upper_annual, wage_interval_hourly, wage_interval_annual) {
  
  # Initial checks and setup
  is_existing <- input$existing_or_proposed == 'existing'
  is_proposed <- input$existing_or_proposed == 'proposed'
  
  # Determine selected traits based on conditions
  selected_traits <- switch(input$existing_or_proposed,
                            'existing' = input$checkGroup_existing,
                            'proposed' = ifelse(input$template_needed == 'yes', 
                                                input$checkGroup_proposed_template, 
                                                input$checkGroup_proposed_no_template))
  
  # Determine weighted_imp and weighted_level
  weighted_imp <- if (is_proposed) input$weighted_imp_proposed else input$weighted_imp_existing
  weighted_level <- if (is_proposed) input$weighted_level_proposed else input$weighted_level_existing
  
  # Process existing occupations
  existing_occupations <- NULL
  occ_code_or_title <- NA
  if (is_existing) {
    occ_code_or_title <- input$occ_code_or_title
    existing_occupations <- if (input$occ_code_or_title == "SOC code") {
      data.frame(OCC_CODE = as.character(input$existing_occupations_code))
    } else {
      occ_title <- data.frame(Title = input$existing_occupations_title)
      left_join(occ_title, occ_codes_and_titles, by = "Title") %>% dplyr::select(OCC_CODE)
    }
  }
  
  # Determine certainty
  certainty <- if (is_proposed) {
    if (input$se_level == "no") "mean" else input$certainty_proposed
  } else {
    input$certainty_existing
  }
  
  # Imp filter setup
  imp_filtered <- input$imp_filtered
  desired_imp_filter <- if (input$imp_filtered == "yes") input$desired_imp_filter else NA
  
  # Constructing the inputs list
  inputs <- list(
    mobility_source = "cps",
    occTOtest_or_testTOocc = input$occTOtest_or_testTOocc,
    existing_or_proposed = input$existing_or_proposed,
    occ_code_or_title = occ_code_or_title,
    existing_occupations_code = if (is_existing && input$occ_code_or_title == 'SOC code') input$existing_occupations_code else NA,
    existing_occupations_title = if (is_existing && input$occ_code_or_title == 'occupation title') input$existing_occupations_title else NA,
    existing_occupations = existing_occupations,
    checkGroup = if (is_existing) input$checkGroup_existing else NA,
    sk_input = selected_traits,
    template_needed = if (is_proposed) input$template_needed else NA,
    se_level = if (is_proposed) input$se_level else NA,
    imp_ratings = if (is_proposed) input$imp_ratings else NA,
    file_upload = if (is_proposed) input$file_upload else NA,
    year = input$year,
    soc_level = "detailed",
    certainty = certainty,
    weighted_imp = weighted_imp,
    weighted_level = weighted_level,
    imp_filtered = imp_filtered,
    desired_imp_filter = desired_imp_filter,
    softskills_filtered = input$softskills_filtered,
    desired_skill_transferability = input$desired_skill_transferability,
    occupation_mobility = input$occupation_mobility,
    mobility_annual_trend = if (input$occupation_mobility == 'yes') input$mobility_annual_trend else NA,
    exits_entries = if (is_existing && input$occupation_mobility == 'yes') input$exits_entries else NA,
    hourly_or_annual = input$hourly_or_annual,
    wage_tested = if (input$hourly_or_annual == "hourly") input$wage_tested_hourly else input$wage_tested_annual,
    geo_level = input$geo_level,
    geo_interest = 'yes',
    area_of_interest = if (input$geo_level == 'state') input$area_of_interest_state else if (input$geo_level == 'msa') input$area_of_interest_msa else NA,
    wage_lower = if (input$hourly_or_annual == "hourly") wage_lower_hourly else wage_lower_annual,
    wage_upper = if (input$hourly_or_annual == "hourly") wage_upper_hourly else wage_upper_annual,
    wage_interval = if (input$hourly_or_annual == "hourly") wage_interval_hourly else wage_interval_annual
  )
  
  cat("\ninputs run!")
  return(inputs)
}

############

loadWageDistributionDataNATIONAL <- function(inputs, national_wage_dist_hourly_2021 , national_wage_dist_annual_2021, national_wage_dist_hourly_2022, national_wage_dist_annual_2022) {
  
  # Extract values from the inputs object
  year <- inputs$year
  hourly_or_annual <- inputs$hourly_or_annual
  
  if (year == "2021") {
    if (hourly_or_annual == "hourly") {
      return(national_wage_dist_hourly_2021)
    } else if (hourly_or_annual == "annual") { 
      return(national_wage_dist_annual_2021)
      }
  } else if (year == "2022") {
    if (hourly_or_annual == "hourly") {
      return(national_wage_dist_hourly_2022)
    } else if (hourly_or_annual == "annual") { 
      return(national_wage_dist_annual_2022)
    }
  }
}

############

loadWageDistributionDataGEO <- function(inputs, msa_wage_dist_hourly_2021, msa_wage_dist_annual_2021, state_wage_dist_hourly_2021, state_wage_dist_annual_2021, msa_wage_dist_hourly_2022, msa_wage_dist_annual_2022, state_wage_dist_hourly_2022, state_wage_dist_annual_2022) {
  
  # Extract values from the inputs object
  year <- inputs$year
  hourly_or_annual <- inputs$hourly_or_annual
  geo_level <- inputs$geo_level
  
  if (year == "2021") {
    if (hourly_or_annual == "hourly") {
      if (geo_level == "msa"){
        return(msa_wage_dist_hourly_2021)
      } else if (geo_level == "state"){
        return(state_wage_dist_hourly_2021)
      }
    } else if (hourly_or_annual == "annual") { 
      if (geo_level == "msa"){
        return(msa_wage_dist_annual_2021)
      } else if (geo_level == "state"){
        return(state_wage_dist_annual_2021)
      }
    }
  } else if (year == "2022") {
    if (hourly_or_annual == "hourly") {
      if (geo_level == "msa"){
        return(msa_wage_dist_hourly_2022)
      } else if (geo_level == "state"){
        return(state_wage_dist_hourly_2022)
      }
    } else if (hourly_or_annual == "annual") { 
      if (geo_level == "msa"){
        return(msa_wage_dist_annual_2022)
      } else if (geo_level == "state"){
        return(state_wage_dist_annual_2022)
      }
    }
  }
}

############

loadOccupationSkillsData <- function(inputs) {
  
  sk_input <- inputs$sk_input
  soc_level <- inputs$soc_level
  year <- inputs$year
  hourly_or_annual <- inputs$hourly_or_annual
  geo_level <- inputs$geo_level
  geo_interest <- inputs$geo_interest
  area_of_interest <- inputs$area_of_interest
  
  allData <- lapply(sk_input, function(input_value) {
    file_path <- switch(substr(input_value, 1, 1),
                        "w" = paste0("./data/occupationWorkActivities_", soc_level, "_", geo_level, "_", year, "_", hourly_or_annual, ".parquet"),
                        "a" = paste0("./data/occupationAbilities_", soc_level, "_", geo_level, "_", year, "_", hourly_or_annual, ".parquet"),
                        "s" = paste0("./data/occupationSkills_", soc_level, "_", geo_level, "_", year, "_", hourly_or_annual, ".parquet"),
                        "k" = paste0("./data/occupationKnowledge_", soc_level, "_", geo_level, "_", year, "_", hourly_or_annual, ".parquet"),
                        stop("Invalid input value.")
    )
    
    if (file.exists(file_path)) {
      read_parquet(file_path)
    } else {
      NULL
    }
  })
  
  allData <- rbindlist(allData, fill = TRUE)
  
  occupationSkills <- allData[GEO_GROUPING == geo_level | GEO_GROUPING == "national"]
  
  if (geo_interest == "yes" & all(!is.na(area_of_interest))) {
    occupationSkills <- occupationSkills[AREA_TITLE %in% area_of_interest | AREA == "99"]
  }
  
  select_cols <- switch(hourly_or_annual,
                        "hourly" = c("AREA", "AREA_TITLE", "PRIM_STATE", "OCC_CODE", "TOT_EMP", "H_MEAN", "H_PCT10", "H_PCT25", "H_PCT50", "H_PCT75", "H_PCT90", "GEO_GROUPING", "Title"),
                        "annual" = c("AREA", "AREA_TITLE", "PRIM_STATE", "OCC_CODE", "TOT_EMP", "A_MEAN", "A_PCT10", "A_PCT25", "A_PCT50", "A_PCT75", "A_PCT90", "GEO_GROUPING", "Title")
  )
  occupations_list <- unique(occupationSkills[, ..select_cols])
  
  return(list(occupationSkills = as_tibble(occupationSkills), 
              detailedOccupations_byGEO = as_tibble(occupationSkills[, .(n_unique_titles = uniqueN(Title)), by = AREA_TITLE]), 
              occupations_list = as_tibble(occupations_list)))
}

############

processTestVector <- function(inputs, occupationSkills) {
  
  existing_or_proposed <- inputs$existing_or_proposed
  file_upload <- inputs$file_upload
  existing_occupations <- as.list(inputs$existing_occupations)
  
  if (existing_or_proposed == "proposed") {
    testVector <- readAndProcessFile(file_upload$datapath)
  } else if (existing_or_proposed == "existing") {
    testVector <- occupationSkills %>%
      filter(OCC_CODE %in% existing_occupations$OCC_CODE) 
  }
  
  # Format the testVector
  cols.num <- c("surveyValue", "standardError")
  cols <- c(paste0(cols.num, "_IM"), paste0(cols.num, "_LV"))
  testVector <- testVector %>%
    mutate(
      OCC_CODE = paste0(OCC_CODE, "*"),
      across(all_of(cols), as.numeric)
    )
  
  return(testVector)
}

############

formatTestNames <- function(inputs, testNames_list, occ_codes_and_titles, testVector) {
  
  existing_or_proposed <- inputs$existing_or_proposed
  
  if (existing_or_proposed == "existing") {
    testNames_df <- data.frame(OCC_CODE = testNames_list)
    formattedocc <- occ_codes_and_titles %>%
      mutate(OCC_CODE = paste0(OCC_CODE, "*"))
    testNames_list_withTITLES <- left_join(testNames_df, formattedocc, by = "OCC_CODE")
  } else if (existing_or_proposed == "proposed") {
    testNames_df <- data.frame(OCC_CODE = testNames_list)
    testNames_list_withTITLES <- left_join(testNames_df, testVector, by = "OCC_CODE") %>% 
      select(OCC_CODE, Title)
  }
  
  return(testNames_list_withTITLES)
}

############

computeOccupationMobilityFlows <- function(inputs, testNames_list, existing_annual_exit, existing_annual_entry, existing_trend_exit, existing_trend_entry, proposed_annual_exit, proposed_trend_exit, mobile_occupations) {
  # Initialization and error checking
  existing_or_proposed <- inputs$existing_or_proposed
  mobility_source <- inputs$mobility_source
  mobility_annual_trend <- inputs$mobility_annual_trend
  year <- inputs$year
  
  mobile_occ_list <- as.vector(mobile_occupations$OCC_CODE)
  
  # Determine exits or entries mode based on input
  exits_entries <- if (existing_or_proposed == "proposed") {"exits"} else {inputs$exits_entries}
  
  # Determine which test names are mobile and which are not
  mobile_found <- data.frame(OCC_CODE = intersect(testNames_list, mobile_occ_list))
  mobile_not_found <- data.frame(OCC_CODE = setdiff(testNames_list, mobile_occ_list))
  
  
  print(paste0("mobile_found: ", mobile_found))
  print(class(mobile_found))
  print(paste0("mobile_not_found: ", mobile_not_found))
  print(class(mobile_not_found))
  
  # Initialize an empty data frame for results
  occupation_mobility_flows <- data.frame()
  
  # Processing based on the mobility source and trend
  if (mobility_source == "cps") {
    if (mobility_annual_trend == "annual") {
      selected_data <- if (exits_entries == "exits") existing_annual_exit else existing_annual_entry
    } else {
      selected_data <- if (exits_entries == "exits") existing_trend_exit else existing_trend_entry
    }
    
    selected_data <- as.data.frame(selected_data)  # Ensure it's a dataframe

    # Process for mobile_found occupations
    if (nrow(mobile_found) > 0) {
      if (mobility_annual_trend == "annual") {
      existing_flows <- selected_data %>%
        filter(YEAR == year, Test %in% mobile_found) %>%
        tidyr::pivot_wider(names_from = Test, values_from = mobility_rate) %>%
        distinct()
      
      print(existing_flows)
      } else if (mobility_annual_trend == "trend") {
        existing_flows <- selected_data %>%
          filter(Test %in% mobile_found) %>%
          tidyr::pivot_wider(names_from = Test, values_from = mobility_rate) %>%
          distinct()
      }
    }
    

    if (nrow(mobile_not_found) > 0 || existing_or_proposed == "proposed") {
      
      if (mobility_annual_trend == "annual") {
        proposed_flows <- proposed_annual_exit %>%
          filter(YEAR == year) %>%
          select("OCC_CODE", "mobility_rate") %>%
          distinct()
      } else if (mobility_annual_trend == "trend") {
        proposed_flows <- proposed_trend_exit %>% 
          distinct()
      }

      
      # Add dummy mobility rates for all tests in mobile_not_found
      for(test_name in mobile_not_found$OCC_CODE) {
        proposed_flows[[test_name]] <- proposed_flows$mobility_rate
      }
      proposed_flows <- proposed_flows %>%
        select(-mobility_rate)

      
    }
    
    if (nrow(mobile_found) == 0){
      occupation_mobility_flows <- proposed_flows
    } else if (nrow(mobile_not_found) ==0){
      occupation_mobility_flows <- existing_flows
    } else {
      occupation_mobility_flows <- full_join(existing_flows, proposed_flows, by = c("OCC_CODE"))
      }
    
    
  }
  
  # Additional logic for other mobility sources can be added here
  
  return(occupation_mobility_flows)
}



############

subsetOccupationSkillsForComputation <- function(occupationSkills, subset_data_for_computation, subset_number, testNames_list) {
  subsetted_data <- subsetOccupationSkills(occupationSkills, subset_data_for_computation, subset_number, testNames_list)
  return(subsetted_data)
}

############

loadDemographicsData <- function(inputs, geo_names_codes, demographics_2021_detailed_state, demographics_2021_detailed_msa, demographics_2021_broad_state, demographics_2021_broad_msa) {
  
  year <- inputs$year
  soc_level <- inputs$soc_level
  geo_level <- inputs$geo_level
  geo_interest <- inputs$geo_interest
  area_of_interest <- inputs$area_of_interest
  
  if (year == "2021") {
    data <- switch(
      paste0(soc_level, geo_level),
      "detailedstate" = demographics_2021_detailed_state,
      "detailedmsa" = demographics_2021_detailed_msa,
      "broadstate" = demographics_2021_broad_state,
      "broadmsa" = demographics_2021_broad_msa,
      stop("Invalid combination of soc_level and geo_level.")
    )
    
    if (geo_interest == "yes") {
      
      places <- geo_names_codes %>% 
        filter(AREA_TITLE %in% area_of_interest)
      
      data <- left_join(data, places, by = "AREA")
    }
    
    return(data)
  } else {
    stop("Invalid year. Only '2021' is supported.")
  }
}

############

prepareOccupationCodeAndTitles <- function(inputs, occ_codes_and_titles, testVector) {
  
  existing_or_proposed <- inputs$existing_or_proposed
  existing_occupations <- inputs$existing_occupations
  
  if (existing_or_proposed == "existing"){
    test_occ_codes_and_titles <- occ_codes_and_titles %>%
      filter(OCC_CODE %in% existing_occupations$OCC_CODE) %>%
      mutate(OCC_CODE = paste0(OCC_CODE, "*")) %>%
      rename(Test = OCC_CODE) %>%
      distinct()
  } else if (existing_or_proposed == "proposed") {
    test_occ_codes_and_titles <- testVector %>%
             select(OCC_CODE, Title) %>%
             rename(Test = OCC_CODE) %>% 
             distinct()
  }
  return(test_occ_codes_and_titles)
}


############

############

############


# Usage:
# Assuming file_upload is a list with a datapath element
# processed_data <- readAndProcessFile(file_upload$datapath)

readAndProcessFile <- function(file_path) {
  # Check if file_path is not NULL
  if (!is.null(file_path)) {
    # Read CSV file
    testVector <- read.csv(file_path, header = TRUE, check.names = FALSE)
    
    # Rename columns
    testVector <- testVector %>%
      rename(
        OCC_CODE = "Proposed Occupation Code",
        Title = "Proposed Occupation Title",
        elementID = "Skill ID",
        elementName = "Skill Name",
        surveyValue_IM = "Importance",
        surveyValue_LV = "Level",
        standardError_IM = "Standard Error (Importance)",
        standardError_LV = "Standard Error (Level)",
        ONET_dataType = "Occupation Trait"
      )

    # Return the processed dataframe
    return(testVector)
  } else {
    stop("file_path is NULL. Please provide a valid file path.")
  }
}

############

# Usage:
# Assuming the necessary variables are defined
# subsetted_data <- subsetOccupationSkills(occupationSkills, subset_data_for_computation, subset_number, testNames_list)


subsetOccupationSkills <- function(occupationSkills, subset_data_for_computation, subset_number, testNames_list) {
  if (subset_data_for_computation == "yes") {
    # Count the total employment for each occupation
    occupation_employment <- occupationSkills %>%
      group_by(Title) %>%
      summarize(total_employment = sum(TOT_EMP, na.rm = TRUE)) %>%
      arrange(desc(total_employment))
    
    # Get the top x unique occupations
    top_occupations <- head(unique(occupation_employment$Title), subset_number)
    
    # Subset the occupationSkills data frame
    occupationSkills <- occupationSkills[(occupationSkills$Title %in% top_occupations | 
                                            occupationSkills$Title %in% testNames_list), ]
    
    # Optionally remove temporary variables to free up memory
    rm(occupation_employment, top_occupations)
    
    # Return the subsetted occupationSkills data frame
    return(occupationSkills)
  } else {
    # Return the original data frame if subsetting is not required
    return(occupationSkills)
  }
}

############


# Define function to filter out soft skills
# To filter out rows from df where elementID is in softskills

filterOutSoftSkills <- function(df, softskills) {
  result <- df %>% 
    filter(!elementID %in% softskills$elementID)
  return(result)
}

############

# Define function to mutate based on desired_imp_filter
runImportanceFiltering <- function(inputs, df) {
  
  threshold <- inputs$desired_imp_filter
  
  df %>% 
    mutate(
      surveyValue_IM = case_when(surveyValue_IM < threshold ~ NA_real_, TRUE ~ surveyValue_IM),
      surveyValue_LV = case_when(surveyValue_IM < threshold ~ NA_real_, TRUE ~ surveyValue_LV),
      standardError_IM = case_when(surveyValue_IM < threshold ~ NA_real_, TRUE ~ standardError_IM),
      standardError_LV = case_when(surveyValue_IM < threshold ~ NA_real_, TRUE ~ standardError_LV)
    )
}

############

createPreparedData <- function(inputs, occupationSkills, detailedOccupations_byGEO, occupations_list, testVector, testNames_list, testNames_list_withTITLES, subset_matrix, top_n_subset_matrix, top_n_subset_matrix_GEO, ui_output_national, GEO_wagedist, cdf_GRAPH, cdf_GRAPH_weighted, transferable_occupations, supplyCurve, networkOccupations, supplyCurve_graphList, geolevel_supplyCurve_graphList, nationalSupplyCurve_graphList, demographics, existing_supplyCurve_graphList, national_supplyCurve_intersect_values, geolevel_supplyCurve_intersect_values, occupation_mobility_flows) {
  
  # Error checks
  if(is.null(inputs$geo_interest) || is.null(inputs$geo_level) ) { #|| is.null(inputs$area_of_interest)
    stop("Geographic data inputs are missing.")
  }
  
  # Load data
  prepped_data <- list()
  
  prepped_data$us_states_outside <- NULL
  prepped_data$us_msas_outside <- NULL
  prepped_data$GEO_wagedist <- NULL
  prepped_data$top_n_subset_matrix_GEO <- NULL
  prepped_data$ui_output_national <- NULL
  
  # Store the outputs
  prepped_data$occupationSkills <- occupationSkills
  prepped_data$detailedOccupations_byGEO <- detailedOccupations_byGEO
  prepped_data$occupations_list <- occupations_list
  prepped_data$testVector <- testVector
  prepped_data$testNames_list <- testNames_list
  prepped_data$testNames_list_withTITLES <- testNames_list_withTITLES
  prepped_data$subset_matrix <- subset_matrix
  prepped_data$top_n_subset_matrix <- top_n_subset_matrix
  prepped_data$national_supplyCurve_intersect_values <- national_supplyCurve_intersect_values
  prepped_data$geolevel_supplyCurve_intersect_values <- geolevel_supplyCurve_intersect_values
  prepped_data$cdf_GRAPH <- cdf_GRAPH
  prepped_data$cdf_GRAPH_weighted <- cdf_GRAPH_weighted
  prepped_data$transferable_occupations <- transferable_occupations
  prepped_data$supplyCurve <- supplyCurve
  prepped_data$networkOccupations <- networkOccupations
  prepped_data$supplyCurve_graphList <- supplyCurve_graphList
  prepped_data$geolevel_supplyCurve_graphList <- geolevel_supplyCurve_graphList
  prepped_data$nationalSupplyCurve_graphList <- nationalSupplyCurve_graphList
  prepped_data$existing_supplyCurve_graphList <- existing_supplyCurve_graphList
  prepped_data$demographics <- NULL 
  prepped_data$occupation_mobility_flows <- occupation_mobility_flows
  
  return(prepped_data)
}

############

# Function to add asterisks to specified SOC codes and duplicate rows and columns
addAsteriskAndDuplicateSocCode <- function(inputs, matrix) {

  soc_codes_list <- as.list(inputs$existing_occupations$OCC_CODE)
  cat("soc_codes_list:")
  print(soc_codes_list)
  
  # Calculate new size
  n <- nrow(matrix)
  additional <- length(soc_codes_list)
  new_size <- n + additional
  
  # Create new matrix with increased size
  new_matrix <- matrix(NA, nrow = new_size, ncol = new_size)
  rownames(new_matrix) <- c(rownames(matrix), paste0(soc_codes_list, "*"))
  colnames(new_matrix) <- c(colnames(matrix), paste0(soc_codes_list, "*"))
  
  # Fill in original values
  new_matrix[1:n, 1:n] <- matrix
  
  # Iterate over each SOC code in the list
  for (i in seq_along(soc_codes_list)) {
    soc_code <- soc_codes_list[[i]]  # Using double brackets for list indexing
    if (soc_code %in% rownames(matrix) && soc_code %in% colnames(matrix)) {
      # Find index for the SOC code in the original matrix
      row_index <- which(rownames(matrix) == soc_code)
      col_index <- which(colnames(matrix) == soc_code)
      
      # Set the original matrix positions to have asterisks
      new_matrix[n + i, 1:n] <- matrix[row_index, ]  # New row from original
      new_matrix[1:n, n + i] <- matrix[, col_index]  # New column from original
      
      # Update the names with asterisks for new duplicated rows and columns
      rownames(new_matrix)[n + i] <- paste0(soc_code, "*")
      colnames(new_matrix)[n + i] <- paste0(soc_code, "*")
    } else {
      warning(paste("SOC code", soc_code, "not found in the matrix row or column names."))
    }
  }
  
  # Return the modified matrix wrapped in a list for consistency with the expected output structure
  return(list("99" = new_matrix))
}

