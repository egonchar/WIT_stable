# app.R

# Clear environment and options setup
rm(list = ls())
rm(list = ls(all = TRUE))

################################################################################
################################################################################

# sets wd if working locally & import data
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
  library(rstudioapi)
  try({
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }, silent = TRUE)  # silent = TRUE to suppress errors in non-interactive sessions
} else{
    source("R/data_import_local.R")
  # to be replaced with database script
}

################################################################################
################################################################################

# source("R/data_import_local.R")
source("R/functions.R")
source("R/functions_modules.R")

source("R/ui_modules/sidebarModule.R")

################################################################################
################################################################################

# Placeholder for credentialling

# if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
#   # define some basic credentials (on data.frame)
#   credentials <- data.frame(
#     user = c("guest"), # mandatory
#     password = c("password"), # mandatory
#     start = c("2023-12-31), # optional (all others)
#     expire = c(NA),
#     admin = c(FALSE),
#     comment = "Simple and secure authentification mechanism for single ‘Shiny’ applications.",
#     stringsAsFactors = FALSE
#   ) } 

################################################################################
################################################################################

# Used for debugging
subset_data_for_computation <<- "no"
subset_number <<- 250

################################################################################
################################################################################

title <- tags$div(
  style = "display: flex; justify-content: space-between; align-items: center; padding-top: 20px;",
  tags$div(
    style = "display: flex; align-items: center;",
    tags$img(
      src = "NNCTA_logo_updated_symbol.png",
      height = "62px",
      width = "65px",
      style = "padding-right: 10px;"
    ),
    tags$h1(
      "Workforce Insight Tool",
      style = "font-weight: bold; white-space: nowrap; margin: 0;"
    )
  ),
  tags$div(
    style = "display: flex; align-items: center; text",
    input_dark_mode(id = "dark_mode", mode = "light", label = tags$div(style = "color: #45a3a4; display: flex; align-items: center;", HTML("Dark mode")))
  )
)


light <- bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Source Sans Pro"), code_font = font_google("Source Sans Pro"),
                  "btn-primary-bg" = "#45a3a4", # Background color for primary buttons
                  "btn-primary-border" = "#45a3a4", # Border color for primary buttons
                  "accordion-button-active-bg" = "#f7f7f7"
  )
  
bullet_style <<- "style='margin-left: 40px;'"
  
################################################################################
################################################################################
################################################################################
################################################################################

ui <- fluidPage(
  theme = light,
  dashboardHeader(
    title = title
  ),
  useShinyjs(),
  htmltools::includeScript("./www/popover.js"),
  htmltools::includeCSS("./www/popover.css"),
  
  HTML("<br>"),
  sidebarLayout(
    sidebarPanel(
      sidebarUI("sidebar1")
    ),
    mainPanel(
        class = "main",
        dashboardBody(
          
          # Accordion structure
          accordion(
            open = c("Unweighted Survival Function Graph"), 
            
            # Second accordion item: survival function graph
            accordion_panel(
              title = "Unweighted Survival Function Graph",
              conditionalPanel(
                condition = "input.run_analysis > 0",
                h5(textOutput("survival_text"), align = "left"),
                withSpinner(plotOutput("cdf_graph_plot", click = "plot_click", width = "100%", height = "400px"), color = "#45a3a4", type = 8),
                p(textOutput("plot_info"), id = "plot_info", style = "text-align: left;"),
                conditionalPanel(
                  condition = "output.plotLoaded",
                  HTML("<br>"),
                  
                  div(style = "text-align: center;", downloadButton('downloadPlot', 'Download Survival Plot')),
                  HTML("<br>")
                )
              )
            )
            ),
          br(),
          
          accordion(
            open = c("Weighted Survival Function Graph"),
            
            accordion_panel(
              title = "Weighted Survival Function Graph",
              
              conditionalPanel(
                condition = "input.run_analysis",
                h4(textOutput("weighted_survival_header_nat"), style = "text-align: center; font-weight: bold;"),
                h5(textOutput("weighted_survival_text"), align = "left"),
                withSpinner(plotOutput("cdf_graph_weighted_plot", click = "plot_click2", width = "100%", height = "400px"), color = "#45a3a4", type = 8),
                p(textOutput("weighted_plot_info"), id = "weighted_plot_info", style = "text-align: left;"),
                conditionalPanel(
                  condition = "output.cdf_graph_weighted_plot",
                  HTML("<br>"),
                  div(style = "text-align: center;", downloadButton('downloadPlot_weighted', 'Download Weighted Survival Plot', class = "btn download.button")),
                  HTML("<br>"),
                ), 
                HTML("<hr>"),
                HTML("<br>"),
                h4(textOutput("weighted_survival_header_GEO"), style = "text-align: center; font-weight: bold;"),
                withSpinner(uiOutput("location_dropdown2_GEO_ui", style = "text-align: left; display: block; margin: auto;"), color = "#45a3a4", type = 8),
                conditionalPanel(
                  condition = "output.location_dropdown2_GEO_ui",
                  HTML("<br>"), 
                h5(textOutput("weighted_survival_GEO_text"), align = "left"),
                withSpinner(plotOutput("cdf_graph_weighted_plot_GEO", click = "plot_click_GEO", width = "100%", height = "400px"), color = "#45a3a4", type = 8),
                p(textOutput("weighted_plot_info_GEO"), id = "weighted_plot_info_GEO", style = "text-align: left;"),
                conditionalPanel(
                condition = "output.cdf_graph_weighted_plot_GEO",
                  HTML("<br>"),
                  div(style = "text-align: center;", downloadButton('downloadPlot_weighted_GEO', 'Download Regional Weighted Survival Plot', class = "btn download.button")),
                  HTML("<br>")  
                )
              )
            )
            )
          ),
          br(),
          
          accordion(
            open = c("National Supply Figures"), 
            accordion_panel(
              "National Supply Figures",
              conditionalPanel(
                condition = "input.run_analysis",
                withSpinner(uiOutput("content_ui_national"), color = "#45a3a4", type = 8)
              )
            )
          ) ,
          
          br(),
          
          accordion(
            open = c("Regional Supply Figures"), 
                    # Area of Interest tab
                    accordion_panel(
                      "Regional Supply Figures",
                      conditionalPanel(
                        condition = "input.run_analysis",
                        withSpinner(uiOutput("content_ui_regional"), color = "#45a3a4", type = 8)
                        )
                      )
          ),
          br(),
          br(),
          br(),
          br(),
          br()
)
  )
),
  # This will adjust the width of the sidebar and set the header color and text color
  shiny::tags$head(
    tags$style(HTML("
    .main-sidebar {
      background-color: #54585A;
    }
    .content-header {
      background-color: #45a3a4;
    }
    .main-header .logo {
      background-color: #45a3a4;
    }
    .box-title a {
      color: red !important;
    }
    .fa-briefcase {
      color:#45a3a4;
    }
    
    .fa-earth-americas {
      color:#45a3a4;
    }
    
    .fa-sliders {
      color:#45a3a4;
    }
    
    .bi-clipboard-data-fill {
      stroke-width: 4;
      color:#45a3a4;
    }
    
    .bi-envelope-fill {
      color:#45a3a4;
      }
    
    .btn.btn-enabled { 
      background-color: #45a3a4; 
      border-color: #45a3a4; 
      color: white; 
      display: block; /* Corrected for proper display */
      margin: 0 auto; /* Center the button horizontally */
      text-align: center; /* Center the text inside the button */
    }
    
    .btn.btn-disabled { 
      background-color: #BBBBBB; 
      color: white; 
      display: block; /* Corrected for proper display */
      margin: 0 auto; /* Center the button horizontally */
      text-align: center; /* Center the text inside the button */
    }
    
    .btn.btn-default.shiny-download-link.shiny-bound-output {
      background-color: #45a3a4;
      border-color: #45a3a4;
      color: white; 
      display: block;
      margin: 0 auto;
      width: 350px;
      text-align: center;
      }
    
    .btn.btn-default {
      background-color: #45a3a4;
      border-color: #45a3a4;
      color: white; 
      display: block;
      margin: 0 auto;
      width: auto;
      max-width: 95%; 
      text-align: center;
    }
    
    .btn.btn-primary {
      background-color: #45a3a4;
      border-color: #45a3a4;
      color: white;
      display: block;
      margin: 0 auto;
      width: 300px;
      text-align: center;
    }
    
    .btn.btn-doc {
      background-color: #45a3a4;
      border-color: #45a3a4;
      color: white;
      display: inline;
      margin: 0 auto; 
      line-height: normal;
      vertical-align: baseline;
      text-align: center; 
      padding: 3px;
    }

    hr {
      border-top: 1px solid #000000;
      }

    .button-container {
      text-align: center;
    }
    
    .skin-blue .main-header .navbar {
      background-color: #45a3a4;
    }
    .content-wrapper {
      width: 100%;
    }
    
    .well {
      border-color: #ebedef;
    }
    
    .dt-buttons {
      float: none;
      text-align: center;
      color: red;
    }
    
    .accordion-button{
      font-weight: bold;
      font-size: 22px;
    }
    
    .accordion-button:not(.collapsed) {
    background-color: #f7f7f7;
    }
    
    .dark-mode .accordion-button:not(.collapsed) {
  background-color: #282c2f;
}


    .accordion-toggle { display: flex; justify-content: space-between; align-items: center; cursor: pointer; margin-top: 5px; color: #45a3a4; padding: 10px; }
    .accordion-text { flex-grow: 1; }
    .accordion-icon { text-align: right; }
        
       .myPlot {
      max-width: 800px;
      padding-bottom: 66.66%; /* Adjust percentage based on your aspect ratio */
      position: relative;
    }
    
    .myPlot > div {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }
    
  hr {
    border-style: solid;
    border-color: #45a3a4;
  }
    .shiny-progress-container {
      /* Style the container of the progress bar */
    }
    .progress-bar {
      background-color: #45a3a4; /* Change to your desired bar color */
      background-image: linear-gradient(45deg, rgba(255,255,255,.15) 25%, transparent 25%, transparent 50%, rgba(255,255,255,.15) 50%, rgba(255,255,255,.15) 75%, transparent 75%, transparent);
      animation: progress-bar-stripes 1s linear infinite;
    }
    @keyframes progress-bar-stripes {
      from { background-position: 1rem 0; }
      to { background-position: 0 0; }
    }
  ")),
    tags$head(HTML("
                <script type='text/javascript' src='keep-alive.js'></script>")),
    tags$script(HTML("
    $(document).ready(function() {
      // Initialize tooltips
      $('[data-toggle=\"tooltip\"]').tooltip();
      
      // Set focus to the 'run_analysis' button
      $('#run_analysis').focus();
    });
  "))
  )
)
  
  ################################################################################

# Login screen  

  # css <- HTML(".btn-primary {
  #                 color: #ffffff;
  #                 background-color: #45a3a4;
  #                 border-color: #45a3a4;
  #             }
  #             .panel-primary {
  #                 border-color: #ffffff;
  #             }
  #           ")
  # 
  # set_labels(
  #   language = "en",
  #   "Please authenticate" = "Enter login credentials",
  #   "Username:" = "Username:",
  #   "Password:" = "Password:"
  # )
  # 
  # # Wrap your UI with secure_app
  # ui <- secure_app(ui,
  #                  # changing theme for the credentials
  #                  theme = shinythemes::shinytheme("flatly"),
  #                  tags_top = tags$div(
  #                    tags$head(tags$style(css)),
  #                    tags$img(
  #                      src = "NNCTA_logo_updated_symbol.png", width = 200, height = 200, alt="Logo not found", deleteFile=FALSE)
  #                  ))
  
  ################################################################################
  ################################################################################
  ################################################################################
################################################`################################

  server <- function(input, output, session) {

    # # Authentication logic
    # res_auth <- secure_server(check_credentials = check_credentials(credentials), timeout = 15, max_users = 3)
    
    # mainContentServer("mainContent1")

    # Dynamically render the "Run Analysis" button
    output$run_analysis_button_ui <- renderUI({
      # Initialize enable_button as FALSE
      enable_button <- FALSE
      
      # Check if any occupation is entered
      occupation_entered <- (!is.null(input$existing_occupations_code) && !any(is.na(input$existing_occupations_code))) ||
        (!is.null(input$existing_occupations_title) && !any(is.na(input$existing_occupations_title)))
      
      # Check if any area of interest is entered
      area_entered <- (!is.null(input$area_of_interest_msa) && !any(is.na(input$area_of_interest_msa))) ||
        (!is.null(input$area_of_interest_state) && !any(is.na(input$area_of_interest_state)))
      
      # Check if skill similarity entered
      skill_entered <- (!is.null(input$desired_skill_transferability) && !(is.na(input$desired_skill_transferability)))
      
      # Check wage not null
      wage_not_null <- (!is.null(input$wage_tested_hourly) && !(is.null(input$wage_tested_annual))) && (!is.na(input$wage_tested_hourly) && !(is.na(input$wage_tested_annual)))
      
      if (input$imp_filtered == "yes"){
        
        imp_filter_not_null <- (!is.null(input$desired_imp_filter) && !(is.na(input$desired_imp_filter)))
        # Update enable_button based on both conditions being true
        enable_button <- occupation_entered && area_entered && skill_entered && wage_not_null && imp_filter_not_null
      } else{
      
      # Update enable_button based on both conditions being true
      enable_button <- occupation_entered && area_entered && skill_entered && wage_not_null
      }
      
      # Render actionButton based on enable_button
      if (enable_button) {
        actionButton("run_analysis", "Run Analysis", class = "btn btn-enabled")
      } else {
        actionButton("run_analysis", "Run Analysis", class = "btn btn-disabled", disabled = TRUE)
      }
    })
    
    # Initialize the inputs reactiveValues object
    user_inputs <- reactiveValues(
      occTOtest_or_testTOocc = NULL,
      existing_or_proposed = 'existing',
      occ_code_or_title = NULL,
      existing_occupations_code = NULL,
      existing_occupations_title = NULL,
      existing_occupations = NULL,
      checkGroup = NULL,
      sk_input = NULL,
      # template_needed = NULL,
      # se_level = NULL,
      imp_ratings = NULL,
      # file_upload = NULL,
      year = NULL,
      soc_level = NULL,
      certainty = NULL,
      weighted_imp = NULL,
      weighted_level = NULL,
      imp_filtered = NULL,
      desired_imp_filter = NULL,
      softskills_filtered = NULL,
      desired_skill_transferability = NULL,
      occupation_mobility = NULL,
      mobility_source = NULL,
      mobility_annual_trend = NULL,
      exits_entries = NULL,
      hourly_or_annual = NULL,
      wage_tested = NULL,
      geo_level = NULL,
      geo_interest = NULL,
      area_of_interest = NULL,
      wage_lower = NULL,
      wage_upper = NULL,
      wage_interval = NULL
    )
    
    prepared_data <- reactiveValues(
      us_msas_outside = NULL,
      us_states_outside = NULL,
      occupationSkills = NULL,
      detailedOccupations_byGEO = NULL,
      occupations_list = NULL,
      testVector = NULL,
      testNames_list = NULL,
      testNames_list_withTITLES = NULL,
      subset_matrix = NULL,
      top_n_subset_matrix = NULL,
      top_n_subset_matrix_GEO = NULL,
      ui_output_national = NULL,
      GEO_wagedist = NULL,
      cdf_GRAPH = NULL,
      cdf_GRAPH_weighted = NULL,
      transferable_occupations = NULL,
      supplyCurve = NULL,
      networkOccupations = NULL,
      supplyCurve_graphList = NULL,
      geolevel_supplyCurve_graphList = NULL,
      nationalSupplyCurve_graphList = NULL,
      existing_supplyCurve_graphList = NULL,
      demographics = NULL,
      indices = NULL,
      highest_skill_sim = NULL, 
      national_supplyCurve_intersect_values = NULL,
      geolevel_supplyCurve_intersect_values = NULL,
      occupation_mobility_flows = NULL
    )
    
    observeEvent(input$run_analysis, {
      
      withProgress(message = 'Analysis request received...', value = 0, {
        
        inputs <- extractInputs(input, occ_codes_and_titles, wage_lower_hourly, wage_lower_annual, wage_upper_hourly, wage_upper_annual, wage_interval_hourly, wage_interval_annual)
    
    # Print a formatted list of the inputs to the console
    cat("\nUser-based Inputs:\n")
    str(inputs)
    
    cat("area_of_interest: \n")
    print(inputs$area_of_interest)
    
    # Update each field in the inputs reactiveValues object
    for (name in names(inputs)) {
      user_inputs[[name]] <- inputs[[name]]
    }
    
    national_wagedist <- loadWageDistributionDataNATIONAL(inputs, national_wage_dist_hourly_2021 , national_wage_dist_annual_2021, national_wage_dist_hourly_2022, national_wage_dist_annual_2022)
    cat("national_wagedist: \n")
    print(national_wagedist)

    GEO_wagedist <- loadWageDistributionDataGEO(inputs, msa_wage_dist_hourly_2021, msa_wage_dist_annual_2021, state_wage_dist_hourly_2021, state_wage_dist_annual_2021, msa_wage_dist_hourly_2022, msa_wage_dist_annual_2022, state_wage_dist_hourly_2022, state_wage_dist_annual_2022)
    
    cat("GEO_wagedist: \n")
    print(GEO_wagedist)
    
    all_wagedist <- rbind(cbind(national_wagedist, AREA = "99", AREA_TITLE = "U.S."), GEO_wagedist)

    result <- loadOccupationSkillsData(inputs)
    
    cat("result here!\n")
    print(result)
    
    occupationSkills <- result$occupationSkills
    detailedOccupations_byGEO <- result$detailedOccupations_byGEO
    occupations_list <- result$occupations_list
    
    rm(result)
    
    testVector <- processTestVector(inputs, occupationSkills)
    
    print("testVector works!")
    print(testVector)
    
    testNames_list <- base::unique(testVector$OCC_CODE)
    
    testNames_list_withTITLES <- formatTestNames(inputs, testNames_list, occ_codes_and_titles, testVector)
    
    if (inputs$occupation_mobility == "yes"){
      occupation_mobility_flows <- computeOccupationMobilityFlows(inputs, testNames_list, existing_annual_exit, existing_annual_entry, existing_trend_exit, existing_trend_entry, proposed_annual_exit, proposed_trend_exit, mobile_occupations)
      
      print(head(occupation_mobility_flows))
      
      print("occupation_mobility_flows works!")
    } else if (inputs$occupation_mobility == "no"){
      occupation_mobility_flows <- NULL
    }
    
    cat("testNames_list: \n")
    print(testNames_list)


    cat("occ_codes_and_titles: \n")
    print(head(occ_codes_and_titles))

    
    test_occ_codes_and_titles <- prepareOccupationCodeAndTitles(inputs, occ_codes_and_titles, testVector)

    
    if (inputs$softskills_filtered == "yes") {
      occupationSkills <- filterOutSoftSkills(occupationSkills, softskills)
      testVector <- filterOutSoftSkills(testVector, softskills)
    }
    
    if (inputs$imp_filtered == "yes") {
      testVector <- runImportanceFiltering(inputs, testVector)
      
      occupationSkills <- occupationSkills %>%
        filter(elementID %in% testVector$elementID)
    }
    
    print("DATA PREP ALL DONE!")
    
    setProgress(value = 0.1, message = "Beginning analytics...")
    
    ##############
    
    cat("CHECK BEFORE COSINE\n")
    print(head(occupationSkills))
    print(head(testVector))
    print(inputs$weighted_imp)
    print(inputs$soc_level)

    if (inputs$weighted_imp== "yes"){
      if (inputs$existing_or_proposed == "existing"){
        imp_name <- paste0("data/cosine/precomputed_cosine_sim_imp_", paste(inputs$sk_input, collapse=""), ".Rds")
        cosine_sim_imp <- readRDS(imp_name)[["99"]]

        cosine_sim_imp <- addAsteriskAndDuplicateSocCode(inputs, cosine_sim_imp)
      } else{
      cosine_sim_imp <- compute_cosine_similarity_imp(inputs, occupationSkills, testVector)
      }
    } else{
      cosine_sim_imp <- NA
    }
    if (inputs$weighted_level== "yes"){
      if (inputs$existing_or_proposed == "existing"){
        level_name <- paste0("data/cosine/precomputed_cosine_sim_level_", paste(inputs$sk_input, collapse=""), ".Rds")
        cosine_sim_level <- readRDS(level_name)[["99"]]
        
        cosine_sim_level <- addAsteriskAndDuplicateSocCode(inputs, cosine_sim_level)
      } else{
    cosine_sim_level <- compute_cosine_similarity_level(inputs, occupationSkills, testVector)
      }
  } else{
    cosine_sim_level <- NA
  }

    subset_matrix <- run_network_function(inputs, occupationSkills, testVector, testNames_list, cosine_sim_imp, cosine_sim_level)

    setProgress(value = 0.25, message = "Similarities computed...")
  
    top_n_subset_matrix_skillSimilarity <- get_top_n_rows(inputs, subset_matrix, testNames_list, n)
    
    top_n_subset_matrix <- left_join(top_n_subset_matrix_skillSimilarity, national_wagedist, by = "OCC_CODE")

    indices <- which(top_n_subset_matrix$edge_weight >= inputs$desired_skill_transferability)
    
    prepared_data$indices <- indices
    
    if (length(indices) == 0){
      prepared_data$highest_skill_sim <- max(top_n_subset_matrix$edge_weight)
    }

    cdf_GRAPH <- graph_cdf_interactive_unweighted(inputs, subset_matrix, testNames_list, test_occ_codes_and_titles)

    subset_matrix_weighted <- left_join(subset_matrix, all_wagedist, by = c("AREA", "AREA_TITLE", "OCC_CODE")) %>% 
      select(!c("MEAN", "PCT10", "PCT25", "PCT50", "PCT75", "PCT90"))

    cdf_GRAPH_weighted <- graph_cdf_interactive_empweighted(inputs, subset_matrix_weighted, testNames_list, test_occ_codes_and_titles)

    setProgress(value = 0.5, message = "Survival curve generated...")
    
    if (length(indices) > 0) {
      matching_codes <- top_n_subset_matrix$TEST_OCC_CODE[indices]


    transferable_occupations <- filter_skill_transferability(inputs, subset_matrix, testVector)

    cat("occupations_list:\n")
    print(head(occupations_list))
    
    networkOccupations <- merge(occupations_list, transferable_occupations, by = c("OCC_CODE", "AREA", "AREA_TITLE"), all.x = TRUE, all.y = TRUE)
    
    cat("networkOccupations:\n")
    print(head(networkOccupations))
    
    cat("inputs$occupation_mobility:")
    print(inputs$occupation_mobility)
    
    if (inputs$occupation_mobility =="yes"){
      if (inputs$existing_or_proposed == "existing"){
        
        supplyCurve_skill <- iterate_labor_supply(inputs, networkOccupations, testNames_list)

        supplyCurve_skill <- left_join(supplyCurve_skill, test_occ_codes_and_titles, by="Test")
        
        supplyCurve_skill <- supplyCurve_skill %>%
          mutate(source = "Skill Similarity")
        
        cat("nrow(occupation_mobility_flows):")
        print(nrow(occupation_mobility_flows))
        
        if (nrow(occupation_mobility_flows) != 0){
          
          cat("flows networkOccupations: \n")
          print(head(networkOccupations))
          print(names(networkOccupations))
          cat("occupation_mobility_flows: \n")
          print(head(occupation_mobility_flows))
          print(names(occupation_mobility_flows))
        
          # Function to append '_skill' to column names if they are in testNames_list
          append_skill <- function(name) {
            if (any(name %in% testNames_list)) { # Ensures the condition is evaluated correctly
              return(paste0(name, "_skill"))
            } else {
              return(name)
            }
          }
          
          # Rename the columns in networkOccupations based on testNames_list
          networkOccupations <- networkOccupations %>%
            dplyr::rename_with(.fn = append_skill, .cols = all_of(testNames_list))

          # Joining the two data frames
          networkOccupations_mobility <- left_join(networkOccupations, as.data.frame(occupation_mobility_flows), by = "OCC_CODE")

          # Capturing names of relevant columns for appending '_mobility' 
          columns_mobility <- intersect(colnames(networkOccupations_mobility), testNames_list)

          # Function to append '_mobility' to column names if they match the test names
          append_mobility <- function(name) {
            if (any(name %in% columns_mobility)) {
              return(paste0(name, "_mobility"))
            } else {
              return(name)
            }
          }

          # Renaming the columns in networkOccupations_mobility based on columns_mobility
          networkOccupations_mobility <- networkOccupations_mobility %>%
            dplyr::rename_with(.fn = append_mobility, .cols = all_of(columns_mobility))

        supplyCurve_mobility <- iterate_labor_supply_mobility(inputs, networkOccupations_mobility, testNames_list)

        supplyCurve_mobility <- left_join(supplyCurve_mobility, test_occ_codes_and_titles, by="Test")
        
        supplyCurve_mobility <- supplyCurve_mobility %>%
          mutate(source = "Occupational Mobility")
        
        supplyCurve <- rbind(supplyCurve_skill, supplyCurve_mobility)
        
        result <- graph_supply_curve_by_area_mobilityANDskill_tool(inputs, supplyCurve)

        } else{
          
          supplyCurve <- rbind(supplyCurve_skill)  
          result <- graph_supply_curve_by_area_title(inputs, supplyCurve)
          
        }
        
        # Access the graph list and intersect values list from the result
        supplyCurve_graphList <- result$graphs
        intersect_values_list <- result$intersects
        
        # Access intersect values for a specific area, e.g., "U.S."
        national_supplyCurve_intersect_values <- intersect_values_list[["U.S."]]
        
        # Generate national supply curve graph from the list
        nationalSupplyCurve_graphList <- supplyCurve_graphList[["U.S."]]

        # Subset the supplyCurve_graphList to exclude "U.S." if needed
        geolevel_supplyCurve_graphList <- supplyCurve_graphList[!names(supplyCurve_graphList) %in% "U.S."]

        # For the intersect values: correctly indexing non-"U.S." areas
        geolevel_supplyCurve_intersect_values <- intersect_values_list[!names(intersect_values_list) %in% "U.S."]

        setProgress(value = 0.75, message = "Supply curves computed...")
        
      } 
    } else if (inputs$occupation_mobility == "no"){
      
      supplyCurve <- iterate_labor_supply(inputs, networkOccupations, testNames_list)

      supplyCurve <- left_join(supplyCurve, test_occ_codes_and_titles, by="Test")
      
      # Execute the function and store the result
      result <- graph_supply_curve_by_area_title(inputs, supplyCurve)
      
      # Access the graph list and intersect values list from the result
      supplyCurve_graphList <- result$graphs
      intersect_values_list <- result$intersects

      # Access intersect values for national
      national_supplyCurve_intersect_values <- intersect_values_list[["U.S."]]
      
      # Generate national supply curve graph from the list
      nationalSupplyCurve_graphList <- supplyCurve_graphList[["U.S."]]

      # Subset the supplyCurve_graphList to exclude "U.S."
      geolevel_supplyCurve_graphList <- supplyCurve_graphList[!names(supplyCurve_graphList) %in% "U.S."]

      # For the intersect values: correctly indexing non-"U.S." areas
      geolevel_supplyCurve_intersect_values <- intersect_values_list[!names(intersect_values_list) %in% "U.S."]

      setProgress(value = 0.75, message = "Supply curves computed...")
      
    }
    
    if (inputs$existing_or_proposed == "existing") {
      formattedocc <- occupations_list %>%
        mutate(OCC_CODE = paste0(OCC_CODE, "*"))
      
      print(head(formattedocc, 5))
      
      existing_emp <- formattedocc %>%
        filter(OCC_CODE %in% testNames_list)
      
      if (!is.null(existing_emp)) {
        existing_supplyCurve <- iterate_labor_supply_existing(inputs, existing_emp)
        
        if (!is.null(existing_supplyCurve)) {
          existing_supplyCurve <- left_join(existing_supplyCurve, formattedocc, by = c("AREA", "AREA_TITLE", "OCC_CODE"))
          existing_supplyCurve_graphList <- graph_supply_curve_by_area_title_existing(inputs, existing_supplyCurve)

        } else{
          existing_supplyCurve_graphList <- NULL}
        
        }} else{
          existing_supplyCurve_graphList <- NULL}
    

    tryCatch({
      print("SANITY")
      
      outputs <- createPreparedData(inputs, occupationSkills, detailedOccupations_byGEO, occupations_list, testVector, testNames_list, testNames_list_withTITLES, subset_matrix, top_n_subset_matrix, top_n_subset_matrix_GEO, ui_output_national, GEO_wagedist, cdf_GRAPH, cdf_GRAPH_weighted, transferable_occupations, supplyCurve, networkOccupations, supplyCurve_graphList, geolevel_supplyCurve_graphList, nationalSupplyCurve_graphList, demographics, existing_supplyCurve_graphList, national_supplyCurve_intersect_values, geolevel_supplyCurve_intersect_values, occupation_mobility_flows)
      
      
      # Update each field in the inputs reactiveValues object
      for (name in names(outputs)) {

        prepared_data[[name]] <- outputs[[name]]
      }
      
    }, error = function(e) {
      # This will print the error message
      cat("Error:", e$message, "\n")
    })
    
    } else {
      prepared_data$inputs <- inputs
      prepared_data$occupationSkills <- occupationSkills
      prepared_data$detailedOccupations_byGEO <- detailedOccupations_byGEO
      prepared_data$occupations_list <- occupations_list
      prepared_data$testVector <- testVector
      prepared_data$testNames_list <- testNames_list
      prepared_data$testNames_list_withTITLES <- testNames_list_withTITLES
      prepared_data$subset_matrix <- subset_matrix
      prepared_data$top_n_subset_matrix <- top_n_subset_matrix
      # prepared_data$top_n_subset_matrix_GEO <- top_n_subset_matrix_GEO
      # prepared_data$ui_output_national <- ui_output_national
      # prepared_data$GEO_wagedist <- GEO_wagedist
      prepared_data$cdf_GRAPH <- cdf_GRAPH
      prepared_data$cdf_GRAPH_weighted <- cdf_GRAPH_weighted
      prepared_data$indices <- indices
      prepared_data$occupation_mobility_flows <- occupation_mobility_flows
      
      }
    
    cat("DATA CALCULATIONS ALL DONE!\n")
    cat("prepared_data: \n")
    print(prepared_data)
    cat("Data prepared successfully!\n")
    
    # Print a formatted list of the inputs to the console
    cat("\nUser-based Inputs:\n")
    str(inputs)

    setProgress(value = 1, message = "Analysis complete! Results rendering...")
    
  })
  })

  ##############  ##############  ##############
  ##############  ##############  ##############
  
  output$survival_text <- renderText({
    req(input$run_analysis)
    paste("National survival function graph of skill similarity to the test occupations considered.")
  })
    
    output$weighted_survival_header_nat <- renderText({
      req(input$run_analysis)
      paste("National")
    })
  
    output$weighted_survival_header_GEO <- renderText({
      req(input$run_analysis)
      paste("Regional")
    })
    
    output$estimated_header_national <- renderText({
      "Estimated"
    })
    output$existing_header_national <- renderText({
      "Existing"
    })
    
    output$estimated_header_regional <- renderText({
      "Estimated"
    })
    output$existing_header_regional <- renderText({
      "Existing"
    })
    
  
  output$weighted_survival_text <- renderText({
    req(input$run_analysis)
    paste("Weighted (by employment) national survival function graph of skill similarity to the test occupations considered.")
  })

  
  output$supply_stock_national <- renderText({
    req(input$run_analysis)
    req(prepared_data$nationalSupplyCurve_graphList)
    
    # percent <- as.character((input$desired_skill_transferability)*100)
    percent <- sprintf("%0.2f", input$desired_skill_transferability)
    
    paste0("Estimated national supply stock curve of labor meeting skill similarity of ", percent, ".")
  })
  
  output$supply_existing_national <- renderText({
    req(input$run_analysis)
    
    year <- as.character(input$year)
    
    paste0("Estimated national supply stock curve of workers employed in existing occupation in ", year, ".")
  })
  
  
  ##############  ##############  ##############
  ##############  ##############  ##############
  
  plot_loaded <- reactiveVal(FALSE)  # Initial value
  
  output$cdf_graph_plot <- renderPlot({
    # Use graph_cdf_interactive() to create the plot
    req(input$run_analysis, prepared_data$cdf_GRAPH)
    
    plot_loaded(TRUE)  # Update reactive value when plot is rendered
    
    return(prepared_data$cdf_GRAPH)
    
  })
  
  output$plotLoaded <- reactive({
    plot_loaded()  # This will be TRUE once the plot is rendered
  })
  
  outputOptions(output, "plotLoaded", suspendWhenHidden = FALSE)  # Keep the reactive updated even when not displayed
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0('cdfCurve_unweighted_national.png')
    },
    content = function(file) {
      req(input$run_analysis)
      req(prepared_data$cdf_GRAPH)
      ggsave(file,
             prepared_data$cdf_GRAPH, width = 14, height = 7.5, dpi = 300)
    }
  )
  
  output$plot_info <- renderText({
    req(input$run_analysis)
    req(prepared_data$cdf_GRAPH)
    
    if (!is.null(input$plot_click)) {
      selected_x <- input$plot_click$x
      selected_y <- input$plot_click$y
      
      if (is.numeric(selected_x) && is.numeric(selected_y)) {
        # Extract the test_name from the clicked point
        clicked_point <- subset(
          prepared_data$cdf_GRAPH$data,
          edge_weight == selected_x & ecdf == selected_y
        )
        test_name <- clicked_point$test_name
        
        if (selected_y >= 0 & selected_y <= 1) {
        
        paste0(
          "At a skill similarity level of ", sprintf("%0.2f", selected_x), ", ",
          (round(selected_y, 2) * 100), "% of occupations of occupations meet that threshold for the ",
          test_name, " occupation."
        )
        } else{
          paste0("Your selection is invalid. Please try again.")
        }
      } else {
        ""
      }
    } else {
      "(Click on the graph for interpretation of values)"
    }
  })  
  
  ##############  ##############  ##############
  ##############  ##############  ##############
  
  observeEvent(c(input$run_analysis, prepared_data$cdf_GRAPH_weighted), {
    
    # Initialize or update reactive values only once within this event
    weightedCDF_national_plot <- reactiveVal(NULL)
    weightedplotLoaded <- reactiveVal(FALSE)
    
    # Assign the plot from prepared data, ensuring it's available
    national_plot <- prepared_data$cdf_GRAPH_weighted[["U.S."]]
    
    # Check for the plot's existence before proceeding
    if (!is.null(national_plot)) {
      weightedCDF_national_plot(national_plot)  # Store the plot for reactive usage
      weightedplotLoaded(TRUE)  # Signal that the plot is now loaded
      
      # Define plot rendering - this doesn't render the plot immediately,
      # but sets up the reactive connection for when the UI requests it.
      output$cdf_graph_weighted_plot <- renderPlot({
          weightedCDF_national_plot()  # Render the stored plot
        })
      
      # Setup the download handler
      output$downloadPlot_weighted <- downloadHandler(
        filename = 'cdfCurve_weighted_national.png',
        content = function(file) {
          req(weightedCDF_national_plot())
          ggsave(file, plot = weightedCDF_national_plot(), width = 14, height = 7.5, dpi = 300)
        }
      )
      
      # Generate information text based on user interaction with the plot
      output$weighted_plot_info <- renderText({
        
        req(weightedCDF_national_plot())  # Ensure prerequisites are met
        
        if (!is.null(input$plot_click2)) {
          selected_x2 <- input$plot_click2$x
          selected_y2 <- input$plot_click2$y
          
          if (is.numeric(selected_x2) && is.numeric(selected_y2)) {
            # Extract the test_name from the clicked point
            clicked_point2 <- subset(
              weightedCDF_national_plot()$data,
              edge_weight == selected_x2 & ecdf == selected_y2
            )
            test_name2 <- clicked_point2$test_name
            
            if (selected_y2 >= 0 & selected_y2 <= 1) {
            paste0(
              "At a skill similarity level of ", sprintf("%0.2f",selected_x2), ", ",
              (round(selected_y2, 2) * 100), "% of occupations of occupations meet that threshold for the ",
              test_name2, " occupation."
            )
            } else {
              paste0("Your selection is invalid. Please try again.")
            }
          } else {
            ""
          }
        } else {
          "(Click on a curve for interpretation of values)"
        }
      })
    }
  })
  
  ##############  ##############  ##############
  ##############  ##############  ##############
  
  observeEvent(c(input$run_analysis, prepared_data$cdf_GRAPH_weighted), {
    req(input$run_analysis, prepared_data$cdf_GRAPH_weighted)  # Ensure necessary inputs are available
    
    # Initialize reactive values
    selected_area_title_GEO <- reactiveVal()
    selected_area_plot_GEO <- reactiveVal()
    
    # Dynamic dropdown menu for GEO data
    output$location_dropdown2_GEO_ui <- renderUI({
      area_titles_GEO <- if (input$geo_level %in% c("state", "msa")) {
        user_inputs$area_of_interest
      } else {
        return(NULL)  # Return NULL to hide the dropdown if conditions are not met
      }
      
      selected_value <- if (length(area_titles_GEO) == 1) {
        area_titles_GEO  # Pre-select if there's only one area of interest
      }
      
      selectInput("location_dropdown2_GEO", "Select Location:", choices = c("", area_titles_GEO), selected = selected_value)
    })
    
    # Observe changes in dropdown and update selected_area_title_GEO
    observeEvent(input$location_dropdown2_GEO, {
      selected_area_title_GEO(input$location_dropdown2_GEO)
    })
    
    # Render text based on selected area
    output$weighted_survival_GEO_text <- renderText({
      req(selected_area_title_GEO())
      paste0("Weighted (by employment) survival function graph for ", selected_area_title_GEO(), ".")
    })
    
    # Render GEO data based plot
    output$cdf_graph_weighted_plot_GEO <- renderPlot({
      req(selected_area_title_GEO())
      selected_area_plot_GEO(prepared_data$cdf_GRAPH_weighted[[selected_area_title_GEO()]])
      req(selected_area_plot_GEO())
      selected_area_plot_GEO()
    })
    
    # Interpret clicks on the weighted plot for regional GEO data
    output$weighted_plot_info_GEO <- renderText({
      # Ensure prerequisites are met
      req(selected_area_plot_GEO())  
      
      # Default instruction for user interaction
      default_message <- "(Click on a curve for interpretation of values)"
      
      # Check if there has been a click on the plot
      if (!is.null(input$plot_click_GEO)) {
        selected_x_GEO <- input$plot_click_GEO$x
        selected_y_GEO <- input$plot_click_GEO$y
        
        # Ensure that clicked coordinates are numeric
        if (is.numeric(selected_x_GEO) && is.numeric(selected_y_GEO)) {
          # Construct the message based on the clicked coordinates
          
          if (selected_y_GEO >= 0 & selected_y_GEO <= 1) {
            paste0(
            "At a skill similarity level of ", sprintf("%0.2f",selected_x_GEO), ", ",
            (round(selected_y_GEO, 2) * 100), "% of occupations meet that threshold."
            )
          } else {
              
            paste0("Your selection is invalid. Please try again.")
            } 
        } else {
          ""  # Return an empty string if clicked coordinates are not numeric
        }
      } else {
        default_message  # Return the default message if there was no click
      }
    })
    
    
    
    # Setup download handler for GEO plot
    output$downloadPlot_weighted_GEO <- downloadHandler(
      filename = function() {
        paste0('cdfCurve_weighted_', selected_area_title_GEO(), '.png')
      },
      content = function(file) {
        req(selected_area_plot_GEO())
        ggsave(file, plot = selected_area_plot_GEO(), width = 14, height = 7.5, dpi = 300)
      }
    )
  })

  ##############  ##############  ##############
  ##############  ##############  ##############

  observe({
    output$showSupplyFigures <- renderText({
      if (!is.null(prepared_data$indices) && length(prepared_data$indices) > 0) {
        return("true")
      } else if (!is.null(prepared_data$indices) && length(prepared_data$indices) == 0)  {
        return("false")
      }
    })
  })
  
  observeEvent(input$run_analysis, {
    # Check if 'indices' remains NULL or is empty after analysis
    if (is.null(prepared_data$indices) || length(prepared_data$indices) == 0) {
      # highestSkillSimText <- paste("The highest skill similarity score computed was:", sprintf("%0.2f", prepared_data$highest_skill_sim))
      
      # Round down highest_skill_sim to 2 decimal places
      highest_skill_sim_rounded_down <- floor(prepared_data$highest_skill_sim * 100) / 100
      highestSkillSimText <- paste("The highest skill similarity score computed was:", sprintf("%0.2f", highest_skill_sim_rounded_down))
      
      showModal(modalDialog(
        title = tags$b("ERROR: Skill Similarity Threshold Binding"), # Bold the title
        tags$p("The desired skill similarity selected is not met by any of the existing occupations, given the parameters selected. Please adjust your parameters and try again."),
        tags$p(highestSkillSimText, style = "font-weight: bold;"), # Ensure this line is bold and on a new line
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })

  observe({
    output$noMobilityMessage <- renderUI({
      if (nrow(prepared_data$occupation_mobility_flows) == 0 && user_inputs$occupation_mobility=="yes") {
        HTML("<div style='text-align:center;'><h3><strong>CPS currently does not provide data for mobility flows for this occupation.</strong></h3></div>")
      } else {
        NULL
      }
    })
  })
  
  # Initialize reactive values to store the latest plots
  latest_national_plot <- reactiveVal()
  latest_national_existing_plot <- reactiveVal()
  
  output$NATIONALsupplyCurve_graph <- renderPlot({
    req(input$run_analysis, prepared_data$nationalSupplyCurve_graphList)
    
    plot <- prepared_data$nationalSupplyCurve_graphList

    if (!is.null(plot)) {
      latest_national_plot(plot)
      return(plot)
    }
    
    return(NULL)
  })
  
  
  output$national_supply_graph_interpretation <- renderUI({
    req(input$run_analysis, latest_national_plot(), prepared_data$national_supplyCurve_intersect_values)
    
    occupation_mobility <- input$occupation_mobility
    
    # Format the wage based on whether it's hourly or annual
    wage <- user_inputs$wage_tested
    if (input$hourly_or_annual == "hourly") {
      wage_interp <- paste0("an hourly wage of $", formatC(wage, format = "f", big.mark = ",", digits = 2))
    } else if (input$hourly_or_annual == "annual") {
      wage_interp <- paste0("an annual salary of $", formatC(wage, format = "f", big.mark = ",", digits = 2))
    }
    
    # Prepare the skill similarity interpretation
    skill_sim_level <- sprintf("%0.2f", input$desired_skill_transferability)
    skill_sim_interp <- paste0(" and a skill similarity level of ", skill_sim_level)

    # Iterate over each row of the sorted data frame
  if (occupation_mobility == "no" || nrow(prepared_data$occupation_mobility_flows) == 0){
    
    # Sorting the data by 'intersect_value'
    sorted_data <- prepared_data$national_supplyCurve_intersect_values[order( prepared_data$national_supplyCurve_intersect_values$intersect_value), ]
    
    # Initialize bullet_points string
    bullet_points <- ""
    
    for(i in 1:nrow(sorted_data)) {
      current_row <- sorted_data[i, ]
      est_supply_formatted <- format(round(current_row$intersect_value), big.mark = ",", decimal.mark = ".", nsmall = 0)
      
      # Create the bullet point string with a single newline after "\n•"
      point <- paste0("<br/><span ", bullet_style, ">• ", est_supply_formatted, " ", current_row$Title, " workers.</span>")
      
      # Concatenate the point to the existing bullet_points string
      bullet_points <- paste(bullet_points, point)
    }
    
    # Combine everything into the final interpretation text
    final_text <- HTML(paste0("At ", wage_interp, skill_sim_interp, ", there is an estimated national supply <b>stock</b> of:", bullet_points))
    
  } else{
    
    # Sorting the data by 'source' and then by 'intersect_value'
    sorted_data <- prepared_data$national_supplyCurve_intersect_values[order(prepared_data$national_supplyCurve_intersect_values$source, prepared_data$national_supplyCurve_intersect_values$intersect_value), ]
    
    # Initialize the bullet_points strings for each source type
    bullet_points_stock <- ""
    bullet_points_flow <- ""
    
    # Iterate through each row of sorted_data
    for(i in 1:nrow(sorted_data)) {
      
      current_row <- sorted_data[i, ]
      
      # Format the intersect_value for display
      est_supply_formatted <- format(round(current_row$intersect_value), big.mark = ",", decimal.mark = ".", nsmall = 0)
      
      # Create the bullet point string with HTML line breaks and bold title
      point <- paste0("<br/><span ", bullet_style, ">• ", est_supply_formatted, " ", current_row$Title, " workers.</span>")
      
      # Check the source and concatenate the point to the appropriate bullet_points string
      if (current_row$source == "Skill Similarity") {
        bullet_points_stock <- paste(bullet_points_stock, point)
      } else if (current_row$source == "Occupational Mobility") {
        bullet_points_flow <- paste(bullet_points_flow, point)
      }
    }
    
    # Combine everything into the final interpretation text
    final_text <- HTML(paste0("At ", wage_interp, skill_sim_interp, ", there is an estimated national supply <b>stock</b> of:", bullet_points_stock, "<br/> <br/> and an estimated national supply <b>flow</b> of:", bullet_points_flow))
  }
    

    return(final_text)
  })
  
  
  # Update the downloadNationalPlot to use the stored plot
  output$downloadNationalPlot <- downloadHandler(
    filename = function() {
      paste('NationalPlot', '.png', sep = '')
    },
    content = function(file) {
      ggsave(file, plot = latest_national_plot(), width = 14, height = 7.5, dpi = 300)
    }
  )
  
  # Observe changes and trigger plot rendering for NATIONAL_existing_supplyCurve
  observe({
    req(input$run_analysis, prepared_data$existing_supplyCurve_graphList)
    
    if (input$existing_or_proposed == "existing") {
      cat("Existing plot condition met!\n")
      
      plot <- prepared_data$existing_supplyCurve_graphList[["U.S."]]

      if (!is.null(plot)) {
        latest_national_existing_plot(plot)
        output$NATIONAL_existing_supplyCurve <- renderPlot({ plot })
      }
    }
  })
  
  
  # Observe changes and update the download button for NATIONAL_existing_supplyCurve
  observe({
    req(latest_national_existing_plot())
    
    output$downloadNationalPlotExisting <- downloadHandler(
      filename = function() {
        paste('NationalPlot_Existing', '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = latest_national_existing_plot(), width = 14, height = 7.5, dpi = 300)
      }
    )
  })
  
  output$plotExists_national <- reactive({
    !is.null(latest_national_plot())
  })
  
  outputOptions(output, "plotExists_national", suspendWhenHidden=FALSE)  # prevent suspension when hidden
  
  output$plotExists_national_existing <- reactive({
    !is.null(latest_national_existing_plot())
  })
  
  outputOptions(output, "plotExists_national_existing", suspendWhenHidden=FALSE)  # prevent suspension when hidden
  
  ##############  ##############  ##############
  ##############  ##############  ##############
  
  # Initialization and reactive value declarations
  latest_area_interest_plot <- reactiveVal()
  latest_area_interest_existing_plot <- reactiveVal()
  selected_location <- reactiveVal(NULL)  # Initialize with NULL
  
  observeEvent(input$run_analysis, {
    req(input$run_analysis, prepared_data$geolevel_supplyCurve_graphList)
    
    # Render the dynamic dropdown menu based on user selection
    output$location_dropdown_ui <- renderUI({
      choices <- if (input$geo_level %in% c("state", "msa")) {
        user_inputs$area_of_interest
      } else {
        return(NULL)  # Return NULL to hide the dropdown if conditions are not met
      }
      
      selected_value <- if (!is.null(choices) && length(choices) == 1) {
        choices  # Pre-select the value if there's only one
      }
      
      selectInput("location_dropdown", "Select Location:", choices = c("", choices), selected = selected_value)
    })
    
    # Observe changes to the dropdown and update the selected location
    observeEvent(input$location_dropdown, {
      cat("Dropdown changed!\n")  # Debugging
      selected_location(input$location_dropdown)  # Update the reactive value based on the selected option
      print(paste("Selected location set to:", selected_location()))  # Debugging
    })
    
    # Respond to changes in the selected location
    observe({
      req(selected_location())  # Ensure a location is selected before proceeding
      
      print(paste("Attempting to render plots for:", selected_location()))  # Debugging
      
      # Reactive plot rendering based on selected location
      latest_area_interest_plot(prepared_data$geolevel_supplyCurve_graphList[[selected_location()]])
      latest_area_interest_existing_plot(prepared_data$existing_supplyCurve_graphList[[selected_location()]])
      
      output$GEOselect_supplyCurve <- renderPlot({ latest_area_interest_plot() })
      
      output$geo_supply_graph_interpretation <- renderUI({
        req(latest_area_interest_plot(), prepared_data$geolevel_supplyCurve_intersect_values)  # Ensure a location is selected before proceeding
        
        selected <- selected_location()
        
        if(selected %in% names(prepared_data$geolevel_supplyCurve_intersect_values)) {
          # Extract the relevant dataframe using the selected key
          data <- prepared_data$geolevel_supplyCurve_intersect_values[[selected]]        
        occupation_mobility <- input$occupation_mobility
        
        # Format the wage based on whether it's hourly or annual
        wage <- user_inputs$wage_tested
        if (input$hourly_or_annual == "hourly") {
          wage_interp <- paste0("an hourly wage of $", formatC(wage, format = "f", big.mark = ",", digits = 2))
        } else if (input$hourly_or_annual == "annual") {
          wage_interp <- paste0("an annual salary of $", formatC(wage, format = "f", big.mark = ",", digits = 2))
        }
        
        # Prepare the skill similarity interpretation
        skill_sim_level <- sprintf("%0.2f", input$desired_skill_transferability)
        skill_sim_interp <- paste0(" and a skill similarity level of ", skill_sim_level)
        
        print(class(data))
        print(head(data))
        
        # Iterate over each row of the sorted data frame
        if (occupation_mobility == "no" || nrow(prepared_data$occupation_mobility_flows) == 0){
          
          # Sorting the data by 'source' and then by 'intersect_value'
          sorted_data <- data[order(data$intersect_value), ]
          
          # Initialize bullet_points string
          bullet_points <- ""
          
          for(i in 1:nrow(sorted_data)) {
            current_row <- sorted_data[i, ]
            est_supply_formatted <- format(round(current_row$intersect_value), big.mark = ",", decimal.mark = ".", nsmall = 0)
            
            # Create the bullet point string with a single newline after "\n•"
            point <- paste0("<br/><span ", bullet_style, ">• ", est_supply_formatted, " ", current_row$Title, " workers.</span>")
            
            # Concatenate the point to the existing bullet_points string
            bullet_points <- paste(bullet_points, point)
          }
          
          # Combine everything into the final interpretation text
          final_text <- HTML(paste0("At ", wage_interp, skill_sim_interp, " in <b>", selected, "</b>,  there is an estimated regional supply <b>stock</b> of:", bullet_points))
          
        } else{
          # Sorting the data by 'source' and then by 'intersect_value'
          sorted_data <- data[order(data$source, data$intersect_value), ]
          
          # Initialize the bullet_points strings for each source type
          bullet_points_stock <- ""
          bullet_points_flow <- ""
          
          # Iterate through each row of sorted_data
          for(i in 1:nrow(sorted_data)) {
            
            current_row <- sorted_data[i, ]
            
            # Format the intersect_value for display
            est_supply_formatted <- format(round(current_row$intersect_value), big.mark = ",", decimal.mark = ".", nsmall = 0)
            
            # Create the bullet point string with HTML line breaks and bold title
            point <- paste0("<br/><span ", bullet_style, ">• ", est_supply_formatted, " ", current_row$Title, " workers.</span>")
            
            # Check the source and concatenate the point to the appropriate bullet_points string
            if (current_row$source == "Skill Similarity") {
              bullet_points_stock <- paste(bullet_points_stock, point)
            } else if (current_row$source == "Occupational Mobility") {
              bullet_points_flow <- paste(bullet_points_flow, point)
            }
          }
          
          # Combine everything into the final interpretation text
          final_text <- HTML(paste0("At ", wage_interp, skill_sim_interp, " in <b>", selected, "</b>, there is an estimated regional supply <b>stock</b> of:", bullet_points_stock, "<br/> <br/> and an estimated regional supply <b>flow</b> of:", bullet_points_flow))
        }
        
        
        return(final_text)
        } else {
          return(NULL)
        }
      })
      
      output$GEOselect_existing_supplyCurve <- renderPlot({ latest_area_interest_existing_plot() })
      
    })
    
    # Supply stock region text rendering
    output$supply_stock_region <- renderText({
      req(selected_location())  # Ensure a location is selected before proceeding
      
      percent <- as.character((input$desired_skill_transferability) * 100)
      paste0("Estimated regional supply stock curve of labor meeting skill similarity of ", percent, "% for ", selected_location(), ".")
    })
  })
  
  # Update the download buttons based on the latest plots
  observe({
    req(latest_area_interest_plot(), latest_area_interest_existing_plot())  # Ensure plots are available
    
    current_location <- selected_location()  # Ensure a location is selected
    
    output$downloadAreaInterestPlot <- downloadHandler(
      filename = function() {
        paste('AreaSupplyPlot_', current_location, '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = latest_area_interest_plot(), width = 14, height = 7.5, dpi = 300)
      }
    )
    
    output$supply_existing_region <- renderText({

      year <- as.character(input$year)
      
      paste0("Approximate supply stock curve for ", current_location, " of workers employed in existing occupation in ", year, ".")
    })
    
    
    output$downloadAreaInterestExistingPlot <- downloadHandler(
      filename = function() {
        paste('AreaSupplyPlot_ExistingOccupation_', current_location, '.png', sep = '')
      },
      content = function(file) {
        ggsave(file, plot = latest_area_interest_existing_plot(), width = 14, height = 7.5, dpi = 300)
      }
    )
  })
  
  # Reactives to check plot existence
  output$plotExists_area <- reactive({
    !is.null(latest_area_interest_plot())
  })
  
  outputOptions(output, "plotExists_area", suspendWhenHidden = FALSE)
  
  output$plotExists_area_existing <- reactive({
    !is.null(latest_area_interest_existing_plot())
  })
  
  outputOptions(output, "plotExists_area_existing", suspendWhenHidden = FALSE)
  
  
  ##############  ##############  ##############
  ##############  ##############  ##############
  
  content_ui_reactive_national <- reactive({
    if (length(prepared_data$indices) != 0 && !is.null(prepared_data$indices)) { 
      tagList(
        h4(textOutput("estimated_header_national"), style = "text-align: center; font-weight: bold;"),
        h5(textOutput("supply_stock_national"), align = "left"),
        HTML("<br>"),
        plotOutput("NATIONALsupplyCurve_graph", width = "100%", height = "400px"),
        HTML("<br>"),
        conditionalPanel(
          condition = "input.run_analysis && output.NATIONALsupplyCurve_graph",
          p(htmlOutput("national_supply_graph_interpretation"), id = "national_supply_graph_interpretation", style = "text-align: left;"),
          HTML("<br>"),
          div(style = "text-align: center;", downloadButton('downloadNationalPlot', 'Download National Plot')),
          HTML("<br>")
        ),
        conditionalPanel(
          condition = "input.existing_or_proposed == 'existing' && output.plotExists_national_existing",
          HTML("<hr>"),
          HTML("<br>"), 
          h4(textOutput("existing_header_national"), style = "text-align: center; font-weight: bold;"),
          h5(textOutput("supply_existing_national"), align = "left"),
          HTML("<br>"),
          plotOutput("NATIONAL_existing_supplyCurve", width = "100%", height = "400px"),
          HTML("<br>"),
          div(style = "text-align: center;", downloadButton('downloadNationalPlotExisting', 'Download Existing National Plot')),
          HTML("<br>")
        )
      )
    } else if (length(prepared_data$indices) == 0 && !is.null(prepared_data$indices)){
      list(
        h5(HTML("<div style='color: #d95f02;'>A supply figure could not be generated as the desired skill similarity selected is not met by any of the existing occupations, given the parameters selected.</div>"))
      )
    }
  })
  
  observeEvent(input$run_analysis, {
    req(input$run_analysis, prepared_data$indices, prepared_data$nationalSupplyCurve_graphList)
    
    content_ui_reactive_national()
    
  })
  
  output$content_ui_national <- renderUI({
    req(input$run_analysis)
    content_ui_reactive_national()
  })
  
  
  ##############  ##############  ##############
  ##############  ##############  ##############
  
  content_ui_reactive_regional <- reactive({
    if (length(prepared_data$indices) != 0 && !is.null(prepared_data$indices)) { 
      tagList(
        # Dynamically generated dropdown for selecting regions
        uiOutput("location_dropdown_ui"),
        HTML("<br>"), 
        # Main plot for selected region with conditional visibility
        conditionalPanel(
          condition = "input.location_dropdown",
          HTML("<br>"), 
          h4(textOutput("estimated_header_regional"), style = "text-align: center; font-weight: bold;"),
          h5(textOutput("supply_stock_region"), align = "left"),
          HTML("<br>"), 
          plotOutput("GEOselect_supplyCurve", width = "100%", height = "400px"),
          p(htmlOutput("geo_supply_graph_interpretation"), id = "geo_supply_graph_interpretation", style = "text-align: left;"),
          HTML("<br>"),
          # Conditional download button for the regional plot
          conditionalPanel(
            condition = "output.GEOselect_supplyCurve",
            div(style = "text-align: center;", downloadButton('downloadAreaInterestPlot', 'Download Regional Plot'))
          ),
          HTML("<br>"),  
          # Additional conditional panels for existing supply curve and no mobility message
          conditionalPanel(
            condition = "input.existing_or_proposed == 'existing' && output.plotExists_area_existing",
            conditionalPanel(
              condition = "output.noMobilityMessage",
              uiOutput("noMobilityMessage")
            ),
            HTML("<hr>"), 
            HTML("<br>"),  
            h4(textOutput("existing_header_regional"), style = "text-align: center; font-weight: bold;"),
            h5(textOutput("supply_existing_region"), align = "left"),
            HTML("<br>"),
            plotOutput("GEOselect_existing_supplyCurve", width = "100%", height = "400px"),
            HTML("<br>"), 
            div(style = "text-align: center;", downloadButton('downloadAreaInterestExistingPlot', 'Download Existing Regional Plot')),
            HTML("<br>")
          )
        )
      )
    } else if (length(prepared_data$indices) == 0 && !is.null(prepared_data$indices)){
      list(
        h5(HTML("<div style='color: #d95f02;'>A regional supply figure could not be generated as the desired skill similarity selected is not met by any of the existing occupations, given the parameters selected.
</div>"))
      )
    }
  })
  
  observeEvent(input$run_analysis, {
    req(input$run_analysis, prepared_data$indices, prepared_data$nationalSupplyCurve_graphList)
    
    content_ui_reactive_regional()
    
  })
  
  output$content_ui_regional <- renderUI({
    req(input$run_analysis)
    content_ui_reactive_regional()
  })
  
  ##############  ##############  ##############
  ##############  ##############  ##############

  # Script for sending emails removed in publicly available code.

  }
  
  
  ################################################################################
  ################################################################################
  ################################################################################
  
  
  shinyApp(ui, server)
