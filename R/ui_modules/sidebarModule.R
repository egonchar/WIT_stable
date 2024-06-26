sidebarUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # CSS for the two-column layout and alignment
    div(id = ns("sidebar"), class = "custom-sidebar",
        h4(HTML(paste(icon("globe-americas", lib = "font-awesome"), "<b> Geographic Parameters</b>"))),
        radioButtons("geo_level", h5("Geographic level"), choices = c("State" = "state", "MSA" = "msa")),
        
        
        conditionalPanel(
          condition = "input.geo_level == 'state'", 
          selectizeInput("area_of_interest_state", h5(HTML("State(s) of interest<b>*</b>")),
                         choices = prim_state_options, multiple = TRUE, options = list(maxItems = 5))
        ),
        
        conditionalPanel(
          condition = "input.geo_level == 'msa'",
          selectizeInput("area_of_interest_msa", h5(HTML("MSA(s) of interest<b>*</b>")),
                         choices = msas_options, multiple = TRUE, options = list(maxItems = 5))
        ),
        
        hr(),
        # h4(HTML("<b>TEST OCCUPATION PARAMETERS</b>")),
        h4(
          HTML(paste(
            icon("briefcase", lib = "font-awesome"), # Generate icon HTML
            "<b> Test Occupation Parameters</b>"
          ))),    
        
        radioButtons("existing_or_proposed", h5("Existing or proposed occupation"),
                     choices = c("Existing" = "existing"), selected = "existing" # , "Proposed" = "proposed"
        ),
        
        conditionalPanel(
          condition = "input.existing_or_proposed == 'existing'",
          radioButtons("occ_code_or_title", h5("Specify by SOC code or occupation title"),
                       choices = c("Occupation Title" = "occupation title", "SOC Code" = "SOC code")
          )
        ),
        conditionalPanel(
          condition = "input.existing_or_proposed == 'existing' && input.occ_code_or_title == 'SOC code'",
          selectizeInput("existing_occupations_code", h5(HTML("Existing occupations<b>*</b>")),
                         choices = occ_code_options, multiple = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.existing_or_proposed == 'existing' && input.occ_code_or_title == 'occupation title'",
          selectizeInput("existing_occupations_title", h5(HTML("Existing occupations<b>*</b>")),
                         choices = occ_title_options, multiple = TRUE
          )
        ),
        conditionalPanel(
          condition = "input.existing_or_proposed == 'existing'",
          titleWithPopover(
            title = shiny::h5("Occupation Traits"),
            popover_title = "Occupation Traits",
            popover_body =  shiny::HTML("O*NET provides attribute information for occupations. You may select the types of attributes you wish to include in the computation of skill similar-occupations.<ul><li><strong>Work Activities</strong> - Summarize the kinds of tasks that may be performed across multiple occupations.</li><li><strong>Abilities</strong> - Enduring attributes of an individual that influence performance.</li><li><strong>Skills</strong> - Developed capacities that facilitate learning and the performance of activities that occur across jobs.</li><li><strong>Knowledge</strong> - Organized sets of principles and facts that apply to a wide range of situations.</li></ul>")
          ),

          checkboxGroupInput("checkGroup_existing",
                             label = NULL,
                             choices = list(
                               "Work Activities" = "w",
                               "Ability" = "a",
                               "Skill" = "s",
                               "Knowledge" = "k"
                             ),
                             selected = c("w", "a","s", "k")
          )
        ),
        
        titleWithPopover(
          title = shiny::h5(HTML("Desired skill similarity<b>*</b>")),
          popover_title = "Skill Similarity",
          popover_body = "Set the desired level of skill similarity between 0 and 1. This value indicates how similar the skills should be relative to the test occupation(s)."
        ),  
        
        numericInput("desired_skill_transferability", label = NULL, value = 0.5, min = 0, max = 1, step = 0.05),

        #############################

        
        hr(),
        div(class = "accordion-toggle",
            div(class = "accordion-text", h5(HTML("<b>Click for Optional Parameters</b>"))),
            div(class = "accordion-icon", icon("plus", lib = "glyphicon"))
        ),
        div(class = "accordion-content", style = "display: none; ", 
            
            radioButtons("occTOtest_or_testTOocc", h5("Is your test occupation the origin or destination occupation?"),
                         choices = c("Destination" = "occTOtest", "Origin" = "testTOocc")),
            radioButtons("year", h5("Year"),
                         choices = c("2021", "2022")
            ),

            hr(),
            h4(HTML(paste(icon("sliders", lib = "font-awesome"),"<b>Model Parameters</b>"))),
            
            titleWithPopover(
              title = shiny::h5("Importance-rating minimum threshold?"),
              popover_title = "Importance Rating",
              popover_body =  shiny::HTML("Limit the analysis to only consider the traits that meet a minimum importance rating for each test occupation.")
            ),
            
            radioButtons("imp_filtered", label = NULL,
                         choices = c("Yes" = "yes", "No" = "no"),
                         selected = "no"
            ),
            
            conditionalPanel(
              condition = "input.imp_filtered == 'yes'",
              titleWithPopover(
                title = shiny::h5("Minimum importance value of skills?"),
                popover_title = "Importance Rating Scale",
                popover_body =  shiny::HTML("Importance Rating Scale: <ul><li>1 = Not Important</li><li>2 = Somewhat Important</li><li>3 = Important</li><li>4 = Very Important</li><li>5 = Extremely Important</li></ul>")
              ),
              numericInput(
                "desired_imp_filter", 
                label = NULL, 
                value = 3, 
                min = 1, 
                max = 5, 
                step = 0.25
              )),

            conditionalPanel(
              condition = "input.existing_or_proposed == 'existing'",
              titleWithPopover(
                title = shiny::h5("Degree of certainty for skill similarity"),
                popover_title = "<b>Degree of Certainty<b/>",
                popover_body =  shiny::HTML("Set how the skill similarity is computed. Can have skill similarity computed based on whether existing occupations: <ul><li> Meet the test occupation's Level values for skill(s) <strong>(Mean)</strong> or </li><li>Meet the test occupation's range of Level values based on the bounds defined by the Level Standard Error values <strong>(Bound)</strong> </li></ul>")
              ),
              radioButtons("certainty_existing", label = NULL,
                           choices = c("Mean" = "mean", "Bound" = "bound")
              )
            ),
            conditionalPanel(
              condition = "input.existing_or_proposed == 'proposed'",
              h5("Apply cosine similarity matrix to:"),
              
              div(style = "margin-left: 20px;",
                  radioButtons("weighted_level_proposed", 
                               label = HTML("<b>Level</b>?"), 
                               choices = c("Yes" = "yes", "No" = "no"),
                               selected = "no"
                  ),
                  
                  # Conditional display for the importance radio buttons
                  conditionalPanel(
                    condition = "input.imp_ratings == 'yes'",
                    radioButtons("weighted_imp_proposed", 
                                 label = HTML("<b>Importance</b>?"), 
                                 choices = c("Yes" = "yes", "No" = "no"),
                                 selected = "no"
                    )
                  )
              )
            ),
            
            conditionalPanel(
              condition = "input.existing_or_proposed == 'existing'",
              h5("Apply cosine similarity matrix to:"),
              
              div(style = "margin-left: 20px;",
                  radioButtons("weighted_level_existing", 
                               label = HTML("<b>Level</b>?"), 
                               choices = c("Yes" = "yes", "No" = "no"),
                               selected = "no"
                  ),
                  radioButtons("weighted_imp_existing", 
                               label = HTML("<b>Importance</b>?"), 
                               choices = c("Yes" = "yes", "No" = "no"),
                               selected = "no"
                  )
              )
            )
            ,
            
            
            titleWithPopover(
              title = shiny::h5("Exclude soft-skills from analysis?"),
              popover_title = "Soft-Skill Filtering",
              popover_body = "If yes is selected, the analysis will exclude the 14 O*NET-designated soft-skills from the computation of skill similarity. <br/>  <br/> <strong>Social Skills</strong> <ul><li> Coordination </li><li> Instructing </li><li> Negotiation </li><li> Persuasion </li><li> Service Orientation </li><li> Social Perceptiveness </li></ul> <br/>  <strong>Thinking Skills </strong> <ul><li> Active Learning </li><li> Active Listening </li><li> Complex Problem Solving  </li><li> Critical Thinking </li><li> Judgment and Decision Making </li><li> Learning Strategies </li><li> Monitoring </li><li> Time Management </li></ul>"
            ),  
            
            radioButtons("softskills_filtered", label = NULL,
                         choices = c("Yes" = "yes", "No" = "no"),
                         selected = "no"
            ),
            
            
            titleWithPopover(
              title = shiny::h5("Incorporate occupational mobility?"),
              popover_title = "Occupational Mobility",
              popover_body = "Option to incorporate occupational mobility data to estimate the labor supply flows in addition to the stock. <br/> <br/> These computations are based on the Current Population Survey (CPS)."
            ),  
            
            radioButtons("occupation_mobility", label = NULL,
                         choices = c("Yes" = "yes", "No" = "no"),
                         selected = "no"
            ),
            
            conditionalPanel(
              condition = "input.occupation_mobility == 'yes'",
              radioButtons("mobility_annual_trend", h5("Occupational mobility coverage:"),
                           choices = c("Annual Mobility" = "annual", "Multi-Year Mobility Trend" = "trend")
              )
            ),
            conditionalPanel(
              condition = "input.existing_or_proposed == 'existing' && input.occupation_mobility == 'yes'",
              radioButtons("exits_entries", h5("Use flows from"),
                           choices = c("Test occupation entries" = "entries", "Existing occupation exits" = "exits"),
                           selected = "exits"
              )
            ),
            
            hr(),
            tags$h4(HTML(paste(bs_icon("clipboard-data-fill"), "<b>Wage Parameters</b>"))),
            
            radioButtons("hourly_or_annual", h5("Hourly or annual wages"),
                         choices = c("Hourly" = "hourly", "Annual" = "annual")
            ),
            
            conditionalPanel(
              condition = "input.hourly_or_annual == 'hourly'",
              numericInput("wage_tested_hourly", h5("Wage tested"), 30, min = 8, max = 115, step = 1)
            ),
            
            conditionalPanel(
              condition = "input.hourly_or_annual == 'annual'",
              numericInput("wage_tested_annual", h5("Wage tested"), 40000, min = 17000, max = 230000, step = 5000)
            ),
            
            # hr(),
            # tags$h4(HTML(paste(bs_icon("envelope-fill"), "<b>Email Results</b>"))),
            # 
            # textInput("email", "Enter your email if want to revisit your results:"),
            
        ),
        hr(),
        fluidRow(align="center", uiOutput("run_analysis_button_ui")),

        tags$script(HTML("
            $(document).on('click', '.accordion-toggle', function() {
              var icon = $(this).find('.accordion-icon i');
              var content = $(this).next('.accordion-content');
              content.slideToggle('fast');
              if (icon.hasClass('glyphicon-plus')) {
                icon.removeClass('glyphicon-plus').addClass('glyphicon-minus');
              } else {
                icon.removeClass('glyphicon-minus').addClass('glyphicon-plus');
              }
            });
          "))
        
    )
  )
}
