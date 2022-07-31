
# User Interface
ui <- navbarPage(
  "2021 NIR",
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("paper"),
  tabPanel("Home", icon = icon("home"),
           mainPanel(
             jumbotron(header = "Welcome to the Preliminary Data Outputs for the 2021 National Indicators Report", 
                       content = message03,
                       button = FALSE
             ),
             h3("Enter Access Key"),
             textInput(inputId = "passcode", label = "Keys are provided by Field to Market staff", 
                       "tlon-uqbar-orbis-tertius", width = "600px"), # tlon-uqbar-orbis-tertius Paste Key Here
             br(),
             br(),
             fluidRow(
               column(6, panel_div("danger", "Data Access Status", 
                                   content = uiOutput("status"))),
               column(6, panel_div("warning", "Latest Website Update", 
                                   content = textOutput("update_date")))
             ),
             br(),
             br(),
             fluidRow(
               column(6, panel_div(class_type = "info", 
                                   panel_title = "Crops Available For Review",
                                   content = uiOutput("crops_granted"))),
               column(6, panel_div(class_type = "success", 
                                   panel_title = "Website Developer Information",
                                   content = uiOutput("contact_info")))
             )
           )
  ),
  tabPanel("Indicators Graphs", icon = icon("chart-line"),
           sidebarPanel(
             width = 2,
             selectInput("crops_available_indic", "Available Crops", choices = ""),
             radioButtons(
               inputId = "main_indic_user",
               label = "Select Indicator",
               choiceNames = user_indic_options,
               choiceValues = user_indic_options
             ),
             tags$hr(style="border-color:black;"),
             checkboxInput("decade_function_user", label = "Plot 10-yr linear functions")
           ),
           mainPanel(
             h4(HTML(message01)),
             HTML(message02),
             br(),
             br(),
             conditionalPanel(
               condition = "input.main_indic_user == 'Energy Use' || 
               input.main_indic_user == 'Greenhouse Gas Emissions' || 
               input.main_indic_user == 'Crop Context: Acres Planted and Production'",
               plotOutput("indic_user_auto_width")
             ),
             conditionalPanel(
               condition = "input.main_indic_user == 'Irrigation Water Use' ||
               input.main_indic_user == 'Land Use' || 
               input.main_indic_user == 'Soil Conservation'",
               plotOutput("indic_user_fixed_width", width = "650px")
             )
           )
           ),
  tabPanel("Indicator Tables", icon = icon("table"),
           sidebarPanel(
             width = 2,
             selectInput("crops_available_table", "Available Crops", choices = "")),
           mainPanel(
             br(),
             HTML("Table of indicators for reference years as modeled by LOESS function with span of 0.75."),
             br(),
             tableOutput("loess_table"),
             br(),
             HTML("Land Use percent change matrix to compare reference years. Based on estimates from 
                  LOESS function. Values above and below main diagonal are mirror images. 
                  Negative values represent improvement for the indicator."),
             uiOutput("LU"),
             br(),
             HTML("Irrigation Water Use percent change matrix to compare reference years. Based on estimates from 
                  LOESS function. Values above and below main diagonal are mirror images. 
                  Negative values represent improvement for the indicator."),
             br(),
             uiOutput("IWU"),
             br(),
             HTML("Energy Use percent change matrix to compare reference years. Based on estimates from 
                  LOESS function. Values above and below main diagonal are mirror images. 
                  Negative values represent improvement for the indicator."),
             br(),
             uiOutput("EU"),
             br(),
             HTML("Greenhouse Gas Emissions percent change matrix to compare reference years. Based on estimates
             from LOESS function. Values above and below main diagonal are mirror images. 
                  Negative values represent improvement for the indicator."),
             br(),
             uiOutput("GHG"),
             br(),
             HTML("Soil Conservation percent change matrix to compare reference years. Based on estimates
             from LOESS function. Values above and below main diagonal are mirror images. 
                  Negative values represent improvement for the indicator."),
             br(),
             uiOutput("SC")
           )
           ),
  tabPanel(
    "Summary Chart", icon = icon("chart-area"),
    sidebarPanel(
      width = 2,
      selectInput("crops_available_radar", "Available Crops", choices = "")
    ),
    mainPanel(
      HTML(message07),
      plotOutput("radar_chart", height = "700px"),
      HTML(message08),
      tableOutput("scaling_vector"),
      HTML(message09),
      tableOutput("raw_radar_data")
    )
  ),
  tabPanel("Energy Use", icon = icon("bolt"),
           sidebarPanel(
             width = 2,
             selectInput("crops_available_EU", "Available Crops", choices = "")
           ),
           mainPanel(
             HTML(message04),
             fluidRow(
               column(6,
                      br(),
                      plotOutput("PYU_energy", height = "600px"),
                      br(),
                      plotOutput("PYU_energy_prop", height = "600px"),
                      br(),
                      plotOutput("PYU_raw_energy_line", height = "600px"),
                      br(),
                      plotOutput("PYU_prop_energy_line", height = "600px"),
                      br(),
                      plotOutput("component_impact_EU", height = "600px")
               ),
               column(6,
                      br(),
                      plotOutput('per_acre_energy', height = "600px"),
                      br(),
                      plotOutput("per_acre_energy_prop", height = "600px"),
                      br(),
                      plotOutput("per_acre_raw_energy_line", height = "600px"),
                      br(),
                      plotOutput("per_acre_prop_energy_line", height = "600px")
                      )
               
             ))
  ),
  tabPanel("GHG Emissions", icon = icon("tractor"),
           sidebarPanel(
             width = 2,
             selectInput("crops_available_GHG", "Available Crops", choices = "")
           ),
           mainPanel(
             HTML(message05),
             fluidRow(
               column(6,
                      br(),
                      plotOutput("PYU_ghg", height = "600px"),
                      br(),
                      plotOutput("PYU_ghg_prop", height = "600px"),
                      br(),
                      plotOutput("PYU_raw_ghg_line", height = "600px"),
                      br(),
                      plotOutput("PYU_prop_ghg_line", height = "600px"),
                      br(),
                      plotOutput("component_impact_GHG", height = "600px")
               ),
               column(6,
                      br(),
                      plotOutput('per_acre_ghg', height = "600px"),
                      br(),
                      plotOutput('per_acre_ghg_prop', height = "600px"),
                      br(),
                      plotOutput("per_acre_raw_ghg_line", height = "600px"),
                      br(),
                      plotOutput("per_acre_prop_ghg_line", height = "600px")
                      )
             ))
  ),
  tabPanel(
    "Agricultural Inputs", icon = icon("leaf"),
    sidebarPanel(
      width = 2,
      selectInput("crops_available_ag_inputs", "Available Crops", choices = ""),
      radioButtons(
        "ag_input_options",
        "Select Ag Input Category",
        choiceNames = c("Fertilizers", "Crop Protectants", "Manure"),
        choiceValues = c("Fertilizers", "Crop Protectants", "Manure")
      )
    ),
    mainPanel(
      HTML(message06),
      plotOutput("ag_inputs_plot", height = "600px")
    )
  ),
  tabPanel(
    "Staff Only", icon = icon("chart-line"),
    sidebarPanel(
      width = 2,
      conditionalPanel(
        condition = "input.passcode == 'tlon-uqbar-orbis-tertius'",
        radioButtons(
          inputId = "main_indic",
          label = "Select Indicator",
          choiceNames = sort(names(shiny_main_indicators)[3:19]),
          choiceValues = sort(names(shiny_main_indicators)[3:19])
        ),
        tags$hr(style="border-color:black;"),
        checkboxInput("decade_function", label = "Plot 10-yr linear functions")
      )
    ),
    mainPanel(
      h4(HTML(message01)),
      HTML(message02),
      plotOutput("all_crops", height = "800px"),
    )
  ),
  tabPanel("Additional Information", icon = icon("info-circle"),
           sidebarPanel(
             width = 2,
             selectInput("crops_available_add_info", "Available Crops", choices = "")),
           HTML(message10),
           br(),
           br(),
           mainPanel(
             br(),
             HTML("Percent of Data Present For Each Crop"),
             br(),
             tableOutput("data_completeness"),
             br(),
             HTML("Data Availability Matrix. Methane Emissions only apply to Rice, and Residue Removal only applies to
                  Barley and Wheat"),
             br(),
             tableOutput("data_avail_matrix"),
             br(),
             HTML("Tillage Fraction"),
             br(),
             plotOutput("tillage_plot"),
             br(),
             HTML("Irrigation Water Rate (acre-inches/acre)"),
             plotOutput("water_rates", width = "600px")
           )
           )
) # this is the end