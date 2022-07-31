
# Server
server <- function(input, output, session) {
  
  ##### Tab: Homepage #####
  
  # Icon to display when access key is entered
  output$status <- renderUI({
    #paste("The data for", project_name(), "are ready for review")
    if (length(crops_available_options()) == 0) {
      icon("lock", "fa-2x")
    } else {
      icon("lock-open", "fa-2x")
    }
  })
  
  # Contact info
  output$contact_info <- renderUI({
    tagList(
      a("Gina Nichols", 
        href = 'mailto:gnichols@fieldtomarket.org?Subject=NIR%2021', target='_top'),
      "- Science and Research Manager at Field to Market")
  })
  
  # Crops available to review
  output$crops_granted <- renderText({
    
    validate(need(length(crops_available_options()) != 0, "None"))
    
    if (length(crops_available_options()) == 1) {
      crops_available_options()
    } else {
      paste0(crops_available_options(), " // ")  
    }
    
  })
  
  # Latest release date
  output$update_date <- renderText({
    "August 26, 2021"
  })
  
  ##### Crop observers #####
  # Crops available to see are dependent on pass key, the UI gets updated
  
  crops_available_options <- reactive({
    crop_keys %>% 
      filter(key == input$passcode) %>% 
      pull(crop)
  })
  
  observe({
    updateSelectInput(session, "crops_available_indic",
                      choices = crops_available_options())
  })
  
  observe({
    updateSelectInput(session, "crops_available_table",
                      choices = crops_available_options())
  })
  
  observe({
      updateSelectInput(session, "crops_available_EU",
                        choices = crops_available_options())
  })
  
  observe({
      updateSelectInput(session, "crops_available_GHG",
                        choices = crops_available_options())
  })
  
  observe({
    updateSelectInput(session, "crops_available_ag_inputs",
                      choices = crops_available_options())
  })
  
  observe({
    updateSelectInput(session, "crops_available_radar",
                      choices = crops_available_options())
  })
  
  observe({
    updateSelectInput(session, "crops_available_add_info",
                      choices = crops_available_options())
  })
  
  ##### Tab: Main Indicators #####
  
  output$indic_user_auto_width <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    if (input$main_indic_user == "Energy Use" & input$decade_function_user == FALSE) {
      
      plot1 <- indic_user_plot_loess(shiny_main_indicators, "BTU/yield unit (adjusted)", 
                                     input$crops_available_indic, 
                                     expression(bold(paste("BTU Yield Unit"^-1))),
                                     "Energy Use Per Yield Unit For")
      
      plot2 <- indic_user_plot_loess(shiny_main_indicators, "BTU/Acre (Millions)", 
                                     input$crops_available_indic, 
                                     expression(bold(paste("Million BTU Acre"^-1))),
                                     "Energy Use Per Acre For")
      
      plot_grid(plot1, plot2)   
      
    } else if (input$main_indic_user == "Energy Use" & input$decade_function_user == TRUE) {
      
      plot1 <- indic_user_plot_linear(shiny_main_indicators, "BTU/yield unit (adjusted)", 
                                      input$crops_available_indic, 
                                      expression(bold(paste("BTU Yield Unit"^-1))),
                                      "Energy Use Per Yield Unit for")
      
      plot2 <- indic_user_plot_linear(shiny_main_indicators, "BTU/Acre (Millions)", 
                                      input$crops_available_indic, 
                                      expression(bold(paste("Million BTU Acre"^-1))),
                                      "Energy Use Per Acre For")
      
      plot_grid(plot1, plot2)
    } else if (input$main_indic_user == "Greenhouse Gas Emissions" & input$decade_function_user == FALSE) {
      
      plot1 <- indic_user_plot_loess(shiny_main_indicators, "CO2e/yield unit (adjusted)", 
                                      input$crops_available_indic, 
                                     expression(bold(paste("Pounds CO"[2], " Eq. ","Yield Unit"^-1))),
                                     "Greenhouse Gas Emissions Per Yield Unit For")
      
      plot2 <- indic_user_plot_loess(shiny_main_indicators, "CO2e/acre (adjusted)", 
                                      input$crops_available_indic, 
                                     expression(bold(paste("Pounds CO"[2], " Eq. ","Acre"^-1))),
                                     "Greenhouse Gas Emissions Per Acre For")
      
      plot_grid(plot1, plot2)
    } else if (input$main_indic_user == "Greenhouse Gas Emissions" & input$decade_function_user == TRUE) {
      
      plot1 <- indic_user_plot_linear(shiny_main_indicators, "CO2e/yield unit (adjusted)", 
                                     input$crops_available_indic, 
                                     expression(bold(paste("Pounds CO"[2], " Eq. ","Yield Unit"^-1))),
                                     "Greenhouse Gas Emissions Per Yield Unit For")
      
      plot2 <- indic_user_plot_linear(shiny_main_indicators, "CO2e/acre (adjusted)", 
                                     input$crops_available_indic, 
                                     expression(bold(paste("Pounds CO"[2], " Eq. ","Acre"^-1))),
                                     "Greenhouse Gas Emissions Per Acre For")
      
      plot_grid(plot1, plot2)
      
    } else if (input$main_indic_user == "Crop Context: Acres Planted and Production" & 
               input$decade_function_user == FALSE) {
      
      plot1 <- indic_user_plot_loess(shiny_main_indicators, "Acres Planted (Millions)", 
                                     input$crops_available_indic, 
                                     "Acres Planted (Million)",
                                     "Acres Planted For")
      
      plot2 <- indic_user_plot_loess(shiny_main_indicators, "Production (Million Yield Units)", 
                                     input$crops_available_indic, 
                                     "Production (Million Yield Units)",
                                     "Production For")
      
      plot_grid(plot1, plot2)
    } else if (input$main_indic_user == "Crop Context: Acres Planted and Production" & 
               input$decade_function_user == TRUE) {
      
      plot1 <- indic_user_plot_linear(shiny_main_indicators, "Acres Planted (Millions)", 
                                     input$crops_available_indic, 
                                     "Acres Planted (Million)",
                                     "Acres Planted For")
      
      plot2 <- indic_user_plot_linear(shiny_main_indicators, "Production (Million Yield Units)", 
                                     input$crops_available_indic, 
                                     "Production (Million Yield Units)",
                                     "Production For")
      
      plot_grid(plot1, plot2)
      
    }
    
  })

  output$indic_user_fixed_width <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    if (input$main_indic_user == "Irrigation Water Use" & input$decade_function_user == FALSE) {
      
      indic_user_plot_loess(shiny_main_indicators, "Irrigation Water Use", 
                            input$crops_available_indic,
                            "Irrigation Water Use",
                            "Irrigation Water Use For")
    } else if (input$main_indic_user == "Irrigation Water Use" & input$decade_function_user == TRUE) {
      
      indic_user_plot_linear(shiny_main_indicators, "Irrigation Water Use", 
                             input$crops_available_indic,
                             "Irrigation Water Use",
                             "Irrigation Water Use For")
      
    } else if (input$main_indic_user == "Land Use" & input$decade_function_user == FALSE) {
      
      indic_user_plot_loess(shiny_main_indicators, "Land Use", 
                            input$crops_available_indic,
                            expression(bold(paste("Planted Acres Yield Unit"^-1))),
                            "Land Use For")
      
    } else if (input$main_indic_user == "Land Use" & input$decade_function_user == TRUE) {
      
      indic_user_plot_linear(shiny_main_indicators, "Land Use", 
                             input$crops_available_indic,
                             expression(bold(paste("Planted Acres Yield Unit"^-1))),
                             "Land Use For")
    } else if (input$main_indic_user == "Soil Conservation" & input$decade_function_user == FALSE) {
      
      indic_user_plot_loess(shiny_main_indicators, "Soil Erosion (tons/acre)", 
                            input$crops_available_indic,
                            expression(bold(paste("Tons Soil Loss Acre"^-1))),
                            "Soil Conservation For")
      
    } else if (input$main_indic_user == "Soil Conservation" & input$decade_function_user == TRUE) {
      
      indic_user_plot_linear(shiny_main_indicators, "Soil Erosion (tons/acre)", 
                             input$crops_available_indic,
                             expression(bold(paste("Tons Soil Loss Acre"^-1))),
                             "Soil Conservation For")
    }
    
  })
  
  ##### Tab: Indicator Tables #####
  
  table_loess_indicators <- reactive({
    
    table_df_filter_1 <- shiny_main_indicators %>% 
      filter(crop == input$crops_available_table)
    
    loess_land_use <- loess(`Land Use` ~ year,  data = table_df_filter_1, span = 0.75)
    loess_irrigation <- loess(`Irrigation Water Use` ~ year,  data = table_df_filter_1, span = 0.75)
    loess_energy <- loess(`BTU/yield unit (adjusted)` ~ year,  data = table_df_filter_1, span = 0.75)
    loess_ghg <- loess(`CO2e/yield unit (adjusted)` ~ year,  data = table_df_filter_1, span = 0.75)
    loess_erosion <- loess(`Soil Erosion (tons/acre)` ~ year,  data = table_df_filter_1, span = 0.75)
    
    table_df_filter_2 <- table_df_filter_1 %>% 
      mutate(LU = predict(loess_land_use),
             IWU = predict(loess_irrigation),
             EU = predict(loess_energy),
             GHG = predict(loess_ghg),
             SC = predict(loess_erosion)) %>%
      ungroup() %>% 
      select(year, 23:27) %>% 
      filter(year %in% c(1980, 1990, 2000, 2010, 2020))
    
  })
  
  output$loess_table <- function() {
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    df1 <- table_loess_indicators()
    
    names(df1) <- c("Year", "Land Use", "Irrigation Water Use", "Energy Use", 
                                         "Greenhouse Gas Emissions", "Soil Conservation")
    
    df1 %>%
      mutate(`Land Use` = round(`Land Use`, 4),
             `Irrigation Water Use` = round(`Irrigation Water Use`, 4),
             `Energy Use` = format(`Energy Use`, big.mark = ",", digits = 0),
             `Greenhouse Gas Emissions` = round(`Greenhouse Gas Emissions`, 1),
             `Soil Conservation` = round(`Soil Conservation`, 1)
             ) %>% 
      knitr::kable(format = "html", escape = F, align = "c") %>%
      kableExtra::kable_styling("striped", full_width = F, position = "center")
      
  }
  
  indic_tables_list <- reactive({ #function() {
    
    var_vector <- c("LU", "IWU", "EU", "GHG", "SC")
    var_name_vector <- c("Land Use", "Irrigation Water Use", "Energy Use", 
                         "Greenhouse Gas Emissions", "Soil Conservation")
    pct_change <- function(ref, year) (year - ref)/ref
    
    list_change <- list()
    
    for (q in 1:length(var_vector)) {
      
      vector1 <- table_loess_indicators() %>% 
        select(var_vector[q]) %>% 
        pull()
      
      names(vector1) <- table_loess_indicators() %>% 
        select(year) %>% 
        pull()
      
      list_change[[q]] <- round(usedist::dist_make(as.matrix(vector1), pct_change) * 100, 1)
    }
    
    list_change
    
  })
  
  output$LU <- renderUI({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    my_list <- indic_tables_list()
    
    item <- print(xtable(as.matrix(my_list[[1]]), align=rep("c", 6)),
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)

    html_item <- paste0("$$", item, "$$")

    list(
      withMathJax(HTML(html_item))
    )
      
  })
    
  output$IWU <- renderUI({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    my_list <- indic_tables_list()
    
    item <- print(xtable(as.matrix(my_list[[2]]), align=rep("c", 6)),
                  floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    
    html_item <- paste0("$$", item, "$$")
    
    list(
      withMathJax(HTML(html_item))
    )
    
  })
  
  output$EU <- renderUI({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    my_list <- indic_tables_list()
    
    item <- print(xtable(as.matrix(my_list[[3]]), align=rep("c", 6)),
                  floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    
    html_item <- paste0("$$", item, "$$")
    
    list(
      withMathJax(HTML(html_item))
    )
    
  })
  
  output$GHG <- renderUI({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    my_list <- indic_tables_list()
    
    item <- print(xtable(as.matrix(my_list[[4]]), align=rep("c", 6)),
                  floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    
    html_item <- paste0("$$", item, "$$")
    
    list(
      withMathJax(HTML(html_item))
    )
    
  })
  
  output$SC <- renderUI({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    my_list <- indic_tables_list()
    
    item <- print(xtable(as.matrix(my_list[[5]]), align=rep("c", 6)),
                  floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    
    html_item <- paste0("$$", item, "$$")
    
    list(
      withMathJax(HTML(html_item))
    )
    
  })
  
  ##### Tab: Summary Radar Chart #####
  
  scaled_crop_df_2 <- reactive({
    
    crop_df <- shiny_main_indicators %>% 
      filter(crop == input$crops_available_radar) %>% 
      ungroup() %>% 
      select(1, 3:5, 17, 19)
    
    # Instead of data just from year 2000, we use a 5-yr average of 1998-2002
    crop_year_avgs_1998_2002 <- crop_df %>% 
      filter(year >= 1998 & year <= 2002) %>% # 5 years
      summarize(
        `Land Use` = mean(`Land Use`),
        `Irrigation Water Use` = mean(`Irrigation Water Use`),
        `Soil Erosion (tons/acre)` = mean(`Soil Erosion (tons/acre)`),
        `BTU/yield unit (adjusted)` = mean(`BTU/yield unit (adjusted)`),
        `CO2e/yield unit (adjusted)` = mean(`CO2e/yield unit (adjusted)`)
      ) %>% 
      mutate(year = 2000) %>% 
      select(year, everything())
    
    scaled_crop_df <- data.frame(mapply('/', crop_df, crop_year_avgs_1998_2002)) %>% 
      mutate(year = 1980:2020,
             decade = c(rep(1:4, each = 10), 4)) %>%
      group_by(decade) %>% 
      summarize_all(mean) %>% 
      select(-c(year, decade)) %>% 
      rename(`Land Use` = Land.Use,
             `Greenhouse Gas Emissions` = CO2e.yield.unit..adjusted.,
             `Irrigation Water Use` = Irrigation.Water.Use,
             `Soil Conservation` = Soil.Erosion..tons.acre.,
             `Energy Use` = BTU.yield.unit..adjusted.)
    
    scaled_crop_df_2 <- rbind(rep(2, 5), rep(0, 5), scaled_crop_df)
    
  })
  
  output$radar_chart <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    my_vlabels <- c("Land\nUse", "Irrigation\nWater\nUse", "Soil\nConservation", 
                    "Energy\nUse", "Greenhouse\nGas\nEmissions")
    
    my_line_colors <- scales::hue_pal(h = c(0, 360) + 25, c = 100, l = 65, h.start = 0, direction = 1)(4)
    
    my_title <- paste("Resource Index Efficiency For", input$crops_available_radar)
    
    radarchart_coronel(scaled_crop_df_2(),
                       axistype = 1, 
                       centerzero = TRUE, 
                       vlabels = my_vlabels,
                       vlcex = 1.3,
                       plwd = 3,
                       seg = 4, # 4 8
                       caxislabels = seq(0, 2, 0.5), # seq(0, 2, 0.5) seq(0, 2, 0.25)
                       axislabcol = "black",
                       pcol = my_line_colors,
                       plty = 1,
                       title = my_title,
                       cglty = 1,
                       cglcol = "gray",
                       cglwd = 2)
    
    # Legend
    legend(x = 0.9, y = 1.3, legend = c("1980-89", "1990-99", "2000-09", "2010-20"), bty = "n", 
           pch = 19, col = my_line_colors , text.col = "black", cex = 1.3, pt.cex = 1.6, text.font = 2, 
           title =  expression(bold("Years Represented"))
    )
    
  })
  
  output$scaling_vector <- function(){#renderTable({
  
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
     crop_df <- shiny_main_indicators %>% 
       filter(crop == input$crops_available_radar) %>% 
       ungroup() %>% 
       select(1, 3:5, 17, 19)
    
    # Instead of data just from year 2000, we use a 5-yr average of 1998-2002
    crop_year_avgs_1998_2002 <- crop_df %>% 
      filter(year >= 1998 & year <= 2002) %>% # 5 years
      summarize(
        `Land Use` = mean(`Land Use`),
        `Irrigation Water Use` = mean(`Irrigation Water Use`),
        `Soil Conservation` = mean(`Soil Erosion (tons/acre)`),
        `Energy Use` = mean(`BTU/yield unit (adjusted)`),
        `Greenhouse Gas Emissions` = mean(`CO2e/yield unit (adjusted)`)
      ) %>%
      gather(key = "Indicator", value = "Value") %>%
      mutate(Value = as.character(signif(Value, 3))) %>% 
      bind_cols(Units = c("Planted Acres Per Yield Unit", "Acre-inches Per Yield Unit Increase",
                       "Tons Soil Loss Per Acre", "BTU Per Yield Unit", "Pounds CO2 Eq. Per Yield Unit")) %>% 
      knitr::kable(format = "html", escape = F, align = "l") %>%
      kableExtra::kable_styling("striped", full_width = F, position = "center")
      
  }#)
  
  output$raw_radar_data <- function() {
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    scaled_crop_df_2() %>% 
      bind_cols(Decade = c("Max", "Min", "1980-89", "1990-99", "2000-09", "2010-20")) %>% 
      filter(!Decade %in% c("Max", "Min")) %>% 
      select(Decade, everything()) %>% 
      knitr::kable(format = "html", escape = F, align = "l") %>%
      kableExtra::kable_styling("striped", full_width = F, position = "center")
    
  }
  
  ##### Tab: Energy Use #####
  output$PYU_energy <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_yield_unit_long, filter_attribute = "BTU", filter_crop = input$crops_available_EU, 
              myxcol = year, myycol = value, fill_attribute = attribute,
              label_vector = energy_per_yield_unit_labels, 
              y_axis_lab = expression(bold(paste("BTU Yield Unit"^-1))), #"Energy Use Values (BTU)", 
              title_string = "Energy Use Per Yield Unit For", fill_lab = "Energy Use Component",
              own_palette = palette_energy_per_yield_unit)
    
  })
  
  output$per_acre_energy <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_acre_long, filter_attribute = "BTU", filter_crop = input$crops_available_EU, 
                myxcol = year, myycol = value, fill_attribute = attribute,
                label_vector = energy_per_acre_labels, 
                y_axis_lab = expression(bold(paste("BTU Acre"^-1))), #"Energy Use Values (BTU)", 
                title_string = "Energy Use Per Acre For", fill_lab = "Energy Use Component",
                own_palette = palette_energy_per_acre)
  
  })
  
  output$PYU_energy_prop <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_yield_unit_long, filter_attribute = "BTU", filter_crop = input$crops_available_EU, 
                myxcol = year, myycol = proportion, fill_attribute = attribute,
                label_vector = energy_per_yield_unit_labels, y_axis_lab = "Proportion", 
                title_string = "Energy Use Proportions Per Yield Unit For", fill_lab = "Energy Use Component",
                own_palette = palette_energy_per_yield_unit)
    
  })
  
  output$per_acre_energy_prop <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_acre_long, filter_attribute = "BTU", filter_crop = input$crops_available_EU, 
                myxcol = year, myycol = proportion, fill_attribute = attribute,
                label_vector = energy_per_acre_labels, y_axis_lab = "Proportion", 
                title_string = "Energy Use Proportions Per Acre For", fill_lab = "Energy Use Component",
                own_palette = palette_energy_per_acre)
    
  })
  
  output$PYU_raw_energy_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_yield_unit_long, filter_attribute = "BTU",
                            filter_crop = input$crops_available_EU,
                            myxcol = year, myycol = value, color_attribute = attribute,
                            label_vector = energy_per_yield_unit_labels, 
                            y_axis_lab = expression(bold(paste("BTU Yield Unit"^-1))),
                            title_string = "Energy Use Per Yield Unit For", color_lab = "Energy Use Component",
                            own_palette = palette_energy_per_yield_unit)
    
  })
  
  output$PYU_prop_energy_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_yield_unit_long, filter_attribute = "BTU",
                       filter_crop = input$crops_available_EU,
                       myxcol = year, myycol = proportion, color_attribute = attribute,
                       label_vector = energy_per_yield_unit_labels, 
                       y_axis_lab = "Proportion",
                       title_string = "Energy Use Proportions Per Yield Unit For", 
                       color_lab = "Energy Use Component",
                       own_palette = palette_energy_per_yield_unit)
    
  })
  
  output$per_acre_raw_energy_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_acre_long, filter_attribute = "BTU", 
                       filter_crop = input$crops_available_EU, 
                            myxcol = year, myycol = value, color_attribute = attribute,
                            label_vector = energy_per_acre_labels, 
                            y_axis_lab = expression(bold(paste("BTU Acre"^-1))),
                            title_string = "Energy Use Per Acre For", 
                       color_lab = "Energy Use Component",
                            own_palette = palette_energy_per_acre)
    
  })
  
  output$per_acre_prop_energy_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_acre_long, filter_attribute = "BTU", 
                       filter_crop = input$crops_available_EU, 
                       myxcol = year, myycol = proportion, color_attribute = attribute,
                       label_vector = energy_per_acre_labels, 
                       y_axis_lab = "Proportion",
                       title_string = "Energy Use Proportions Per Acre For", 
                       color_lab = "Energy Use Component",
                       own_palette = palette_energy_per_acre)
    
  })
  
  output$component_impact_EU <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    component_impact_plot(shiny_per_acre_long, input$crops_available_EU, "BTU", 
                          "Average Energy Use Component Impact For")
    
  })
  
  ##### Tab: GHG #####
  
  output$PYU_ghg <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_yield_unit_long, filter_attribute = "CO2e", filter_crop = input$crops_available_GHG, 
                myxcol = year, myycol = value, fill_attribute = attribute,
                label_vector = CO2e_per_yield_unit_labels, 
                y_axis_lab = expression(bold(paste("Pounds CO"[2], " Eq. ","Yield Unit"^-1))), #expression(bold(paste("GHG Emission Values (lb CO"[2], "e)"))),
                title_string = "Greenhouse Gas Emissions Per Yield Unit For", fill_lab = "GHG Component",
                own_palette = palette_CO2e_per_yield_unit)
    
  })
  
  output$per_acre_ghg <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_acre_long, filter_attribute = "CO2e", filter_crop = input$crops_available_GHG, 
                myxcol = year, myycol = value, fill_attribute = attribute,
                label_vector = CO2e_per_acre_labels, 
                y_axis_lab = expression(bold(paste("Pounds CO"[2], " Eq. ","Acre"^-1))), #expression(bold(paste("GHG Emission Values (lb CO"[2], "e)"))),
                title_string = "Greenhouse Gas Emissions Per Acre For", fill_lab = "GHG Component",
                own_palette = palette_CO2e_per_acre)
    
  })
  
  output$PYU_ghg_prop <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_yield_unit_long, filter_attribute = "CO2e", filter_crop = input$crops_available_GHG, 
                myxcol = year, myycol = proportion, fill_attribute = attribute,
                label_vector = CO2e_per_yield_unit_labels, 
                y_axis_lab = "Proportion",
                title_string = "Greenhouse Gas Emissions Proportions Per Yield Unit For", 
                fill_lab = "GHG Component",
                own_palette = palette_CO2e_per_yield_unit)
    
  })
  
  output$per_acre_ghg_prop <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    mybarplot01(mydf = shiny_per_acre_long, filter_attribute = "CO2e", filter_crop = input$crops_available_GHG, 
                myxcol = year, myycol = proportion, fill_attribute = attribute,
                label_vector = CO2e_per_acre_labels, 
                y_axis_lab = "Proportion",
                title_string = "Greenhouse Gas Emissions Proportions Per Acre For", fill_lab = "GHG Component",
                own_palette = palette_CO2e_per_acre)
  
    })
  
  output$PYU_raw_ghg_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_yield_unit_long, filter_attribute = "CO2e", 
                       filter_crop = input$crops_available_GHG,
                       myxcol = year, myycol = value, 
                       color_attribute = attribute,
                       label_vector = CO2e_per_yield_unit_labels, 
                       y_axis_lab = expression(bold(paste("Pounds CO"[2], " Eq. ","Yield Unit"^-1))),
                       title_string = "Greenhouse Gas Emissions Per Yield Unit For", color_lab = "GHG Component",
                       own_palette = palette_CO2e_per_yield_unit)
    
  })
  
  output$PYU_prop_ghg_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_yield_unit_long, filter_attribute = "CO2e", 
                       filter_crop = input$crops_available_GHG,
                       myxcol = year, myycol = proportion, 
                       color_attribute = attribute,
                       label_vector = CO2e_per_yield_unit_labels, 
                       y_axis_lab = "Proportion",
                       title_string = "Greenhouse Gas Emissions Proportions Per Yield Unit For", 
                       color_lab = "GHG Component",
                       own_palette = palette_CO2e_per_yield_unit)
    
  })
  
  output$per_acre_raw_ghg_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_acre_long, filter_attribute = "CO2e", 
                       filter_crop = input$crops_available_GHG,
                       myxcol = year, myycol = value, 
                       color_attribute = attribute,
                       label_vector = CO2e_per_acre_labels, 
                       y_axis_lab = expression(bold(paste("Pounds CO"[2], " Eq. ","Acre"^-1))),
                       title_string = "Greenhouse Gas Emissions Per Acre For", color_lab = "GHG Component",
                       own_palette = palette_CO2e_per_acre)
    
  })
  
  output$per_acre_prop_ghg_line <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    line_plot_function(mydf = shiny_per_acre_long, filter_attribute = "CO2e", 
                       filter_crop = input$crops_available_GHG,
                       myxcol = year, myycol = proportion, 
                       color_attribute = attribute,
                       label_vector = CO2e_per_acre_labels, 
                       y_axis_lab = "Proportion",
                       title_string = "Greenhouse Gas Emissions Proportions Per Acre For", color_lab = "GHG Component",
                       own_palette = palette_CO2e_per_acre)
    
  })
  
  output$component_impact_GHG <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    component_impact_plot(shiny_per_acre_long, input$crops_available_GHG, "CO2", 
                          "Average Greenhouse Gas Emissions Component\nImpact For")
    
  })
  
  #### Tab: Ag Inputs ####
  
  output$ag_inputs_plot <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    if (input$ag_input_options == "Fertilizers") {
      ag_inputs_tillage %>% 
        filter(crop == input$crops_available_ag_inputs) %>% 
        select(1:2, 9:11) %>% 
        gather(key = "attribute", value = "value", -c(year, crop)) %>% 
        mutate(attribute = factor(attribute, levels = c("n_rate", "p_rate", "k_rate"), 
                                  labels = c("Nitrogen", "Phosphorus", "Potassium"))) %>% 
        ggplot(aes(year, value, group = crop)) +
        labs(x = "Year", y = expression(bold(paste("Pounds of Fertilizer Acre Planted"^-1))), 
             title = paste("Application Rate of Fertilizers For", input$crops_available_ag_inputs)) +
        geom_point() +
        geom_smooth(method = "loess", se = F, span = 0.75, formula = y ~ x, color = "black") +
        facet_wrap(~ attribute, scales = "free", ncol = 3) +
        ggthemes::theme_base() +
        theme(
          text = element_text(color = "black", face = "bold"),
          strip.text = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black", face = "bold"),
          axis.title = element_text(color = "black", face = "bold"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          rect = element_blank(),
          panel.border = element_rect(color = "black")) +
        geom_blank(aes(y = 0))
    } else if (input$ag_input_options == "Crop Protectants") {
      ag_inputs_tillage %>% 
        select(1:2, 12:16) %>% 
        filter(crop == input$crops_available_ag_inputs) %>% 
        gather(key = "attribute", value = "value", -c(year, crop)) %>% 
        mutate(attribute = factor(attribute, levels = c("herb_rate", "insect_rate", "fung_rate", 
                                                        "fumi_rate", "growth_rate"),
                                  labels = c("Herbicide", "Insecticide", "Fungicide",
                                             "Fumigant", "Growth Regulator"))) %>%
        filter(value > 0) %>% 
        ggplot(aes(year, value, group = crop)) +
        labs(x = "Year", y = expression(bold(paste("Pounds of Active Ingredient Acre Planted"^-1))), 
             title = paste("Application Rate of Crop Protectants For", input$crops_available_ag_inputs)) +
        geom_point() +
        geom_smooth(method = "loess", se = F, span = 0.75, formula = y ~ x, color = "black") +
        facet_wrap(~ attribute, scales = "free", ncol = 3) +
        ggthemes::theme_base() +
        theme(
          text = element_text(color = "black", face = "bold"),
          strip.text = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black", face = "bold"),
          axis.title = element_text(color = "black", face = "bold"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          rect = element_blank(),
          panel.border = element_rect(color = "black")) +
        geom_blank(aes(y = 0))
    } else {
      ag_inputs_tillage %>%
        mutate(total_N_per_acre = n_rate + manure_N_lbs_per_acre) %>% 
        select(1:4, total_N_per_acre) %>% 
        filter(crop == input$crops_available_ag_inputs) %>% 
        gather(key = "attribute", value = "value", -c(year, crop)) %>% 
        mutate(attribute = factor(attribute, levels = c("manure_N_lbs_per_acre", "manure_rate_all_acreage",
                                                        "total_N_per_acre"),
                                  labels = c("Manure Pounds N Per Acre", "Manure Effective Pounds N Per Acre",
                                             "Total N Per Acre (Organic + Inorganic)"))) %>%
        filter(value > 0) %>% 
        ggplot(aes(year, value, group = crop)) +
        labs(x = "Year", y = expression(bold(paste("Pounds of N Acre Planted"^-1))), 
             title = paste("Application Rate of Manure N For", input$crops_available_ag_inputs)) +
        geom_point() +
        geom_smooth(method = "loess", se = F, span = 0.75, formula = y ~ x, color = "black") +
        facet_wrap(~ attribute, scales = "free", ncol = 3) +
        ggthemes::theme_base() +
        theme(
          text = element_text(color = "black", face = "bold"),
          strip.text = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black", face = "bold"),
          axis.title = element_text(color = "black", face = "bold"),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          rect = element_blank(),
          panel.border = element_rect(color = "black")) +
        geom_blank(aes(y = 0))
      
    }
  })
  
  #### Tab: Main Indicators for Staff Only ####
  
  output$all_crops <- renderPlot({
    
    validate(need(input$passcode == "tlon-uqbar-orbis-tertius", "Enter Valid Key"))
    
    if (input$decade_function) {
      myindicators02(shiny_main_indicators, crops_available_options(), input$main_indic)
    } else {
      myindicators01(shiny_main_indicators, crops_available_options(), input$main_indic)
    }
    
  })
  
  #### Tab: Additional information ####
  
  output$data_completeness <- function () {
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    data_completeness_table %>%
      filter(crop == input$crops_available_add_info) %>% 
      rename(Crop = crop,
             `Percent Data Present` = percent_completeness) %>% 
      knitr::kable(format = "html", escape = F, align = "l") %>%
      kableExtra::kable_styling("striped", full_width = F, position = "center")
      
  }
  
  output$tillage_plot <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    ag_inputs_tillage %>% 
      select(1:2, 5:7) %>%
      filter(crop == input$crops_available_add_info) %>% 
      gather(key = "attribute", value = "value", -c(year, crop)) %>% 
      mutate(attribute = factor(attribute, levels = c("conventional_till_share", "reduced_till_share",
                                                      "no_till_share"),
                                labels = c("Conventional Tillage Share", "Reduced Tillage Share",
                                           "No-Till Share"))) %>%
      filter(value > 0) %>% 
      ggplot(aes(year, value, group = crop)) +
      labs(x = "Year", y = expression(bold(paste("Acreage Tillage Fraction"))), 
           title = paste("Tillage Fraction By Type For", input$crops_available_add_info)) +
      geom_point() +
      geom_smooth(method = "loess", se = F, span = 0.75, formula = y ~ x, color = "black") +
      facet_wrap(~ attribute, scales = "free", ncol = 3) +
      ggthemes::theme_base() +
      theme(
        text = element_text(color = "black", face = "bold"),
        strip.text = element_text(color = "black", face = "bold"),
        axis.text = element_text(color = "black", face = "bold"),
        axis.title = element_text(color = "black", face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        rect = element_blank(),
        panel.border = element_rect(color = "black")) +
      geom_blank(aes(y = 0))
  })
  
  output$data_avail_matrix <- function() {
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    avail_matrix %>% 
      filter(crop == input$crops_available_add_info) %>% 
      select(year, crop, 
             acres_harvested,
             acres_irrigated,
             manure_rate_all_acreage,
             chemical_herbicide,
             fertilizer_nitrogen,
             conventional_till_share,
             seeding_rate_lbs_per_acre,
             erosion_rate_tons_acre,
             percent_area_burned,
             methane_CO2e_acre,
             residue_removal_percent_acreage
      ) %>% 
      mutate(across(3:13, ~ ifelse(test = is.na(.),
                                   no = 1000,
                                   yes = 9999))) %>%
      mutate(across(3:13, ~ ifelse(test = . == 9999,
                         no = kableExtra::cell_spec(., background = "green", align = "center",
                                         background_as_tile = T, bold = T, color = "green"),
                         yes = kableExtra::cell_spec(., background = "red", align = "center",
                                        background_as_tile = T, color = "red")))) %>%
      rename(
        Year = year,
        `Acreage and Production` = acres_harvested,
        `Irrigation Survey` = acres_irrigated,
        `ARMS Manure` = manure_rate_all_acreage,
        `Chemical Usage` = chemical_herbicide,
        `Fertilizer Usage` = fertilizer_nitrogen,
        `Tillage` = conventional_till_share,
        `Seeding Rate` = seeding_rate_lbs_per_acre,
        `Soil Conservation` = erosion_rate_tons_acre,
        `Residue Burning` = percent_area_burned,
        `Methane Emissions` = methane_CO2e_acre,
        `Residue Removal` = residue_removal_percent_acreage
      ) %>% 
      select(-crop) %>% 
      knitr::kable(format = "html", escape = F, align = "c") %>%
      kableExtra::kable_styling("striped", full_width = T)
  }
  
  output$water_rates <- renderPlot({
    
    validate(need(length(crops_available_options()) != 0, "Enter Valid Key"))
    
    ag_inputs_tillage %>% 
      filter(crop == input$crops_available_add_info) %>% 
      ggplot(aes(year, water_applied_acre_inches, group = crop)) +
      labs(x = "Year", y = expression(bold(paste("Acre-inches of Water Acre"^-1))), 
           title = paste("Irrigation Water Rate For", input$crops_available_add_info)) +
      geom_point() +
      geom_line() +
      #geom_smooth(method = "loess", se = F, span = 0.75, formula = y ~ x, color = "black") +
      #facet_wrap(~ attribute, scales = "free", ncol = 3) +
      ggthemes::theme_base() +
      theme(
        text = element_text(color = "black", face = "bold"),
        strip.text = element_text(color = "black", face = "bold"),
        axis.text = element_text(color = "black", face = "bold"),
        axis.title = element_text(color = "black", face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        rect = element_blank(),
        panel.border = element_rect(color = "black")) +
      geom_blank(aes(y = 0))
      
  })
  
} # this is the end
