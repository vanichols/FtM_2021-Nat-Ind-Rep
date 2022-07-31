
# Global

#--make sure user has pacman installed
install.packages(setdiff("pacman", rownames(installed.packages())))  
library(pacman)

#--this loads libraries you have, 
#---downloads and loads ones you don't
p_load(shiny, tidyverse, scales,
       shinyLP, rlang, cowplot, 
       ggrepel, xtable,
       shinythemes, tidytext,
       ggthemes,
       rcartocolor,
       kableExtra,
       usedist)



# Read data and color palettes
#load("outputs_for_ShinyApp_egc.rda")
load("outputs_for_ShinyApp_egc.rda")
load("my_palettes.rda")
load("avail_matrix.rda")

# suppress scientific notation
options(scipen = 999)

# password
# tlon-uqbar-orbis-tertius

# Vectors
# Crops
crops <- sort(unique(shiny_main_indicators$crop))
# Pass keys
passkeys <- c("QX36QqYfuB", rep("9wGmAbrTCZ", 2), "nJQZ4Yuyvm", "GKN8qDjB9T", "UsR8VhCTXe",
              "RM39W5H4aF", "dGC7znENhN", "F8aWcMMTXC", "8Rktr3vpQN", "mxk9zqJqkC",
              rep("tlon-uqbar-orbis-tertius", 11))

# passkey ref table
crop_keys <- data.frame(
  crop = rep(crops, 2),
  key = passkeys
)

message01 <- "Explore the preliminary Indicators available in the left-hand menu"

message02 <- "Default trend is a LOESS function with a span of 0.75 (in blue). Click the checkbox at the bottom of the left-hand Indicator menu to plot a linear function for each decade (in red)."

message03 <- "Preview the 2021 NIR Preliminary Data!"

message04 <- "The bar plots below show the contribution of each component to the Energy Use Indicator. Both raw values and proportions are shown."

message05 <- "The bar plots below show the contribution of each component to the GHG Emissions Indicator. Both raw values and proportions are shown."

message06 <- "Explore national rates for fertilizers, crop protectants, and manure N per acre planted. Trend shown is a LOESS function with a span of 0.75."

message07 <- "Summary chart of resource use over time. Data are presented in index form, where all indicators have been scaled by indicators averages for the period 1998-2002. A 0.1 point change is equal to a 10 percent difference. Index values allow for comparison of change across multiple dimensions with different units of measure. For Energy Use and Greenhouse Gas Emissions, the indicators per unit of production were used. A smaller area represents improvement (i.e. greater resource efficiency) over time."

message08 <- "Indicators averages for the period 1998-2002 used to scale the Indicators for each decade."

message09 <- "Raw data to graph the radar chart above."

message10 <- "This is a place to present an assortment of useful graphs and tables to interpret the 2021 National Indicators Report."

indic_names <- c("Acres Planted",
                 "BTU/acre (adjusted)",
                 "BTU/acre (unadjusted)",
                 "BTU/yield unit (adjusted)",
                 "BTU/yield unit (unadjusted)",
                 "CO2e/acre (adjusted)",
                 "CO2e/acre (unadjusted)",
                 "CO2e/yield unit (adjusted)",
                 "CO2e/yield unit (unadjusted)",
                 "Soil Erosion (tons/acre)",
                 "Irrigation Water Use",
                 "Land Use",
                 "Production (yield units)",
                 "Total BTU (adjusted)",
                 "Total BTU (unadjusted)",
                 "Total CO2e (adjusted)",
                 "Total CO2e (unadjusted)")

indic_names_2 <- c(
  acres_planted = "Acres Planted",
  BTUs_per_acre_adjusted = "BTU/acre (adjusted)",
  BTUs_per_acre_unadjusted = "BTU/acre (unadjusted)",
  BTUs_per_yield_unit_adjusted = "BTU/yield unit (adjusted)",
  BTUs_per_yield_unit_unadjusted = "BTU/yield unit (unadjusted)",
  CO2e_per_acre_adjusted = "CO2e/acre (adjusted)",
  CO2e_per_acre_unadjusted = "CO2e/acre (unadjusted)",
  CO2e_per_yield_unit_adjusted = "CO2e/yield unit (adjusted)",
  CO2e_per_yield_unit_unadjusted = "CO2e/yield unit (unadjusted)",
  erosion_rate_tons_acre = "Soil Erosion (tons/acre)",
  irrigation_water_use_fp = "Irrigation Water Use",
  land_use_fp = "Land Use",
  production = "Production (yield units)",
  total_BTUs_adjusted = "Total BTU (adjusted)",
  total_BTUs_unadjusted = "Total BTU (unadjusted)",
  total_CO2e_adjusted = "Total CO2e (adjusted)",
  total_CO2e_unadjusted = "Total CO2e (unadjusted)"
)

# Label vector as lookup table for energy per acre
energy_per_acre_labels <- c(
  application_BTUs_per_acre = "Application",
  chemicals_BTUs_per_acre_global_adjustment = "Crop Protection",
  drying_BTUs_per_acre = "Drying",
  fertilizer_BTUs_per_acre_adjusted = "Fertilizer",
  irrigation_BTUs_per_acre = "Irrigation",
  management_BTUs_per_acre  = "Management",
  seed_BTUs_per_acre = "Seed",
  transportation_BTUs_per_acre = "Transportation"
)

# Label vector as lookup table for CO2e per acre
CO2e_per_acre_labels <- c(
  application_CO2e_per_acre = "Application",
  chemicals_CO2e_per_acre_global_adjustment = "Crop Protection",
  drying_CO2e_per_acre_eGRID_adjusted = "Drying",
  fertilizer_CO2e_per_acre_adjusted = "Fertilizer",
  irrigation_CO2e_grid_adjusted_per_acre = "Irrigation",
  management_CO2e_per_acre  = "Management",
  n2o_CO2e_per_acre = "Nitrous Oxide",
  residue_burning_CO2e_per_acre = "Residue Burning",
  rice_methane_CO2e_per_acre = "Methane (Rice)",
  seed_CO2e_per_acre = "Seed",
  transportation_CO2e_per_acre = "Transportation"
)

# Label vector as lookup table for energy per yield unit
energy_per_yield_unit_labels <- c(
  application_BTUs_per_yield_unit = "Application",
  chemicals_BTUs_global_adjustment_per_yield_unit = "Crop Protection",
  drying_BTUs_per_yield_unit = "Drying",
  fertilizer_BTUs_adjusted_per_yield_unit = "Fertilizer",
  irrigation_BTUs_per_yield_unit = "Irrigation",
  management_BTUs_per_yield_unit  = "Management",
  seed_BTUs_per_yield_unit = "Seed",
  transportation_BTUs_per_yield_unit = "Transportation"
)

# Label vector as lookup table for CO2e per yield unit
CO2e_per_yield_unit_labels <- c(
  application_CO2e_per_yield_unit = "Application",
  chemicals_CO2e_global_adjustment_per_yield_unit = "Crop Protection",
  drying_CO2e_eGRID_adjusted_per_yield_unit = "Drying",
  fertilizer_CO2e_adjusted_per_yield_unit = "Fertilizer",
  irrigation_CO2e_grid_adjusted_per_yield_unit = "Irrigation",
  management_CO2e_per_yield_unit  = "Management",
  n2o_CO2e_per_yield_unit = "Nitrous Oxide",
  residue_burning_CO2e_per_yield_unit = "Residue Burning",
  rice_methane_CO2e_per_yield_unit = "Methane (Rice)",
  seed_CO2e_per_yield_unit = "Seed",
  transportation_CO2e_per_yield_unit = "Transportation"
)

# Plot functions. Bar chart
mybarplot01 <- function(mydf, filter_attribute, filter_crop, myxcol, myycol, fill_attribute, 
                        label_vector, y_axis_lab, title_string, fill_lab, own_palette) {
  
  # vector_of_attributes <- mydf %>%
  #   distinct(attribute) %>% 
  #   filter(str_detect(attribute, filter_attribute)) %>%
  #   pull(attribute)
  # 
  # coronel_palette01 <- setNames(object = scales::hue_pal(direction = -1)(length(vector_of_attributes)), nm = vector_of_attributes)
  
  mydf %>% 
    # Keep an eye on this filtering. It removes any 0-values
    filter(str_detect(attribute, filter_attribute) & crop == filter_crop & {{myycol}} != 0) %>%
    ggplot(aes(x = {{myxcol}}, y = {{myycol}}, fill = {{fill_attribute}})) +
    geom_col(color = "black") +
    labs(x = "", 
         y = y_axis_lab, 
         title = paste(title_string, "\n", filter_crop), 
         fill = fill_lab) +
    #theme_bw() +
    ggthemes::theme_base() +
    scale_y_continuous(expand = expansion(mult = c(0, .01)), breaks = scales::pretty_breaks(n = 5)) +
    #scale_fill_discrete(labels = label_vector) +
    #scale_fill_manual(labels = label_vector, values = own_palette) +
    scale_fill_carto_d(palette = "Vivid", labels = label_vector) +
    theme(
      text = element_text(face  = "bold"),
      plot.title = element_text(size = rel(1), face = "bold"),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "black", face = "bold"), # size = rel(0.8)),
      axis.title = element_text(color="black", face="bold"), #, size = 13),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      legend.position = "bottom", # bottom right none
      legend.direction = "vertical",
      rect = element_blank(),
      panel.border = element_rect(color = "black")) +
    geom_blank(aes(y = 0))
}

# Indicators at a glance

decade_desig <- data.frame(
  year = 1980:2020,
  decade = c(rep(1:4, each = 10), 4)
)

myindicators01 <- function(df, crop_filter, yvar) {
  df %>% 
    filter(crop %in% crop_filter) %>% 
    ggplot(aes(year, get(yvar), group = crop)) +
    labs(x = "Year", y = yvar, title = paste("Indicator:", yvar)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "loess", se = F, span = 0.75, formula = y ~ x) +
    facet_wrap(~ crop, scales = "free", ncol = 4) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    #theme_bw() +
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

myindicators02 <- function(df, crop_filter, yvar) {
  df %>% 
    filter(crop %in% crop_filter) %>%
    left_join(decade_desig) %>% 
    ggplot(aes(year, get(yvar), group = crop)) +
    labs(x = "Year", y = yvar, title = paste("Indicator:", yvar)) +
    geom_point() +
    geom_line() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    geom_smooth(method = "lm", se = FALSE, aes(group = decade), color = "red") +
    facet_wrap(~ crop, scales = "free", ncol = 4) +
    #theme_bw() +
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

# User Indicator options
user_indic_options <- c("Energy Use", "Greenhouse Gas Emissions", "Irrigation Water Use", "Land Use", "Soil Conservation", "Crop Context: Acres Planted and Production")

# main indic

# Acreage (divide)
# Production (divide)
# Energy Use (divide), per yield unit and per acre
# GHG Emissions, per yield unit and per acre
# Soil Erosion
# Land Use
# Irrigation Water Use

indic_user_plot_loess <- function(df, yvar, crop_user, y_label, title_text) {
  df %>% 
    filter(crop == crop_user) %>%
    ggplot(aes(year, get(yvar), group = crop)) +
    labs(x = "Year", 
         y = y_label, 
         title = paste(title_text, "\n", crop_user)) +
    geom_point(size = 2) +
    geom_line() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    geom_smooth(method = "loess", se = F, span = 0.75, formula = y ~ x) +
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

indic_user_plot_linear <- function(df, yvar, crop_user, y_label, title_text) {
  df %>% 
    filter(crop == crop_user) %>%
    left_join(decade_desig) %>% 
    ggplot(aes(year, get(yvar), group = crop)) +
    labs(x = "Year", 
         y = y_label, 
         title = paste(title_text, "\n", crop_user)) +
    geom_point(size = 2) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE, aes(group = decade), color = "red") +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
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

# Adapted radar chart
radarchart_coronel <- function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6, 
                                plwd = 1, pdensity = NULL, pangle = 45, pfcol = NA, cglty = 3, 
                                cglwd = 1, cglcol = "navy", axislabcol = "blue", 
                                title = "", maxmin = TRUE, na.itp = TRUE, centerzero = FALSE, 
                                vlabels = NULL, vlcex = NULL, caxislabels = NULL, calcex = NULL, 
                                paxislabels = NULL, palcex = NULL, ...) 
{
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE, 
       axes = FALSE, xlab = "", ylab = "", main = title, font = 4,
       asp = 1, ...)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg + 
                                                             CGap), lty = cglty, lwd = cglwd, border = cglcol)
    if (axistype == 1 | axistype == 3) 
      CAXISLABELS <- paste(i/seg * 100, "(%)")
    if (axistype == 4 | axistype == 5) 
      CAXISLABELS <- sprintf("%3.2f", i/seg)
    if (!is.null(caxislabels) & (i < length(caxislabels))) 
      CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | axistype == 
        5) {
      if (is.null(calcex)) 
        text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
             col = axislabcol, font = 2) # added
      else text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS, 
                col = axislabcol, cex = calcex, font = 2) # added
    }
  }
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty, 
           length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy * 
             1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels)) 
    PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    if (is.null(palcex)) 
      text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, font = 2) # added
    else text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol, 
              cex = palcex, font = 2) # added
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels)) 
    VLABELS <- vlabels
  if (is.null(vlcex)) 
    text(xx * 1.2, yy * 1.2, VLABELS, font = 2) # added
  else text(xx * 1.2, yy * 1.2, VLABELS, cex = vlcex, font = 2) # added
  series <- length(df[[1]])
  SX <- series - 2
  if (length(pty) < SX) {
    ptys <- rep(pty, SX)
  }
  else {
    ptys <- pty
  }
  if (length(pcol) < SX) {
    pcols <- rep(pcol, SX)
  }
  else {
    pcols <- pcol
  }
  if (length(plty) < SX) {
    pltys <- rep(plty, SX)
  }
  else {
    pltys <- plty
  }
  if (length(plwd) < SX) {
    plwds <- rep(plwd, SX)
  }
  else {
    plwds <- plwd
  }
  if (length(pdensity) < SX) {
    pdensities <- rep(pdensity, SX)
  }
  else {
    pdensities <- pdensity
  }
  if (length(pangle) < SX) {
    pangles <- rep(pangle, SX)
  }
  else {
    pangles <- pangle
  }
  if (length(pfcol) < SX) {
    pfcols <- rep(pfcol, SX)
  }
  else {
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1, 
    ] - df[2, ]) * seg/(seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", 
                  i, df[i, ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) + 
              xx[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) + 
              yy[left] * (df[i, left] - df[2, left])/(df[1, 
                                                         left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) + 
              xx[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + CGap)
            yyright <- yy[right] * CGap/(seg + CGap) + 
              yy[right] * (df[i, right] - df[2, right])/(df[1, 
                                                            right] - df[2, right]) * seg/(seg + CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright * 
                                 xxleft)/(yy[j] * (xxright - xxleft) - xx[j] * 
                                            (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) * 
            seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] * 
            (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) * 
            seg/(seg + CGap)
        }
      }
      if (is.null(pdensities)) {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], col = pfcols[i - 
                                                                                                      2])
      }
      else {
        polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i - 
                                                            2], border = pcols[i - 2], density = pdensities[i - 
                                                                                                              2], angle = pangles[i - 2], col = pfcols[i - 
                                                                                                                                                         2])
      }
      points(xx * scale, yy * scale, pch = ptys[i - 2], 
             col = pcols[i - 2])
    }
  }
}

# Functions and other items for average component proportion
per_acre_labels <- c(energy_per_acre_labels, CO2e_per_acre_labels)

attribute_names_per_acre <- data.frame(var_name = per_acre_labels) %>% 
  rownames_to_column(var = "attribute")

component_impact_plot <- function(df, crop_filter, detect, my_title) {
  
  # df %>% 
  #   ungroup() %>% 
  #   filter(crop == crop_filter & proportion != 0 & str_detect(attribute, detect)) %>% # BTU CO2
  #   group_by(attribute) %>% 
  #   summarize(avg_proportion = mean(proportion)) %>% 
  #   ungroup() %>% 
  #   left_join(attribute_names_per_acre, by = "attribute") %>% 
  #   mutate(var_name = forcats::fct_reorder(var_name, avg_proportion, .desc = F)) %>%
  #   ggplot(aes(var_name, avg_proportion)) +
  #   geom_col(fill = "#F15D22") +
  #   labs(x = "", y = "Average Proportion", title = paste(my_title, crop_filter)) +
  #   ggthemes::theme_base() +
  #   theme(
  #     rect = element_blank(),
  #     panel.border = element_rect(color = "black"),
  #     text = element_text(face  = "bold"),
  #     plot.title = element_text(size = rel(1), face = "bold"),
  #     axis.text = element_text(color = "black", face = "bold"), 
  #     axis.title = element_text(color="black", face="bold"),
  #     axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
  #   ) +
  #   geom_blank(aes(y = 0)) +
  #   coord_flip()
 
  df %>% 
    ungroup() %>% 
    filter(crop == crop_filter & proportion != 0 & str_detect(attribute, detect)) %>% # BTU CO2
    left_join(decade_desig, by = "year") %>% 
    group_by(decade, attribute) %>% 
    summarize(avg_proportion = mean(proportion)) %>% 
    ungroup() %>% 
    left_join(attribute_names_per_acre, by = "attribute") %>% 
    group_by(decade) %>% 
    mutate(var_name2 = tidytext::reorder_within(x = var_name, by = avg_proportion, within = decade),
           decade = factor(decade, levels = c(1, 2, 3, 4), 
                           labels = c("1980-89", "1990-99", "2000-09", "2010-20"))) %>%
    ungroup() %>% 
    ggplot(aes(var_name2, avg_proportion)) +
    geom_col(fill = "#F15D22") +
    labs(x = "", y = "Average Proportion", 
         title = paste(my_title, "\n", crop_filter)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
    tidytext::scale_x_reordered() +
    facet_wrap(~ decade, scales = "free") +
    ggthemes::theme_base() +
    theme(
      rect = element_blank(),
      panel.border = element_rect(color = "black"),
      text = element_text(face  = "bold"),
      plot.title = element_text(size = rel(1), face = "bold"),
      axis.text = element_text(color = "black", face = "bold"), 
      axis.title = element_text(color="black", face="bold"),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
    ) +
    geom_blank(aes(y = 0)) +
    coord_flip()
  
}

# Data completeness table
# Calculation in file "data completeness for context.R"
data_completeness_table <- structure(list(crop = c("Barley", "Corn, grain", "Corn, silage", 
                        "Cotton", "Peanuts", "Potatoes", "Rice", "Sorghum", "Soybeans", 
                        "Sugar beets", "Wheat"), 
                        percent_completeness = c(26.4, 38.1, 38.1, 38.7, 28.2, 34, 27.8, 29.9, 38.6, 24, 35.6)), 
                        row.names = c(NA, -11L), class = "data.frame")

# For line graphs with labels
# Functions and other items for average component proportion
all_labels <- c(energy_per_acre_labels, CO2e_per_acre_labels, energy_per_yield_unit_labels, CO2e_per_yield_unit_labels)

attribute_names_all_labels <- data.frame(var_name = all_labels) %>% 
  rownames_to_column(var = "attribute")

deprecated_line_plot_function <- function(mydf, filter_attribute, filter_crop, myxcol, myycol, color_attribute, 
                                    label_vector, y_axis_lab, title_string, color_lab, own_palette) {
  mydf2 <<- mydf %>% 
    # Keep an eye on this filtering. It removes any 0-values
    filter(str_detect(attribute, filter_attribute) & crop == filter_crop & {{myycol}} != 0) %>% 
    left_join(attribute_names_all_labels, by = "attribute")
  
  mydf2 %>%
    ggplot(aes(x = {{myxcol}}, y = {{myycol}}, color = {{color_attribute}})) +
    geom_line(size = 1.2) +
    labs(x = "", y = y_axis_lab, 
         title = paste(title_string, "\n", filter_crop), 
         color = color_lab) +
    ggthemes::theme_base() +
    scale_y_continuous(expand = expansion(mult = c(0, .01)), 
                       breaks = scales::pretty_breaks(n = 5)) +
   # scale_color_manual(labels = label_vector, values = own_palette) +
    scale_color_carto_d(labels = label_vector, 
                        palette = "Vivid") +
    theme(
      text = element_text(face  = "bold"),
      plot.title = element_text(size = rel(1), face = "bold"),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "black", face = "bold"), # size = rel(0.8)),
      axis.title = element_text(color="black", face="bold"), #, size = 13),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      legend.position = "none", # bottom right none
      #legend.direction = "vertical",
      rect = element_blank(),
      panel.border = element_rect(color = "black")) +
    geom_blank(aes(y = 0)) +
    ggrepel::geom_label_repel(
      aes(label = var_name), 
      data = subset(mydf2, year == 2015)
    )
}

line_plot_function <- function(mydf, filter_attribute, filter_crop, myxcol, myycol, color_attribute, 
                               label_vector, y_axis_lab, title_string, color_lab, own_palette) {
  mydf2 <<- mydf %>% 
    # Keep an eye on this filtering. It removes any 0-values
    filter(str_detect(attribute, filter_attribute) & crop == filter_crop & {{myycol}} != 0) %>% 
    left_join(attribute_names_all_labels, by = "attribute")
  
  mydf2 %>%
    ggplot(aes(x = {{myxcol}}, y = {{myycol}}, color = {{color_attribute}})) +
    geom_line(size = 1.2) +
    labs(x = "", y = y_axis_lab, 
         title = paste(title_string, "\n", filter_crop), 
         color = color_lab) +
    ggthemes::theme_base() +
    scale_y_continuous(expand = expansion(mult = c(0, .01)), breaks = scales::pretty_breaks(n = 5)) +
    scale_color_carto_d(labels = label_vector, palette = "Vivid") +
    theme(
      text = element_text(face  = "bold"),
      plot.title = element_text(size = rel(1), face = "bold"),
      axis.ticks = element_blank(),
      axis.text = element_text(color = "black", face = "bold"), # size = rel(0.8)),
      axis.title = element_text(color="black", face="bold"), #, size = 13),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      legend.position = "bottom", # bottom right none
      legend.direction = "vertical",
      rect = element_blank(),
      panel.border = element_rect(color = "black")) +
    geom_blank(aes(y = 0)) #+
    #ggrepel::geom_label_repel(
    #  aes(label = var_name), 
    #  data = subset(mydf2, year == 2015)
    #)
}

