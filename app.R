#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages used by the app. Install when needed:
library(shiny)
library(shinyWidgets)
library(bslib)
library(ggplot2)
library(gitlink)
library(pdfetch)
library(mFilter)
library(xts)
library(zoo)
library(ggiraph)

# Step 1: Load the Data
DAT1 <- pdfetch_FRED(c("GDPPOT","GDPC1","JHDUSRGDPBR"))
DAT1 <- na.omit(DAT1["1970/"])
DAT1 <- DAT1[complete.cases(DAT1[ ,'GDPC1']), ]

start <- index(DAT1$JHDUSRGDPBR[which(diff(DAT1$JHDUSRGDPBR)==1)])
end   <- index(DAT1$JHDUSRGDPBR[which(diff(DAT1$JHDUSRGDPBR)==-1)-1])
  if (length(end) - length(start) == 1) { end <- end[-1] }
  if (length(start) - length(end) == 1) { start <- start[-1] }
  recession <- data.frame(start=start, end=end)

DAT1 <- fortify.zoo(DAT1)
  quarter_labels <- format(as.yearqtr(DAT1$Index), "%Y Q%q")

HP <- hpfilter(DAT1$GDPC1,freq = 1600)
HP1 <- hpfilter(DAT1$GDPC1,freq = 400000)
  HP_components <- cbind(HP$trend, HP$cycle,HP1$trend, HP1$cycle)
  colnames(HP_components) <- c("HPtrend","HPcycle","HPtrendC","HPcycleC")
  
  DAT1 <- cbind(DAT1,HP_components)
  DAT1$POTGAP <- DAT1$GDPC1 - DAT1$GDPPOT

# Step 2: the UI (user-interface)
ui <- page_navbar(
  
  theme = bs_theme(version = 5),
  
  title = "Long-Run and Short-Run Output",
  
  sidebar = sidebar(
    
    # Select Data Range
    sliderTextInput(
      inputId = "quarter_range",
      label = "Select Data Range:",
      choices = quarter_labels,
      selected = c(quarter_labels[1], quarter_labels[length(quarter_labels)]), # Default to full range
      ),
    
    # Select Date Conditional on Range
    sliderTextInput(
      inputId = "quarter_date",
      label = "Select Date:",
      choices = quarter_labels,
      selected = quarter_labels[1] # Default to first date
    ),
    
    # Select LR Option(s)
    checkboxGroupInput(
      inputId = "LR_choice",
      label = "Select Long-Run Identification:",
      choices = c("Potential GDP" = "POT",
                  "Quarterly Trend" = "HPT",
                  "Credit Trend" = "HPTC"),
      selected = c("POT")
    ),
    
 p("(Dressler and Granera, 2025)")
    
    ),
  
  nav_spacer(),   
  
  nav_panel("Long-Run Analysis", 
            girafeOutput("PLOT1")),
 
  nav_panel("Short-Run Analysis", 
           girafeOutput("PLOT2"))
  
  ) # end page_navbar


# Define server 
server <- function(input, output, session) {
  
# Reactive values to store filtered data and avoid recalculation
  filtered_data <- reactive({
    req(input$quarter_range)
    
    mindate_idx <- match(input$quarter_range[1], quarter_labels)
    maxdate_idx <- match(input$quarter_range[2], quarter_labels)
    
    # Handle case where match() returns NA
    if(is.na(mindate_idx)) mindate_idx <- 1
    if(is.na(maxdate_idx)) maxdate_idx <- length(quarter_labels)
    
    DAT1[mindate_idx:maxdate_idx, ]
  })
  
  filtered_quarter_labels <- reactive({
    req(input$quarter_range)
    
    mindate_idx <- match(input$quarter_range[1], quarter_labels)
    maxdate_idx <- match(input$quarter_range[2], quarter_labels)
    
    # Handle case where match() returns NA
    if(is.na(mindate_idx)) mindate_idx <- 1
    if(is.na(maxdate_idx)) maxdate_idx <- length(quarter_labels)
    
    quarter_labels[mindate_idx:maxdate_idx]
  })
  
  LTID <- reactive({
    LR_choice <- input$LR_choice
  })
  
# Update Slider Info:
  observeEvent(input$quarter_range, {
    
    req(input$quarter_range)
    
    # Get the selected min and max from the first slider
    min_val <- as.yearqtr(input$quarter_range[1], format = "%Y Q%q")
    max_val <- as.yearqtr(input$quarter_range[2], format = "%Y Q%q")
    
    dates <- seq(min_val, max_val, by = 0.25)
    quarter_labels2 <- format(dates, "%Y Q%q")
    
    # Update the second min & max
    updateSliderTextInput(session, "quarter_date",
                     choices = quarter_labels2,
                     selected = quarter_labels2[1])
    })

# LR Plot:  
  output$PLOT1 <- renderGirafe({
    
    DAT2 <- filtered_data()
    labels <- filtered_quarter_labels()
    LR_choice <- LTID()
    
    req(input$quarter_date)
    vdate_idx <- match(input$quarter_date, labels)
    if(is.na(vdate_idx)) vdate_idx <- 1  # fallback
    PTS2 <- DAT2[vdate_idx, ]
    
    breaks1 <- c("Real GDP")
    values1 <- c("Real GDP" = 'black')
    
    p <- ggplot() + 
      geom_line(data = DAT2, aes(x = Index, y = GDPC1, color = 'Real GDP'), linewidth = 0.5) +
      geom_vline_interactive(xintercept = DAT2$Index[vdate_idx], linetype = "dotted") + 
      geom_point_interactive(data = PTS2, aes(x = Index, y = GDPC1, tooltip = paste("Real GDP", "<br>Value:", round(GDPC1,2)), data_id = GDPC1), color = "black")

    if ("POT" %in% LR_choice) {
      p <- p + 
        geom_line(data = DAT2, aes(x = Index, y = GDPPOT, color = 'Potential GDP'), linewidth = 0.5) + 
        geom_point_interactive(data = PTS2, aes(x = Index, y = GDPPOT, tooltip = paste("Potential GDP", "<br>Value:", round(GDPPOT,2)), data_id = GDPPOT), color = "darkred")
      
      breaks1 <- c(breaks1,"Potential GDP")
      values1 <- c(values1,"Potential GDP" = "darkred")
          }
    if ("HPT" %in% LR_choice) {
      p <- p + 
        geom_line(data = DAT2, aes(x = Index, y = HPtrend, color = 'Quarterly Trend'), linewidth = 0.5) +
        geom_point_interactive(data = PTS2, aes(x = Index, y = HPtrend, tooltip = paste("Quarterly Trend", "<br>Value:", round(HPtrend,2)), data_id = HPtrend), color = "steelblue")
      
      breaks1 <- c(breaks1,"Quarterly Trend")
      values1 <- c(values1,"Quarterly Trend" = "steelblue")
    }
    if ("HPTC" %in% LR_choice) {
      p <- p + 
        geom_line(data = DAT2, aes(x = Index, y = HPtrendC, color = 'Credit Trend'), linewidth = 0.5) +
        geom_point_interactive(data = PTS2, aes(x = Index, y = HPtrendC, tooltip = paste("Credit Trend", "<br>Value:", round(HPtrendC,2)), data_id = HPtrendC), color = "springgreen")
      
      breaks1 <- c(breaks1,"Credit Trend")
      values1 <- c(values1,"Credit Trend" = "springgreen")
    }
    
    p <- p +
      geom_rect(data = recession, 
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.15) + 
      xlim(DAT2$Index[1], DAT2$Index[nrow(DAT2)]) +
      scale_color_manual(name = "Data Lines:", breaks = breaks1, values = values1) +
      labs(
        title = "Real Gross Domestic Product",
        caption = "Source: Federal Reserve Bank of St. Louis Database (FRED)",
        x = "", y = "Billions of Chained 2017 Dollars") +
      theme_grey() +
      theme(legend.title = element_blank(), legend.position = "inside", 
            legend.position.inside = c(.2,.85),
            plot.title = element_text(size = 20), # Change plot title font size
            axis.title = element_text(size = 16), # Change axis title font size
            axis.text = element_text(size = 12),  # Change axis text font size
            legend.text = element_text(size = 12)) # Change legend text font size
    
    girafe(ggobj = p,
           width_svg = 9, 
           height_svg = 6,
           options = list(opts_tooltip(use_fill = TRUE),
                          opts_sizing(rescale = TRUE)))
  })
  
# SR Plot:
  output$PLOT2 <- renderGirafe({
    
    DAT2 <- filtered_data()
    labels <- filtered_quarter_labels()
    LR_choice <- LTID()
    
    req(input$quarter_date)
    vdate_idx <- match(input$quarter_date, labels)
    if(is.na(vdate_idx)) vdate_idx <- 1  # fallback
    PTS2 <- DAT2[vdate_idx, ]
    
    breaks1 <- c("")
    values1 <- c("")
    
    p <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_vline_interactive(xintercept = DAT2$Index[vdate_idx], linetype = "dotted")
      
    if ("POT" %in% LR_choice) {
      p <- p + 
        geom_line(data = DAT2, aes(x = Index, y = POTGAP, color = "GDP - Potential"), linewidth = 0.5) +
        geom_point_interactive(data = PTS2, aes(x = Index, y = POTGAP, tooltip = paste("GDP - Potential", "<br>Value:", round(POTGAP,2)), data_id = POTGAP), color = "darkred") 
        
      breaks1 <- c(breaks1,"GDP - Potential")
      values1 <- c(values1,"GDP - Potential" = "darkred")
    }
    if ("HPT" %in% LR_choice) {
      p <- p + 
        geom_line(data = DAT2, aes(x = Index, y = HPcycle, color = "GDP - Quarterly Trend"), linewidth = 0.5) +
        geom_point_interactive(data = PTS2, aes(x = Index, y = HPcycle, tooltip = paste("GDP - Quarterly Trend", "<br>Value:", round(HPcycle,2)), data_id = HPcycle), color = "steelblue") 
        
      breaks1 <- c(breaks1,"GDP - Quarterly Trend")
      values1 <- c(values1,"GDP - Quarterly Trend" = "steelblue")
    }
    if ("HPTC" %in% LR_choice) {
      p <- p + 
        geom_line(data = DAT2, aes(x = Index, y = HPcycleC, color = 'GDP - Credit Trend'), linewidth = 0.5) +
        geom_point_interactive(data = PTS2, aes(x = Index, y = HPcycleC, tooltip = paste("GDP - Credit Trend", "<br>Value:", round(HPcycleC,2)), data_id = HPcycleC), color = "springgreen") 
      
      breaks1 <- c(breaks1,"GDP - Credit Trend")
      values1 <- c(values1,"GDP - Credit Trend" = "springgreen")
    }
    
    p <- p +
      geom_rect(data = recession, 
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.15) + 
      xlim(DAT2$Index[1], DAT2$Index[nrow(DAT2)]) +
      scale_color_manual(name = "Data Lines:", breaks = breaks1, values = values1) +
      labs(
        title = "Real Gross Domestic Product",
        caption = "Source: Federal Reserve Bank of St. Louis Database (FRED)",
        x = "", y = "Billions of Chained 2017 Dollars") +
      theme_grey() +
      theme(legend.title = element_blank(), legend.position = "inside", 
            legend.position.inside = c(.2,.1),
            plot.title = element_text(size = 20), # Change plot title font size
            axis.title = element_text(size = 16), # Change axis title font size
            axis.text = element_text(size = 12),  # Change axis text font size
            legend.text = element_text(size = 12)) # Change legend text font size
            
    girafe(ggobj = p,
           width_svg = 9, 
           height_svg = 6,
           options = list(opts_tooltip(use_fill = TRUE),
                          opts_sizing(rescale = TRUE)))
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

  
  