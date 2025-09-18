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
if (length(end) - length(start) == 1) {
  end <- end[-1]
}
if (length(start) - length(end) == 1) {
  start <- start[-1]
}

  recession <- data.frame(start=start, end=end)

DAT1 <- fortify.zoo(DAT1)
quarter_labels <- format(as.yearqtr(DAT1$Index), "%Y Q%q")

HP = hpfilter(DAT1$GDPC1,freq = 1600)
  HP_components <- cbind(HP$trend, HP$cycle)
  colnames(HP_components) <- c("HPtrend","HPcycle")
  
  DAT1 <- cbind(DAT1,HP_components)
  DAT1$POTGAP <- DAT1$GDPC1 - DAT1$GDPPOT

  rec_dates <- data.frame(
    start = as.Date(c("1970-03-31", "1973-12-31", "1979-06-30", "1981-06-30", "1989-12-31", "2001-03-31", "2007-12-31", "2020-03-31")),
    end = as.Date(c("1970-12-31", "1975-03-31", "1980-03-31", "1982-06-30", "1991-03-31", "2001-09-30", "2009-06-30", "2020-06-30"))
  )
  
# Step 2: the UI (user-interface)
ui <- page_navbar(
  
  theme = bs_theme(version = 5),
  
  title = "Potential, Trend, and Current RGDP",
  
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
    
    
    p("(Dressler and Granera, 2025)")
    
    ),
  nav_panel("Long-Run Trends",
            girafeOutput("PLOT1", width = 800, height = 800)),
  
  nav_panel("Short-Run Cycles", 
            girafeOutput("PLOT2", width = 800, height = 800))
  
) # end page_navbar

# Define server 
server <- function(input, output, session) {
  
  # Update Slider Info:
  observeEvent(input$quarter_range, {
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
  

  output$PLOT1 <- renderGirafe({
    
    mindate <- which(quarter_labels == input$quarter_range[1])
    maxdate <- which(quarter_labels == input$quarter_range[2])
    
    DAT2 <- as.data.frame(DAT1[mindate:maxdate, ])
    
    vdate   <- which(quarter_labels[mindate:maxdate] == input$quarter_date)
    PTS2 <- as.data.frame(DAT2[vdate, ])
    
    p <-  ggplot() + 
      geom_line(data = DAT2, aes(x = Index,y = GDPC1, color = "GDP"), linewidth = 0.5) +
      geom_line(data = DAT2, aes(x = Index,y = GDPPOT, color = "Potential GDP"), linewidth = 0.5) + 
      geom_line(data = DAT2, aes(x = Index,y = HPtrend, color = "Trend GDP"), linewidth = 0.5) +
      geom_rect(data = recession, 
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.15) + 
      xlim(DAT1$Index[mindate],DAT1$Index[maxdate]) +
      labs(
        title = "Real Gross Domestic Product",
        caption = "Source: Federal Reserve Bank of St. Louis Database (FRED)",
        x = "", y = "Billions of Chained 2017 Dollars") + 
      scale_color_manual(breaks=c('GDP','Potential GDP','Trend GDP'),
                         values=c('GDP' = 'black',
                                  'Potential GDP' = 'darkred',
                                  'Trend GDP' = 'steelblue')) +
      theme_grey() +
      theme(legend.title = element_blank(), legend.position = "inside", 
            legend.position.inside = c(.2,.85)) + 
      geom_vline_interactive(xintercept = DAT2$Index[vdate], linetype = "dotted") + 
      geom_point_interactive(data = PTS2, aes(x = Index, y = GDPC1, tooltip = round(GDPC1,2), data_id = GDPC1)) + 
      geom_point_interactive(data = PTS2, aes(x = Index, y = GDPPOT, tooltip = round(GDPPOT,2), data_id = GDPPOT)) +
      geom_point_interactive(data = PTS2, aes(x = Index, y = HPtrend, tooltip = round(HPtrend,2), data_id = HPtrend)) 
    
     girafe(ggobj = p)
    
  })
  
  output$PLOT2 <- renderGirafe({
    
    mindate <- which(quarter_labels == input$quarter_range[1])
    maxdate <- which(quarter_labels == input$quarter_range[2])
    
    DAT2 <- as.data.frame(DAT1[mindate:maxdate, ])
    
    vdate   <- which(quarter_labels[mindate:maxdate] == input$quarter_date)
    PTS2 <- as.data.frame(DAT2[vdate, ])
    
    p2 <-  ggplot() + 
      geom_line(data = DAT2, aes(x = Index,y = POTGAP, color = "GDP - Potential"), linewidth = 0.5) + 
      geom_line(data = DAT2, aes(x = Index,y = HPcycle, color = "GDP - Trend"), linewidth = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed") + 
      geom_rect(data = recession, 
                aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                fill = "blue", alpha = 0.15) + 
      xlim(DAT1$Index[mindate],DAT1$Index[maxdate]) + 
      labs(
        title = "Real Gross Domestic Product",
        caption = "Source: Federal Reserve Bank of St. Louis Database (FRED)",
        x = "", y = "Billions of Chained 2017 Dollars") + 
      scale_color_manual(breaks=c('GDP - Potential','GDP - Trend'),
                         values=c('GDP - Potential' = 'darkred',
                                  'GDP - Trend' = 'steelblue')) +
      theme_grey() +
      theme(legend.title = element_blank(), legend.position = "inside", 
                          legend.position.inside = c(.2,.1)) + 
      geom_vline_interactive(xintercept = DAT2$Index[vdate], linetype = "dotted") + 
      geom_point_interactive(data = PTS2, aes(x = Index, y = POTGAP, tooltip = round(POTGAP,2), data_id = POTGAP)) + 
      geom_point_interactive(data = PTS2, aes(x = Index, y = HPcycle, tooltip = round(HPcycle,2), data_id = HPcycle)) 
    
    girafe(ggobj = p2)
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

  
  