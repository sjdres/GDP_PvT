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
library(plotly)

# Step 1: Load the Data
DAT1 <- pdfetch_FRED(c("GDPPOT","GDPC1","JHDUSRGDPBR"))
DAT1 <- na.omit(DAT1["1970/"])
DAT1 <- DAT1[complete.cases(DAT1[ ,'GDPC1']), ]
DAT1 <- fortify.zoo(DAT1)
quarter_labels <- format(as.yearqtr(DAT1$Index), "%Y Q%q")

HP = hpfilter(DAT1$GDPC1,freq = 1600)
  HP_components <- cbind(HP$trend, HP$cycle)
  colnames(HP_components) <- c("HPtrend","HPcycle")
  
  DAT1 <- cbind(DAT1,HP_components)
  DAT1$POTGAP <- DAT1$GDPC1 - DAT1$GDPPOT
  
  trend <- lm(log(DAT1$GDPC1) ~ seq(1,nrow(DAT1),1))
  Ltrend <- exp(fitted(trend))
  Lcycle <- DAT1$GDPC1 - Ltrend
  
#DAT1 <- as.xts(cbind(DAT1,Ltrend,Lcycle))
  DAT1 <- cbind(DAT1,Ltrend,Lcycle)

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
    
    checkboxGroupInput(
      "Trendz",
      "Definition of Long Run:",
      c(
        "Potential GDP" = "LR1",
        "Trend (Linear)"  = "LR2",
        "Trend (HP)"      = "LR3"
      )
    ),
    
    p("(Dressler and Granera, 2025)")
    
    ),
  nav_panel("Output and Long Run", 
            #plotOutput("PLOT1", click = "click_info")),
            plotlyOutput("PLOT1")),
  
  nav_panel("Short-Run Output", "Page 2 content")
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
  
  #
  #output$PLOT1 <- renderPlot({
  output$PLOT1 <- renderPlotly({
    
    mindate <- which(quarter_labels == input$quarter_range[1])
    maxdate <- which(quarter_labels == input$quarter_range[2])
    vdate   <- which(quarter_labels[mindate:maxdate] == input$quarter_date)
    
    DAT2 <- as.data.frame(DAT1[mindate:maxdate, ])
    PTS2 <- as.data.frame(DAT2[vdate, ])
    
    p <-  ggplot(DAT2) + 
      geom_line(aes(x = Index,y = GDPC1, color = "GDP"), linewidth = 1) +
      geom_line(aes(x = Index,y = GDPPOT, color = "Potential GDP"), linewidth = 1) + 
      geom_line(aes(x = Index,y = HPtrend, color = "Trend GDP"), linewidth = 1) +
      geom_vline(xintercept = DAT2$Index[vdate], linetype = "dotted") + 
      geom_point(data = PTS2, aes(x = Index, y = GDPC1)) + 
      geom_point(data = PTS2, aes(x = Index, y = GDPPOT)) +
      geom_point(data = PTS2, aes(x = Index, y = HPtrend)) +
      labs(
        title = "Real Gross Domestic Product",
        caption = "Source: Federal Reserve Bank of St. Louis Database (FRED)",
        x = "", y = "Billions of Chained 2017 Dollars") + 
      scale_color_manual(breaks=c('GDP','Potential GDP','Trend GDP'),
                         values=c('GDP' = 'black',
                                  'Potential GDP' = 'darkred',
                                  'Trend GDP' = 'steelblue')) +
      theme_grey() + 
      theme(legend.title = element_blank(),legend.position = "bottom")
    
    # Add vline if a click event occurred
    #if (!is.null(input$click_info)) {
    #  p <- p + geom_vline(xintercept = input$click_info$x, color = "blue", linetype = "dotted")
    #}
    #p
    
    ggplotly(p)
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

  
  