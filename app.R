#
# SDS 313 Shiny App - Project #2
#

#Loading necessary packages
library(shiny)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(bslib)
library(kableExtra)

#Importing dataset
Disasters <- read.csv('Climate-related_Disasters_Frequency.csv')

#Removing unused variables
Disasters <- select(Disasters, -ObjectId, -ISO2, -ISO3, -Unit, -Source, -CTS_Code, -CTS_Name, -CTS_Full_Descriptor)

#Renaming the 'Indicators' variable to 'Disaster'
Disasters <- rename(Disasters, Disaster = Indicator)

#Using a for loop to reformat the disaster variable so only the disaster name is left 
for (i in seq_along(Disasters$Disaster)){
  Disasters$Disaster[i] <- gsub("Climate related disasters frequency, Number of Disasters: ", "", Disasters$Disaster[i])
}

#Using a for loop and if statement to go through my year columns and replace all NA's with a numeric 0
for (i in 3:45) {
  if (any(is.na(Disasters[[i]]))) {
    Disasters[[i]][is.na(Disasters[[i]])] <- as.numeric(0)
  }
}

#Reformatting the dataset to make graphing easier
GraphDisasters <- Disasters %>%
  gather(key = "Year", value = "Frequency", -Country, -Disaster)
#Making the new year variable a numeric
GraphDisasters$Year <- parse_number(GraphDisasters$Year)





ui <- fluidPage(
  #Changing the font
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=EB+Garamond"),
    tags$style(HTML("body {font-family: 'EB Garamond', sans-serif;}"))
  ),
  
  #Changing the theme; bg is the background color and fg is the font color
  theme = bs_theme(bg = "#FBFBFD", fg = "#6699CC"),
  
  #Application title
  titlePanel(h1(strong("Frequency of Climate-Related Disasters"))),
  
  #Sideba 
  sidebarLayout(
    sidebarPanel(
      
      #Including an image
      img(src = "honduras-floodsBanner-WordPress.jpeg", width = 447, height = 155),
      
      hr(),
      
      #Sidebar with a slider input for the year range
      sliderInput("bins",
                  h4("Year:"),
                  min = min(GraphDisasters$Year),
                  max = max(GraphDisasters$Year),
                  value = min(GraphDisasters$Year),
                  sep = ""
      ),
      
      #App description
      tags$div(
        h4(strong("About this Application")),
        p("This application allows you to explore the frequency of climate-related disasters over the years."),
        p("You can use the slider above to select a specific year within the range of 1980 to 2022. The main panel will display graphs and tables based on the year selected."),
        p("The graph, ", em("Cumulated Frequency of Disasters"), ", displays the total frequency of the selected disasters from all the years, including and leading up to the one you've chosen."),
        p("The graph, ", em("Frequency of Disaster in Selected Year"), ", displays the frequency of the selected disasters in the chosen year."),
        p("You can customize the type of disasters and statistics displayed using the checkboxes below. You can also use the radio buttons to change the colors of the graphs."),
      ),
      
      #Putting the checkboxes side-by-side
      fluidRow(
        column(6, 
               #Checkbox for which disasters to graph
               checkboxGroupInput("Disasters",
                                  label = h4(strong("Disaster(s) to Display:")),
                                  choices = c("Storm", "Flood", "Drought", "Landslide", "Wildfire", "Extreme temperature"),
                                  selected = c("Storm", "Flood", "Drought", "Landslide", "Wildfire", "Extreme temperature"))
        ),
        
        column(6, 
               #Checkbox for which statistics to display
               checkboxGroupInput("Statistics",
                                  label = h4(strong("Statistic(s) to Display:")),
                                  choices = c("Mean", "Median"),
                                  selected = "Mean")
        )
      ),
      
      #Options for different palettes for the graphs
      radioButtons("radio", label = h4(strong("Graph Colors:")),
                   choices = list("Blue" = "Blues", "Green-Blue" = "GnBu", "Purple-Blue-Green" = "PuBuGn", "Purple" = "Purples"), 
                   selected = "Blues",
                   inline = TRUE
                   ),
    
    hr(),
      
      #References
      h4(strong("References")),
      tags$ul(
        tags$li(
          p("CRED. ", strong("“Dat - The International Disaster Database.”"), " EM, ",
            tags$a(href = "https://www.emdat.be/", "https://www.emdat.be/"), ". Accessed 9 Nov. 2023.")
        ),
        tags$li(
          p("“Climate-Related Disasters Frequency.” ", strong("Climate Change Indicators Dashboard, "),
            tags$a(href = "https://climatedata.imf.org/datasets/b13b69ee0dde43a99c811f592af4e821_0/about", "https://climatedata.imf.org/datasets/b13b69ee0dde43a99c811f592af4e821_0/about"), ". Accessed 9 Nov. 2023.")
        ),
        tags$li(
          p("“Climate Change and Disasters.” ShelterBox USA, 31 May 2023",
            tags$a(href = "https://www.shelterboxusa.org/climate-change/", "https://www.shelterboxusa.org/climate-change/"), ".")
        )
      )
    ),
    
    #Main panel
    mainPanel(
      #Outputting the graph for cumulated frequency of selected disasters
      plotOutput("CumulatedFrequency"),
      #Outputting the mean/median for cumulated frequency of selected disasters
      uiOutput("MeanMedianCumulated"),
      
      hr(),
      
      #Outputting the graph for frequency of selected disasters in the selected year
      plotOutput("FrequencyDisastersinYear"),
      #Outputting the mean/median for the frequency of selected disasters in the selected year
      uiOutput("MeanMedianYear"),
    )
  )
)


#Server logic
server <- function(input, output, session) {
  
  #Cumulated frequency graph
  output$CumulatedFrequency <- renderPlot({
    #Filtering the data so it's the data for all the years before and during the selected year (cumulative)
    FilteredData <- subset(GraphDisasters, Year <= input$bins)
    
    #Filtering the data so what's graphed depends on which disaster was selected
    FilteredData2 <- FilteredData %>%
      filter(Disaster %in% input$Disasters)
    
    #Using ggplot to make a bar graph
    ggplot(FilteredData2, aes(x = Disaster, y = Frequency, fill = Disaster)) +
      geom_bar(stat = "identity") +
      labs(title = 'Cumulated Frequency of Disasters',
           x = 'Disasters',
           y = 'Total Frequency') +
      theme_minimal() + 
      #Palette dependent on what is selected 
      scale_fill_brewer(palette = input$radio)
  })
  
  #Selected year frequency graph
  output$FrequencyDisastersinYear <- renderPlot({
    #Filtering the data so it's the data for only the selected year
    FilteredData <- subset(GraphDisasters, Year == input$bins)
    
    #Filtering the data so what's graphed depends on which disaster was selected
    FilteredData2 <- FilteredData %>%
      filter(Disaster %in% input$Disasters)
    
    #Using ggplot to make a bar graph
    ggplot(FilteredData2, aes(x = Disaster, y = Frequency, fill = Disaster)) +
      geom_bar(stat = "identity") +
      labs(title = 'Frequency of Disaster in Selected Year',
           x = 'Disasters',
           y = 'Frequency') +
      theme_minimal() + 
      #Palette dependent on what is selected 
      scale_fill_brewer(palette = input$radio)
  })
  

  #Mean/median for the cumulated graph
  output$MeanMedianCumulated <- renderUI({ 
    #Filtering the data so it's the data for all the years before and during the selected year (cumulative)
    FilteredData <- subset(GraphDisasters, Year <= input$bins)
    
    #Filtering the data so what's in the table depends on which disaster was selected
    FilteredData2 <- FilteredData %>%
      filter(Disaster %in% input$Disasters)
    
    #If both checkboxes are selected than both mean and median will be displayed
    if ("Mean" %in% input$Statistics & "Median" %in% input$Statistics) {
      MeanMedianCumulated <- FilteredData2 %>%
        #Getting rid of irrelevant data (NA's or in this case 0's)
        filter(Frequency != 0) %>%
        group_by(Disaster) %>%
        summarize(
          MeanCumulated = round(mean(Frequency),2),
          MedianCumulated = median(Frequency)
        ) %>%
        kable("html",
              col.names = c("Disaster Type",
                            "Cumulated Mean",
                            "Cumulated Median")) %>%
        kable_styling()
      HTML(MeanMedianCumulated)
    }
    #If only the mean box is selected than only mean will be displayed
    else if ("Mean" %in% input$Statistics) {
      MeanMedianCumulated <- FilteredData2 %>%
        #Getting rid of irrelevant data (NA's or in this case 0's)
        filter(Frequency != 0) %>%
        group_by(Disaster) %>%
        summarize(
          MeanCumulated = round(mean(Frequency),2)) %>%
        kable("html",
              col.names = c("Disaster Type",
                            "Cumulated Mean")) %>%
        kable_styling()
      HTML(MeanMedianCumulated)
    }
    #If only the median box is selected than only median will be displayed
    else if ("Median" %in% input$Statistics) {
      MeanMedianCumulated <- FilteredData2 %>%
        #Getting rid of irrelevant data (NA's or in this case 0's)
        filter(Frequency != 0) %>%
        group_by(Disaster) %>%
        summarize(
          MedianCumulated = median(Frequency)) %>%
        kable("html",
              col.names = c("Disaster Type",
                            "Cumulated Median")) %>%
        kable_styling()
      HTML(MeanMedianCumulated)
    }
  })
  
  #Mean/median for the selected year graph
  output$MeanMedianYear <- renderUI({ 
    #Filtering the data so it's the data for only the selected year
    FilteredData <- subset(GraphDisasters, Year == input$bins)
    
    #Filtering the data so what's in the table depends on which disaster was selected
    FilteredData2 <- FilteredData %>%
      filter(Disaster %in% input$Disasters)
    
    #If both checkboxes are selected than both mean and median will be displayed
    if ("Mean" %in% input$Statistics & "Median" %in% input$Statistics) {
      MeanMedianYear <- FilteredData2 %>%
        #Getting rid of irrelevant data (NA's or in this case 0's)
        filter(Frequency != 0) %>%
        group_by(Disaster) %>%
        summarize(
          MeanYear = round(mean(Frequency),2),
          MedianYear = median(Frequency)
        ) %>%
        kable("html",
              col.names = c("Disaster Type",
                            "Mean in Selected Year",
                            "Median in Selected Year")) %>%
        kable_styling()
      HTML(MeanMedianYear)
    }
    #If only the mean box is selected than only mean will be displayed
    else if ("Mean" %in% input$Statistics) {
      MeanMedianYear <- FilteredData2 %>%
        #Getting rid of irrelevant data (NA's or in this case 0's)
        filter(Frequency != 0) %>%
        group_by(Disaster) %>%
        summarize(
          MeanYear = round(mean(Frequency),2)) %>%
        kable("html",
              col.names = c("Disaster Type",
                            "Mean in Selected Year")) %>%
        kable_styling()
      HTML(MeanMedianYear)
    }
    #If only the median box is selected than only median will be displayed
    else if ("Median" %in% input$Statistics) {
      MeanMedianYear <- FilteredData2 %>%
        #Getting rid of irrelevant data (NA's or in this case 0's)
        filter(Frequency != 0) %>%
        group_by(Disaster) %>%
        summarize(
          MedianYear = median(Frequency)) %>%
        kable("html",
              col.names = c("Disaster Type",
                            "Median in Selected Year")) %>%
        kable_styling()
      HTML(MeanMedianYear)
    }
  })
}

#Running the application 
shinyApp(ui = ui, server = server)