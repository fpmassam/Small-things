library(readr)
library(tidyverse)
library(extrafont)
library(shiny)
speed_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
speed_data <- speed_data %>% select(date, location, new_cases)
speed_data$new_cases[is.na(speed_data$new_cases)] <- 0
speed_data = split(speed_data, f = speed_data$location)
speed_data = lapply(speed_data, function(x){
  date = as.data.frame(x$date)
  date = date[-1,]
  diff = diff(x$new_cases)
  location = unique(x$location)
  a = data.frame(
    location = location,
    date = date,
    difference = diff
  )
  a = a %>%
    mutate(`20-days average` = zoo::rollmean(difference, k = 20, fill = NA),
           `7-days average` = zoo::rollmean(difference, k = 7, fill = NA))
  return(a)
})
speed_data = do.call(rbind, speed_data)
speed_data = reshape2::melt(speed_data, id.var = c('date', 'location'))
speed_data = subset(speed_data, variable != 'difference')

picci = theme_minimal()  + theme(legend.position = 'bottom',
                                 plot.title = element_text(family = 'Garamond',
                                                           face = 'bold',
                                                           size = 18),
                                 plot.subtitle = element_text(family = 'Garamond',
                                                              face = 'plain',
                                                              colour = 'grey27',
                                                              size = 16),
                                 text = element_text(family = 'Garamond'),
                                 plot.caption = element_text(size = 11),
                                 axis.text = element_text(face = 'bold',
                                                          size = 12),
                                 panel.grid.major.y = (element_line(color = 'grey27')),
                                 panel.grid.minor.y = (element_blank()),
                                 panel.grid.major.x = (element_blank()),
                                 panel.grid.minor.x = element_blank(),
                                 axis.ticks.x = (element_line()),
                                 plot.background = element_rect(fill =
                                                                  'white',
                                                                color = 'white')) 
# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Global tracker about COVID's speed "),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'Country',
        label = 'Countries and territories',
        choices = c(unique(speed_data$location)),
        multiple = FALSE,
        selected = 'Europe'
      ),p('This basic apps measures how fast the pandemic is moving globally measuring its acceleration. 
                   In simple terms, I calculated the daily difference in new cases per country and smoothed it via a rolling mean at 7 and 20 days. 
                   This allows to read the pandemic data with more ease, without getting too crazy. 
                   This is an experimental project. Its experimental nature is the reason why charts are, so far, so basic.'),
                   p('Francesco Piccinelli Casagrande, @f_picci_journo')
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ggplot"), 
      p(paste('Latest update:', max(speed_data$date))
    )
  )
)
)

# Define server
server <- function(input, output,session) {
  
  dat = reactive({
    speed_data %>% 
      filter(location == input$Country)
  })
  
  output$ggplot <- renderPlot({ggplot(dat(), aes(
    x = date, y = value)) + geom_line(aes(group = location, 
                                          color = variable)) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_brewer(palette = 'Set1') + 
      picci + theme(legend.title = element_blank(),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(title = paste('How fast the pandemic moves in', dat()$location),
           subtitle = 'Daily new cases difference (roll mean at 7 and 20 days)',
           caption = 'SOURCE: Our world in data',
           x = '',
           y = '')
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)