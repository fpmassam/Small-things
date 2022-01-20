require(rtweet)
require(syuzhet)
require(tidyverse)
require(shiny)
require(ggiraph)

api_key = ''
api_secret = ''
bearer_token = ''
access_token = ''
access_secret = ''
tw_token = create_token('your_app', access_token = access_token, access_secret = access_secret,
                        consumer_key = api_key, consumer_secret = api_secret
)

twitter_quirinale =  search_tweets('#Quirinale', n = 500, token = tw_token, include_rts = FALSE)
twitter_quirinale = rtweet::flatten(twitter_quirinale)

most_active = as.data.frame(table(twitter_quirinale$name))

twitter_sent = twitter_quirinale %>% select(created_at, name, screen_name, status_id, text) %>%
  mutate(get_nrc_sentiment(text, language = 'italian'))
twitter_sent_time = data.frame(twitter_sent[1], twitter_sent[6:13])
twitter_sent_time = reshape2::melt(twitter_sent_time, id.vars = 'created_at')
totla = aggregate(value~created_at, data = twitter_sent_time, FUN = sum)
colnames(totla)[2] = 'total'
twitter_sent_time = merge(twitter_sent_time, totla)
twitter_sent_time$share = round(twitter_sent_time$value/twitter_sent_time$total, 1)

picci = theme_minimal()  + theme(legend.position = 'none',
                                 plot.title = element_text(face = 'bold'
                                 ),
                                 plot.subtitle = element_text(face = 'plain',
                                                              colour = 'grey27'
                                                              
                                 ),
                                 axis.text = element_text(face = 'bold'),
                                 panel.grid.major.y = (element_blank()),
                                 panel.grid.minor.y = (element_blank()),
                                 panel.grid.major.x = (element_blank()),
                                 panel.grid.minor.x = element_blank(),
                                 axis.ticks.x = (element_line()),
                                 axis.ticks.y = element_line(),
                                 plot.background = element_rect(fill =
                                                                  'white',
                                                                color = 'white'),
                                 panel.background = element_rect(
                                   fill = 'grey95', 
                                   color = 'white' )) 




ui <- fluidPage(
  tags$style(HTML("
  @import url('https://fonts.googleapis.com/css2?family=Cormorant+Garamond&display=swap');
  
         body {
        background-color: white;
        color: black;
        font-size: medium;
        font-family:'Cormorant Garamond';
        
         }
      h1 {
      face: 'bold';
      font-size: 'big';
      font-family:'Cormorant Garamond';
      }
      h2 {
        font-family: 'Cormorant Garamond';
      }
      .shiny-input-container {
        color: '#474747';
      }
      h3 {
        font-family: 'Cormorant Garamond';
        size = '24px'
        face: 'bold';
      }            ")), 
  
  column(12, offset = 0, 
         h1("How Twitter is reacting to the Italian presidential election"),
         h2("Real time sentiment analysis of the last 500 Tweets mentioning 'Quirinale'")),
  column(12, offset = 0, 
         ggiraphOutput('chart', height = '500%', 
                       width = '100%'))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dat = reactive(twitter_sent_time)
  
  output$chart <- renderGirafe({gg_stuff = ggplot(dat(), aes(x = created_at, 
                                                             y = value)) + 
    geom_area(aes(fill = variable)) +
    geom_line(aes(color = variable)) + 
    scale_color_brewer(palette = 'Set3') +
    scale_fill_brewer(palette = 'Set3') +
    facet_wrap(~variable, ncol = 2) + picci +
    labs(x = '', y = '', caption = 'SOURCE: NRC Analysis on Twitter API data')
  ggiraph(code = print(gg_stuff))})
}



# Run the application 
shinyApp(ui = ui, server = server)
