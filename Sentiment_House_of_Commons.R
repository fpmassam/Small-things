require(rtweet)
require(tidyverse)
require(syuzhet)
library(ppcor)
require(psych)
require(ggfortify)
require(topicmodels)
require(tm)
require(tidytext)
require(quanteda)
require(quanteda.textstats)

MPsonTwitter_list_followers <- read_csv("MPsonTwitter_list_followers.csv")
###Make sure to have an updated list of MPs from https://www.politics-social.com/list/followers
View(MPsonTwitter_list_followers)

api_key = ""
api_secret = ""

## authentication via web browser
token <- create_token(
  app = "EuPolitics",
  consumer_key = api_key, 
  consumer_secret = api_secret,
  access_token = '',
  access_secret = ''
) 
#Get tweets in batches not to overload Twitter's api, bind the datasets together removing leftovers
mps_tweet = get_timelines(MPsonTwitter_list_followers[1:100,]$`Screen name`, n = 800, token = token)
save.image()
mps_tweet_2 = get_timelines(MPsonTwitter_list_followers[101:200,]$`Screen name`, n = 800, token = token)
save.image()
mps_tweet_3 = get_timelines(MPsonTwitter_list_followers[201:300,]$`Screen name`, n = 800, token = token)
save.image()
mps_tweet_4 = get_timelines(MPsonTwitter_list_followers[301:400,]$`Screen name`, n = 800, token = token)
save.image()
mps_tweet_5 = get_timelines(MPsonTwitter_list_followers[401:500,]$`Screen name`, n = 800, token = token)
save.image()
mps_tweet_6 = get_timelines(MPsonTwitter_list_followers[501:588,]$`Screen name`, n = 800, token = token)
save.image()
mps_tweet = rbind(
  mps_tweet, 
  mps_tweet_2,
  mps_tweet_3,
  mps_tweet_4,
  mps_tweet_5,
  mps_tweet_6
)

rm(
  mps_tweet_2,
  mps_tweet_3,
  mps_tweet_4,
  mps_tweet_5,
  mps_tweet_6
)

#Partycolors 
party_color = unique(MPsonTwitter_list_followers$Party)
party_color = 
  data.frame(
    Party = party_color, 
    Color = c('#DC241f', '#0087DC', '#FDBB30',
              '#FFFF00','#000000', '#D46A4C',
              '#326760', '#FFFFFF', '#005B54',
              '#8a2be2', '#2AA82C', '#528D6B')
  )

#Select leaders 
front_benchers = c('Boris Johnson',
           'Keir Starmer',
           'Edward Davey',
           'Ian Blackford',
           'Lindsay Hoyle')


#Select only what you need to reduce the size of the project 
mps_tweet = flatten(mps_tweet)
mps_tweet = mps_tweet %>%
  select(screen_name, 
         status_id,
         created_at, text, 
         retweet_count)



#Perform a sentiment analysis 
mps_tweet_list = mps_tweet %>%
  select(status_id, text)

sentiment_nrc_tweets = function(x){
  require(pbapply)
  testi = split(x, f = x$status_id)
  foo = function(a){
    tokens = get_tokens(a$text)
    }
  sent = pblapply(testi, foo)
  return(sent)
}
sentiment = sentiment_nrc_tweets(mps_tweet_list)
sentiment = pblapply(sentiment, get_nrc_values)
sentiment = do.call(rbind, sentiment)
save.image()


#Dimensional reduction of Sentiment 
pcor(sentiment)
inds_matrix = cor(sentiment)
KMO(inds_matrix)
fanone <- fa(r=sentiment, nfactors = 3, rotate="varimax",fm="pa")
fa.diagram(fanone)
head(fanone$scores)
scores = data.frame(status_id = rownames(fanone$scores), 
                    fanone$scores)  
colnames(scores)[2:4] = c('Populism',
                          'Confidence',
                          'Expectation')
rownames(scores) = NULL
melt_score = reshape2::melt(scores)
separated = mps_tweet %>% select(status_id, Party)
separated = merge(separated, party_color, by = 'Party')
separated = merge(separated, melt_score, by = 'status_id')
rm(melt_score)

ggplot(separated, aes(x = Party, y = value)) + 
  geom_jitter(fill = separated$Color, shape = 21, alpha = .5) + 
  coord_flip() +
  facet_wrap(.~variable) + 
  labs(title = 'More emotion from the two main parties',
         subtitle = 'One point represents one tweet',
         caption = 'SOURCE: Own calculations on Twitter API data',
         x = '', 
         y = '') + picci + theme(
           panel.grid.major.y = element_blank(), 
           panel.grid.major.x = element_line(),
           (element_line(color = 'black')),
           plot.title.position = 'plot',
           axis.text.y = element_text(size = '8',
                                      face = 'plain')
           
         )
ggsave('sentiment_analysis.png', width = 20, height = 16, units = 'cm')


mps_tweet = merge(mps_tweet, scores, by = 'status_id')
mps_tweet$`Screen.name` = paste0('@', mps_tweet$screen_name)
mps_tweet = merge(MPsonTwitter_list_followers, mps_tweet, by = 'Screen.name')
ggplot(mps_tweet, aes(x = Party, y = Populism,
                      fill = Party), shape = 21) + geom_jitter(alpha = .7, shape = 21) + coord_flip()

#Collect and plot Populism average by party and by MP
averages = aggregate(Populism~Party, data = mps_tweet, FUN = mean)
ggplot(averages, aes(x = Party, y = Populism,
                      fill = Party)) + geom_col() + coord_flip()

averages_mp = aggregate(Populism~Name + Party + Constituency, data = mps_tweet, FUN = mean)
front_average = subset(averages_mp, Name %in% front_benchers)
averages_mp = merge(averages_mp, party_color, by = 'Party')

ggplot(averages_mp, aes(x = Party, y = Populism), shape = 21) + 
  geom_jitter(alpha = .7, shape = 21, fill = averages_mp$Color,
              size = 4) +
   coord_flip() +
  labs(title = 'Culture wars within the main parties?',
       subtitle = 'One point represents one MP',
       caption = 'SOURCE: Own calculation on Twitter API data',
       x = '') +
  picci +  theme(
     panel.grid.major.y = element_line(), 
     panel.grid.major.x = element_line(),
     plot.title.position = 'plot',
     axis.text.y = element_text(size = '10',
                                face = 'bold')
   )
ggsave('average_mp.png', width = 20, height = 16, units = 'cm')

top_20_populism = averages_mp %>% 
  top_n(n = 20, wt = Populism)

ggplot(top_20_populism, aes(x = reorder(Name, Populism), y = Populism)) + 
  geom_col(fill = top_20_populism$Color, color = 'black') + 
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(labels =  unique(flesh_average$Party), 
                    values = unique(flesh_average$Color)) +
  guides(fill = guide_legend(title = NULL, override.aes = list(size = 3),
                             keywidth = 0, ncol = 3)) +
  geom_text(aes(y = 0.01, x = Name, hjust = 0,
                 label = paste0('Populism', ':',' ', round(Populism,2), ' ', Party))) +
 coord_flip() + 
  labs(title = 'Is the Labour a populist party?',
       subtitle = 'Top-20 MPs by mean populism on their Twitter feed',
       caption = 'SOURCE: Own calculations on Twitter API',
       x = '',
       y = '') + 
  picci + theme(
   panel.grid.major.y = element_blank(),
   axis.ticks.x = element_blank(),
   axis.text.x = element_blank(),
   plot.title.position = 'plot'
 )
  
ggsave('top-20.png', width = 20, height = 15, units = 'cm')
save.image()


#Readability
mps_tweet = data.frame(mps_tweet, textstat_readability(mps_tweet$text))
mps_tweet_flesch = subset(mps_tweet, Flesch > 0)
save.image()
ggplot(mps_tweet_flesch, aes(x = Flesch, y = Populism)) + geom_point(aes(fill = Party),
                                                                     shape = 21)
flesh_average = aggregate(Flesch~Name+Constituency, data = mps_tweet_flesch,
                          FUN = mean)
flesh_average = merge(averages_mp, flesh_average, by = 'Name')
party = data.frame(Name = MPsonTwitter_list_followers$Name,
                   Party = MPsonTwitter_list_followers$Party)

flesh_average = merge(flesh_average, party, by = 'Name')
flesh_average = merge(flesh_average, party_color, by = 'Party')
flesh_front = subset(flesh_average, Name %in%
                       front_benchers)

ggplot(flesh_average, aes(x = Flesch, y = Populism)) +
  geom_point(aes(fill = Party), shape = 21, size = 3, alpha = .7) +
  geom_point(data = flesh_front, aes(fill = Party), shape = 21, size = 5, alpha = 1) +
  scale_fill_manual(labels =  unique(flesh_average$Party), 
                     values = unique(flesh_average$Color)) +
  geom_label(data = flesh_front, aes(x = Flesch, y = Populism,
                                                    label = Name), 
             nudge_x = 1, nudge_y = -.08) +
  guides(fill = guide_legend(title = NULL, override.aes = list(size = 3),
                             keywidth = 0, ncol = 3)
         ) +
  labs(title = 'Readability does not predict populism',
       subtitle = 'Mean populism and Flesch readability scale per MP',
       caption = 'SOURCE: Own calculation on Twitter API data',
       x = 'Flesch readability score') + 
  picci
ggsave('readability_populism.png', width = 20, height = 18, units = 'cm')


general_election_results_2019 %>% select(constituency_name, )

pop_results = data.frame(Constituency = general_election_summary_2019$constituency_name,
                         Result = general_election_summary_2019$result)

pop_results = merge(pop_results, flesh_average, by = 'Constituency')
save.image()
pop_small = pop_results %>% filter(
  Result %in% c(
    'Lab hold',
    'Con hold',
    'SNP gain from Con',
    'SNP gain from Lab',
    'Con gain from Lab'
  )
) 

ggplot(pop_small, aes(x = Result, 
           y = Populism)) + geom_jitter(aes(size = Flesch), 
                                        fill = pop_small$Color, shape = 21, alpha = .7) + 
  labs(title = 'Populist rhetoric damages the Labour',
       subtitle = 'One point represents one MP, color represents current affiliation',
       x = '',
       caption = 'SOURCE: Own calculations on Twitter API data') +
  coord_flip() + 
  picci + theme(
    panel.grid.major.y = element_line(), 
    panel.grid.major.x = element_line(),
    plot.title.position = 'plot',
    axis.text.y = element_text(size = '10',
                               face = 'bold')
  )

ggsave('results_populism.png', width = 20, height = 16, units = 'cm')


Education_constituency <- read_delim("Education_constituency.csv", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                                     skip = 1)
View(Education_constituency)
Education_constituency = Education_constituency %>% 
  filter(Year == 2019) %>%
  select(
  `Constituency Name`,
  `Cons. % Outstanding (Secondary)`)

Education_constituency = aggregate(`Cons. % Outstanding (Secondary)` ~ `Constituency Name`,
                                   data = Education_constituency, FUN = sum)

Education_language = merge(Education_constituency, flesh_average, by.x = 'Constituency Name',
      by.y = 'Constituency')
colnames(Education_language)[2] = 'share_excel_sec'

Education_language = subset(Education_language, share_excel_sec > 0 & Party %in% c
       ('Labour', 'Conservative'))

ggplot(Education_language, aes(x = share_excel_sec, y = Populism)) +
  geom_point(aes(fill = Party), shape = 21, size = 3, alpha = .7) +
  scale_fill_manual(labels =  unique(Education_language$Party), 
                    values = unique(Education_language$Color)) +
  guides(fill = guide_legend(title = NULL, override.aes = list(size = 3),
                             keywidth = 0, ncol = 2)
  ) + geom_abline(aes(intercept=-0.5,slope=1), color = 'black', linetype = 'dashed') +
  labs(title = 'Educational attainment is not the key',
       subtitle = 'Populism as predicted by the % of outstading students in Secondary education',
       caption = 'SOURCE: Own calculation on Twitter API and House of Commons data',
       x = '% Outstanding students (Secondary)') + 
  picci
ggsave('populism_edu.png', width = 20, height = 18, units = 'cm')

#Produce a time series for all parties and their Populism
time_series =  data.frame(
  date = as.Date(mps_tweet$created_at),
  party = mps_tweet$Party,
  Populism = mps_tweet$Populism
)

time_series_agg_pop = aggregate(
  Populism~date+party, data = time_series, FUN = mean
)

time_series_agg_pop = merge(time_series_agg_pop, 
                            party_color, 
                            by.x = 'party',
                            by.y = 'Party')

time_series_agg_pop$Mean_pop = zoo::rollmean(
  time_series_agg_pop$Populism, k = 20,fill = NA)
time_series_agg_pop_1 = na.omit(time_series_agg_pop)
focus = subset(time_series_agg_pop_1, party %in% c('Labour', 'Conservative'))
time_series_annotations = split(time_series_agg_pop_1, f =
                                  time_series_agg_pop_1$party)
time_series_annotations = do.call(rbind,
  lapply(time_series_annotations, function(df){
  subset(df, Mean_pop == max(Mean_pop))})) 

events = data.frame(
  date = c(
    '2016-06-23',
    '2017-01-26',
    '2017-06-01',
    '2019-07-24',
    '2019-12-12'),
  event = c('Brexit referendum',
            'Theresa May becomes PM',
            'General Election',
            'Boris Johnson becomes PM',
            'General Election'
  ))

events$date = as.Date(events$date)
time_series_agg_pop_1$Color[time_series_agg_pop_1$party == 'Independent'] <- 'ivory2'
ggplot(time_series_agg_pop_1, aes(x = date, group = party)) +
  geom_line(aes(y = Mean_pop, color = party),alpha = .3) + 
  scale_color_manual(
    values = unique(time_series_agg_pop_1$Color),
    guide = 'legend'
  ) +
  guides(color = guide_legend(title = NULL, override.aes = list(size = 3, alpha = 1),
                             keywidth = 0, ncol = 3)) +
  geom_line(data = focus,
            aes(x = date, y = Mean_pop), 
            color = focus$Color) + 
  geom_vline(data = events, aes(xintercept = as.Date(date)),
             linetype = 'dashed') +
  geom_text(data = events, aes(x = as.Date(date) + 60, y =.4, label = event, 
                                              group = date),
                           angle = 90,
                           vjust = .5,
                           hjust = .5) +
  labs(title = "Populism rising in Westminster's twittesphere",
       subtitle = '20-days rolling mean of Populism by party',
       caption = 'SOURCE: Own calculations on Twitter API data',
       x = '',
       y = '') +
  picci
ggsave('timeseries_populism.png', width = 20, height = 18, units = 'cm')

plot_grid(tseries,annotation)


daily_tweets = data.frame(
  table(time_series$date)
)


ggplot(daily_tweets, aes(x = as.Date(Var1), 
                         y = Freq)) +
  scale_y_continuous(limits = c(0,3000), labels = scales::comma) + geom_area(
                           fill = 'Steelblue3'
                         ) + picci




