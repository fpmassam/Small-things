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
require(readr)
require(rayshader)

MPsonTwitter_list_followers <- read_csv("MPsonTwitter_list_followers.csv")
View(MPsonTwitter_list_followers)


api_key = ''
api_secret = ''
bearer_token = ''
access_token = ''
access_secret = ""
tw_token = rtweet_app(bearer_token)

create_token(app = "EuPolitics", api_key, api_secret, access_token, access_secret)


#Get tweets in batches not to overload Twitter's api, bind the datasets together removing leftovers
mps_tweet = get_timeline(MPsonTwitter_list_followers[1:100,]$`Screen name`, n = 200, token = tw_token, 
                         verbose = TRUE)
save.image()
mps_tweet_2 = get_timeline(MPsonTwitter_list_followers[101:200,]$`Screen name`, n = 200, token = tw_token)
save.image()
mps_tweet_3 = get_timeline(MPsonTwitter_list_followers[201:300,]$`Screen name`, n = 200, token = tw_token)
save.image()
mps_tweet_4 = get_timeline(MPsonTwitter_list_followers[301:400,]$`Screen name`, n = 200, token = tw_token)
save.image()
mps_tweet_5 = get_timeline(MPsonTwitter_list_followers[401:500,]$`Screen name`, n = 200, token = tw_token)
save.image()
mps_tweet_6 = get_timeline(MPsonTwitter_list_followers[501:588,]$`Screen name`, n = 200, token = tw_token)
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
save.image()
#Partycolors 
party_color = unique(MPsonTwitter_list_followers$Party)
party_color = 
  data.frame(
    Party = party_color, 
    Color = c('#0087DC', 'ivory4', '#E4003B',
              "#528D6B","#FAA61A",'#FDF38E',
              '#2AA82C', 'purple4', '#326760', 
              '#D46A4C', '#005B54','#F6CB2F')
  )

#Select leaders 
front_benchers = c('Rishi Sunak',
                   'Keir Starmer'
                   )


#Select only what you need to reduce the size of the project 
mps_tweet =  rtweet::flatten(mps_tweet)
mps_tweet_1 = data.frame(
  name = mps_tweet$name,
  screen_name = mps_tweet$screen_name,
  status_id = mps_tweet$status_id,
  created_at = mps_tweet$created_at,
  retweet_count = mps_tweet$retweet_count,
  text = mps_tweet$text
)
  
  
#Perform a sentiment analysis 
mps_tweet_list = mps_tweet_1 %>% dplyr::select(status_id, text)

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
party_key = MPsonTwitter_list_followers %>% dplyr::select(`Screen name`, Name, Party)
separated = mps_tweet %>% dplyr::select(status_id, screen_name, name, created_at)
party_key$screen_name = gsub("@", "", party_key$`Screen name`)
separated = merge(party_key, separated, by = "screen_name")
separated = merge(separated, party_color, by = 'Party')
separated = merge(separated, melt_score, by = 'status_id')
separated[4] = NULL
rm(melt_score)

#Draw a chart for the factor analysis 

positions = data.frame(
  sentiment = c(
    "Negative: 0.9",
    "Fear: 0.8",
    "Anger: 0.8",
    "Sadness: 0.7",
    "Disgust: 0.7",
    "Positive: 0.8",
    "Trust: 0.7",
    "Surprise: 0.7",
    "Joy: 0.7",
    "Anticipation: 0.6",
    "Populism",
    "Populism",
    "Populism",
    "Populism",
    "Populism",
    "Confidence",
    "Confidence",
    "Expectation",
    "Expectation",
    "Expectation"
  ),
  position_x = c("a", "a","a", "a","a", "a","a", "a","a", "a",
                 "b", "b","b", "b","b", "b","b", "b","b", "b"),
  position_y = c(10,9,8,7,6,5,4,3,2,1, 8,8,8,8,8,4.7,4.7,2,2,2),
  group = c("a1","a2","a3","a4","a5","b1","b2","c1","c2","c3",
            "a1","a2","a3","a4","a5","b1","b2","c1","c2","c3"),
  color = c("a","a","a","a","a",
            "b","b", "c","c", "c","a","a","a","a","a",
            "b","b", "c","c", "c"),
  loadings = c(0.9, 0.8, 0.8, 0.7, 0.7,
               0.8,0.7, 0.7,0.7,0.6)
  
)


ggplot(positions, aes(x = position_x, y = position_y)) + 
  geom_line(aes(group = group, color = color), size = .5) +
  scale_color_brewer(palette =   "Set1") +
  geom_label(aes(label = sentiment), family = "EB Garamond", fontface  = 'bold') + picci + 
  theme(axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'none') + labs(title = 'Understanding sentiment in British politics',
                                         subtitle = "Factor analysis from an NRC sentiment analysis",
                                         caption = "SOURCE: Own calculations on Twitter data",
                                         x = '', y = '')


ggsave("factors.png", width = 20, height = 16, units = 'cm')




#Define events
events = data.frame(
  date = c(
    '2016-06-23',
    '2017-01-26',
    '2017-06-01',
    '2019-07-24',
    '2019-12-12',
    "2022-09-05",
    "2020-04-04"),
  event = c('Brexit referendum',
            'Theresa May becomes PM',
            'General Election',
            'Boris Johnson becomes PM',
            'General Election',
            'Liz Truss becomes PM',
            "Keir Stramer becomes Leader"
  ))


#generate a time series of the different sentiments

separated$date = as.Date(separated$created_at)
separated = split(separated, f = separated$variable)

separated = pblapply(separated, function(x){
    aggregate(
    value~ + variable + date + Party + Color, data = x, FUN = mean
  )
  
})



mean_sentiments = function(df) {
  list = split(df, f = df$Party) 
  roll_mean = function(pippo) {
    foo = data.frame(
      pippo,
      roll_var = zoo::rollmean(
        pippo$value, k = 20,fill = NA))
    return(foo)
  }
  new_data = pblapply(list, roll_mean)
  new_data = do.call(rbind, new_data)
  rownames(new_data) = NULL
  return(new_data)

}

time_series_agg_pop = lapply(separated, mean_sentiments)
time_series_agg_pop = do.call(rbind, time_series_agg_pop)
time_series_agg_pop_short = subset(time_series_agg_pop, date > as.Date("2015-12-31"))

focus = subset(time_series_agg_pop_short, Party %in% c('Labour', 'Conservative'))


ggplot(time_series_agg_pop_short, aes(x = date, y = roll_var, group = Party)) + 
  geom_line(aes(color = Party), alpha = .5) +
  scale_color_manual(
    values = unique(time_series_agg_pop$Color),
    guide = 'legend'
  ) +
  guides(color = guide_legend(title = NULL, override.aes = list(size = 3, alpha = 1),
                              keywidth = 0, ncol = 3)) +
  geom_line(data = focus,
            aes(x = date, y = roll_var), 
            color = focus$Color) +
  facet_wrap( ~ variable, ncol = 1) + 
  geom_vline(data = events, aes(xintercept = as.Date(date)),
             linetype = 'dashed') +
  geom_text(data = events, aes(x = as.Date(date) + 60, y =.4, label = event, 
                               group = date, face = 'bold'),
            angle = 90,
            vjust = .2,
            hjust = .5) +
  picci + labs(title = "A lack of confidence in Westminster?",
               subtitle = "Sentiment of current MPs since 2016", 
               caption = "SOURCE: Own calculations on Twitter data", 
               x = '', y = '')

ggsave("sentiment_three_way_UK.png", width = 20, height = 30, units = 'cm', dpi = 300)


#This is a test for a 3D model of the previous chart 
model_ar = ggplot(time_series_agg_pop_short, aes(x = date, y = Party)) + 
  geom_tile(aes(fill = roll_var)) +
  guides(color = guide_legend(title = NULL, override.aes = list(size = 3, alpha = 1),
                              keywidth = 0, ncol = 3)) + 
  facet_wrap( ~ variable, ncol = 1) + picci + 
   labs(title = "A lack of confidence in Westminster?",
               subtitle = "Sentiment of current MPs since 2016", 
               caption = "SOURCE: Own calculations on Twitter data", 
               x = '', y = '') + picci + theme(
                 panel.grid.major.y = element_blank()
               )


rayshader::plot_gg(model_ar, width = 10, height = 10)

save_obj("sentiment.obj")

  


#collect the last polls and smooth conservatives and labour numbers 
uk_polls = read_csv("gb-gbn.csv", col_types = cols(Precision = col_number(), 
                                                                    `Conservative Party` = col_number(), 
                                                                    `Labour Party` = col_number(), `Liberal Democrats` = col_number(), 
                                                                    `Scottish National Party` = col_number(), 
                                                                    `Plaid Cymru` = col_number(), `Scottish National Party + Plaid Cymru` = col_number(), 
                                                                    `UK Independence Party` = col_number(), 
                                                                    `Green Party` = col_number(), `Brexit Party` = col_number(), 
                                                                    `Change UK` = col_number(), Other = col_number()))

#Smooth parties 
labour_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1))
labour_spline = merge(labour_spline, uk_polls, by.x = 'date', by.y = 'Fieldwork End', all.x = TRUE)
labour_spline = spline(x = labour_spline$date, y = labour_spline$`Labour Party`, length(unique(labour_spline$date)))
labour_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1),
                  poll = labour_spline$y, Party = "Labour")


conservative_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1))
conservative_spline = merge(conservative_spline, uk_polls, by.x = 'date', by.y = 'Fieldwork End', all.x = TRUE)
conservative_spline = spline(x = conservative_spline$date, y = conservative_spline$`Conservative Party`, length(unique(conservative_spline$date)))
conservative_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1),
                           poll = conservative_spline$y, Party = "Conservative")


conservative_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1))
conservative_spline = merge(conservative_spline, uk_polls, by.x = 'date', by.y = 'Fieldwork End', all.x = TRUE)
conservative_spline = spline(x = conservative_spline$date, y = conservative_spline$`Conservative Party`, length(unique(conservative_spline$date)))
conservative_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1),
                                 poll = conservative_spline$y, Party = "Conservative")


libdem_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1))
libdem_spline = merge(libdem_spline, uk_polls, by.x = 'date', by.y = 'Fieldwork End', all.x = TRUE)
libdem_spline = spline(x = libdem_spline$date, y = libdem_spline$`Liberal Democrat`, length(unique(libdem_spline$date)))
libdem_spline = data.frame(date = seq(min(uk_polls$`Fieldwork End`), max(uk_polls$`Fieldwork End`), 1),
                                 poll = libdem_spline$y, Party = "Liberal Democrat")


polls_smoothed = rbind(labour_spline, conservative_spline)
time_series_w_polls = merge(time_series_agg_pop, polls_smoothed, by = c("date", "Party"))
time_series_w_polls = subset(time_series_w_polls, date > as.Date('2019-12-31'))


#visualise the effect of sentiment on the three major party smoothed share in the polls 

ggplot(time_series_w_polls, aes(x = roll_var, y = poll)) + 
  geom_point(aes(fill = Party),shape = 21) + 
  scale_fill_manual(values = unique(time_series_w_polls$Color),
                    guide = 'legend') +
  picci + geom_smooth(method = 'lm', color = 'black') +
  scale_y_continuous(labels = scales::label_number(suffix = '%')) +
  facet_grid(Party ~ variable) + theme(legend.position = 'none') +
  labs(title = "What works for the Labour does not for Conservatives?",
       subtitle = "Sentiment and effect on polls",
       x = "", y = "", caption = "SOURCE: Own calculations on Twitter and Europe Elects data")

ggsave("sentiment_party.png", width = 20, height = 20, units = 'cm')

time_series_w_polls_sound = time_series_w_polls
time_series_w_polls_sound[5] = NULL

time_series_w_polls_sound = time_series_w_polls %>%  dplyr::select(date, Party, variable, roll_var)

list = split(time_series_w_polls_sound, f = time_series_w_polls_sound$variable)

colnames(list[["Populism"]])[4] <- 'Populism'
colnames(list[["Confidence"]])[4] <- 'Confidence'
colnames(list[["Expectation"]])[4] <- 'Expectation'

list[["Populism"]][3] <- NULL
list[["Confidence"]][3] <- NULL
list[["Expectation"]][3] <- NULL

#Different in sentiment affecting polls 
days = uk_polls$`Fieldwork End` - uk_polls$`Fieldwork Start`
days = round(mean(days))    

time_series_w_polls_sound = merge(merge(list[["Populism"]], list[["Confidence"]]), list[["Expectation"]])

time_series_w_polls_sound_lab = subset(time_series_w_polls_sound, Party == 'Labour') 
time_series_w_polls_sound_lab = merge(time_series_w_polls_sound_lab, polls_smoothed, by = c("date", "Party"))
time_series_w_polls_sound_lab = time_series_w_polls_sound_lab

time_series_w_polls_sound_cons = subset(time_series_w_polls_sound, Party == 'Conservative') 
time_series_w_polls_sound_cons = merge(time_series_w_polls_sound_cons, polls_smoothed, by = c("date", "Party"))
time_series_w_polls_sound_cons = time_series_w_polls_sound_cons

time_series_w_polls_sound_lab$Populism_diff = time_series_w_polls_sound_lab$Populism -  dplyr::lag(time_series_w_polls_sound_lab$Populism, 2)
time_series_w_polls_sound_cons$Populism_diff = time_series_w_polls_sound_cons$Populism -  dplyr::lag(time_series_w_polls_sound_cons$Populism, 2)

time_series_w_polls_sound_lab$Confidence_diff = time_series_w_polls_sound_lab$Confidence -  dplyr::lag(time_series_w_polls_sound_lab$Confidence, 2)
time_series_w_polls_sound_cons$Confidence_diff = time_series_w_polls_sound_cons$Confidence -  dplyr::lag(time_series_w_polls_sound_cons$Confidence, 2)

time_series_w_polls_sound_lab$Expectation_diff = time_series_w_polls_sound_lab$Expectation -  dplyr::lag(time_series_w_polls_sound_lab$Expectation, 2)
time_series_w_polls_sound_cons$Expectation_diff = time_series_w_polls_sound_cons$Expectation -  dplyr::lag(time_series_w_polls_sound_cons$Expectation, 2)
senteffect = rbind(time_series_w_polls_sound_lab, time_series_w_polls_sound_cons)
summary(lm(poll ~ Populism_diff + Confidence_diff + Expectation_diff, data = senteffect))

ggplot(time_series_w_polls_sound_lab, aes(x = Populism_diff, y = poll)) + geom_point()

senteffect =  data.frame(senteffect[1:2], 
                         senteffect[6:9])
colnames(senteffect)[4:6] = c("Populism", "Confidence", "Expectation")
senteffect = reshape2::melt(senteffect, id.var= c("date", "Party", "poll"))
senteffect =  merge(senteffect, party_color)


ggplot(senteffect, aes(x = value, y = poll)) + geom_point(aes(fill = Party), shape = 21, alpha = .7) + 
  scale_fill_manual(values = unique(senteffect$Color), 
                    guide = 'legend') + scale_y_continuous(labels = scales::label_number(suffix = '%')) +
  facet_wrap( ~ variable) + picci + labs(title = "Consistency is key for political parties",
                                         subtitle = "Two-days sentiment variation on polling",
                                         caption = "SOURCE: Own calculations on Twitter and Europe Elects data", 
                                         x = "", y = '') 


ggsave("sentiment_delta.png", width = 20, height = 16, units = 'cm')



#Plot variation in polls

time_series_w_polls_sound_lab$poll_diff = time_series_w_polls_sound_lab$poll -  dplyr::lag(time_series_w_polls_sound_lab$poll, 2)
time_series_w_polls_sound_cons$poll_diff = time_series_w_polls_sound_cons$poll -  dplyr::lag(time_series_w_polls_sound_cons$poll, 2)
differences = rbind(time_series_w_polls_sound_cons, time_series_w_polls_sound_lab)
differences[3:6] = NULL
differences = na.omit(differences)
summary(lm(poll_diff ~ Populism_diff*Party + Confidence_diff*Party + Expectation_diff*Party, data = differences))



differences_plot = reshape2::melt(differences, id.vars = c('date', 'Party', "poll_diff"))
differences_plot = merge(differences_plot, party_color)

differences_plot$delta[differences_plot$variable == 'Populism_diff'] <- "Δ Populism"
differences_plot$delta[differences_plot$variable == 'Confidence_diff'] <- "Δ Confidence"
differences_plot$delta[differences_plot$variable == 'Expectation_diff'] <- "Δ Expectation"


ggplot(differences_plot, aes(x = value, y = poll_diff)) + geom_point(aes(fill = Party), shape = 21) + 
  scale_fill_manual(values = unique(differences_plot$Color)) +
  
  facet_grid(Party~delta) +
  picci + labs(title = "Changes in sentiment do not alter public opinion polls",
               subtitle = "Two-days changes in sentiment and two days percent point changes in polls",
               caption = "SOURCE: Own calculations on Twitter and Europe Elects data", 
               x = "Δ Sentiment", y = "Δ Poll (percent points change)") + theme(legend.position = 'none')


ggsave("delta_delta.png", width = 20, height = 16, units = 'cm')

#Topic modelling 
textcleaner <- function(x){
  require(textclean)
  require(stringr)
  x <- as.character(x)
  
  x <- x %>%
    str_to_lower() %>%  # convert all the string to low alphabet
    replace_contraction() %>% # replace contraction to their multi-word forms
    replace_internet_slang() %>% # replace internet slang to normal words
    replace_emoji() %>% # replace emoji to words
    replace_emoticon() %>% # replace emoticon to words
    replace_hash(replacement = "") %>% # remove hashtag
    replace_word_elongation() %>% # replace informal writing with known semantic replacements
    replace_number(remove = T) %>% # remove number
    replace_date(replacement = "") %>% # remove date
    replace_time(replacement = "") %>% # remove time
    replace_html(replacement = "") %>%
    replace_symbol() %>%
    replace_white() %>%
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim() # removes whitespace from start and end of string
  
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, stopwords("en"))
  
  # convert corpus to document term matrix
  return(DocumentTermMatrix(xdtm))
  
}

world = mps_tweet_1 %>% filter(
  created_at > as.Date("2015-12-31")) %>%
  dplyr::select(created_at,
         screen_name,
         text)
world = merge(party_key, world)

small_world = world[seq(1, nrow(world), 5), ]
n = nrow(small_world)
train_data <- sample(1:n, 0.25*n, replace = FALSE)
train_data_df <- small_world[train_data,]
deploy_df <- small_world[- train_data,]
train_clean = textcleaner(train_data_df)
freqterm_5 <- findFreqTerms(train_clean,1)
train_clean <- train_clean[,freqterm_5]
rownum_5 <- apply(train_clean,1,sum)
train_clean <- train_clean[rownum_5>0,]
lda_5 <- LDA(train_clean,k = 10,control = list(seed = 1502))
topic_5 <- tidy(lda_5,matrix = "beta")
top_terms_5 <- topic_5 %>%
  group_by(topic) %>%
  top_n(2,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)


#Individual MPs averages 

tweet_keys = mps_tweet_1[1:4]
tweet_score = merge(tweet_keys, scores)
tweet_score = tweet_score %>% filter(created_at > as.Date("2019-12-31"))
tweet_score = reshape2::melt(tweet_score, id.vars = c("status_id", "created_at", "screen_name", "name"))
tweet_score_mean = aggregate(value ~ variable + screen_name, data = tweet_score, FUN = mean)
colnames(tweet_score_mean)[3] <- "mean_sentiment"
tweet_score_sd = aggregate(value ~ variable + screen_name, data = tweet_score, FUN = sd)
colnames(tweet_score_sd)[3] <- "sd_sentiment"
mp_scores = merge(tweet_score_mean, tweet_score_sd)
mp_scores = merge(party_key, mp_scores)
mp_scores = merge(mp_scores, party_color)

selected = mp_scores%>% filter(Party %in% c("Conservative", "Labour"))
ggplot(selected, aes(x = mean_sentiment, y = Party)) + scale_fill_manual(values= unique(selected$Color)) +
  geom_jitter(aes(fill = Party), shape = 21) + ggrepel::geom_text_repel(aes(label = Name), max.overlaps = 30) +
  facet_wrap(~variable) + picci + theme(legend.position = 'none') + labs(title = "Unpredictable MPs",
                                                                         subtitle = "Mean sentiment per MP",
                                                                         x = "", y = '', caption = "SOURCE: Own calculations on Twitter data")
ggsave("mean.png", width = 20, height = 20, units = 'cm')



#Leaders of the Labour and the Conservatives 
PM_opp = mp_scores %>% filter(Name %in% front_benchers)
PM_opp = merge(PM_opp, party_color)
leaders_tweet = subset(mps_tweet_1, screen_name %in% PM_opp$screen_name)
molten_scores = reshape2::melt(scores)
leaders_tweet = merge(leaders_tweet, molten_scores, by = "status_id")
leaders_tweet = merge(leaders_tweet, party_key, by = 'screen_name')
leaders_tweet = merge(leaders_tweet, party_color, by = 'Party')


ggplot(PM_opp, aes(y = Name, x = mean_sentiment)) + 
  geom_linerange(aes(xmin = mean_sentiment - sd_sentiment,
                     xmax = mean_sentiment + sd_sentiment)) +
  geom_jitter(data = leaders_tweet, aes(x = value, y = name, fill = Party), 
              shape = 21, alpha = .1) +
  geom_point(aes(fill = Party), shape = 21, size = 4) + 
  scale_fill_manual(values = unique(PM_opp$Color)) +
  facet_wrap(~variable, ncol = 1) + picci + theme(panel.grid.major.y = element_blank(),
                                                  axis.ticks.y = element_line(), 
                                                  legend.position = 'bottom') + 
  scale_x_continuous(expand = c(0,0)) +
  labs(title = "Tweet it like a Prime Minister",
       subtitle = "Sentiment: average (big dot), standard deviation (line), single tweet (points)", 
       caption = "SOURCE: Own calculations on Twitter data", 
       x = '', y = '')

ggsave("leadership_contest.png", width = 20, height = 16, units = 'cm')
