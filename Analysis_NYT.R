require(nytimes)
require(tidyverse)
require(tm)
require(topicmodels)
require(tidyverse)

#Take articles
key = nytimes_key('')
sept_01 = 
  list(ny_archive(2001,9),
       ny_archive(2001,10),
       ny_archive(2001,11),
       ny_archive(2001,12),
       ny_archive(2002,01),
       ny_archive(2002,02),
       ny_archive(2002,03),
       ny_archive(2002,04),
       ny_archive(2002,05),
       ny_archive(2002,06))
get_url_date = function(list){
  a = function(x){
    data.frame(
      date = x$pub_date,
      headline = x$headline$main,
      abstract = x$abstract,
      by = x$byline$original,
      url = x$web_url,
      section = x$section_name,
      type = x$document_type,
      word_count = x$word_count)}  
  b = pbapply::pblapply(list, a)
  data.table::rbindlist(b)
}

sept_01 = lapply(sept_01, get_url_date)
sept_01 = data.table::rbindlist(sept_01)
sept_01 = subset(sept_01, !(by == ''))


sept_01_1 = list(
       ny_archive(2002,07),
       ny_archive(2002,08),
       ny_archive(2002,09),
       ny_archive(2002,10),
       ny_archive(2002,11),
       ny_archive(2002,12),
       ny_archive(2003,01),
       ny_archive(2003,02),
       ny_archive(2003,03),
       ny_archive(2003,04))
sept_01_1 = lapply(sept_01_1, get_url_date)
sept_01_1 = data.table::rbindlist(sept_01_1)
sept_01_1 = subset(sept_01_1, !(by == ''))
save.image()


sept_01_2 = list(
       ny_archive(2003,05),
       ny_archive(2003,06),
       ny_archive(2003,07),
       ny_archive(2003,08),
       ny_archive(2003,09),
       ny_archive(2003,10),
       ny_archive(2003,11),
       ny_archive(2003,12),
       ny_archive(2004,01))
sept_01_2 = lapply(sept_01_2, get_url_date)
sept_01_2 = data.table::rbindlist(sept_01_2)
sept_01_2 = subset(sept_01_2, !(by == ''))
save.image()
       
sept_01_3 = list(     
       ny_archive(2004,02),
       ny_archive(2004,03),
       ny_archive(2004,04),
       ny_archive(2004,05),
       ny_archive(2004,06),
       ny_archive(2004,07),
       ny_archive(2004,08),
       ny_archive(2004,09),
       ny_archive(2004,10)
)
sept_01_3 = lapply(sept_01_3, get_url_date)
sept_01_3 = data.table::rbindlist(sept_01_3)
sept_01_3 = subset(sept_01_3, !(by == ''))
save.image()

sept_01_4 = list(        
       ny_archive(2004,11),
       ny_archive(2004,12),
       ny_archive(2005,01),
       ny_archive(2005,02),
       ny_archive(2005,03),
       ny_archive(2005,04),
       ny_archive(2005,05),
       ny_archive(2005,06),
       ny_archive(2005,07))
sept_01_4 = lapply(sept_01_4, get_url_date)
sept_01_4 = data.table::rbindlist(sept_01_4)
sept_01_4 = subset(sept_01_4, !(by == ''))
save.image()

sept_01_5 = list(
       ny_archive(2005,08),
       ny_archive(2005,09),
       ny_archive(2005,10),
       ny_archive(2005,11),
       ny_archive(2005,12),
       ny_archive(2006,01),
       ny_archive(2006,02),
       ny_archive(2006,03),
       ny_archive(2006,04))
sept_01_5 = lapply(sept_01_5, get_url_date)
sept_01_5 = data.table::rbindlist(sept_01_5)
sept_01_5 = subset(sept_01_5, !(by == ''))
save.image()

sept_01_6 = list(
  ny_archive(2005,08),
  ny_archive(2005,09),
  ny_archive(2005,10),
  ny_archive(2005,11),
  ny_archive(2005,12),
  ny_archive(2006,01),
  ny_archive(2006,02),
  ny_archive(2006,03),
  ny_archive(2006,04),
  ny_archive(2006,05))
sept_01_6 = lapply(sept_01_6, get_url_date)
sept_01_6 = data.table::rbindlist(sept_01_6)
sept_01_6 = subset(sept_01_6, !(by == ''))
save.image()

sept_01_7 = list(
       ny_archive(2006,06),
       ny_archive(2006,07),
       ny_archive(2006,08),
       ny_archive(2006,09),
       ny_archive(2006,10),
       ny_archive(2006,11),
       ny_archive(2006,12),
       ny_archive(2007,01),
       ny_archive(2007,02),
       ny_archive(2007,03))
sept_01_7 = lapply(sept_01_7, get_url_date)
sept_01_7 = data.table::rbindlist(sept_01_7)
sept_01_7 = subset(sept_01_7, !(by == ''))
save.image()

sept_01_8 = list(
       ny_archive(2007,04),
       ny_archive(2007,05),
       ny_archive(2007,06),
       ny_archive(2007,07),
       ny_archive(2007,08),
       ny_archive(2007,09),
       ny_archive(2007,10),
       ny_archive(2007,11),
       ny_archive(2007,12),
       ny_archive(2008,01))
sept_01_8 = lapply(sept_01_8, get_url_date)
sept_01_8 = data.table::rbindlist(sept_01_8)
sept_01_8 = subset(sept_01_8, !(by == ''))
save.image()

sept_01_9 = list(
       ny_archive(2008,02),
       ny_archive(2008,03),
       ny_archive(2008,04),
       ny_archive(2008,05),
       ny_archive(2008,06),
       ny_archive(2008,07),
       ny_archive(2008,08),
       ny_archive(2008,09),
       ny_archive(2008,10),
       ny_archive(2008,11))
sept_01_9 = lapply(sept_01_9, get_url_date)
sept_01_9 = data.table::rbindlist(sept_01_9)
sept_01_9 = subset(sept_01_9, !(by == ''))
save.image()

sept_01_10 = list(
       ny_archive(2008,12),
       ny_archive(2009,01),
       ny_archive(2009,02),
       ny_archive(2009,03),
       ny_archive(2009,04),
       ny_archive(2009,05),
       ny_archive(2009,06),
       ny_archive(2009,07),
       ny_archive(2009,08),
       ny_archive(2009,09))
sept_01_10 = lapply(sept_01_10, get_url_date)
sept_01_10 = data.table::rbindlist(sept_01_10)
sept_01_10 = subset(sept_01_10, !(by == ''))
save.image()

sept_01_13 = list(
  ny_archive(2009,10),
  ny_archive(2009,11),
  ny_archive(2009,12),
  ny_archive(2010,01),
  ny_archive(2010,02),
  ny_archive(2010,03),
  ny_archive(2010,04),
  ny_archive(2010,05),
  ny_archive(2010,06),
  ny_archive(2010,07))
sept_01_11 = lapply(sept_01_11, get_url_date)
sept_01_11 = data.table::rbindlist(sept_01_11)
sept_01_11 = subset(sept_01_11, !(by == ''))
save.image()

sept_01_11 = list(
  ny_archive(2010,08),
  ny_archive(2010,09),
  ny_archive(2010,10),
  ny_archive(2010,11),
  ny_archive(2010,12),
  ny_archive(2011,01),
  ny_archive(2011,02),
  ny_archive(2011,03),
  ny_archive(2011,04),
  ny_archive(2011,05))
sept_01_11 = lapply(sept_01_11, get_url_date)
sept_01_11 = data.table::rbindlist(sept_01_11)
sept_01_11 = subset(sept_01_11, !(by == ''))
save.image()
 
sept_01_12 = list(
  ny_archive(2011,07),
  ny_archive(2011,08),
  ny_archive(2011,09),
  ny_archive(2011,10),
  ny_archive(2011,11),
  ny_archive(2011,12),
  ny_archive(2012,01),
  ny_archive(2012,02),
  ny_archive(2012,03),
  ny_archive(2012,04)
)
sept_01_12 = lapply(sept_01_12, get_url_date)
sept_01_12 = data.table::rbindlist(sept_01_12)
sept_01_12 = subset(sept_01_12, !(by == ''))
save.image()

sept_01_13 = list(       
       ny_archive(2012,05),
       ny_archive(2012,06),
       ny_archive(2012,07),
       ny_archive(2012,08),
       ny_archive(2012,09),
       ny_archive(2012,10),
       ny_archive(2012,11),
       ny_archive(2012,12),
       ny_archive(2014,01),
       ny_archive(2014,02))
sept_01_13 = lapply(sept_01_13, get_url_date)
sept_01_13 = data.table::rbindlist(sept_01_13)
sept_01_13 = subset(sept_01_13, !(by == ''))

sept_01_14 = list(ny_archive(2014,03),
                  ny_archive(2014,04),
                  ny_archive(2014,05),
                  ny_archive(2014,06),
                  ny_archive(2014,07),
                  ny_archive(2014,08),
                  ny_archive(2014,09),
                  ny_archive(2014,10),
                  ny_archive(2014,11),
                  ny_archive(2014,12))
sept_01_14 = lapply(sept_01_14, get_url_date)
sept_01_14 = data.table::rbindlist(sept_01_14)
sept_01_14 = subset(sept_01_14, !(by == ''))
save.image()

sept_01_15 = list(
       ny_archive(2015,01),
       ny_archive(2015,02),
       ny_archive(2015,03),
       ny_archive(2015,04),
       ny_archive(2015,05),
       ny_archive(2015,06),
       ny_archive(2015,07),
       ny_archive(2015,08),
       ny_archive(2015,09),
       ny_archive(2015,10))
sept_01_15 = lapply(sept_01_15, get_url_date)
sept_01_15 = data.table::rbindlist(sept_01_15)
sept_01_15 = subset(sept_01_15, !(by == ''))
save.image()


sept_01_16 = list(
       ny_archive(2015,11),
       ny_archive(2015,12),
       ny_archive(2016,01),
       ny_archive(2016,02),
       ny_archive(2016,03),
       ny_archive(2016,04),
       ny_archive(2016,05),
       ny_archive(2016,06),
       ny_archive(2016,07),
       ny_archive(2016,08)
)
sept_01_16 = lapply(sept_01_16, get_url_date)
sept_01_16 = data.table::rbindlist(sept_01_16)
sept_01_16 = subset(sept_01_16, !(by == ''))
save.image()

sept_01_17 = list(
       ny_archive(2016,09),
       ny_archive(2016,10),
       ny_archive(2016,11),
       ny_archive(2016,12),
       ny_archive(2017,01),
       ny_archive(2017,02),
       ny_archive(2017,03),
       ny_archive(2017,04),
       ny_archive(2017,05),
       ny_archive(2017,06))
sept_01_17 = lapply(sept_01_17, get_url_date)
sept_01_17 = data.table::rbindlist(sept_01_17)
sept_01_17 = subset(sept_01_17, !(by == ''))
save.image()

sept_01_18 = list(
       ny_archive(2017,07),
       ny_archive(2017,08),
       ny_archive(2017,09),
       ny_archive(2017,10),
       ny_archive(2017,11),
       ny_archive(2017,12),
       ny_archive(2018,01),
       ny_archive(2018,02),
       ny_archive(2018,03),
       ny_archive(2018,04)
)
sept_01_18 = lapply(sept_01_18, get_url_date)
sept_01_18 = data.table::rbindlist(sept_01_18)
sept_01_18 = subset(sept_01_18, !(by == ''))
save.image()


sept_01_19 = list(
       ny_archive(2018,05),
       ny_archive(2018,06),
       ny_archive(2018,07),
       ny_archive(2018,08),
       ny_archive(2018,09),
       ny_archive(2018,10),
       ny_archive(2018,11),
       ny_archive(2018,12),
       ny_archive(2019,01),
       ny_archive(2019,02))
sept_01_19 = lapply(sept_01_19, get_url_date)
sept_01_19 = data.table::rbindlist(sept_01_19)
sept_01_19 = subset(sept_01_19, !(by == ''))
save.image()


sept_01_20 = list(
  ny_archive(2019,03),
  ny_archive(2019,04),
  ny_archive(2019,05),
  ny_archive(2019,06),
  ny_archive(2019,07),
  ny_archive(2019,08),
  ny_archive(2019,09),
  ny_archive(2019,10),
  ny_archive(2019,11),
  ny_archive(2019,12) 
)
sept_01_20 = lapply(sept_01_20, get_url_date)
sept_01_20 = data.table::rbindlist(sept_01_20)
sept_01_20 = subset(sept_01_20, !(by == ''))
save.image()

sept_01_21 = list(
       ny_archive(2020,01),
       ny_archive(2020,02),
       ny_archive(2020,03),
       ny_archive(2020,04),
       ny_archive(2020,05),
       ny_archive(2020,06),
       ny_archive(2020,07),
       ny_archive(2020,08),
       ny_archive(2020,09),
       ny_archive(2020,10))

sept_01_21 = lapply(sept_01_21, get_url_date)
sept_01_21 = data.table::rbindlist(sept_01_21)
sept_01_21 = subset(sept_01_21, !(by == ''))
save.image()

sept_01_23 = list(
  ny_archive(2013,01),
  ny_archive(2013,02),
  ny_archive(2013,03),
  ny_archive(2013,04),
  ny_archive(2013,05),
  ny_archive(2013,06),
  ny_archive(2013,07),
  ny_archive(2013,08),
  ny_archive(2013,09),
  ny_archive(2013,10))
sept_01_23 = lapply(sept_01_23, get_url_date)
sept_01_23 = data.table::rbindlist(sept_01_21)
sept_01_23 = subset(sept_01_23, !(by == ''))
save.image()

sept_01_24 = list(
  ny_archive(2013,11),
  ny_archive(2013,12),
  )
sept_01_24 = lapply(sept_01_24, get_url_date)
sept_01_24 = data.table::rbindlist(sept_01_21)
sept_01_24 = subset(sept_01_24, !(by == ''))
save.image()


sept_01_22 = list(
  ny_archive(2020,11),
  ny_archive(2020,12))
sept_01_22 = lapply(sept_01_22, get_url_date)
sept_01_22 = data.table::rbindlist(sept_01_22)
sept_01_22 = subset(sept_01_22, !(by == ''))
save.image()


dfs = paste0('sept_01_', seq(1,24,1))
objects = mget(dfs)
sept_00 = do.call(rbind, objects)
sept_01 = rbind(sept_00, sept_01)
sept_00 = rbind(sept_00, sept_01_3)

#Number of daily articles 
daily_stories = table(as.Date(as.character(sept_00$date)))
daily_stories = as.data.frame(daily_stories)
save.image()

ggplot(daily_stories, aes(x = as.Date(daily_stories), y = Freq )) + geom_area(alpha = .8,
fill = 'steelblue2',
)  +
geom_smooth(color = 'red') +
geom_vline(data = Events, aes(xintercept = Date), linetype = 'dashed', color = 'black', size = .2) +
ggrepel::geom_text_repel(data = Events, aes(x= Date, y = 550, label = Event),
angle = 90,
vjust = .5,
hjust = .5)+
scale_x_date(expand = c(0,0)) +
labs(title = 'Stability after two decades of experiments',
subtitle = 'Daily published articles by The New York Times',
caption = 'SOURCE: The New York Times API',
x = '',
y = '') + picci
ggsave('articles_pub.png', width = 20, height = 15, units = 'cm')


#Study of multimedia articles 
daily_multi = sept_00 %>% select(
  date, type
) %>% filter(type == 'multimedia')
daily_multi = table(as.Date(daily_multi$date))
daily_multi = as.data.frame(daily_multi)
colnames(daily_multi)[2] = 'multimedia'
daily_multi = merge(daily_multi, daily_stories, by.x = 'Var1', by.y = 'daily_stories',
      all.y = TRUE)
daily_multi$multimedia[is.na(daily_multi$multimedia)] <- 0
daily_multi = 
  daily_multi %>% mutate(share_multi = round(multimedia/Freq,3))

ggplot(daily_multi, aes(x = as.Date(Var1), y = share_multi)) + 
  geom_area(alpha = .8,fill = 'steelblue2')  + 
  geom_smooth(color = 'red') +
  geom_vline(data = Events, aes(xintercept = Date), linetype = 'dashed', color = 'black', size = .2) +
  ggrepel::geom_text_repel(data = Events, aes(x= Date, y = 0.15, label = Event),
                           angle = 90,
                           vjust = .5,
                           hjust = .5)+
  scale_x_date(expand = c(0,0)) + scale_y_continuous(expand = c(0,0))+
  labs(title = 'The rise of multimedia',
       subtitle = '% of multimedia articles published by The New York Times',
       caption = 'SOURCE: The New York Times API',
       x = '',
       y = '') + picci
ggsave('articles_multi.png', width = 20, height = 15, units = 'cm')






#Lenght Articles 
lenght_stories_tab =  table(lenght_stories$word_count)

lenght_stories_tab = as.data.frame(lenght_stories_tab)
lenght_stories_agg = aggregate(word_count~as.Date(date), FUN = mean, data = lenght_stories)
lenght_stories_se = aggregate(word_count~as.Date(date), FUN = sd, data = lenght_stories)
colnames(lenght_stories_se) = c('date', 'standard_error')
colnames(lenght_stories_agg) = c('date', 'daily_mean')
lenght_stories_agg = merge(lenght_stories_agg, lenght_stories_se, by = 'date')

ggplot(lenght_stories_agg, aes(x = as.Date(date), y = daily_mean)) + geom_ribbon(aes(
ymin = daily_mean - standard_error, ymax = daily_mean + standard_error), alpha = .40,
fill = 'grey43') +
geom_line(color = 'steelblue1', alpha = 1) +
geom_smooth(color = 'red') + scale_x_date(expand = c(0,0)) + scale_y_continuous(limits = c(0,2000), labels = comma,
expand = c(0,0)) +
geom_label(aes(x = as.Date(x = '2015-01-01'), y = 300, label = 'Standard Deviation'),
color = 'grey43') +
geom_text(aes(x = as.Date(x = '2007-06-02'), y = 900, label = 'Daily mean'),
alpha = .8,
color = 'white') +
geom_vline(data = Events, aes(xintercept = Date), linetype = 'dashed', color = 'black', size = .2) +
ggrepel::geom_text_repel(data = Events, aes(x= Date, y = 1000, label = Event),
angle = 90,
vjust = .5,
hjust = .5)+ picci +
labs(title = 'Longer articles after the paywall',
subtitle = 'Average daily number of words per article',
caption = 'SOURCE: The New York Times API',
x= '',
y = '')
ggsave('lenght.png', width = 20, height = 15, units = 'cm')




#Study Sections 
sections = c('Business Day', 'World', 'U.S.', 'New York',
'Opinion', 'Sports', 'Washington', 'Climate', 'Blogs', 'Books')
section_day_1 = subset(section_day, as.character(section_day$Var1) %in% sections)
section_day_2 = subset(section_day, !(as.character(section_day$Var1) %in% sections))
section_day_2$Section = 'Other'
section_day_1$Section = section_day_1$Var1
section_day = rbind(section_day_1, section_day_2)
section_day = aggregate(Freq~Section + date, FUN = sum, data = section_day)
section_month = section_day
section_month = aggregate(Freq~Section + month, FUN = sum, data = section_month)
section_month$lab = as.Date(paste("01-", section_month$month, sep = ""), format = "%d-%B %Y")







gif_articles = section_month %>% mutate(name = fct_relevel(as.character(Section),
                                            'Other',
                                            'World',
                                            'Washington',
                                            'Sports',
                                            'Opinion',
                                            'U.S.',
                                            'New York',
                                            'Climate',
                                            'Business Day',
                                            'Books',
                                            'Blogs'),
) %>%
  arrange(lab) %>%
  mutate(month = factor(month, unique(month))) %>%
  group_by(name) %>%
  ggplot(aes(x = name, y = Freq, fill = name)) +
  geom_col(fill = 'steelblue2') +
  geom_text(aes(x = name, y = Freq + 300, label = comma(Freq))) +
  scale_y_continuous(labels= comma, expand = c(0,0)) +
  coord_flip() +
  labs(
    title = 'Sections in The New York Times',
    subtitle = '{closest_state}',
    caption = 'SOURCE: The New York Times API',
    x = '', y = ''
  ) +
  transition_states(month) +
  enter_fade() +
  exit_fade() + 
  picci_h_barplot
save.image()
gif_articles
giffe = animate(gif_articles, height = 15, width = 20, units = 'cm', res = 300, device =
                  'png', nframes = 500,
                renderer = gifski_renderer())
giffe



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
    str_remove_all(pattern = "[[:punct:]]") %>% # remove punctuation
    str_remove_all(pattern = "[^\\s]*[0-9][^\\s]*") %>% # remove mixed string n number
    str_squish() %>% # reduces repeated whitespace inside a string.
    str_trim() # removes whitespace from start and end of string
  
  xdtm <- VCorpus(VectorSource(x)) %>%
    tm_map(removeWords, stopwords("en"))
  
  # convert corpus to document term matrix
  return(DocumentTermMatrix(xdtm))
  
}

View(sept_00)
world = sept_00 %>% filter(
  section %in% c('World'
                 )) %>%
    select(date,
           section,
           abstract)
  
us = sept_00 %>% filter(
  section %in% c('U.S.'
  )) %>%
  select(date,
         section,
         abstract)

save.image()

small_world = world[seq(1, nrow(world), 30), ]
n = nrow(small_world)
train_data <- sample(1:n, 0.25*n, replace = FALSE)
train_data_df <- small_world[train_data,]
deploy_df <- small_world[- train_data,]
train_clean = textcleaner(train_data_df$abstract)
freqterm_5 <- findFreqTerms(train_clean,1)
train_clean <- train_clean[,freqterm_5]
rownum_5 <- apply(train_clean,1,sum)
train_clean <- train_clean[rownum_5>0,]
lda_5 <- LDA(train_clean,k = 6,control = list(seed = 1502))
topic_5 <- tidy(lda_5,matrix = "beta")
top_terms_5 <- topic_5 %>%
  group_by(topic) %>%
  top_n(15,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

top_terms_5$Subject[top_terms_5$topic == 1] <- 'Transatlantic relations'
top_terms_5$Subject[top_terms_5$topic == 2] <- 'Israel-Palestine'
top_terms_5$Subject[top_terms_5$topic == 3] <- 'Generic Middle East affairs'
top_terms_5$Subject[top_terms_5$topic == 4] <- 'War in Afghanistan'
top_terms_5$Subject[top_terms_5$topic == 5] <- 'Russia'
top_terms_5$Subject[top_terms_5$topic == 6] <- 'Nuclear proliferation'

plot_topic_5 <- top_terms_5 %>%
  mutate(term = reorder_within(term, beta, Subject)) %>%
  ggplot(aes(term, beta)) +
  geom_col(show.legend = FALSE, fill = 'Steelblue2') +
  facet_wrap(~ Subject, scales = "free") +
  coord_flip() +
  scale_x_reordered(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
  
plot_topic_5 + picci +
  theme(legend.position = 'none',
                             axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 6),
                             strip.text = element_text(size = 10),
                             panel.grid = element_blank(),
        panel.grid.major.y = element_blank()) + 
  labs(title = 'A different kind of globalization',
       subtitle = 'Analysis on 1,293 abstracts',
       caption = 'SOURCE:LDA on data from The New York Times API',
       x = '',
       y = '')

ggsave('topics.png', width = 20, height = 15 , units = 'cm')
save.image()

deploy = textcleaner(deploy_df$abstract)
freqterm_6 <- findFreqTerms(deploy,1)
deploy <- deploy[,freqterm_6]
# only choose words that appear once in each rows
rownum_6 <- apply(deploy,1,sum)
deploy <- deploy[rownum_6>0,]
world_topic =  posterior(lda_5,deploy)
nrow(world_topic[[2]])
lda_5

save.image()
plot_topic_world = data.frame(
  date = deploy_df$date,
  world_topic$topics
)
 
tidy_deploy = tidy(deploy)

world_topic_plot =
  data.frame(id = unique(tidy_deploy$document), world_topic$topics)

world_id = deploy_df %>%
  select(date) %>% mutate(id = rownames(deploy_df))

world_topic_plot = merge(world_id, world_topic_plot, by = 'id')
world_topic_plot = reshape2::melt(world_topic_plot, id.var = c('date', 'id'))
world_topic_plot = aggregate(value~variable + date, data = world_topic_plot,
                            FUN = mean)


world_topic_plot$Subject[world_topic_plot$variable == 'X1'] <- 'Transatlantic relations'
world_topic_plot$Subject[world_topic_plot$variable == 'X2'] <- 'Israel-Palestine'
world_topic_plot$Subject[world_topic_plot$variable == 'X3'] <- 'Generic Middle East affairs'
world_topic_plot$Subject[world_topic_plot$variable == 'X4'] <- 'War in Afghanistan'
world_topic_plot$Subject[world_topic_plot$variable == 'X5'] <- 'Russia'
world_topic_plot$Subject[world_topic_plot$variable == 'X6'] <- 'Nuclear proliferation'


ggplot(world_topic_plot, aes(x = date, y = value)) + geom_area(fill = 
                                                                     'Steelblue2'), alpha = 1) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(.~Subject) + picci + theme(legend.position = 'none',
                                        axis.text = element_text(size = 8),
                                        strip.text = element_text(size = 10)) + 
  labs(title = 'Not so global affairs',
       subtitle = '% topic likehood over 3,882 NYT articles',
       caption = 'SOURCE:LDA on data from The New York Times API',
       x = '',
       y = '')

ggsave('evolution.png', width = 20, height = 15, units = 'cm')








save.image()



