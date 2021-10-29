require(rvest)
require(spotifyr)
require(readr)
require(tidyverse)
require(data.table)
require(psych)

#Get Access to Spotify
Client = '9cf32ced039f4c9391febae7c65db74f'
Client_Secret =  '354489a2808c44eca970ef50eea26ea5'
spotify_token = get_spotify_access_token(Client, Client_Secret)


artists = c(
  '69lxxQvsfAIoQbB20bEPFC',
  '2LvhyFvUCDJ7gFuEBOcrM8',
  '2uFUBdaVGtyMqckSeCl0Qj',
  '14YzutUdMwS9yTnI0IFBaD',
  '4cU35zPQE7ZwPk72IM4wHv',
  '2P6ygesd9xg5DPOBnda2jg',
  '2VZNmg4vCnew4Pavo8zDdW',
  '5M1ZBrPeHjV8y3qFKnq7hO',
  '0Xgcm8bSs8DiQTdJPZ3mrK',
  '1nIUhcKHnK6iyumRyoV68C'
)


battiato = get_artist_audio_features('4lianjyuR1tqf6oUX8kjrZ', 
                                     authorization = spotify_token)

Glass =  get_artist_audio_features('69lxxQvsfAIoQbB20bEPFC', authorization = spotify_token)
Nyman = get_artist_audio_features('2LvhyFvUCDJ7gFuEBOcrM8', authorization = spotify_token)
Einaudi = get_artist_audio_features('2uFUBdaVGtyMqckSeCl0Qj', authorization = spotify_token)
Rani = get_artist_audio_features('14YzutUdMwS9yTnI0IFBaD', authorization = spotify_token)
Allevi = get_artist_audio_features('4cU35zPQE7ZwPk72IM4wHv', authorization = spotify_token)
Part = get_artist_audio_features('2P6ygesd9xg5DPOBnda2jg', authorization = spotify_token)
Richter = get_artist_audio_features('2VZNmg4vCnew4Pavo8zDdW', authorization = spotify_token)
Berio = get_artist_audio_features('5M1ZBrPeHjV8y3qFKnq7hO', authorization = spotify_token)
Andre = get_artist_audio_features('0Xgcm8bSs8DiQTdJPZ3mrK', authorization = spotify_token)
Morricone = get_artist_audio_features('1nIUhcKHnK6iyumRyoV68C', authorization = spotify_token)
Vangelis = get_artist_audio_features('4P70aqttdpJ9vuYFDmf7f6', authorization = spotify_token)





dataset = rbind.data.frame(Allevi, Andre, battiato, Berio, Einaudi, Glass, 
                           Morricone, Nyman, Part, Rani, Richter, Vangelis)

artist_key = function(x){
  a = split(x, x$artist_name)
  cazzo = function(list){
    mene = function(df){
      data.frame(artist_name = unique(df$artist_name),
                 table(df$mode_name))
      
    }
    b = lapply(list, mene)
    b = do.call(rbind, b)
  }
  c = cazzo(a)
  artist = split(c, f = c$artist_name)
  get_percent = function(pippo){
    pippo <- pippo %>%
      mutate(share = round(Freq/sum(Freq), 2))
  }
  minnie = lapply(artist, get_percent)
  minnie = do.call(rbind, minnie)
  rm(c)
  rownames(minnie) = NULL
  return(minnie)
  

}
keys = artist_key(dataset)
ggplot(keys, 
       aes(x = artist_name, 
           y = share)) + geom_col(aes(fill = forcats::fct_rev(Var1)
                                        ), position = 'stack') + 
  scale_fill_brewer(palette = 'Set1') + 
  geom_text(aes(label = share),  color = 'white', size = 4, position = position_fill(vjust = .5)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() + 
  labs(title = 'Major (or minor) musical footprints',
       subtitle = '% of key mode',
       caption = 'SOURCE: Own calculations on Spotify API',
       x = '',
       y = '') +
    guides(fill = guide_legend(title = 'Key mode',
                               title.position = 'left',
                               reverse = TRUE
                               )) +
  picci_h_barplot + theme(
         axis.ticks.x = element_blank(),
         legend.position = 'bottom',
         legend.text.align = 1,
         legend.box = 'horizontal',
         axis.text.x = element_blank()
         )
ggsave('major_minor.png', width = 20, height = 10, units = 'cm')

#Visualize most common keys 
most_common_key = function(x){
  a = split(x, x$artist_name)
  cazzo = function(list){
    mene = function(df){
      data.frame(artist_name = unique(df$artist_name),
                 table(df$key_mode))
      
    }
    b = lapply(list, mene)
    b = do.call(rbind, b)
    
  }
  c = cazzo(a)
  artist = split(c, f = c$artist_name)
  get_percent = function(pippo){
    pippo <- pippo %>%
      mutate(share = round(Freq/sum(Freq), 2))
  }
  minnie = lapply(artist, get_percent)
  minnie = lapply(minnie, function(x){
    top_n(x, 1)
  })
  minnie = do.call(rbind, minnie)
  rm(c)
  rownames(minnie) = NULL
  return(minnie)
  
  
}
common_keys = most_common_key(dataset)
common_keys %>% ggplot(aes(
  fct_reorder(artist_name,
                      share),share)) + geom_col(fill = 'steelblue3') + 
  geom_text(aes(label = Var1, y = .001, hjust = 0), color = 'white') + 
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() + labs(title = 'Simple keys for minimalist composers',
                        subtitle = '% of keys over tracks per artist',
                        x = '',
                        y = '',
                        caption = 'SOURCE: Own calculations via Spotify API') +
  picci_h_barplot
ggsave('most_common.png', width = 20, height = 10, units = 'cm')




#Analyze other parameters 
for_factor = data.frame(
  dataset[1], dataset[30], dataset[9:10], dataset[12], dataset[14:19]
)


library(ppcor)
require(psych)
require(ggfortify)
for_factor[5] = NULL
pcor(for_factor[3:9])
inds = for_factor[3:9]
inds_matrix = cor(inds)
KMO(inds_matrix)
fanone <- fa(r=inds, nfactors = 2, rotate="varimax",fm="pa")
fa.diagram(fanone)
head(fanone$scores) 
for_factor = cbind(for_factor,fanone$scores)
for_logit = cbind(dataset,fanone$scores)

ggplot(for_factor, aes(PA2, PA1)) + 
  geom_point(aes(fill = artist_name), shape = 21,
                                               alpha = .5) + scale_fill_brewer(palette = 'Set3') +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dashed') +
  geom_hline(yintercept = 0, color = 'red', linetype = 'dashed') +
  
  facet_wrap(.~artist_name) +
  labs(title = "Minimalism's sweet spot",
       subtitle = "Factorial analysis on audio features by composer",
       caption = 'SOURCE: Own calculations on Spotify API',
       y ='Cheerfulness',
       x = 'Strenght')+
  picci  + theme(legend.position = 'none', 
                 axis.line.x = element_blank(),
                 axis.ticks.x = element_line(),
                 panel.grid.minor.x = element_blank())
ggsave('factors.png', width = 20, height = 15, units = 'cm')

correlation <- cor(inds)
ggcorrplot(correlation) + theme_minimal()


mean_lenght = function(x){
  a = aggregate(duration_ms~artist_name, data =
                  dataset, FUN = mean)
  b = aggregate(duration_ms~artist_name, data =
                  dataset, FUN = sd)
  c = merge(a, b, by = 'artist_name')
  colnames(c)[2:3] = c('duration', 'standard_error')
  return(c)
}
mean_duration = mean_lenght(dataset)
ggplot(mean_duration, aes(
  fct_reorder(duration, artist_name), duration)) + 
  geom_jitter(data = dataset, aes(x = artist_name, y = duration_ms, 
                                  fill = artist_name), alpha = .06, shape = 21) + 
  theme(legend.position = 'none') +
  coord_flip()


ggplot(mean_duration, aes(
  fct_reorder(artist_name, standard_error), round(standard_error/1000))) + 
    geom_col(aes(y = 0 + standard_error), fill = 'steelblue3') +
  geom_col(aes(y = 0 - standard_error), fill = 'steelblue3') +
  geom_text(aes(y = 0, label = paste0(round(standard_error/1000,1), 's')),
            color = 'white') +
    coord_flip() + labs(
    title = 'Funneling success',
    subtitle = 'Standard deviation, mean as point of reference',
    x = '',
    y = '',
    caption = 'SOURCE: Own calculations on Sotify API data'
  ) + picci_h_barplot + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave('standard_deviation.png', width = 20, height = 10, units = 'cm')
  
  

  

#Valence average 
valence = aggregate(valence~artist_name, data = dataset, FUN = mean)
ggplot(valence, aes(fct_reorder(artist_name, valence), valence
  )) + geom_col(fill = 'steelblue3') + 
  scale_y_continuous(expand = c(0,0)) +
  geom_text(aes(label = round(valence, 1), y = 0.002), hjust = 0, color = 'white') + coord_flip() +
  picci_h_barplot + theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank()) +
  labs(
    title = 'Is minimalist music inherently sad?', 
    subtitle = 'Mean valence (happyness) in tracks',
    caption = 'SOURCE: Own calculations on Spotify API data',
    x = '',
    y = ''
  )
ggsave('mean_valence.png', width = 20, height = 10, units = 'cm')


#Number of tracks 
number_tracks = 
  data.frame(
    table(dataset$artist_name)  
  )
  
ggplot(number_tracks, aes(fct_reorder(Var1, Freq), Freq
)) + geom_col(fill = 'steelblue3') + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 11000),
                     labels = scales::comma) +
  geom_text(aes(label = scales::comma(Freq), y = Freq+100), hjust = 0, color = 'black') + coord_flip() +
  picci_h_barplot + theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank()) +
  labs(
    title = 'Age is important', 
    subtitle = 'Number of tracks on Spotify by artist',
    caption = 'SOURCE: Own calculations on Spotify API data',
    x = '',
    y = ''
  )
ggsave('total_tracks.png', width = 20, height = 10, units = 'cm')

#Tempo 
tempo = aggregate(tempo~artist_name, FUN = mean, data = dataset)
ggplot(tempo, aes(fct_reorder(artist_name, tempo), tempo
)) + geom_col(fill = 'steelblue3') + 
  scale_y_continuous(expand = c(0,0)) +
  geom_text(aes(label = round(tempo, 1), y = 1), hjust = 0, color = 'white') + coord_flip() +
  picci_h_barplot + theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank()) +
  labs(
    title = 'Different tempo for different artists', 
    subtitle = 'Mean tempo in beats per minute',
    caption = 'SOURCE: Own calculations on Spotify API data',
    x = '',
    y = ''
  )

ggsave('tempo.png', width = 20, height = 10, units = 'cm')



ggplot(mean_duration, aes(
fct_reorder(artist_name, duration/1000), duration/1000)) + 
  geom_jitter(data = dataset, aes(x = artist_name, y = duration_ms/1000, 
                                  fill = artist_name), shape = 21, alpha = .7) + 
  scale_fill_brewer(palette = 'Set3') + coord_flip() + picci_h_barplot + 
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = 'none') +
  labs(title = 'Classics do it longer',
       subtitle = 'Track duration in seconds',
       caption = 'SOURCE: Own calculations on Spotify API data',
       x = '',
       y = '')
ggsave('all_tracks_duration.png', width = 20, height = 10, units = 'cm')
for_logit = cbind(dataset,fanone$scores)
log_chart = for_logit %>% 
  select(artist_name,
         mode_name,
         PA1,
         PA2)

log_chart = reshape2::melt(log_chart)
gsub(log_chart$variable, "PA1", "Cheerfulness")
gsub(log_chart$variable, 'PA2', 'Cheerfulness')

log_chart$mode_name = str_to_title(log_chart$mode_name)

ggplot(log_chart, aes(x = variable, y = mode_name)) + geom_jitter(aes(fill = value,
                                                                      ),
                                                                  shape = 21,
                                                                  alpha = .5
                                                                  
                                                                  ) + 
  scale_x_discrete(labels = c('Cheerfulness', 'Strenght')) +
  scale_y_discrete(limits = rev(levels(log_chart$mode_name))) +
  scale_fill_distiller(palette = 'Spectral') +
  facet_wrap(.~artist_name) + picci + 
  guides(fill = guide_legend(title = 'Factor values')) +
  theme(legend.position = 'right',
        axis.text.x =  element_text(angle = 90, vjust = 0.5, hjust=1,
                                    size = 8),
        axis.text.y = element_text(size = 8)
        ) +
  labs(title = 'Inconclusive evidence from factors and keys',
       subtitle = 'Keys and factor per song and artist (1 point = 1 track)',
       caption = 'SOURCE: Own calculations on Spotify API data',
       x = '',
       y = '')

ggsave('variance.png', width = 20, height = 16, units = 'cm')

for_logit_1 = for_logit %>% select(mode_name,
                     PA1,
                     PA2)

test = glm(as.factor(mode_name)~`PA1`+`PA2`, data = for_logit_1, family = 'binomial')
plot(test)



