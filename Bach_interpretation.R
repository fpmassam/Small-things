#Require packages and graphics 


require("spotifyr")
require("tidyverse")
require(ggbeeswarm)
require(ggh4x)
require(ggfx)

token = get_spotify_access_token(
  client_id = "9cf32ced039f4c9391febae7c65db74f",
  client_secret = "354489a2808c44eca970ef50eea26ea5"
)


#Download albums from pianitsts 

Sviatoslav_Richter = get_album("4OsweMAATSbKd9awYvOEEj", authorization =  token)

Sviatoslav_Richter_data = data.frame(
  track = seq(1,24,1),
  artist = "Sviatoslav Richter",
  name = head(Sviatoslav_Richter$tracks$items$name, 24),
  get_track_audio_features(id = head(Sviatoslav_Richter$tracks$items$id, 24), 
                         authorization = token))


Glenn_Gould = get_album("5NqQLA1NCo6hP2GHiHxip5", authorization =  token)
Glenn_Gould_data = data.frame(
  track = seq(1,24,1),
  artist = "Glenn Gould",
   name = head(Glenn_Gould$tracks$items$name, 24),
                                     get_track_audio_features(id = 
                                                                head(
                                                                Glenn_Gould$tracks$items$id,,
                                                                24),
                                                              authorization = token))


Maurizio_Pollini = get_album("66zilH1HJzRLEorco0u6bS", authorization =  token)

Maurizio_Pollini_data = data.frame(
  track = seq(1,24,1),
  artist = "Maurizio Pollini",
  name = head(Maurizio_Pollini$tracks$items$name, 24),
  get_track_audio_features(id = head(Maurizio_Pollini$tracks$items$id, 24), 
                           authorization = token))


Ottavio_Dantone = get_album("0arsHKZD7XW3CArTECMm3V", authorization =  token)
Ottavio_Dantone_data = data.frame(
  track = seq(1,24,1),
  artist = "Ottavio Dantone",
  name = head(Ottavio_Dantone$tracks$items$name, 24),
  get_track_audio_features(id = head(Ottavio_Dantone$tracks$items$id, 24), 
                           authorization = token))
Ottavio_Dantone_data$instrument = 'Harpsichord'

Maurizio_Pollini = 
  get_album("3sHGzXs7ZKRRFmJxBp4YTv", authorization =  token)

Maurizio_Pollini_data = data.frame(
  track = seq(1,24,1),
  artist = "Maurizio Pollini",
  name = head(Maurizio_Pollini$tracks$items$name, 24),
  get_track_audio_features(id = head(Maurizio_Pollini$tracks$items$id, 24), 
                           authorization = token))


full_dataframe_piano =  rbind(Glenn_Gould_data, Daniel_Baremboim_data,
      Maurizio_Pollini_data)
full_dataframe_piano = data.frame(instrument = 'Piano',
                                  full_dataframe_piano)


  
ggplot(full_dataframe, 
       aes(x = duration_ms, 
           y = track)) + geom_point(aes(group = artist, fill = artist),
                                    shape = 21)
#Get g_pitches 

gould_c_major = Glenn_Gould_data[1,]$id

gould_major = get_track_audio_analysis(gould_c_major,
                                         authorization = token)
g_pitches  = gould_major[["segments"]][["pitches"]]
g_pitches = do.call(rbind, g_pitches)
g_pitches = as.data.frame(g_pitches)
g_pitches = data.frame(segment = rownames(g_pitches),
                     g_pitches)
g_pitches = reshape2::melt(g_pitches)

g_pitches$note[g_pitches$variable == "V1"] <- "C"
g_pitches$note[g_pitches$variable == "V2"] <- "C#"
g_pitches$note[g_pitches$variable == "V3"] <- "D"
g_pitches$note[g_pitches$variable == "V4"] <- "D#"
g_pitches$note[g_pitches$variable == "V5"] <- "E"
g_pitches$note[g_pitches$variable == "V6"] <- "F"
g_pitches$note[g_pitches$variable == "V7"] <- "F#"
g_pitches$note[g_pitches$variable == "V8"] <- "G"
g_pitches$note[g_pitches$variable == "V9"] <- "G#"
g_pitches$note[g_pitches$variable == "V10"] <- "A"
g_pitches$note[g_pitches$variable == "V11"] <- "A#"
g_pitches$note[g_pitches$variable == "V12"] <- "B"

ggplot(g_pitches, aes(x =  as.integer(segment), y = note)) + 
  labs(title = "Glenn Gould's uncompromising clarity",
       subtitle = "Bach C Major prelude, BWV 846",
       caption = "SOURCE: Spotify API",
       x = "Segment (0 = start)",
       y = "Dominant note") + scale_fill_brewer(palette = "Set3") +
  scale_x_reverse() +
  coord_flip() +
  guides(fill = "none", 
         alpha = "none") +
         geom_tile(aes(alpha = value, fill = note)) + picci()

ggsave("gould_c_major.png", width = 20, height = 16, units = 'cm')


#Clavichord players 

John_Butt = get_album("5nTDSiFT4HtQTnoaUsO5Rp", authorization =  token)

John_Butt_data = data.frame(
  track = seq(1,24,1),
  artist = "John Butt",
  name = head(John_Butt$tracks$items$name, 24),
  get_track_audio_features(id = head(John_Butt$tracks$items$id, 24), 
                           authorization = token))


David_Ezra = get_album("2Fs6otbM1scm0Vu2afHcYR", authorization =  token)
David_Ezra_data = data.frame(
  track = seq(1,24,1),
  artist = "David Ezra",
  name = head(David_Ezra$tracks$items$name, 24),
  get_track_audio_features(id = 
                             head(
                               David_Ezra$tracks$items$id,,
                               24),
                           authorization = token))

#https://open.spotify.com/album/2Fs6otbM1scm0Vu2afHcYR?si=6-s549LRTxGtyTh4y1HBXg


Steven_Devine = get_album("1TPdCHoEvDL8hwBfgUewfn", authorization =  token)

Steven_Devine_data = data.frame(
  track = seq(1,24,1),
  artist = "Steven Devine",
  name = head(Steven_Devine$tracks$items$name, 24),
  get_track_audio_features(id = head(Steven_Devine$tracks$items$id, 24), 
                           authorization = token))
#https://open.spotify.com/album/1TPdCHoEvDL8hwBfgUewfn?si=gxpDMjjtTL6BJdD5fxm0OQ


full_dataframe_clavi =  rbind(John_Butt_data, David_Ezra_data,
                              Steven_Devine_data)
full_dataframe_clavi = data.frame(instrument = 'Harpsichord',
                                  full_dataframe_clavi)

full_dataframe = rbind(full_dataframe_clavi, full_dataframe_piano)

ggplot(full_dataframe, 
       aes(x = duration_ms, 
           y = track)) + geom_point(aes(group = artist, fill = artist),
                                    shape = 21) + facet_wrap(~ instrument)


#One further artist

Andras_Schiff_data$instrument = "Piano"


full_dataframe= rbind(full_dataframe, Andras_Schiff_data, 
                      Ottavio_Dantone_data)






#Factor analysis on bif dataset 


for_factor = data.frame(full_dataframe[5:15],
                        full_dataframe[21])


#Perform a bidimensional varimax to study countries worldwide


library(ppcor)
require(psych)
require(ggfortify)
pcor(for_factor)
inds = for_factor
inds_matrix = cor(inds)
KMO(inds_matrix)
scree(inds)
fanone <-  fa(r=inds, nfactors = 2, rotate="varimax",fm="pa")
plot(fanone)

fact_chart = data.frame(rownames(head(fanone$loadings)),  
                             head(fanone$loadings))

fact_chart = data.frame(variables = rownames(fanone$loadings[,1:2]),
                        fanone$loadings[,1:2])
fact_chart = reshape2::melt(fact_chart)
fact_chart$factor[fact_chart$variable == "PA1"] <- 'Force'
fact_chart$factor[!(fact_chart$variable == "PA1")] <- 'Grace'

fact_chart$fillo[fact_chart$value > 0] <- 'b'
fact_chart$fillo[fact_chart$value < 0] <- 'a'
fact_chart$variables[fact_chart$variables == 'duration_ms'] <- "duration"


ggplot(fact_chart, aes(x = value, 
                       y = stringr::str_to_sentence(variables))) + 
  geom_col(aes(fill = fillo, alpha = abs(value))) + 
  geom_text(aes(label = round(value, 2), x = 0), family = 'EB Garamond',
            size = 10, fontface = 'bold') +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "A complex set of interactions",
       subtitle = 'Loadings for the factor analysis', 
       caption = 'SOURCE: DaNumbers calculations on Spotify data',
    x = '', y = '') +
  facet_wrap(~factor(factor, 
                     levels = c('Force', "Grace"))) + picci() + theme(
    panel.grid.major.y = element_blank(), 
    legend.position = 'none', 
    plot.title.position = 'plot'
  )
ggsave('loadings.png', width = 20, height = 16, units = "cm")

factors_bach = data.frame(full_dataframe, fanone$scores)
factors_bach_plot = factors_bach %>%
  dplyr::select(instrument, track, artist, PA1, PA2)
factors_bach_plot = reshape2::melt(factors_bach_plot, id.vars = c('instrument',
                                                                  'track',
                                                                  'artist'))
factors_bach_plot$factor[factors_bach_plot$variable == "PA1"] <- 'Force'
factors_bach_plot$factor[!(factors_bach_plot$variable == "PA1")] <- 'Grace'

factors_bach_plot_avg = aggregate(
  value ~ factor + artist + instrument, data = factors_bach_plot, FUN = mean
)

factors_bach_plot_sd = aggregate(
  value ~ factor + artist + instrument, data = factors_bach_plot, FUN = sd
)
colnames(factors_bach_plot_sd)[4] = "sd"
factors_bach_plot_avg = merge(factors_bach_plot_avg, factors_bach_plot_sd)


ggplot(factors_bach_plot, aes(y = factor, x = value)) +
  scale_y_discrete(limits=rev)+
  geom_beeswarm(aes(fill = artist), shape = 21, alpha = 1, 
                                                                     size = 3, 
                                                                     method = "hex", cex = 3,
                                                                     corral = "wrap", corral.width = 0.9) +
  with_shadow(geom_point(data = factors_bach_plot_avg, aes(fill = artist), shape = 21, size = 8), 
              sigma = 12) +
  facet_wrap(~instrument, ncol = 2) + scale_fill_brewer(palette = "Set3") + 
  labs(title = 'Harpsichord versus piano: two clear ways',
       subtitle = "Factor analysis, 1 smaller point = 1 track; 1 bigger point = average per artist", 
       x = '',
       y = '', 
       caption = 'SOURCE: DaNumbers calculations on Spotify data') + picci() + 
  theme(plot.title.position = 'plot',
        legend.title = element_blank()
         
       )

ggsave('factor_analysis_results.png', width = 20, height = 16, units = "cm")


#Logistical regression 
factors_bach$Boolean[factors_bach$instrument == 'Piano'] <- TRUE
factors_bach$Boolean[!(factors_bach$instrument == 'Piano')] <- FALSE
logit = 
  factors_bach %>% 
  dplyr::select(Boolean, PA1, PA2)

  
logit_r = logit %>% sample_n(30)

model = glm(Boolean ~ ., data = logit_r, family = 'binomial')

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(model))


factors_bach$y_axis[factors_bach$Boolean == TRUE] <- 1
factors_bach$y_axis[factors_bach$Boolean == FALSE] <- 0

factors_bach$prob = predict(model, factors_bach, type = 'response')
annotations_a = data.frame(
  position = c(0.60, 0.40),
  lab = c('Piano', 'Harpsichord')
)


ggplot(factors_bach, aes(y = prob, x = instrument)) +
  geom_quasirandom(aes(fill = artist), 
                   shape = 21, alpha = .7, 
                   size = 4, 
                   method = 'smiley') +
  scale_fill_brewer(palette = 'Set3') +
  scale_y_continuous(labels = scales::percent) +
  picci() + labs(
    title = "Spotify data can predict the instrument of a track",
    subtitle = "Probability (0-100%) it is a piano; 1 point = 1 track",
    caption = "SOURCE: DaNumbers calculations on Spotify data", 
    x = '', y = '') +
  theme(axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(), 
        plot.title.position = 'plot')

ggsave('probabiliry.png', width = 20, height = 16, units = "cm")



#Comparison of the c-prelude 

c_m_Prelude = full_dataframe %>% 
  dplyr::filter(track == 3 & !(artist %in% 
                                 c('Ottavio Dantone', "Andras Schiff"))) 

c_m_Prelude = data.frame(c_m_Prelude[1],
                         c_m_Prelude[3],
                        c_m_Prelude[5:15],
                        c_m_Prelude[21])

c_m_Prelude = reshape2::melt(c_m_Prelude, vars.id = c('instrument', 'artist'))
c_m_Prelude_m = aggregate(value ~ variable + instrument, FUN = mean, 
                          data = c_m_Prelude)
c_m_Prelude_m_dur = subset(c_m_Prelude, variable == "duration_ms")
c_m_Prelude_m = subset(c_m_Prelude_m, !(variable %in% c("duration_ms",
                                                      'tempo', 'loudness', 'key',
                                                      'mode')))

c_m_Prelude_m = reshape2::dcast(c_m_Prelude_m, variable ~ instrument)
c_m_Prelude_m$variable = stringr::str_to_sentence(c_m_Prelude_m$variable)


ggplot(c_m_Prelude_m, aes(y = variable)) +
  geom_segment(aes(y = variable, yend = variable, x = Piano, xend = Harpsichord), color = 'black') +
  geom_point(aes(x = Piano), size = 6, shape = 21, fill = "SteelBlue3") +
  geom_point(aes(x = Harpsichord), size = 6, shape = 21, fill = "SteelBlue4") +
  ggrepel::geom_label_repel(aes(x = Piano, label = paste("P:", round(Piano, 2))),
                           size = 8, max.overlaps = 1, family = 'EB Garamond') +
  ggrepel::geom_label_repel(aes(x = Harpsichord, label = paste("H:", round(Harpsichord, 2))), 
                           size = 8, max.overlaps = 1, family = 'EB Garamond',nudge_y =  -.5) +
  picci() + labs(title = 'Is harpsichord a metal instrument?', 
                 subtitle = "Mean vaulues; P = Piano, H = Harpsichord", 
                 x = '', y = '', caption = 'SOURCE: DaNumbers Calculations on Spotify data') +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_line())

ggsave('cleveland_chart.png', width = 20, height = 16, units = "cm")


c_m_Prelude_md = aggregate(c_m_Prelude_m_dur, (value/1000) ~ instrument, FUN = mean)


#Music analyisis 
music_analysis = full_dataframe %>% 
  dplyr::filter(track == 3 & !(artist %in% 
                                 c('Ottavio Dantone', "Andras Schiff"))) 


musics_analysis_d = split(music_analysis, 
                         f = music_analysis$name)



musics_analysis_d = 
  lapply(musics_analysis_d,
         function (x) {
           id = x$id
track = get_track_audio_analysis(id,
                                         authorization = token)
g_pitches  = track[["segments"]][["pitches"]]
g_pitches = do.call(rbind, g_pitches)
g_pitches = as.data.frame(g_pitches)
g_pitches = data.frame(segment = rownames(g_pitches),
                     g_pitches)
g_pitches = reshape2::melt(g_pitches)
           
         })
 
musics_analysis_d= map_df(musics_analysis, ~as.data.frame(.x), .id="name")

musics_analysis_d$note[musics_analysis_d$variable == "V1"] <- "C"
musics_analysis_d$note[musics_analysis_d$variable == "V2"] <- "C#"
musics_analysis_d$note[musics_analysis_d$variable == "V3"] <- "D"
musics_analysis_d$note[musics_analysis_d$variable == "V4"] <- "D#"
musics_analysis_d$note[musics_analysis_d$variable == "V5"] <- "E"
musics_analysis_d$note[musics_analysis_d$variable == "V6"] <- "F"
musics_analysis_d$note[musics_analysis_d$variable == "V7"] <- "F#"
musics_analysis_d$note[musics_analysis_d$variable == "V8"] <- "G"
musics_analysis_d$note[musics_analysis_d$variable == "V9"] <- "G#"
musics_analysis_d$note[musics_analysis_d$variable == "V10"] <- "A"
musics_analysis_d$note[musics_analysis_d$variable == "V11"] <- "A#"
musics_analysis_d$note[musics_analysis_d$variable == "V12"] <- "B"


musics_analysis_chart = musics_analysis_d %>% 
  dplyr::filter(note %in% c('C', "D#", 'G', 'B') )

musics_analysis_chart = merge(music_analysis, musics_analysis_chart)
musics_analysis_chart$note = gsub("D#", 'Eb', musics_analysis_chart$note)

ggplot(musics_analysis_chart, aes(x=factor(note, 
                                              levels = c('C', "Eb", 'G', 'B')), 
       y = as.numeric(segment))) + 
  scale_y_reverse() + 
  scale_fill_brewer(palette = "Set2") +
  geom_tile(aes(alpha = value, fill = note)) + facet_nested(~ instrument + artist) +
  picci() + theme(legend.position = 'none',
                  strip.text.x = element_text(size = 33)
                           ) + labs(x = 'Prelude No.2 in C minor BWV847', y = '', 
                                    title = 'Indivudual taste matters', 
                                    subtitle = 'Prelude No.2 in C minor BWV847',
                                    caption = 'SOURCE: Spotify data')

ggsave('C_minor.png', width = 20, height = 20, units = "cm")



#Preditcion 



#Factor analysis on bif dataset 


Glenn_Gould_goldberg = get_album("1aCpHSQE5ghxibsQ5gkBe0", authorization =  token)
Glenn_Gould_goldberg_data = data.frame(
  track = seq(1,24,1),
  artist = "Glenn Gould",
  name = head(Glenn_Gould_goldberg$tracks$items$name, 24),
  get_track_audio_features(id = 
                             head(
                               Glenn_Gould_goldberg$tracks$items$id,
                               24),
                           authorization = token))

Glenn_Gould_goldberg_data$instrument = 'Piano'


Jean_Rondeau_goldberg = get_album("7B3b72e20IXzqf46sbltcJ", authorization =  token)
Jean_Rondeau_goldberg_data = data.frame(
  track = seq(1,24,1),
  artist = "Jean Rondeau",
  name = head(Jean_Rondeau_goldberg$tracks$items$name, 24),
  get_track_audio_features(id = 
                             head(
                               Jean_Rondeau_goldberg$tracks$items$id,
                               24),
                           authorization = token))

Jean_Rondeau_goldberg_data$instrument = "Harpsichord"


deploy_df = rbind(Glenn_Gould_goldberg_data, Jean_Rondeau_goldberg_data)

for_deploy = data.frame(deploy_df[4:14],
                        deploy_df[20])

library(ppcor)
require(psych)
require(ggfortify)
pcor(for_deploy)
inds = for_deploy
inds_matrix = cor(inds)
KMO(inds_matrix)
scree(inds)
dep <- fa(r=inds, nfactors = 5, rotate="varimax",fm="pa",
             plot = TRUE)

deploy_bach = data.frame(deploy_df, dep$scores)

deploy_bach$Boolean[deploy_bach$instrument == 'Piano'] = TRUE
deploy_bach$Boolean[!(deploy_bach$instrument == 'Piano')] = FALSE

prediction = 
  deploy_bach %>% 
  dplyr::select(Boolean, PA1, PA2)

deploy_bach$prob = predict(model, prediction, type = 'response')

deploy_bach_plot = deploy_bach %>% 
  dplyr::select(track, artist, name, instrument, prob)

deploy_bach$correct[deploy_bach$prob > .5 & deploy_bach$instrument == 'Piano'] <- 'Correct'
deploy_bach$correct[deploy_bach$prob < .5 & deploy_bach$instrument == 'Piano'] <- 'Wrong'
deploy_bach$correct[deploy_bach$prob < .5 & deploy_bach$instrument == 'Harpsichord'] <- 'Correct'
deploy_bach$correct[deploy_bach$prob > .5 & deploy_bach$instrument == 'Harpsichord'] <- 'Wrong'

 
evaluation = data.frame(table(deploy_bach$correct, deploy_bach$instrument))

ggplot(deploy_bach, 
       aes(x = prob, y = artist)) + geom_beeswarm(aes(fill = correct), shape = 21, alpha = 1, 
                                                  size = 3, 
                                                  method = "hex", cex = 3,
                                                  corral = "wrap", corral.width = 0.9) + 
  scale_x_continuous(labels = scales::percent) + scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(title = "Different music, different models",
                 subtitle = 'Probability of piano using the Well-Tempered clavier for training',
                 caption = "SOURCE: Own calculations on Spotify data", 
                 x = '', y = '') + 
  picci() + 
  theme(
    plot.title.position = 'plot',
    legend.title = element_blank()
  )
  
ggsave('goldberg.png', width = 20, height = 14, units = "cm")



dep_chart = data.frame(rownames(head(dep$loadings)),  
                        head(dep$loadings))

dep_chart = data.frame(variables = rownames(dep$loadings[,1:2]),
                        dep$loadings[,1:2])
dep_chart = reshape2::melt(dep_chart)
dep_chart$depor[dep_chart$variable == "PA1"] <- 'Force'
dep_chart$depor[!(dep_chart$variable == "PA1")] <- 'Grace'

dep_chart$fillo[dep_chart$value > 0] <- 'b'
dep_chart$fillo[dep_chart$value < 0] <- 'a'
dep_chart$variables[dep_chart$variables == 'duration_ms'] <- "duration"


ggplot(dep_chart, aes(x = value, 
                       y = stringr::str_to_sentence(variables))) + 
  geom_col(aes(fill = fillo, alpha = abs(value))) + 
  geom_text(aes(label = round(value, 2), x = 0), family = 'EB Garamond',
            size = 10, fontface = 'bold') +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Is it the same Bach after all?",
       subtitle = 'Loadings for the factor analysis', 
       caption = 'SOURCE: DaNumbers calculations on Spotify data',
       x = '', y = '') +
  facet_wrap(~factor(depor, 
                     levels = c('Force', "Grace"))) + picci() + theme(
                       panel.grid.major.y = element_blank(), 
                       legend.position = 'none', 
                       plot.title.position = 'plot'
                     )


ggsave('goldberg_fc.png', width = 20, height = 16, units = "cm")


dep_bach = data.frame(deploy_df, dep$scores)
dep_bach_plot = dep_bach %>%
  dplyr::select(instrument, track, name, artist, PA1, PA2)
dep_bach_plot = reshape2::melt(dep_bach_plot, id.vars = c('instrument',
                                                          'name',
                                                                  'track',
                                                                  'artist'))
dep_bach_plot$factor[dep_bach_plot$variable == "PA1"] <- 'Force'
dep_bach_plot$factor[!(dep_bach_plot$variable == "PA1")] <- 'Grace'

dep_bach_plot_avg = aggregate(
  value ~ factor + artist + instrument, data = dep_bach_plot, FUN = mean
)

dep_bach_plot_sd = aggregate(
  value ~ factor + artist + instrument, data = dep_bach_plot, FUN = sd
)
colnames(dep_bach_plot_sd)[4] = "sd"
dep_bach_plot_avg = merge(dep_bach_plot_avg, dep_bach_plot_sd)



ggplot(dep_bach_plot, aes(y = factor, x = value)) +
  scale_y_discrete(limits=rev)+
  geom_beeswarm(aes(fill = artist), shape = 21, alpha = 1, 
                size = 3, 
                method = "hex", cex = 3,
                corral = "wrap", corral.width = 0.9) +
  scale_fill_brewer(palette = "Set2") + 
  labs(title = 'One universe at the time',
       subtitle = "Factor analysis, 1 smaller point = 1 track", 
       caption = 'SOURCE: DaNumbers calculations on Spotify data', 
       x = '', y = '') + picci() + 
  theme(plot.title.position = 'plot',
        legend.title = element_blank()) 


ggsave('factor_analysis_results_dep.png', width = 20, height = 16, units = "cm")









#Predict (Goldberg variations )



