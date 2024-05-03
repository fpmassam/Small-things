require(spotifyr)
require(tidyverse)
require(ggrepel)
require(ggbeeswarm)
#Downloads the track to appreciate change over years 
client_id <-  ""
client_secret <- ""

token = get_spotify_access_token(
  client_id = client_id,
  client_secret = client_secret
)





eurovision_winners =  get_playlist(
  playlist_id = "6L1dDpGgwBSR1b2f978phz", 
  authorization = token)


tracks =  get_playlist_tracks("6L1dDpGgwBSR1b2f978phz", authorization = token)
stats = get_track_audio_features(tracks$track.id, authorization = token)

years = c(seq(1956,1968, 1), 1969,1969,1969, 1969, seq(1970, 2019, 1), c(2021, 2022, 2023))



data_set = data.frame(year = years, 
                      stats,
                      song = tracks$track.name)


ggplot(data_set, aes(x = year, y = tempo)) +
  geom_area(fill = "SteelBlue3", color = "SteelBlue3", alpha = .7) +
  labs(title =  "Spot a trend if you can",
    subtitle = "Tempo of the Eurovision winners over time",
       x = "Year",
       y = "Tempo (BPMs)", caption = "SOURCE: Data from the Spotify API") + picci()

ggsave("eurovision_tempo.png", width = 20, height = 16, dpi = 300, units = "cm")

#Perform the factor analysis 

for_fa = data.frame(data_set[2:12])
library(ppcor)
require(psych)
require(ggfortify)
pcor(for_fa)
inds = for_fa
inds_matrix = cor(inds)
KMO(inds_matrix)
scree(inds)
pca = principal(inds, nfactors = 2, rotate = "varimax")
plot(pca)
loadings = data.frame(rownames(pca$loadings[1:11,]), pca$loadings[1:11,])
colnames(loadings) = c("Variable", "Dance potential", "Melody potential")
ladings = reshape2::melt(loadings, id.vars = "Variable")
ladings$color = ifelse(ladings$value > 0, "positive", "negative")
ladings$Variable = stringr::str_to_title(ladings$Variable)
ladings$variable = stringr::str_to_sentence(ladings$variable)

ggplot(ladings, aes(x = Variable, y = value, fill = color)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  coord_flip() + 
  geom_text(aes(label = round(value, 2)), 
                   family = "EB Garamond", size = 8, hjust = 1) +
  facet_wrap(~variable) +
  labs(title = "The results of the Principal Component Analysis",
       subtitle = "Dance and Melody potential",
       x = "",
       y = "", caption = "SOURCE:Data from the Spotify API") + 
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(expand = c(.3,0)) +
  picci() + 
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title.position = 'plot')

ggsave("eurovision_PCA.png", width = 20, height = 10, dpi = 300, units = "cm")

data_set = data.frame(data_set, pca$scores)

label =  rbind(subset(data_set, RC1 > 1.5),
               subset(data_set, RC2 > (1.5)), 
              subset(data_set, RC1 < 1.5),
              subset(data_set, RC2 < 1.5))

label = unique(label)
                      

ggplot(data_set, aes( x= year)) + 
  scale_x_continuous(breaks = seq(1956, 2023, 10)) +
  geom_ribbon(aes(ymin = RC1, ymax = RC2), alpha = 0.2, fill = 'SteelBlue3') +
  geom_line(aes(y = RC1, color = "Dance potential"), linewidth = 1) + 
  geom_line(aes(y = RC2, color = "Melody potential"), linewidth = 1) + 
  labs(title = "Eurovision winners' potential over time",
       subtitle = "Dance and Melody potential over time",
       x = "Edition",
       y = "", caption = "Data from the Spotify API") + 
  scale_color_manual(values = c("Dance potential" = "SteelBlue1", "Melody potential" = "firebrick1")) + 
  picci() + 
  theme(legend.position = "top", 
        legend.title = element_blank())

ggsave("eurovision_tempo.png", width = 20, height = 16, dpi = 300, units = "cm")




#predict the 2024 winner 
rc1_pred =  forecast::auto.arima(data_set$RC1, seasonal = TRUE)
rc2_pred =  forecast::auto.arima(data_set$RC2, 1,1, 1,seasonal = TRUE)


rc_pred_1 = data.frame(variable = "Dance potential", level = rc1_pred$coef, se = sd(rc1_pred$residuals))
rc_pred_2 = data.frame(variable = "Melody potential", level = rc2_pred$coef, se = sd(rc2_pred$residuals))

pred_24 = rbind(rc_pred_1, rc_pred_2)

colnames(data_set)[21:22] = c("Dance potential", "Melody potential")
show_data = data.frame(data_set[1], data_set[21:22])
show_data = reshape2::melt(show_data, id.vars = "year")

show_data = aggregate(value ~ year + variable, show_data, mean)
ribbon_1 = aggregate(`Dance potential` ~ year, data_set, mean)
ribbon_2 = aggregate(`Melody potential` ~ year, data_set, mean)
ribbon = merge(ribbon_1, ribbon_2, by = "year")
show_data$variable = gsub("\\.", " ", show_data$variable)

#Describe factor evolution over time  
ggplot(show_data, aes(x = year))  + 
  geom_hline(yintercept = 0, color = 'black') +
  geom_line(aes(y = value, group= variable, color = variable), size = 1) + 
  geom_ribbon(data = ribbon, aes(ymin =  `Dance potential`, ymax =`Melody potential`), alpha = .2, 
              fill = 'SteelBlue3') + 
  geom_smooth(aes(y = value, color = variable), method = "loess", se = FALSE, linetype = 'dashed', 
              size = .5) +
  scale_x_continuous(breaks = seq(1956, 2024, 10), expand = c(0,0)) +
  scale_color_brewer(palette = "Set1") + 
  labs(title = "From melody to rythm at Eurovision",
       subtitle = "How the winners' potential has evolved over time ",
       x = "",
       y = "", caption = "SOURCE: Data from the Spotify API") + picci() + 
  theme(axis.ticks.y = element_line(),
        panel.grid.major.y = element_blank(), 
        legend.title = element_blank())



ggsave("eurovision_trends.png", width = 20, height = 16, dpi = 300, units = "cm")

pred_24_value = reshape2::dcast(pred_24, .~ variable, value.var = "level")
pred_24_sd = reshape2::dcast(pred_24, .~ variable, value.var = "se")


#Show how 2024 will evolve from 2023

last_year = subset(data_set, year == 2023)
last_year = data.frame(last_year[1],  last_year[c(21, 22)])
last_year = reshape2::melt(last_year, id.vars = "year")
pred_24_change = data.frame(year = 2024, pred_24)
colnames(pred_24_change)[2] = 'variable'
colnames(pred_24_change)[3] = 'value'
last_year$se = NA
change = rbind(last_year, pred_24_change)
change$variable = gsub("\\.", " ", change$variable)


ggplot(change, aes(x = year, y = value)) + geom_point(aes(color = variable), size = 3) + 
  geom_text_repel(aes(label = paste(variable, round(value, 2))), 
                  family = "EB Garamond", size = 12) +
  geom_errorbar(aes(ymin = value - se, ymax = value + se), width = 0.1) + 
  geom_line(aes(group = variable, color = variable), linetype = 'dashed') + 
  scale_x_continuous(breaks = seq(2023, 2024, 1)) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(~variable, ncol = 2) +
  labs(title = "A boring song for 2024?",
       subtitle = "ARIMA model to predict the 2024 winner",
       x = "",
       y = "", caption = "SOURCE: Data from the Spotify API") + picci() + 
  theme(axis.ticks.y = element_line(),
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(),
        legend.title = element_blank(), 
        legend.position = 'none')

ggsave("evolution for 2024.png", width = 20, height = 16, dpi = 300, units = "cm")


# Parameters
h <- pred_24_value$`Dance potential`  # x-coordinate of the center
k <- pred_24_value$`Melody potential`  # y-coordinate of the center
a <- pred_24_sd$`Dance potential`  # length of the semi-major axis
b <- pred_24_sd$`Melody potential`  # length of the semi-minor axis
n_points <- 100  # Number of points to calculate
# Calculate points
theta <- seq(0, 2*pi, length.out = 100)
x <- h + a * cos(theta)
y <- k + b * sin(theta)
ellipse_data <- data.frame(x = x, y = y)

#Show the evolution over time 

ggplot(data_set, aes(x = `Dance potential`, y = `Melody potential`)) + 
  geom_hline(yintercept = 0, color = 'black') +
  geom_vline(xintercept = 0, color = 'black') +
  geom_segment(x = pred_24_value$`Dance potential`, xend = pred_24_value$`Dance potential`, 
               y = pred_24_value$`Melody potential`, yend = -3, 
               color = "red", size = .5, linetype = "dashed") +
  geom_polygon(data = ellipse_data, aes(x = x, y = y), fill = 'red', color = "red", 
               alpha = .1) +
  geom_point(data = pred_24_value, aes(x = `Dance potential`, y = `Melody potential`), fill = "red",
             color = "black", shape = 21, size = 4) +
  geom_point(size = 2, fill = 'SteelBlue3', shape = 21) + 
  ggrepel::geom_text_repel(aes(label = paste(song, paste('-',year))), 
                          box.padding = 0.5, family = 'EB Garamond', size = 12, 
                          segment.color = 'SteelBlue3', max.overlaps = 6) + 
  geom_label(x = pred_24_value$`Dance potential`, y = - 3,
             label = "Potential 2024 winner", fill = "white", color = "black", family = 'EB Garamond', size = 12) +
   labs(title = 'Spotting the potential 2024 Eurovision winner',
                 subtitle = 'The red ellipse represents the 95% confidence interval of the prediction',
                 x = "Dance potential", y = "Melody potential", caption = "SOURCE: Data from the Spotify API") +
  picci() + 
  theme(panel.grid.major.y = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
        axis.ticks.y = element_line())

ggsave("winner_location_trends.png", width = 20, height = 16, dpi = 300, units = "cm")


##Show the location of where the 2024 winner is expected to be

ggplot(data_set, aes(x = `Dance potential`, y = `Melody potential`)) + 
  geom_density_2d_filled() +
  scale_fill_brewer(palette = "Set3") +
  geom_hline(yintercept = 0, color = 'black') +
  geom_vline(xintercept = 0, color = 'black') +
  geom_segment(x = pred_24_value$`Dance potential`, xend = pred_24_value$`Dance potential`, 
               y = pred_24_value$`Melody potential`, yend = -3, 
               color = "red", size = .5, linetype = "dashed") +
  geom_polygon(data = ellipse_data, aes(x = x, y = y), fill = 'red', color = "red", 
               alpha = .1) +
  geom_label(x = pred_24_value$`Dance potential`, y = - 3,
             label = "Potential 2024 winner", fill = "white", color = "black", family = 'EB Garamond', size = 12) +
  labs(title = 'Spotting the potential 2024 Eurovision winner',
       subtitle = 'The red ellipse represents the 95% confidence interval of the prediction',
       x = "Dance potential", y = "Melody potential", caption = "SOURCE: Data from the Spotify API") +
  picci() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_line(), 
        legend.position = "none")


ggplot(data_set, aes(x = `Dance potential`, y = `Melody potential`)) + 
  geom_density_2d_filled() +
  scale_fill_brewer(palette = "Set3") + theme_void() + theme(legend.position = 'none')

ggsave("eurovision_bs.png", width = 20, height = 10, dpi = 300, units = "cm")


#Identify the songs in the SE who won the Eurovision

sample_1 = data_set[data_set$`Dance potential` < pred_24_value$`Dance potential`+pred_24_sd$`Dance potential`  & 
                     data_set$`Melody potential` < pred_24_value$`Melody potential` + pred_24_sd$`Melody potential` &
                      data_set$`Dance potential` > pred_24_value$`Dance potential`- pred_24_sd$`Dance potential` &
                      data_set$`Melody potential` > pred_24_value$`Melody potential` - pred_24_sd$`Melody potential`,]

sample_1[22] = NULL
sample_1[21] = NULL




tracks_24 =  get_playlist_tracks("37i9dQZF1DWVCKO3xAlT1Q", authorization = token)
stats_24 = get_track_audio_features(tracks_24$track.id, authorization = token)
data_set_24 = data.frame(year = 2024, 
                      stats_24,
                      song = tracks_24$track.name)


#Perform the prediction

predict_factor = rbind(sample_1, 
                       data_set_24)



for_fa_pred = data.frame(predict_factor[2:12])
library(ppcor)
require(psych)
require(ggfortify)
pcor(for_fa_pred)
inds = for_fa_pred
inds_matrix = cor(inds)
KMO(inds_matrix)
scree(inds)
pca_1 = principal(inds, nfactors = 3, rotate = "varimax")
plot(pca_1)
loadings_1 = pca_1$loadings

predict_factor = data.frame(predict_factor, pca_1$scores)

pre_24_analysis = subset(predict_factor, !(year == 2024))
average_pre_24 = colMeans(pre_24_analysis[,21:23])
average_pre_24 = sd(pre_24_analysis[,21:23])
se_pre_24 = sapply(pre_24_analysis[,21:23], sd)

averages_24 = data.frame(components = names(average_pre_24), 
           mean = average_pre_24, 
           ss = se_pre_24)

songs_24 = subset(predict_factor, year == 2024)
songs_24 = songs_24[20:23]
songs_24 = reshape2::melt(songs_24)

ggplot(averages_24, aes(x = mean, y = components)) +
  geom_errorbarh(aes(xmin = mean - ss, xmax = mean + ss), height = .2) +
  geom_point(size = 8, fill = 'red', shape = 21,) +
  geom_beeswarm(data = songs_24, aes(x = value, y = variable, fill = variable),size = 4, 
             shape = 21, cex = 3)+ 
  scale_fill_brewer(palette = "Set3") +
  picci() + 
  labs(title = "2024 Eurovision winner potential",
       subtitle = "The red dot represents the average of pre-2024 winners",
       x = "", y = "", caption = "SOURCE: Data from the Spotify API") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_line(),
        legend.position = "none")

ggsave("who_is_winning.png", width = 20, height = 16, dpi = 300, units = "cm")


#Predict the winner of the 2024 Eurovision measuring the distance of the songs to the average of the previous winners


songs_24_h = subset(predict_factor, year == 2024)

songs_24_h$dist = rowMeans(
  data.frame(
    abs(songs_24_h$RC1 - average_pre_24[1])*.55, 
    abs(songs_24_h$RC2 - average_pre_24[2]*.24), 
    abs(songs_24_h$RC3 - average_pre_24[3]*.21)
))

songs_24_h = songs_24_h[order(songs_24_h$dist),]
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


songs_24_h$dist_norm = normalize(songs_24_h$dist)

set.seed(123)
ggplot(songs_24_h, aes(x = dist_norm, y = 0)) + 
  geom_point(aes(x = mean(dist_norm)), size = 10, color = 'red') + 
  geom_point(aes(x = dist_norm), size = 5, fill = "SteelBlue3", shape = 21) +
  geom_linerange(aes(xmin = mean(dist_norm) - sd(dist_norm), xmax = mean(dist_norm) + sd(dist_norm))) + 
  geom_label_repel(aes(label = song), fill = "white", color = "black", family = 'EB Garamond', size = 6, 
                   max.overlaps = 15) + 
  labs(title = "A lousy prediction after all?",
       subtitle = "Red dot = average distance of '24 songs from potential winner",
       x = "Normalized distance (0-1)", y = "", caption = "SOURCE: Data from the Spotify API") +
  picci() + theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(),
        legend.position = "none", 
        axis.text.y = element_blank())


ggsave("prediction.png", width = 20, height = 6, dpi = 300, units = "cm")