require('tidyverse')
require('rnaturalearth')
require('ggrepel')
require('acled.api')
require('gganimate')
require('sf')

#Collect Data
Migrants <- read_csv("MissingMigrants-Global-2021-04-26T13-10-04.csv")
Migrants <- Migrants %>% filter(Region %in% c('Mediterranean', 'North Africa')) 
Migrants = tidyr::separate(Migrants, into = c('Lat', 'Long'), sep = ', ', col = `Location Coordinates`)
Migrants$Lat = as.numeric(Migrants$Lat)
Migrants$Long = as.numeric(Migrants$Long)
Migrants21 = subset(Migrants, `Reported Year` == 2021)
world <- ne_countries(scale = "medium", returnclass = "sf")
Migrants20 = subset(Migrants, `Reported Year` < 2021)

#Prepare labels for the first map
world_points<- data.frame(Country = c('Italy', 'Greece', 'Algeria', 'Tunisia', 'Libya', 'Malta', 'Egypt', 'Niger','Chad'),
                                      Lat = c(39, 39, 34, 33, 31, 36, 31, 19, 19),
                                      Lon = c(14, 24, 5, 10, 19, 15, 26, 10,20))

Lampedusa = data.frame(Name = 'Lampedusa',
                       Lat = 35.508621,
                       Lon = 12.592920)
Melilla =  data.frame(Name = 'Melilla',
                      Lat = 35.29369,
                      Lon = -2.93833)  

#Migrant deaths and missing map (2014-2020)
Med_map = ggplot(data = world) + 
  geom_sf(fill = 'lightsteelblue4', color = 'ivory4') +
  coord_sf(xlim = c(2, 28), ylim = c(18, 40), expand = FALSE)+
  geom_point(data = subset(Migrants20, `Total Dead and Missing`>0), aes(y = Lat, x = Long, size = `Total Dead and Missing`,
            fill = forcats::fct_rev(as.factor(`Reported Year`))), shape = 21, alpha = .5) + scale_fill_brewer(palette = 'Set1') +
  geom_text(data= world_points,aes(x=Lon, y=Lat, label=Country), fontface = 'bold') +
  ggrepel::geom_label_repel(data = Lampedusa, aes(x = Lon, y = Lat, label = Name), 
                            nudge_x = 8) +
  scale_alpha_continuous() + guides(fill = guide_legend(title = 'Year',
                                                        override.aes = list(size = 3,
                                                                            alpha = 1)
                                                        )) + xlab('') +
  scale_size_continuous(range = c(1,5)) +
  picci_map + theme(panel.background = element_rect(fill = '#E8F6FF'),
                    axis.text = element_blank(),
                    legend.position = 'right',
                    legend.text.align = 0,
                    legend.key.size = unit(1, 'line'),
                    legend.text = element_text(size = 8),
                    panel.grid = element_blank(),
                    
  ) + labs(title = 'Dead migrants in Libya and the Mediterranean',
             subtitle = 'From 2014 to 2020',
             caption = 'SOURCE: IOM - Missing Migrant Project')
)
Med_map
ggsave('mappa_2020.png', width = 20, height = 15, units = 'cm')

#Labels for the second map
world_points_2<- data.frame(Country = c('Italy', 'Greece', 'Algeria', 'Tunisia', 'Libya', 'Malta', 'Egypt', 'Morocco','Spain', 'Niger', 'Chad'),
                            Lat = c(39, 39.2, 30, 32, 30, 36, 31, 34, 39.5, 19, 19),
                            Lon = c(14, 20, 5, 10, 19, 15, 26, -4,-5, 10, 20))

#Density map showing how migrants deaths moved 
Med_map_1 = ggplot(data = world) + 
  geom_sf(fill = 'lightsteelblue4', color = 'ivory4') +
  coord_sf(xlim = c(-10, 28), ylim = c(18, 40), expand = FALSE)+
  geom_density2d(data = Migrants20, aes(y = Lat, x = Long, size = `Total Dead and Missing`,
                                    color = forcats::fct_rev(as.factor(`Reported Year`)))) + scale_color_brewer(palette = 'Set1') +
  geom_point(data = Migrants20, aes(y = Lat, x = Long), alpha = .07, fill = 'lightsteelblue3', shape = 21) +
  geom_text(data= world_points_2,aes(x=Lon, y=Lat, label=Country), fontface = 'bold') +
  ggrepel::geom_label_repel(data = Lampedusa, aes(x = Lon, y = Lat, label = Name), 
                            nudge_x = 8) +
  ggrepel::geom_label_repel(data = Melilla, aes(x = Lon, y = Lat, label = Name), nudge_x = 8) +
  scale_alpha_continuous() + guides(color = guide_legend(title = 'Year',
                                                        override.aes = list(size = 3)
  )) + xlab('') +
  scale_size_continuous(range = c(1,10)) +
  picci_map + theme(panel.background = element_rect(fill = '#E8F6FF'),
                    axis.text = element_blank(),
                    legend.position = 'right',
                    legend.text.align = 0,
                    legend.key.size = unit(1, 'line'),
                    legend.text = element_text(size = 8),
                    panel.grid = element_blank(),
                    
  ) + labs(title = 'Events density - Mediterrarenan and North Africa',
           subtitle = 'From 2014 to 2020',
           caption = 'SOURCE: IOM - Missing Migrant Project')
Med_map_1
ggsave('mappa_denisty.png', width = 20, height = 15, units = 'cm')

ggplot(deaths_year, aes(x = `Reported Year`, y = `Number Dead`)) + geom_col(aes(
  fill = `Migration Route`)
)



#The situation in 2021
##Data selection
Migrants21 = subset(Migrants21, Region == 'Mediterranean')
#Selecting label
world_points_1<- data.frame(Country = c('Italy', 'Greece', 'Algeria', 'Tunisia', 'Libya', '', 'Egypt', 'Morocco','Spain'),
                          Lat = c(39, 39.2, 30, 32, 30, 36, 31, 30, 39.5),
                          Lon = c(14, 20, 5, 10, 19, 15, 26, -10,-5))
#Drawing the 2021 map 
Med_map_2 = ggplot(data = world) + 
  geom_sf(fill = 'lightsteelblue4', color = 'ivory4') + 
  coord_sf(xlim = c(-15, 30), ylim = c(28, 42), expand = FALSE)+
  geom_point(data = subset(Migrants21, `Total Dead and Missing`>0), aes(y = Lat, x = Long, size = `Total Dead and Missing`)
                                                                      , shape = 21, alpha = .5, fill = 'lightsteelblue2') +
  geom_text(data= world_points_1,aes(x=Lon, y=Lat, label=Country), fontface = 'bold') +
  ggrepel::geom_label_repel(data = Lampedusa, aes(x = Lon, y = Lat, label = Name), 
                            nudge_x = 4, nudge_y = 2,
                            segment.linetype = 5,
                            segment.curvature = .15,
                            segment.color = 'red') +
  ggrepel::geom_label_repel(data = Melilla, aes(x = Lon, y = Lat, label = Name), 
                            nudge_x = 4, nudge_y = -2,
                            segment.linetype = 5,
                            segment.curvature = .15,
                            segment.color = 'red') +
  ggrepel::geom_text_repel(data = subset(Migrants21, `Total Dead and Missing`>0), aes(y = Lat, x = Long, label = `Reported Date`), max.overlaps = 18,
                            size = 3, segment.alpha = .5) +
  scale_alpha_continuous() + xlab('') +
  scale_size_continuous(range = c(1,10)) +
  picci_map + theme(panel.background = element_rect(fill = '#E8F6FF'),
                    axis.text = element_blank(),
                    legend.position = 'bottom',
                    legend.text.align = 0,
                    legend.key.size = unit(1, 'line'),
                    legend.text = element_text(size = 8),
                    panel.grid = element_blank(),
                    
  ) + labs(title = 'Dead migrants in Libya and the Mediterranean',
           subtitle = 'January 1-April 21, 2021',
           caption = 'SOURCE: IOM - Missing Migrant Project')
Med_map_2
ggsave('morti_med.png', width = 20, height = 12, units = 'cm')


#Drawing bar chart for deaths in the Mediterranean
med_deaths = Migrants %>% select(`Reported Year`, `Total Dead and Missing`,
                                 Region) %>% filter(Region == 'Mediterranean')
deaths_year = aggregate(`Total Dead and Missing`~`Reported Year`, data = med_deaths,
                        FUN = sum)
x_axis_labels <- min(deaths_year$`Reported Year`):max(deaths_year$`Reported Year`)

morti_timeline <- ggplot(deaths_year, aes(x = `Reported Year`, y = `Total Dead and Missing`)) + 
  geom_col(fill = 'lightsteelblue4') +  xlab('') +
  geom_text(aes(x = `Reported Year`, y = `Total Dead and Missing`+ 150, label = format(`Total Dead and Missing`, big.mark = ",", scientific = FALSE))) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  labs(title = 'Total dead and missing in the Mediterranean',
       subtitle = 'As of April 21, 2021',
       caption =  'SOURCE: IOM - Missing Migrant Project') + picci + theme(
         panel.grid = element_blank(),
         panel.grid.major.y = element_blank(),
         axis.text.y = element_blank()
       )

ggsave('morti_timeline.png', width = 20, height = 15, units = 'cm')

#Drawing a bar chart for the number of deaths in North Africa
na_deaths = Migrants %>% select(`Reported Year`, `Total Dead and Missing`,
                                 Region) %>% filter(Region == 'North Africa')
deaths_year_na = aggregate(`Total Dead and Missing`~`Reported Year`, data = na_deaths,
                        FUN = sum)
ggplot(deaths_year_na, aes(x = `Reported Year`, y = `Total Dead and Missing`)) + 
  geom_col(fill = 'lightsteelblue4') +  xlab('') +
  geom_text(aes(x = `Reported Year`, y = `Total Dead and Missing`+ 50, label = format(`Total Dead and Missing`, big.mark = ",", scientific = FALSE))) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  labs(title = 'Total dead and missing in North Africa',
       subtitle = 'As of April 21, 2021',
       caption =  'SOURCE: IOM - Missing Migrant Project') + picci + theme(
         panel.grid = element_blank(),
         panel.grid.major.y = element_blank(),
         axis.text.y = element_blank()
       )

ggsave('morti_timeline_north_africa.png', width = 20, height = 15, units = 'cm')








death_share = Migrants %>% select(`Reported Year`,
                                  Region,
                                  `Migration Route`,
                                  `Total Dead and Missing`) %>%
  filter(Region == 'Mediterranean')
death_share$`Migration Route`<- ifelse(is.na(death_share$`Migration Route`), 'Unknown', death_share$`Migration Route`)
death_share = aggregate(`Total Dead and Missing`~., data = death_share, FUN = sum)
death_share  = split(death_share, f = death_share$`Reported Year`)

death_share = lapply(death_share, function(x){
  x %>% mutate(
    Share = `Total Dead and Missing`/sum(`Total Dead and Missing`))
})

death_share = data.table::rbindlist(death_share)



ggplot(death_share, aes(x = `Reported Year`, y = Share)) + geom_col(aes(fill = forcats::fct_rev(`Migration Route`))) + 
  geom_text(aes(x = `Reported Year`, y = Share, group = forcats::fct_rev(`Migration Route`),
                label = paste0(round(Share, 2)*100, '%')), position = position_stack(vjust = .5), fontface = 'bold',
            size = 4, check_overlap = TRUE) +
  xlab('') + 
  scale_fill_brewer(palette = 'Set3') + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  guides(fill = guide_legend('Route:',
                             title.position = 'top',
                             nrow = 2)) +
  labs(title = '% deaths in the Mediterranean by route (2014-2021)',
       subtitle = 'Central Mediterranean is the most dangeours place for migrants',
       caption = 'SOURCE: IOM - Missing Migrant Project') +
  picci + theme(
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(
      color = 'grey25')
  ) 

ggsave('death_share.png', width = 20, height = 15, units = 'cm')






ggsave('morti_timeline.png', width = 20, height = 15, units = 'cm')



scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)





