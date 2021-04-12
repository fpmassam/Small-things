require('tidyverse')
require('rnaturalearth')
require('ggrepel')
require('acled.api')
require('gganimate')
#Download data and build the map 
acled.key #Use your own key
##Download the data for the map
world <- ne_countries(scale = "medium", returnclass = "sf")
Evidence = data.frame(sovereignt = 'Mozambique',
           Icare = 'YES')
world = merge(world, Evidence, by = 'sovereignt', all.x = TRUE)
world$Icare = ifelse(world$sovereignt != 'Mozambique', 'NO', 'YES')
Mozambique_admin = ne_states(country = 'Mozambique', returnclass = 'sf')
cabo_delgado = subset(Mozambique_admin, name == 'Cabo Delgado')
##DOwnload conflict data 
Mozambique = acled.api('email', acled.key, country = 'Mozambique', all.variables = TRUE)
regions = c('Northern Africa', 'Western Africa', 'Southern Africa', 'Eastern Africa')
Africa = acled.api('email', acled.key, region = regions, all.variables = TRUE)

Mozambique_2021 = subset(Mozambique, year == 2021)
Mozambique_latest = subset(Mozambique, year > 2016)


Moz_map = ggplot(data = world) + 
  geom_sf(fill = 'lightsteelblue4', color = 'ivory4') + 
  geom_sf(data = Mozambique_admin, fill = 'lightsteelblue3') + 
  geom_sf(data = cabo_delgado, fill = 'lightsteelblue1') +
  coord_sf(xlim = c(24, 65), ylim = c(-28.599, -8), expand = FALSE) +
  geom_point(data = Mozambique_2021, aes(y = as.numeric(as.vector(latitude)), x = as.numeric(as.vector(longitude)),
                                     fill = actor1, size = fatalities), color = 'black', shape = 21, alpha = .5) +
  geom_point(data = Mozcities, aes(x = lng, y = lat, shape = capital), size = 1.5, fill = 'grey23', alpha = .7) +
  scale_shape_manual(values = c(21,23))+ guides(shape = FALSE,
                                                fill = guide_legend('Responsible actors',
                                                                    override.aes = list(
                                                                      size = 3,
                                                                      alpha = 1
                                                                    ),
                                                                    direction = 'vertical'),
                                                size = guide_legend('Fatalities',
                                                                    direction = 'horizontal',
                                                                    title.position = 'top')
                                                ) +
  geom_label_repel(data = Mozcities, aes(x = lng, y = lat, label = city)) +
  geom_point(aes(y = -10.7833302, x = 40.4833314), fill = 'red', size = 1.5, shape = 25) +
  geom_label(aes(y = -10.7833302, x = 41.0, label = 'Palma', fontface = 'bold'), alpha = .7, nudge_x = 1.25) +
  scale_fill_brewer(palette = 'Set3') +
  scale_size(range = c(4,12)) +
  xlab('') + ylab('') + picci_map() +
  theme(panel.background = element_rect(fill = '#E8F6FF'),
                       axis.text = element_blank(),
                       legend.position = c(.72, .55),
                       legend.text.align = 0,
                       legend.key.size = unit(1, 'line'),
                       legend.text = element_text(size = 8),
                       legend.background = element_rect(fill = alpha('white', .7),
                                                        colour = alpha('white',.7)),
                       legend.spacing = unit(.2, 'cm'),
                       panel.grid = element_blank(),
                       ) + labs(title = "A bloody 2021 in Mozambique",
                                subtitle = 'Northern region Cabo Delgado is the epicenter of the conflict',
                                caption = 'SOURCE: Acled, Natural Earth, World Cities')
ggsave('mozmap.png', width = 20, height = 15, units = 'cm')

#Study the mean fatalities per year
Africa_post_10 = subset(Africa, year > 2015)
yearly_means = aggregate(fatalities~year+country, data = Africa_post_10, FUN = mean)
yearly_no_countries = aggregate(fatalities~year, data = Africa_post_10, FUN = mean)
yearly_no_countries_sd = aggregate(fatalities~year, data = Africa_post_10, FUN = sd)
selected_yearly = yearly_means %>% filter(country %in% c('South Sudan',
                                                       'Niger',
                                                       'Libya',
                                                       'Mozambique'))
p1 = ggplot(yearly_no_countries_sd, aes(x = year, y =yearly_no_countries$fatalities+fatalities)) + 
  geom_area(fill = 'grey90', alpha = .9) + geom_area(data = yearly_no_countries, aes(x = year, y = fatalities),
                                         fill = 'grey80', alpha = .7) +
  geom_point(data = selected_yearly, aes(x = year, y = fatalities, color = country), size = 3) + 
  geom_line(data = selected_yearly, aes(x = year, y = fatalities, color = country),size = 1.5) +xlab('') +
  scale_color_brewer(palette = 'Set1') + 
  guides(color = guide_legend('Country:')) +
  geom_text(aes(x = 2016.7, y = 7.5, label = 'Standard deviation'), size = 5, color = 'grey55') + 
  geom_text(aes(x = 2018.5, y = .5, label = 'Mean fatalities per clash'), size = 5, color = 'grey45') + 
    transition_reveal(year) +
  picci + theme(panel.grid.minor.x = element_blank()) +
  labs(title = 'Yearly mean fatalities per armed clash',
                                     subtitle = 'Violent confrontations rarely cause dozens of deaths',
                                     caption = 'SOURCE: Elaboration on Acled data')

animate(p1, height = 15, width = 20, units = 'cm', res = 600, device = 
          'png')


#Study how many violent events are there per year in absolute terms...
events_distribution =  as.data.frame(table(Africa_post_10$event_type, Africa_post_10$year))
events_distribution = subset(events_distribution, Var2 != '2021')
events_per_year = aggregate(Freq~Var2, FUN = sum, data = events_distribution)

ggplot(events_distribution, aes(x = Var2, y = Freq)) + geom_col(aes(fill = Var1)) + 
  geom_text(aes(x = Var2, y = Freq, label = format(Freq, big.mark = ",", scientific = FALSE), group = Var1), position =  position_stack(vjust = .5)) + 
  geom_text(data = events_per_year, aes(x = Var2, y = Freq + 1000, label = paste('Total:',
                  format(Freq, big.mark = ",", scientific = FALSE))), fontface = 'bold', position =  position_nudge(y = 10)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  picci + xlab('') + guides(fill = guide_legend('Type of event:',
                                                title.position = 'top')) +
  scale_fill_brewer(palette = 'Set3') + labs(title = 'Violent events per type and year in Africa',
                                             subtitle = 'The Continent faces increasing political instability',
                                             caption = 'SOURCE: Elaboration on ACLED data')
ggsave('events_dist.png', width = 20, height = 15, units = 'cm')

#... and in relative terms
events_total = aggregate(Freq~Var1, FUN = sum, data = events_distribution)
events_total = events_total %>% mutate(
  Share = Freq/sum(Freq))

ggplot(events_total, aes(x = 1, y = Share)) + geom_col(aes(fill = Var1)) + 
  geom_text(aes(x = 1, y = Share, group = Var1,
                              label = paste0(round(Share, 2)*100, '%')), position = position_stack(vjust = .5), fontface = 'bold',
            size = 5) +
  coord_flip() + xlab('') + 
  scale_fill_brewer(palette = 'Set3') + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend('Type of event:',
                             title.position = 'top')) +
  labs(title = '% of clash types in Africa (2016-2020)',
       subtitle = 'Battles and political unrest rule',
       caption = 'SOURCE: Elaboration on ACLED data') +
  picci + theme(
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(
      color = 'grey25')
  ) 

ggsave('events_share.png', width = 20, height = 8, units = 'cm')

events_year_country =  as.data.frame(table(Africa_post_10$country,
                                           Africa_post_10$year))

colnames(events_year_country) = c('country', 
                                  'year', 'events')
#Read data on displaced people
Displaced <- read_excel("idmc_displacement_all_dataset.xlsx")
Displaced = Displaced %>% select(Name, 
                                 Year, 
                                 `Conflict New Displacements`)
Displaced = Displaced[-1,]
events_year_country$year =  as.character(events_year_country$year)
colnames(Displaced)[1:2] = c('country', 'year')
events_displaced = merge(events_year_country, Displaced, by = c('country', 'year'))
events_displaced$`Conflict New Displacements` = as.numeric(events_displaced$`Conflict New Displacements`)
events_displaced$`Conflict New Displacements` = ifelse(is.na(events_displaced$`Conflict New Displacements`), 0, events_displaced$`Conflict New Displacements`)
label = colnames(events_displaced)[4]
colnames(events_displaced)[4] = 'new_displacement'

#Relation between displaced and eventes 
##Check a possible statistically significant relation
summary(lm(new_displacement~events, data = events_displaced))

##Plot the chart
ggplot(events_displaced, aes(x = events, y = new_displacement, label = country)) +
  geom_point(color = 'black', fill = 'grey25', alpha = .5, shape = 21) + geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', color = 'red') +
  geom_label_repel(max.overlaps = 5) + 
  scale_y_continuous(label =scales::label_number_si()) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  picci +
  facet_wrap(.~year) + labs(title = 'Violent events and new internally displaced people',
                          subtitle = 'The more events (horizontal) the more displaced (vertical)?',
                          caption = 'SOURCE: Elaboration on ACLED data and Global Internal Displacement Database')

ggsave('events_displaced.png', width = 20, height = 15, units = 'cm')

#Democracy and displacede 
##Upload Vdem data and subset the dataset
`V-Dem-CY-Core-v11.1` <- readRDS("~/Mozambique/V-Dem-CY-Core-v11.1.rds")
Vdem = `V-Dem-CY-Core-v11.1` %>% select(
  country_name,
  year,
  v2x_libdem,
  v2x_regime
) %>% filter(country_name %in% events_displaced$country, 
             year > 2015)

Vdem$Regime[Vdem$v2x_regime == 0] <- 'Closed autocracy'
Vdem$Regime[Vdem$v2x_regime == 1] <- 'Electoral autocracy'
Vdem$Regime[Vdem$v2x_regime == 2] <- 'Electoral democracy'
Vdem$Regime[Vdem$v2x_regime == 3] <- 'Liberal democracy'
Vdem$year = as.character(Vdem$year)
colnames(Vdem)[1] = 'country'

events_displaced = merge(events_displaced, Vdem, by = c('country', 'year'))

regime = ggplot(unique(events_displaced), aes(x = Regime, y = new_displacement, label = country)) + geom_point(aes(size = events, fill = Regime), shape = 21, alpha = .7) + 
  geom_label_repel(aes(x = Regime, y = new_displacement), size = 3, max.overlaps = 13,
                   fill = alpha(c("white"),0.5)) + picci +
  coord_flip() + ylab('New displaced people') + scale_fill_brewer(palette = 'Set1') +
  scale_size_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE),
                        range = c(1,10)) +
  scale_y_continuous(label =scales::label_number_si()) +
  facet_wrap(.~year) + guides(fill = FALSE, size = guide_legend(title = 'Number of events',
                                                                 )) 

regime + cavoletti

cavoletti = labs(title = 'Intenally displaced by regime type',
                 subtitle = 'IDPs number are a mistery yet to be solved',
                 caption = 'SOURCE: Vdem and Global Internal Displacement Database')


ggsave('regimes_displaced.png', width = 20, height = 15, units = 'cm')

#Plot the yearly mean of new internally displaced
new_displacement_year = aggregate(new_displacement~country, FUN = mean, data = events_displaced)
new_displacement_year =  subset(new_displacement_year, new_displacement > 0)
ggplot(new_displacement_year, aes(x = reorder(country,new_displacement), y = new_displacement, 
                                  label = format(round(new_displacement,0), big.mark = ','))) + 
  geom_col(fill = 'grey25', alpha = .7) + geom_text(nudge_y = 100000) + coord_flip() + 
  scale_y_continuous(limits = c(0, 1400000)) +
  picci + theme(axis.line = element_blank(),
                panel.grid = element_blank(),
                panel.grid.major.y = element_blank(),
                axis.text.x = element_blank(),
                axis.title.x = element_blank()) + labs(title = 'New internally displaced, yearly mean*',
                                                       subtitle = 'High numbers in Sub-saharian Africa',
                                                       caption = 'SOURCE: Global Internal Displacement Database; *2016,2019')

ggsave('mean_displaced.png', width = 20, height = 15, units = 'cm')

#Study the relation between war fatalities and new displaced people 
events_displced =  merge(events_displaced, yearly_means, by = c('country', 'year'))

##Look for a statistically significant relation 
summary(lm(new_displacement ~ fatalities, data = events_displced))

##Do the plot
ggplot(events_displced , aes(x = fatalities, y = new_displacement, label = country)) +
  geom_point(color = 'black', fill = 'grey25', alpha = .5, shape = 21) + geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', color = 'red') +
  geom_label_repel(max.overlaps = 5) + 
  scale_y_continuous(label =scales::label_number_si()) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  picci + xlab('') +
  facet_wrap(.~year) + labs(title = 'Fatalities and new internally displaced people',
                            subtitle = 'The more casualties (horizontal) the more displaced (vertical)',
                            caption = 'SOURCE: Elaboration on ACLED data and Global Internal Displacement Database')

ggsave('killed_displaced.png', width = 20, height = 15, units = 'cm')

