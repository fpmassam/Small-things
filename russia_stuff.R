library(readr)
library(tidyverse)
library(reshape2)
library(ggpmisc)
library(tidyverse) 
library(maps)
require(acled.api)
require(wbstats)


#Weaponry export Russia 
TIV_Export_RUS_1989_2020 <- read_csv("TIV-Export-RUS-1989-2020.csv", 
                                       na = "0", skip = 10)
export = melt(TIV_Export_RUS_1989_2020, id.vars = c('...1', 'Total'))
export$value[is.na(export$value)] <- 0
export$Total[is.na(export$Total)] <- 0
export$Total = as.numeric(export$Total)
export_general = aggregate(value~variable, data = export, FUN = sum)
export_general$Group = 'Russia'
 
export_general = unique(export_general)

ggplot(export_general, aes(x = as.Date(variable, '%Y'), y = value, group = Group)) + 
  geom_area(color = 'SteelBlue3', fill = 'SteelBlue3', alpha = .5) +
  scale_y_continuous(expand = c(0,0), labels = scales::comma) +
  geom_vline(aes(xintercept = as.Date('2014-12-19')), linetype = 'dashed') + 
  geom_text(aes(x = as.Date('2014-12-01'), y = 7000, label = 'U.S. sanctions on Russia over Ukraine'), 
            family = 'Garamond', angle = 90, vjust = -.2) +
  annotate(geom = "text_npc",
           npcx = "left",
           npcy = "top",
           label = c('Trends Indicator Values measure\nthe capabilities of exported weaponry'), 
           family = 'Garamond') +
  labs(x = '',
       y = '', 
       title = 'The decline of Russian arms export', 
       subtitle = 'Thousands of Trends Indicator Values',
       caption = 'SOURCE: Stockholm International Peace Institute') + picci

ggsave('russian_export.png', width = 20, height = 16, units = 'cm')

#Russian gas imported by the EU over time 
nrg_ti_gasm_custom_2072129_linear_csv <- read_csv("nrg_ti_gasm__custom_2072129_linear.csv.gz")
nrg_europe = nrg_ti_gasm_custom_2072129_linear_csv %>%
  filter(geo %in% c('FR', 'DE', 'IT', 'EU27_2020')) %>% select(TIME_PERIOD, geo, OBS_VALUE) %>%
  filter(TIME_PERIOD %in% c('2016-11', '2021-11'))
nrg_europe = nrg_europe[9:16,]
nrg_europe$Country[nrg_europe$geo == 'EU27_2020'] <- 'European Union'
nrg_europe$Country[nrg_europe$geo == 'DE'] <- 'Germany'
nrg_europe$Country[nrg_europe$geo == 'FR'] <- 'France'
nrg_europe$Country[nrg_europe$geo == 'IT'] <- 'Italy'
nrg_europe$time[nrg_europe$TIME_PERIOD == '2016-11'] <- 'Nov. 2016'
nrg_europe$time[nrg_europe$TIME_PERIOD == '2021-11'] <- 'Nov. 2021'
nrg_europe_left = subset(nrg_europe, TIME_PERIOD == '2016-11')
nrg_europe_right = subset(nrg_europe, TIME_PERIOD == '2021-11')


ggplot(nrg_europe, aes(x = time, y = OBS_VALUE)) + 
  geom_vline(xintercept = c('Nov. 2016',
                            'Nov. 2021'), linetype = 'dashed') +
  geom_point(aes(color = Country), size = 4) + geom_line(aes(
  group = Country, color = Country), size = 1.5) + 
  scale_color_brewer(palette = 'Set2') +
  ggrepel::geom_text_repel(data = nrg_europe_left, aes(x = time, y = OBS_VALUE,
                                                       label = 
                                                         format(OBS_VALUE,
                                                                big.mark = ',', scientific = FALSE),
                                                       segment.alpha = 0), 
            nudge_x = -.1, family = 'Garamond') + ggrepel::geom_text_repel(data = nrg_europe_right, aes(x = time, y = OBS_VALUE,
                                                                   label = format(
                                                                     round(OBS_VALUE), 
                                                                     big.mark = ',', scientific = FALSE)),
                                                                   family = 'Garamond', nudge_x = .1, 
                                                                   segment.alpha = 0) + 
  scale_y_continuous(limits = c(0, 60000), 
                     labels = scales::comma, 
                     expand = c(0,0), 
                     breaks = c(0, 20000, 40000, 60000)) +
  scale_x_discrete(expand = c(.1,.1)) +
  labs(title = "EU's addiction to Russian natural gas", 
       subtitle = 'Natural gas, millions of cubic meters',
       caption = 'SOURCE: Eurostat', 
       x = '', 
       y = '') +
  picci + theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    legend.text = element_text(size = 12)
  )
  
ggsave('russian_export_gas.png', width = 20, height = 15, units = 'cm')



#Flights over ukraine 
state_vectors <- getIntervalStateVectors(startTime = "2022-02-10 07:00:00",
                                         endTime = "2022-02-12 15:00:00",
                                         timeZone = "Europe/Madrid",
                                         minLatitude = 43,
                                         minLongitude = 20,
                                         maxLatitude = 53,
                                         maxLongitude = 42,
                                         username = 'fpmassam',
                                         password = 'Tcv3I98s3tt3@@@@')
username = 'fpmassam'
pwd = 'Tcv3I98s3tt3@@@@'


flight = GET('https://opensky-network.org/api/states/all?AE1499')
flight1 = GET('https://opensky-network.org/api/states/all?424317')


spy = data.frame(fromJSON('https://opensky-network.org/api/states/all?AE1499', flatten = TRUE))
spy1 = data.frame(fromJSON('https://opensky-network.org/api/states/all?424317', flatten = TRUE))
spy1$time =  as_datetime(as.POSIXlt(spy1$time, origin="1970-01-01"))
spy$time =  as_datetime(as.POSIXlt(spy$time, origin="1970-01-01"))
spy = subset(spy, states.3 == 'Ukraine')
spy$states.6 = as.numeric(spy$states.6)
spy$states.7 = as.numeric(spy$states.7)


worldmap = spData::world
ggplot() + geom_sf(data = worldmap) + 
  geom_line(data = spy, aes(x = states.6, y = states.7))


ggplot(spy, aes(x = states.6, y = states.7)) + geom_line()




ggplot(spy, aes(x= as.numeric(states.6), y = as.numeric(states.7))) + geom_line()


#Gdp per capita 
countries = wb_countries()
gdp_warsaw = wb_data(country = c('Poland', 'Russian Federation', 'Estonia', 'Latvia', 'Lithuania', 
               'Moldova', 'Czech Republic', 'Slovak Republic', 'Ukraine', 'Hungary', 
               'Bulgaria', 'Romania', 'Belarus', 'Albania'), indicator = 'NY.GDP.PCAP.CD')
gdp_warsaw = subset(gdp_warsaw, date > 1999)
gdp_warsaw$EU[gdp_warsaw$country %in% c('Poland', 'Estonia', 'Latvia', 'Lithuania', 
                             'Czech Republic', 'Slovak Republic', 'Hungary') & gdp_warsaw$date > 2003] <- 'EU member'

gdp_warsaw$EU[gdp_warsaw$country %in% c('Romania', 'Bulgaria') & gdp_warsaw$date > 2006] <- 'EU member'
gdp_warsaw$EU[gdp_warsaw$country %in% c('Russian Federation', 'Ukraine') & gdp_warsaw$date > 2013] <- 'Russia annexed Crimea'
gdp_warsaw$EU[is.na(gdp_warsaw$EU)] <- 'Non-EU member'
labs_max = subset(gdp_warsaw, date == max(gdp_warsaw$date))
enlargement = data.frame(date = c('2004', '2007', '2014'),
                         labels = c('First EU enlargement',
                                    'Second EU enlargement', 
                                    'Annexation of Crimea'))

ggplot(gdp_warsaw, aes(x = as.Date(as.character(date), '%Y'))) + 
  geom_col(aes(fill = EU, color = EU, y = NY.GDP.PCAP.CD)) + 
  scale_color_brewer(palette = 'Set1') +
  scale_fill_brewer(palette = 'Set1')+ facet_wrap(~country, nrow = 7) +
  scale_y_continuous(labels = scales::comma, breaks = c(0, 10000, 20000)) + labs(title = 'Winners and losers in the former Eastern bloc',
                                                      subtitle = 'GDP per capita, current US dollars', 
                                                      caption = 'SOURCE: World Bank, EU Commission', 
                                                      x = '', 
                                                      y = '') + picci + theme(
                                                        strip.text.x = element_text(size = 12), 
                                                        legend.position = 'top',
                                                        legend.title = element_blank(),
                                                        legend.text = element_text(size = 12),
                                                        legend.justification = 'left'
                                                      )






ggsave('Winners_losers.png', width = 20, height = 30, units = 'cm')

#Resource rents 
rent_resource = wb_data(country = c('Poland', 'Russian Federation', 'Estonia', 'Latvia', 'Lithuania', 
                                                 'Moldova', 'Czech Republic', 'Slovak Republic', 'Ukraine', 'Hungary', 
                                                 'Bulgaria', 'Romania', 'Belarus', 'Albania'), indicator = 'NY.GDP.TOTL.RT.ZS')
rent_resource = subset(rent_resource, date == 2019)
rent_resource$EU[rent_resource$country %in% c('Poland', 'Estonia', 'Latvia', 'Lithuania', 
                                              'Czech Republic', 'Slovak Republic', 'Hungary',
                                              'Romania', 'Bulgaria')] <- 'EU member'
rent_resource$EU[is.na(rent_resource$EU)] <- 'Non-EU member'

ggplot(rent_resource, aes(x = reorder(country,NY.GDP.TOTL.RT.ZS), y = 100)) + 
  geom_col(fill = 'grey89') +
  geom_col(aes(y = NY.GDP.TOTL.RT.ZS, fill = EU)) + 
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() + 
  geom_text(aes(y = 1, label = paste(paste0(country, ':'), paste0(
    round(NY.GDP.TOTL.RT.ZS, 1), '%')
  ), family = 'Garamond', hjust = 0)) +
  scale_fill_brewer(palette = 'Set1') + picci + labs(title = 'Russian GDP relies on natural resources',
                                                     subtitle = 'Resource rent, % of GDP', 
                                                     caption = 'SOURCE: World Bank, EU Commission', 
                                                     x = '',
                                                     y = '') + theme(
                                                       axis.text = element_blank(),
                                                       panel.grid.major.y = element_blank(),
                                                       axis.ticks.x = element_blank(), 
                                                       legend.title = element_blank()
                                                     )


ggsave('Natuarl_resources.png', width = 20, height = 16, units = 'cm')


# Resource curse in democracy 
global_resource = wb_data(indicator = 'NY.GDP.TOTL.RT.ZS')
global_resource = subset(global_resource, date == 2019)
renter_gdp_per_capita = wb_data(country = global_resource$country, indicator = 'NY.GDP.PCAP.CD')
renter_gdp_per_capita_10 = subset(renter_gdp_per_capita, date == 2010)
renter_gdp_per_capita_20 = subset(renter_gdp_per_capita, date == 2020)
renter_gdp_per_capita_20 = renter_gdp_per_capita_20 %>% select(country, date, NY.GDP.PCAP.CD) %>% mutate(
  
   'GDP_2020' = NY.GDP.PCAP.CD) %>% select(country, GDP_2020)
renter_gdp_per_capita_10 = renter_gdp_per_capita_10 %>% select(country, date, NY.GDP.PCAP.CD) %>% mutate(
  
  'GDP_2010' = NY.GDP.PCAP.CD) %>% select(country, GDP_2010)

renter_gdp = merge(renter_gdp_per_capita_10, renter_gdp_per_capita_20, by = 'country')
renter_gdp = na.omit(renter_gdp)
renter_gdp = renter_gdp %>% mutate('change' = (GDP_2020-GDP_2010)/GDP_2010)
renter_gdp = merge(global_resource, renter_gdp)
renter_gdp$res_rent = renter_gdp$NY.GDP.TOTL.RT.ZS/100

summary(lm(change~res_rent + GDP_2020, data = renter_gdp))
renter_gdp$color[renter_gdp$country == 'Russian Federation'] <- 'Brown4'
renter_gdp$color[renter_gdp$country != 'Russian Federation'] <- 'SteelBlue3'
renter_rus = subset(renter_gdp, country == 'Russian Federation')
ggplot(renter_gdp, aes(x = NY.GDP.TOTL.RT.ZS/100, y = change)) + geom_point(fill = renter_gdp$color, 
                                                                        color = 'black', shape = 21, 
                                                                        alpha = .7,
                                                                        aes(size = GDP_2020)) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0,0.55)) +
  scale_size_continuous(labels = scales::comma) + 
  labs(title = 'Is the reseource curse real?',
       subtitle = 'Resource rents do not predict change in GDP per capita', 
       caption = 'SOURCE: World Bank', 
       size = 'GDP per capita (2020) - current US$', 
       y = '% change in GDP per capita (2020-2010)',
       x = 'Resource Rents (% of GDP)') +
  ggrepel::geom_text_repel(aes(label = country), family = 'Garamond', size = 4, max.overlaps = 5) +
  geom_text(data = renter_rus, aes(x = NY.GDP.TOTL.RT.ZS/100, y = change, label = 'Russia'), size = 6, 
            family = 'Garamond', face = 'bold', nudge_x = 0.01, nudge_y =  -.06, color = 'Brown4') +
  geom_smooth(color = 'red', se = F, linetype = 'dashed', method = 'lm') + picci + theme(
    legend.position = 'bottom'
  )

ggsave('Resource_curse.png', width = 20, height = 20, units = 'cm')


#Democracy and GDP (this is a pre-saved version of the V-Demd dataset)
Vdem <- read_csv("~/random_covid/Vdem.csv")
Vdem = merge(Vdem, renter_gdp, by.x = 
        'country_text_id', by.y = 
        'iso3c')

summary(lm(change~v2x_libdem+Regime+GDP_2010, data = Vdem))

russia_vdem = subset(Vdem, country == 'Russian Federation')

ggplot(Vdem, aes(x = v2x_libdem, y = change)) + geom_point(
  aes(fill = Regime, size = GDP_2020), color = 'black', shape = 21, size = 4) + 
  geom_point(data = russia_vdem, aes(x = v2x_libdem, y = change), shape = 21, color = 'Brown4', size = 6, alpha = 1,
             stroke = 2) +
  geom_text(data = russia_vdem, aes(x = v2x_libdem, y = change, label = country_name), 
            family = 'Garamond', fontface = 'bold', nudge_x = 0.07, nudge_y = -0.0, color = 'Brown4', 
            size = 6) +
  scale_y_continuous(labels = scales::percent) + 
  labs(title = 'It is just politics, stupid',
       subtitle = 'Democracy level and do not predict change in GDP per capita', 
       y = '% change, GDP per capita (2020-2010)', 
       x = 'Democracy level', 
       caption = 'SOURCE: V-Dem, World Bank') +
  scale_fill_brewer(palette = 'Set3') + picci


ggsave('Demo_change.png', width = 20, height = 20, units = 'cm')

#Democracy in eastern europe

`V-Dem-CY-Core-v11.1` <- readRDS("~/Mozambique/V-Dem-CY-Core-v11.1.rds")
v_east = subset(`V-Dem-CY-Core-v11.1`, country_text_id %in% unique(gdp_warsaw$iso3c) 
                & year > 1989)
v_east = v_east %>% select(country_text_id, country_name, year, v2x_libdem, v2x_regime)
v_east$`Regime type`[v_east$v2x_regime == 0] <- 'Closed Autocracy'
v_east$`Regime type`[v_east$v2x_regime == 1] <- 'Electoral Autocracy'
v_east$`Regime type`[v_east$v2x_regime == 2] <- 'Electoral Democracy'
v_east$`Regime type`[v_east$v2x_regime == 3] <- 'Liberal Democracy'

ggplot(v_east, aes(x = year, y = country_name)) + 
  geom_tile(aes(fill = `Regime type`), color = 'black') + 
  scale_x_continuous(expand = c(0,1)) +
  scale_y_discrete(limits = rev) +
  scale_fill_brewer(palette = 'Set3') + picci + 
  guides(fill = guide_legend(title.position = "top", ncol = 2)) +
  labs(title = 'Democracy in the former Eastern bloc', 
       subtitle = 'Regime type by year', 
       caption = 'SOURCE: V-Dem', 
       x = '', 
       y = '') + theme(panel.grid.major.y = element_blank())

ggsave('Demo_dev.png', width = 20, height = 16, units = 'cm')
rm(`V-Dem-CY-Core-v11.1` )







