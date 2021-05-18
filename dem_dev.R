require(tidyverse)
require(rvest)
library(RColorBrewer)
library(hdr)
url = 'https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=141329'
codes = read_html(url) %>%
  html_element('table') %>%
  html_table()

#Regimes in Europe 
V_DEM = read_csv("Country_Year_V-Dem_Core_CSV_v11.1/V-Dem-CY-Core-v11.1.csv")
V_DEM = subset(V_DEM, year > 1989)

V_DEM_RIW = V_DEM %>% filter(country_text_id %in% c(codes$`Alpha-3`)) %>%
  select(country_name,
         year,
         v2x_regime)
V_DEM_RIW = subset(V_DEM_RIW, country_name != 'United Kingdom')
V_DEM_RIW$`Regime type`[V_DEM_RIW$v2x_regime == 0] <- 'Closed Autocracy'
V_DEM_RIW$`Regime type`[V_DEM_RIW$v2x_regime == 1] <- 'Electoral Autocracy'
V_DEM_RIW$`Regime type`[V_DEM_RIW$v2x_regime == 2] <- 'Electoral Democracy'
V_DEM_RIW$`Regime type`[V_DEM_RIW$v2x_regime == 3] <- 'Liberal Democracy'
V_DEM_RIW <- with(V_DEM_RIW,  V_DEM_RIW[order(V_DEM_RIW$country_name) , ])

ggplot(V_DEM_RIW, aes(x = year, y = country_name, fill = `Regime type`)) + geom_tile(color = 'black') +
  guides(fill = guide_legend(
    title.position = 'top',
    nrow = 2,
    keywidth = 1,
    keyheight = .5)) + scale_fill_manual(values = rev(brewer.pal(4, "Set3"))) +
  scale_x_continuous(expand = c(0,.3)) + xlab('') + ylab('') +
  scale_y_discrete(limits = rev) +
    labs(title = 'Regime types by year in the European Union',
       subtitle = 'Hungary is the only Autocracy in the bloc',
       caption = 'SOURCE: V-Dem Version 11.1') + picci +
  theme(axis.text.x = element_text(hjust = .5),
        panel.grid = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title.position = 'plot',
        legend.justification = 'left',
        legend.position = 'top')

ggsave('regimes_europe.png', width = 20, height = 16, units = 'cm')

V_DEM_Libdem = V_DEM %>% filter(country_text_id %in% c(codes$`Alpha-3`)) %>%
  select(country_name,
         year,
         v2x_libdem)
V_DEM_Libdem = subset(V_DEM_Libdem, country_name != 'United Kingdom')
selection = c("Hungary", "Poland", 'Latvia', 'Hungary',
              'Slovakia', 'Bulgaria', 'Slovenia', 'Czech Republic')
V_DEM_Libdem_1 = subset(V_DEM_Libdem, country_name %in% selection)

ggplot(V_DEM_Libdem, aes(x = year, y = v2x_libdem, group = country_name)) + 
  geom_line(alpha = .1) +
  geom_point(alpha = .1) + 
  geom_line(data = V_DEM_Libdem_1, aes(x = year, y = v2x_libdem, color = country_name,
                                       group = country_name)) + 
  ylim(c(0,1)) + scale_color_brewer(palette = 'Set3') +
  labs(title = 'Democracy levels in the European Union',
       subtitle = 'Probelm in Central-eastern Europe',
       caption = 'SOURCE: V-Dem Version 11.1') +
  guides(color = guide_legend(title = 'Country',
                              title.position = 'left')) +
  geom_point(data = V_DEM_Libdem_1, aes(x = year, y = v2x_libdem, color = country_name,
                                       group = country_name)) + xlab('') + ylab('Liberal democracy') +  picci +
  theme(legend.position = 'bottom',
        legend.justification = 'left',
        plot.title.position = 'plot')
  
ggsave('democracy_europe.png', width = 20, height = 16, units = 'cm')

v_dem_time_series = reshape2::dcast(V_DEM_Libdem, country_name ~ year)

dem_change =data.frame(
country_name = v_dem_time_series$country_name,
`One year change` =  v_dem_time_series$`2020` - v_dem_time_series$`2019`,
`Five year change` =v_dem_time_series$`2020` - v_dem_time_series$`2015`,
`Ten year change` = v_dem_time_series$`2020` - v_dem_time_series$`2010`
)
dem_change = reshape2::melt(dem_change)
dem_change$variable =  gsub('\\.', ' ', dem_change$variable)

ggplot(dem_change, aes(x = value, y = country_name)) +
  geom_vline(xintercept = 0) + 
  guides(fill = guide_legend(title = '')) +
  geom_point(aes(fill = variable), color = 'black', shape = 21, 
                                          size = 4, alpha = .7) +
  picci + labs(title = 'Democracy variation the EU',
               subtitle = 'Reference year = 2020',
               caption = 'SOURCE: V-Dem Version 11.1') +
  theme(
    plot.title.position = 'plot'
  )

#Collect HDI from the UNDP API
HDI_2019 = jsonlite::fromJSON('http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/indicator_id=137506/year=2019')
HDI_2019 = unlist(HDI_2019$indicator_value)
HDI_2019 = data.frame(HDI_2019)
HDI_2019 = data.frame(rownames(HDI_2019),
                      HDI_2019)

HDI_2019 = HDI_2019 %>% separate(rownames.HDI_2019.,sep = '[//.]', into = c('country_text_id', 'indicator', 'year'),
                      fill = 'warn')

rownames(HDI_2019) = NULL

Vdem_world = V_DEM %>% select(country_name,
                              country_text_id,
                              year,
                              v2x_polyarchy,
                              v2x_regime)
scatterplot =  merge(Vdem_world, HDI_2019, by = c('country_text_id', 'year'))
scatterplot$`Regime type`[scatterplot$v2x_regime == 0] <- 'Closed Autocracy'
scatterplot$`Regime type`[scatterplot$v2x_regime == 1] <- 'Electoral Autocracy'
scatterplot$`Regime type`[scatterplot$v2x_regime == 2] <- 'Electoral Democracy'
scatterplot$`Regime type`[scatterplot$v2x_regime == 3] <- 'Liberal Democracy'

HDI = jsonlite::fromJSON('http://ec2-54-174-131-205.compute-1.amazonaws.com/API/HDRO_API.php/indicator_id=137506')
HDI = unlist(HDI$indicator_value)
HDI = data.frame(HDI)
HDI = data.frame(rownames(HDI),
                      HDI)

HDI = HDI %>% separate(rownames.HDI.,sep = '[//.]', into = c('country_text_id', 'indicator', 'year'),
                                 fill = 'warn')

#Build the density plot (1st chart in the Newsletter)
densitypolot =  merge(Vdem_world, HDI, by = c('country_text_id', 'year'))
densitypolot$`Regime type`[densitypolot$v2x_regime == 0] <- 'Closed Autocracy'
densitypolot$`Regime type`[densitypolot$v2x_regime == 1] <- 'Electoral Autocracy'
densitypolot$`Regime type`[densitypolot$v2x_regime == 2] <- 'Electoral Democracy'
densitypolot$`Regime type`[densitypolot$v2x_regime == 3] <- 'Liberal Democracy'
ggplot(densitypolot, aes(x = HDI, y = v2x_polyarchy, label = country_name)) + scale_fill_brewer(palette = 'Blues') +
  geom_density_2d_filled() + geom_point(aes(alpha = year), color = 'black', fill = 'gold3', shape = 21) +
  geom_line(aes(alpha = year, group = country_name), color = 'gold3') +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + xlab('Human Development Index') + ylab('Democracy Level') +
  scale_alpha_continuous(range = c(.01, .2)) + picci + labs(title = 'Democracy and Human development',
                                                            subtitle = 'Density comparision (1990-2019)',
                                                            caption = 'SOURCE: V-Dem Version 11.1, UNDP Human Development Report') + 
  theme(legend.position = 'none',
        plot.title.position = 'plot')

ggsave('democracy_hdi.png', width = 20, height = 16, units = 'cm')


#Prepare animation
p1 = ggplot(na.omit(densitypolot), aes(x = HDI, y = v2x_polyarchy, label = country_name, )) +
  geom_vline(xintercept = 0.8) + geom_hline(yintercept = .65) +
  geom_point(aes(fill = `Regime type`), shape = 21, color = 'black', size = 3, alpha = .5) +
  scale_fill_manual(values = rev(brewer.pal(4, "Set3"))) +
  transition_time(as.integer(year)) +
  guides(fill = guide_legend(
    title.position = 'top',
    nrow = 2)) +
  xlim(c(0.2,1)) + 
  ylim(c(0,1)) +
  xlab('Human Development Index') + ylab('Democracy Level') +
  scale_alpha_continuous(range = c(.01, .2)) + picci + labs(title = 'Democracy and Human development',
                                                            subtitle = "Year: {frame_time}",
                                                            caption = 'SOURCE: V-Dem Version 11.1, UNDP Human Development Report')
animate(p1, height = 15, width = 20, units = 'cm', res = 300, device = 'png', 
        nframes = 300, fps = 24)

anim_save('history.gif')

#Easter egg 
 
Highly_developed_countries <- subset(scatterplot, HDI_2019 > 0.8) 
dev_dem = as.data.frame(table(Highly_developed_countries$`Regime type`))
dev_dem = dev_dem %>% mutate(
  Share = Freq/sum(Freq))

ggplot(dev_dem, aes(x = 1, y = Freq)) + geom_col(aes(fill = Var1)) + 
  geom_text(aes(x = 1, y = Freq, group = Var1,
                label = Freq), position = position_stack(vjust = .5), fontface = 'bold',
            size = 5) +
  coord_flip() + xlab('') + 
  scale_fill_manual(values = rev(brewer.pal(4, "Set3")))  + 
  scale_x_continuous(expand = c(0, 0)) +
  guides(fill = guide_legend('Regime type',
                             title.position = 'top')) +
  labs(title = '# of countries by regime type',
       subtitle = 'Highly developed countries tend to be democratic',
       caption = 'SOURCE: V-Dem Version 11.1, UNDP Human Development Report') +
  picci + theme(
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_line(
      color = 'grey25')
  ) 


HDI_year = split(HDI, f = HDI$year)
max_HDI = function(x){
  data.frame(year = unique(x$year),
             max_hdi = max(x$HDI),
             min_hdi = min(x$HDI))
}

HDI_year = do.call(rbind, lapply(HDI_year, max_HDI))
HDI_year$delta = HDI_year$max_hdi - HDI_year$min_hdi
HDI_year$position = (HDI_year$max_hdi + HDI_year$min_hdi)/2
HDI_year$positions[as.numeric(levels(HDI_year$year)) %% 2] <- (HDI_year$max_hdi + HDI_year$min_hdi)/2 - (1/3*(HDI_year$max_hdi + HDI_year$min_hdi)/2)
ggplot(HDI_year, aes(x =  forcats::fct_rev(year))) + ylim(c(0,1))+
  geom_segment(aes(y = min_hdi, yend = max_hdi, xend = year), color = 'grey85') +
  geom_point(aes(y = max_hdi), size = 4, fill = 'lightsteelblue3', shape = 21) + 
  geom_point(aes(y = min_hdi), size = 4, fill = 'lightsteelblue4', shape = 21) +
  geom_text(aes(y = position, label = paste('Spread:', round(delta, 3))), typeface = 'bold') +
  ylab('Human Development Index') + picci + xlab('') +
  coord_flip() + theme(
   panel.grid.major.y = element_blank(),
   panel.grid.minor.x = element_blank(),
   plot.title.position = 'plot'
  ) + labs(title = 'Most developed, least developed country: difference',
           subtitle = 'Least developed countries slowly closing the gap',
           caption = 'UNDP Human Development Report') 
  
ggsave('delta_hdi.png', width = 20, height = 15, units = 'cm')

v_dem_time_series_world = reshape2::dcast(Vdem_world, country_name + country_text_id ~ year, value.var = 'v2x_polyarchy')
dem_change_world =data.frame(
  country_name = v_dem_time_series_world$country_name,
  country_text_id = v_dem_time_series_world$country_text_id,
  `One-year change` =  v_dem_time_series_world$`2020` - v_dem_time_series_world$`2019`,
  `Five-years change` =v_dem_time_series_world$`2020` - v_dem_time_series_world$`2015`,
  `10-years change` = v_dem_time_series_world$`2020` - v_dem_time_series_world$`2010`
)
dem_change = reshape2::melt(dem_change_world)
dem_change$variable =  gsub('\\.', ' ', dem_change$variable)
dem_change$variable =  gsub('X', '', dem_change$variable)
colnames(dem_change)[4] = 'Dem_difference'

#Change the format of the HDI dataset for the Cleveland dot plot (Chart #3)

HDI_time_series_world = reshape2::dcast(HDI, country_text_id ~ year, value.var = 'HDI')
HDI_change_world =data.frame(
  country_text_id = HDI_time_series_world$country_text_id,
  `One-year change` =  HDI_time_series_world $`2019` - HDI_time_series_world $`2018`,
  `Five-years change` =HDI_time_series_world $`2019` - HDI_time_series_world $`2014`,
  `10-years change` = HDI_time_series_world $`2019` - HDI_time_series_world $`2009`
)

hdi_change = reshape2::melt(HDI_change_world)
hdi_change$variable =  gsub('\\.', ' ', hdi_change$variable)
hdi_change$variable =  gsub('X', '', hdi_change$variable)
colnames(hdi_change)[3] = 'HDI_difference'

hdi_change = merge(dem_change, hdi_change, by = c('country_text_id', 'variable'))


ggplot(hdi_change, aes(x = HDI_difference, y = Dem_difference, group = variable)) + geom_point(aes(fill = variable), shape = 21, alpha = .7, size = 3) +
  xlab("One year-lagged HDI change") + ggrepel::geom_text_repel(aes(label = country_name)) + 
  guides(fill = guide_legend(title = '')) + scale_fill_brewer(palette = 'Set3') +
  ylab("Democratic change") + labs(title = 'Change in HDI and democratic difference',
                                   subtitle = 'HDI difference is lagged by one year',
                                   caption = 'SOURCE: V-Dem Version 11.1, UNDP Human Development Report') + picci + 
  theme(plot.title.position = 'plot')

ggsave('change_hdi.png', width = 20, height = 15, units = 'cm')

#Democratic satisfaction. Collecte the data and do the ridgeplot
require(ggridges)
dem_sat <- read_csv("dem_sat_1.csv")
dem_sat =  na.omit(dem_sat)
dem_sat = reshape2::melt(dem_sat)
dem_sat$variable <- gsub('Extremely dissatisfied', 0, dem_sat$variable)
dem_sat$variable <- gsub('Extremely satisfied', 10, dem_sat$variable)
dem_sat$variable =  as.integer(dem_sat$variable)
colnames(dem_sat)[1] <- 'country'

ggplot(dem_sat, aes(height = value, group = country, y = country, x = variable)) + 
  geom_ridgeline(aes(scale = .05), alpha = .5, fill = 'Steelblue3') +
  xlab('How satisfied with the way democracy works in country (0-10)') + ylab('') + 
  scale_y_discrete(limits = rev) +
  labs(title = 'Democracy satisfaction in Europe',
       subtitle = 'Zero = Extremely dissatisfied, 10 = Extremely satisfied',
       caption = 'SPURCE: ESS edition 3.1') + picci + 
  scale_x_continuous(expand = c(0, 0), breaks = c(0,1,2,3,4,5,5,6,7,8,9,10)) +
  theme(axis.text.x = element_text(size = 8),
        axis.text.y = element_text(vjust = .2),
        axis.ticks = element_line(),
        plot.title.position = 'plot',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


ggsave('dem_sat_europe.png', width = 20, height = 16, units = 'cm')