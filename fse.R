
require(tidyverse)
library(readxl)
require(zoo)
require(gganimate)
require(ggrepel)

Monthly_detections_of_IBC_2022_06_07 <- read_excel("Monthly_detections_of_IBC_2022_06_07.xlsx")
View(Monthly_detections_of_IBC_2022_06_07)     



nations = data.frame(nations = unique(Monthly_detections_of_IBC_2022_06_07$Nationality))

FSE = subset(nations, nations %in% c(
  "Russian Federation",
  'Russia',
  "Kazakhstan",
  "Kyrgyzstan",
  "Tajikistan",
  "Ukraine",
  "Uzbekistan",
  "Georgia",
  "Armenia",
  "Turkmenistan",
  "Azerbaijan",
  "Belarus",
  'Moldova'
))

grand_totals = reshape2::melt(Monthly_detections_of_IBC_2022_06_07, id.vars = c('Border type or inland', 'Nationality',
                                                        'Route'))
sum(grand_totals$value)
nantions = as.data.frame(table(unique(grand_totals$Nationality)))


FSE_illegals <- subset(Monthly_detections_of_IBC_2022_06_07, Nationality %in% FSE$nations)
FSE_illegals <-reshape2::melt(FSE_illegals, id.vars = c('Border type or inland', 'Nationality',
                                         'Route'))
FSE_illegals$variable <- as.character(FSE_illegals$variable)
FSE_illegals$variable = as.Date(as.yearmon(dates$`unique(FSE_illegals$variable)`))
total_illegals = aggregate(value~variable, FUN = sum, data = FSE_illegals)
colnames(total_illegals)[2] <- 'total_fse'
FSE_illegals = merge(FSE_illegals, total_illegals, by = 'variable')
rm(total_illegals)
FSE_illegals$Route <- factor(FSE_illegals$Route, levels=c("Eastern Borders Route", 
                                                          "Circular Route from Albania to Greece",
                                                          "Black Sea Route",
                                                          "Western Balkan Route",
                                                          "Eastern Mediterranean Route",
                                                          "Central Mediterranean Route",
                                                          "Other"
                                                          ))

#Spot Migrants from Central Asia
c_asia_illegals = subset(FSE_illegals, Nationality %in% c
                              ("Kazakhstan",
                                "Kyrgyzstan",
                                "Tajikistan",
                                "Uzbekistan",
                                "Turkmenistan"))


ggplot(c_asia_illegals, aes(x = variable, y = value, group = Nationality)) + 
  geom_line(aes(color = Nationality)) + facet_wrap(~Route, ncol = 2) + picci + 
  scale_color_brewer(palette = "Set2") + labs(title = "Who cares about Central Asia?",
                                              subtitle = "Monthly recorded illegal migrants",
                                              caption = "SOURCE: Frontex", 
                                              x = "", y = "Number of migrants")
ggsave('illegal_central_asia.png', width = 20, height = 16, units = 'cm')

#Spot Migrants from Other FSE
NOT_asia_illegals = subset(FSE_illegals, !(Nationality %in% c
                         ("Kazakhstan",
                           "Kyrgyzstan",
                           "Tajikistan",
                           "Uzbekistan",
                           "Turkmenistan")))

ggplot(NOT_asia_illegals, aes(x = variable, y = value, group = Nationality)) + 
  geom_line(aes(color = Nationality)) + facet_wrap(~Route, ncol = 2) + picci + 
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set2") + labs(title = "Illegals from other ex-USSR countries",
                                              subtitle = "Monthly recorded illegal migrants",
                                              caption = "SOURCE: Frontex", 
                                              x = "", y = "Number of migrants")

ggsave('illegal_NOT_asia.png', width = 20, height = 16, units = 'cm')
#Share per route and nationality


FSE_illegals$share = FSE_illegals$value/FSE_illegals$total_fse 

p = ggplot(FSE_illegals, aes(y = Route, x = share, label = Nationality)) + geom_point(
  size = 6, color = "black", fill = 'SteelBlue3', shape = 21, alpha = .65
)  + 
  transition_time(variable, ) +
  scale_y_discrete(limits=rev) +
  scale_x_continuous(labels = scales::percent) +
  geom_text_repel() + 
  labs(title = "Illegal FSE migrants to the EU",
       subtitle = "% of arrivals: {format(frame_time, '%B %Y')}",
       x = '', y ='', caption = "SOURCE: Frontex") + 
  picci_h_barplot + theme(plot.title.position = 'plot',
                          panel.grid.major.x = element_line(size = .5, 
                                                            color = "black")) +
  ease_aes('linear')

animate(p, nframes = 100, fps = 16, duration = 45, width = 16, height = 10, units = "cm",
        res = 100)


#Rentier Countries 
Personal_remittance <- read_csv("API_BX.TRF.PWKR.DT.GD.ZS_DS2_en_csv_v2_4151867.csv", 
                                skip = 3)
Personal_remittance = data.frame(Personal_remittance[1:3], Personal_remittance[54:65])
Personal_remittance = reshape2::melt(Personal_remittance)
Personal_remittance$variable = gsub("X", "", Personal_remittance$variable)
Personal_remittance$variable = ISOdate(Personal_remittance$variable, 1,1)
Personal_remittance = subset(Personal_remittance, Country.Name %in% c(
  "Russian Federation",
  "Kazakhstan",
  "Kyrgyz Republic",
  "Tajikistan",
  "Ukraine",
  "Uzbekistan",
  "Georgia",
  "Armenia",
  "Turkmenistan",
  "Azerbaijan",
  "Belarus",
  'Moldova'
))

Personal_remittance$Country.Name = gsub("Russian Federation", "Russia",
                                        Personal_remittance$Country.Name)

Personal_remittance$Country.Name = gsub("Kyrgyz Republic", "Kyrgyzstan",
                                        Personal_remittance$Country.Name)

Personal_remittance$share = (Personal_remittance$value/100)
Personal_remittance$share = round(Personal_remittance$share, 2)
ggplot(Personal_remittance, aes(variable, share, 0)) + geom_col(fill= 'SteelBlue3') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  facet_wrap(~Country.Name, ncol = 2) + picci + labs(title = "Remittances crucial for some countries",
                                                     subtitle = "% of personal remittance on national GDP",
                                                     caption = "SOURCE: World Bank",
                                                     x = "",
                                                     y = "% of GDP")

ggsave('remittance.png', width = 20, height = 20, units = 'cm')

#Democracy deterioration
`V-Dem-CY-Core-v12` <- readRDS("~/Documents/FSE migration/Country_Year_V-Dem_Core_R_v12/V-Dem-CY-Core-v12.rds")

`V-Dem-CY-Core-v12` <- `V-Dem-CY-Core-v12` %>% select(country_name, year, historical_date, v2x_libdem) %>%
  filter(year > 2008, country_name %in% FSE$nations)

ggplot(`V-Dem-CY-Core-v12`, aes(historical_date, v2x_libdem)) + geom_area(fill= 'SteelBlue3',
                                                                          color = 'SteelBlue3',
                                                                          alpha = .75) + 
  facet_wrap(~country_name, ncol = 2) + picci + labs(title = "Deteriorating democracy more than the economy?",
                                                     subtitle = "Level of democracy as measured by the V-Dem Institute",
                                                     caption = "SOURCE: V-Dem",
                                                     x = "",
                                                     y = "Level of liberal democracy")
ggsave('democracy.png', width = 20, height = 20, units = 'cm')

#Asylum seekers
migr_asyappctza_1_Data <- read_csv("migr_asyappctza_1_Data.csv")
migr_asyappctza_1_Data <- subset(migr_asyappctza_1_Data, CITIZEN %in% FSE$nations)
migr_asyappctza_1_Data$Value[migr_asyappctza_1_Data$Value == ':'] <- 0
migr_asyappctza_1_Data <- subset(migr_asyappctza_1_Data, !(GEO == "European Union - 27 countries (from 2020)"))
migr_asyappctza_1_Data$Value <- gsub(",", "", migr_asyappctza_1_Data$Value)
first_timer = migr_asyappctza_1_Data %>% filter(ASYL_APP == "First time applicant", TIME == 2021)
first_timer_total = aggregate(as.numeric(Value)~CITIZEN, FUN = sum, data = first_timer)
colnames(first_timer_total)[2] <- "Value"
first_timer_total_2 = migr_asyappctza_1_Data %>% filter(ASYL_APP == "First time applicant")
first_timer_total_2 = aggregate(as.numeric(Value)~CITIZEN+TIME, FUN = sum, data = first_timer_total_2)
colnames(first_timer_total_2)[3] <- "Value"
first_timer_total_2 = subset(first_timer_total_2, CITIZEN %in%
                               c("Georgia",
                                 "Ukraine",
                                 "Moldova",
                                 "Russia",
                                 "Belarus"))
first_timer_total_2$TIME <- ISOdate(first_timer_total_2$TIME, 1,1)

first_timer_total$lab = format(as.numeric(first_timer_total$Value), big.mark = ",", scientific = FALSE)

ggplot(first_timer_total, aes(x = Value, y =reorder(CITIZEN, Value))) + geom_col(
  fill = "SteelBlue3"
) + scale_x_continuous(limits = c(0,15000), expand = c(0,0)) +
  geom_text(aes(label = lab,
                x = Value, hjust = 0)) +
  picci_h_barplot + theme(axis.text.x = element_blank(),
                          axis.ticks.x = element_blank()) + labs(
                            x = '',
                            y = '',
                            title = "Georgians and Ukrainians applying for asylum the most",
                            subtitle = "Number of first-time asylum seekers in the EU (2021)",
                            caption = "SOURCE: Eurostat")
ggsave('Asylum.png', width = 20, height = 10, units = 'cm')

colnames(first_timer_total_2)[1] <- "Citizenship"
first_timer_total_2$lab = format(first_timer_total_2$Value, big.mark = ",", scientific = FALSE)
ggplot(first_timer_total_2, aes(x = TIME, y = Value, group = Citizenship), size = 2) + 
  scale_y_continuous(labels = scales::comma) +
  geom_line(aes(color = Citizenship)) + scale_color_brewer(palette = "Set2") + 
  geom_label_repel(aes(label = 
                         lab
                       ), max.overlaps = 1, alpha = .75,
                   suwe = .80) +
  picci + 
    labs(
    title = "Where do they come from?",
    subtitle = "First-time asylum applications by year and nationality",
    x = '',
    y = '',
    caption = "SOURCE: Eurostat"
  )

ggsave('Asylum_over_time.png', width = 20, height = 16, units = 'cm')
