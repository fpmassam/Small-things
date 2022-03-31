require(tidyverse)
library(readxl)

#Get WVS data
### Given the size of the dataset it is better to download the Excel files from the WVS database (even if it is a pain in the proverbial)
Democracy_People_choose_their_leaders_in_free_elections <- 
  read_excel("Democracy_People_choose_their_leaders_in_free_elections.xls", 
                                                                      +     skip = 6)
Importance_of_democracy_1_ <- read_excel("Importance_of_democracy (1).xls", 
                                         skip = 6)

HOMOLIB_Welzel_choice_1_Homosexuality_acceptane. <- read_excel("HOMOLIB-_Welzel_choice-1_Homosexuality_acceptanc. (1).xls", 
                                                               skip = 6)

The_only_acceptable_religion_is_my_religion <- read_excel("The_only_acceptable_religion__is_my_religion.xls", 
                                                          skip = 6)


Political_system_Having_a_democratic_political_system_ts <- read_excel("Political_system_Having_a_democratic_political_system_ts.xls")
View(Political_system_Having_a_democratic_political_system_ts)
Political_system_Having_a_democratic_political_system_ts  = 
  reshape2::melt(Political_system_Having_a_democratic_political_system_ts)
Political_system_Having_a_democratic_political_system_ts  = 
  Political_system_Having_a_democratic_political_system_ts %>%
  filter(Answer %in% c('Very good', 'Fairly good'))

citation = 'SOURCE: WVS-Integrated Values Surveys 1981-2022'

#Import the vdem dataset
V-Dem-CD-v12` <- readRDS("~/Documents/rstudio-export (1)/V-Dem-CD-v12.rds")

#Dempcracy satisfaction
Political_system_Having_a_democratic_political_system_ts %>%
  ggplot(aes(x = variable, y = value, label = paste0(value, '%'))) + 
  geom_col(aes(fill = Answer)) + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = 50, color = 'black', linetype = 'dashed') + 
  geom_label(position = position_stack(vjust = .5))  + 
  labs(title = "Getting there, democracy and its contradictions",
       subtitle = "Q. How good is having a democratic political system?",
       caption = citation, x = '', y = '') + scale_fill_brewer(palette = 'Set1') + 
  picci_h_barplot + facet_wrap(~Country, ncol = 1) 

ggsave('democracy_pattern.png', width = 20, height = 30, units = 'cm')


#Importance of democracy
require(forcats)
dem_imp = Importance_of_democracy_1_ %>% select(`0...1`, Poland, Russia, `United States`, Ukraine, Russia, Sweden) %>%
  filter(`0...1` == "Absolutely important")
dem_imp = reshape2::melt(dem_imp)

ggplot(dem_imp, aes(y = fct_reorder(variable, value))) + geom_col(aes(x = 100), fill = 'grey50', alpha = .7) +
  geom_col(aes(x = value), fill = 'SteelBlue3') + 
  geom_vline(xintercept = 50, color = 'black', linetype = 'dashed') + 
  scale_x_continuous(labels = function(x) paste0(x, "%")) + 
  geom_text(aes(x = 1, label = paste(paste0(variable,':'), paste0(value, '%')
                                     )), family = 'EB Garamond', 
            fontface = 'bold', hjust = 0) + picci_h_barplot + theme(axis.text.y = element_blank()) + 
  labs(title = "Demoracy more important in Kyiv than in Moscow",
       subtitle = 'Q: How important is democracy? A: Absolutely important (% of answers)',
       x = '',
       y = '',
       caption = citation)

ggsave('democracy_importance.png', width = 20, height = 10, units = 'cm')


#Homophobic tendencies 
homo_tolerance = HOMOLIB_Welzel_choice_1_Homosexuality_acceptane. %>% select(`0...1`, Poland, Russia, `United States`, Ukraine, Russia, Sweden) %>%
  filter(`0...1` == "Low")
homo_tolerance = reshape2::melt(homo_tolerance)

ggplot(homo_tolerance, aes(y = fct_reorder(variable, value))) + geom_col(aes(x = 100), fill = 'grey50', alpha = .7) +
  geom_col(aes(x = value), fill = 'SteelBlue3') + 
  geom_vline(xintercept = 50, color = 'black', linetype = 'dashed') + 
  scale_x_continuous(labels = function(x) paste0(x, "%")) + 
  geom_text(aes(x = 1, label = paste(paste0(variable,':'), paste0(value, '%')
  )), family = 'EB Garamond', 
  fontface = 'bold', hjust = 0) + picci_h_barplot + theme(axis.text.y = element_blank()) + 
  labs(title = "Homophobia in Eastern Europe?",
       subtitle = 'Q: Acceptance of homosexuality A: Low (% of answers)',
       x = '',
       y = '',
       caption = citation)

ggsave('homo_tolerance.png', width = 20, height = 10, units = 'cm')

#Reduce Vdem Dataset
reduce = `V-Dem-CD-v12` %>% select(country_name, year, v2xel_frefair, v2x_libdem,
                                   v2x_freexp_altinf, v2psoppaut_ord) %>%
filter(year > 1989) %>% filter(country_name %in% c("Sweden",
                                                   "Ukraine",
                                                   "Russia",
                                                   "Poland",
                                                   "United States of America"))
reduce = na.omit(reduce)
reduce$country_name = gsub('United States of America', 'United States', reduce$country_name)
reduce$`Opposition parties autonomy`[reduce$v2psoppaut_ord == 0] <- 'Opposition parties are not allowed'
reduce$`Opposition parties autonomy`[reduce$v2psoppaut_ord == 1] <- 'There are no autonomous, independent opposition parties'
reduce$`Opposition parties autonomy`[reduce$v2psoppaut_ord == 2] <- 'At least some opposition parties are autonomous and independent'
reduce$`Opposition parties autonomy`[reduce$v2psoppaut_ord == 3] <- 'Most significant opposition parties are autonomous and independent'
reduce$`Opposition parties autonomy`[reduce$v2psoppaut_ord == 4] <- 'All opposition parties are autonomous and independent'



#Free elections

ggplot(reduce, aes(x = year, y = v2xel_frefair)) + geom_area(fill = 'SteelBlue3',
                                                             color = 'SteelBlue3',
                                                             alpha = .85) + 
  scale_x_continuous(expand = c(0,0)) +
  picci + labs(title = 'Elections were improving in Ukraine',
               subtitle = "1 = the ideal of free and fair elections is respected",
               caption = "SOURCE: V-Dem - V12 dataset",
               x = '', y = '') + facet_wrap(~ country_name, ncol = 1)

ggsave('free_elections.png', width = 20, height = 30, units = 'cm')


#Opposition parties
ggplot(reduce, aes(x = year, y = country_name)) + 
  geom_tile(aes(fill = fct_reorder(`Opposition parties autonomy`,
                                   -v2psoppaut_ord)
  ), color = 'black') + scale_fill_brewer(palette = 'Set3') +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = 'Give Ukraine a chance',
       subtitle = 'Independence of opposition parties',
       caption = "SOURCE: V-Dem - V12 dataset",
       x = '', y = '') +
  picci_h_barplot + theme(legend.title = element_blank(),
                          legend.direction = 'vertical',
                          axis.text.y = element_text(size = 11))
ggsave('free_opposition.png', width = 20, height = 16, units = 'cm')

