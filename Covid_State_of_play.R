require(data.table)
require(tidyverse)
require(gganimate)
require(gifski)


#Covid state of play get the data 
iso_3 <- read_delim("iso_3.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
iso_3 = iso_3 %>% select(ISO3)
iso_3 = iso_3$ISO3


oid_data <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
oid_data = subset(oid_data, iso_code %in% iso_3)

smaller_dataset = oid_data %>% select(date, continent, location, iso_code,
                                      new_cases, total_cases, 
                                 new_deaths, total_deaths, 
                                 total_cases_per_million,
                                 total_deaths_per_million,
                                 human_development_index, 
                                 aged_70_older,
                                 aged_65_older)

#Data Integrity studying new deaths number  
resurrexit = smaller_dataset %>% select(date, continent, location, new_deaths) %>%
  filter(new_deaths < 0)

resurrexit_50 = subset(resurrexit, new_deaths < -50)
resurrexit_250 = subset(resurrexit, new_deaths < -200)
resurrexit_250$label = paste(resurrexit_250$date, '/n',
  paste0(resurrexit_250$location, ':'), resurrexit_250$new_deaths)
                             

ggplot(resurrexit_250, aes(y = reorder(location, -new_deaths), x = new_deaths)) + 
  geom_col(fill = 'SteelBlue3') + geom_text(aes(x = -10, label = paste(
    scales::comma(new_deaths), '|', date)),
  hjust = 1, family = 
    'Garamond') + 
  labs(title = 'Limited correction in the deaths count',
       subtitle = 'Negative new deaths, correction of more than 250 cases', 
       x= '',
       y = '', 
       caption = 'SOURCE: Our world in data') +
  scale_y_discrete(position = "right") +
  scale_x_continuous(expand = c(0,0)) +
  picci + 
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
      )
  
ggsave('corrections.png', width = 20, height = 8, units = 'cm')

resurrexit_summary = aggregate(new_deaths~location+continent, data = resurrexit, FUN = sum)

positive_falses = smaller_dataset %>% select(date, continent, location, new_cases) %>%
  filter(new_cases < 0)

positives_falses_summary = aggregate(new_cases~location+continent, data =  positive_falses, FUN = sum)



#Patterns of countries 
summary_end = smaller_dataset %>% filter(date == max(date))
summary_end_cases = summary_end %>% select(location, continent, total_cases_per_million, 
                           aged_70_older, human_development_index) %>% mutate(aged_70_older = aged_70_older/100)
summary_end_deaths = summary_end %>% select(location, continent, total_deaths_per_million, 
                                           aged_70_older, human_development_index)

require(ggrepel)

ggplot(summary_end_cases, aes(x = aged_70_older, y = total_cases_per_million)) +
  geom_point(fill = 'SteelBlue3', shape = 21, size = 4, ) + 
  geom_smooth(method = 'lm', se = FALSE, color = 'red', linetype = 'dashed') + 
  geom_text_repel(aes(label = location), max.overlaps = 2, 
                  family = 'Garamond') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  geom_label(aes(x = 0.15, y = 2.6e+05, label = 'Older countries/More cases'), vjust = 1, 
            alpha = .5, family = 'Garamond', face = 'bold') + 
  geom_label(aes(x = .05, y = -.15e+05, label = 'Younger countries/Fewer cases'), vjust = 1,
             alpha = .5, family = 'Garamond', face = 'bold') +
    labs(title = 'A virus for older nations',
         subtitle = '% of population 70 or older; Cases per 1 million people (total)', 
         x = '', y = '',
         caption = 'SOURCE: Our world in data') + scale_fill_brewer(
           palette = 'Set3' 
         ) + picci



ggsave('age_cases.png', width = 20, height = 16, units = 'cm')

summary_end_cases$
summary(lm(total_cases_per_million~aged_70_older, data = summary_end_cases))
summary(lm(total_cases_per_million~human_development_index, data = summary_end_cases))


who_has_less = subset(summary_end_cases, total_cases_per_million < 50000)
less_cases_table = as.data.frame(table(who_has_less$continent))
ggplot(less_cases_table, aes(y = reorder(Var1, Freq))) + geom_col(
  aes(x = Freq),
  fill = 'SteelBlue3') +
  scale_x_continuous(expand = c(0,0)) +
  geom_text(aes(x = 0.1, label = paste(paste0(Var1,':'), Freq)), 
            hjust = 0, family = 'Garamond') +
  picci + 
  theme(axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) + 
  labs(title = 'Not an African virus',
       subtitle = 'Countries with less than 50,000 cases per million', 
       caption = 'SOURCE: Our world in data', 
       x = '',
       y = '')
ggsave('country_few_cases.png', width = 20, height = 8, units = 'cm')

ggplot(summary_end_cases, aes(x = human_development_index, y = total_cases_per_million)) + 
  geom_point(aes(fill = continent), shape = 21, size = 4) + scale_y_continuous(labels = scales::comma) + 
  scale_fill_brewer(palette = 'Set3') + geom_smooth(color = 'red', linetype = 'dashed',
                                                    se = FALSE, method = 'lm') +
  labs(title = 'Human Development Indiex predicts cases',
       subtitle = 'HDI and total cases per million people',
       caption = 'SOURCE: Our world in data',
       x = '',
       y = '') + picci + theme(legend.title = element_blank())

ggsave('hdi_cases.png', width = 20, height = 16, units = 'cm')


#Testing policies and new cases 
testing_policy = oid_data %>% select(date,location, new_cases_per_million, tests_per_case)
is.na(testing_policy$tests_per_case) <-0
cases = testing_policy %>% select(
  date, location, new_cases_per_million
)

tests = testing_policy %>% select(
  date, location, tests_per_case
)

cases = split(cases,f = cases$location)
cases = lapply(cases, function(x){
  x = x %>% mutate(new_cases_roll_mean = zoo::rollmean(new_cases_per_million, fill = NA, align = 'right',
                                                       k = 20))
})

cases = do.call(rbind,cases)

tests = split(tests,f = cases$location)
tests = lapply(tests, function(x){
  x = x %>% mutate(tests_roll_mean = zoo::rollmean(tests_per_case, fill = NA, align = 'right',
                                                       k = 20))
})



tests = do.call(rbind, tests)
combined = merge(tests, cases, by = c('date', 'location'))
combined = subset(combined, date > '2021-11-01')
cases = subset(cases, date > '2021-11-01')
tests = subset(tests, date > '2021-11-01')
combined = combined %>%  select(date, location, new_cases_roll_mean, 
                                tests_roll_mean)

combined = reshape2::melt(combined, id.vars = c('date', 'location'))
combined$name[combined$variable == 'new_cases_roll_mean'] <- 'New cases per million (20-days moving average)'
combined$name[combined$variable == 'tests_roll_mean'] <- 'Tests per case (20-days moving average)'
selection = c('United Arab Emirates',  'United Kingdom', 
              'Slovenia', 'Taiwan', 'Italy')

combined_select = subset(combined, location %in% selection)

ggplot(combined, aes(x = date, y = value, group = location)) + geom_line(
  alpha = .5, color = 'grey') + scale_y_continuous(labels = scales::comma) +
  geom_line(data = combined_select, aes(x = date, y = value, group = location, 
                                        color = location), size = 1) + scale_color_brewer(palette = 'Set2') +
  facet_wrap(~name, nrow = 2, scales = "free_y") + picci + 
  theme(legend.title = element_blank()) +
  labs(
    title = 'Testing policies, compared', 
    subtitle = 'New cases and tests since November 1', 
    caption = 'SOURCE: Our world in data', 
    x = '', 
    y = ''
  )

ggsave('testing_policies_ts.png', width = 20, height = 16, units = 'cm')


#Vaccination 

vacc_policy = oid_data %>% select(date, location, continent, new_cases_per_million, new_deaths_per_million, 
                    people_fully_vaccinated_per_hundred, total_boosters_per_hundred)

vacc_policy =  subset(vacc_policy, date > '2021-12-01')

require(gganimate)
require(gifski)

vacc_policy = subset(vacc_policy, date > '2021-01-31' & new_deaths_per_million > 0)

ggplot(vacc_policy, aes(x= people_fully_vaccinated_per_hundred/100, y = new_deaths_per_million)) + 
  geom_point(aes(fill= continent), 
             shape = 21, size = 4) + 
  scale_fill_brewer(palette = 'Set3') +
  picci + 
  theme(legend.title = element_blank()) +
  labs(
    title = {'Vaccines save lives | Date: {frame_time}'},
    subtitle =  'New cases per million; share of vaccinated ', 
      x = '', 
      y = '', 
      caption = 'SOURCE: Our world in data'
  ) +
  transition_time(vacc_policy$date) + ease_aes('linear') + enter_fade()

anim_save('vaccines_save_lifes.gif', width = 20, height = 16, units ='cm')
