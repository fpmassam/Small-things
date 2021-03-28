require(tidyverse)
require(wbstats)
library(readr)

#Get data on debt and COVID cases 
KEdk6qBp <- read_csv("KEdk6qBp.csv")
debt <- read_csv('read.csv')
debt_2020_2021 = 
  debt %>% select(
    Country, 
    `2019`,
    `2020`
  )

#Do a Cleveland dot-plot about debt and countries 
debt_2020_2021 = debt_2020_2021 %>% 
  mutate(difference = `2020` - `2019`)

sample_debt = debt_2020_2021 %>% filter(
  Country %in% c('Argentina','Brasil', 'France', 'Germany',
               'Greece', 'Italy', 'Japan', 'Norway', 'Spain', 
               'Sweden', 'United Kingdom', 'United States')
)

sample_debt = sample_debt %>% arrange(`2020`) %>%
  mutate(Country = factor(Country, levels = Country))


ggplot(sample_debt, aes(y = Country)) + 
  geom_segment(aes(y = Country, yend = Country, x = `2019`, xend = `2020`), color = 'black') +
  geom_point(aes(x = `2020`), size = 4, color = 'red') + 
  geom_point(aes(x = `2019`), size = 4, color = 'grey35') +
  ggrepel::geom_text_repel(aes(x = `2020`, label = paste0("2020:", ' ',`2020`, '%')), size = 3.5, max.overlaps = 1, nudge_x = 1, nudge_y = .2) +
  ggrepel::geom_text_repel(aes(x = `2019`, label = paste0("2019:",' ',`2019`, '%')), size = 3.5, max.overlaps = 1, nudge_x = -1, nudge_y = -0.2) + labs(
    title = 'A World war-like debt explosion', subtitle = 'Countries in damage control because of COVID', caption = 'SOURCE: IMF'
  ) + picci1 + theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 10, face = 'bold'),
            panel.grid.major.y = element_blank(),
            ) + guides() + ylab('') + xlab('Debt/GDP ratio')
ggsave('debt_.png', width = 20, height = 15, units = 'cm')


#Do a scatterplot with COVID total cases per 1M and changes in debt/GDP ratio
end2020 = subset(KEdk6qBp, year_week == "2020-53")
end2020 = end2020 %>% select(country, population, indicator, cumulative_count) %>% filter(indicator == 'cases') %>%
  mutate(`Cases per million` = round((cumulative_count/population)*100000, 2)) %>% select(country, `Cases per million`)


debt_cases = merge(debt_2020_2021, end2020, by.x = "Country" , by.y = 'country')

ggplot(debt_cases, aes(x = `Cases per million`/10, y = difference)) +  geom_smooth(method = 'lm', alpha = .07, color = 'red', fill = 'black') + geom_point(alpha = .7, color = 'grey30') +
  ggrepel::geom_text_repel(
  aes(label = Country),
max.overlaps = 5) + picci + labs(title = 'The more cases, the more debt (kind of...)', subtitle = 'Public debt dynamics and relative COVID-cases', 
                           caption = 'SOURCE: IMF, ECDC') + ylab('2020 debt/GDP change') + xlab('COVID cases per million (2020)')

ggsave('debt_cases.png', width = 20, height = 15, units = 'cm')

#Studi the relation between debt and GDP growth 
gdp_related_stuff = wb_search('gdp growth')
gdp_growt =  wb_data('NY.GDP.MKTP.KD.ZG')
gdp_growt = subset(gdp_growt, date > 2014)
gdp_growt[6:9] = NULL
gdp_growt = na.omit(gdp_growt)
gdp_growt = aggregate(`NY.GDP.MKTP.KD.ZG`~country, FUN = mean, data = gdp_growt)
gdp_growt$NY.GDP.MKTP.KD.ZG = round(gdp_growt$NY.GDP.MKTP.KD.ZG, 2)
colnames(gdp_growt)[2] = 'GDPgrowth5prev'
gdp_debt = merge(gdp_growt, debt_2020_2021, by.x  = 'country', by.y  = 'Country')

ggplot(gdp_debt, aes(y = 1, x = difference)) + geom_vline(xintercept = 0) + geom_point() + 
  geom_point(aes(x = mean(difference)), color = 'red') + geom_errorbarh(aes(xmi))


gdp_per_capita = wb_data('NY.GDP.PCAP.CD')
gdp_per_capita = subset(gdp_per_capita, date > 2014)
gdp_per_capita = gdp_per_capita[3:5] 
gdp_per_capita = na.omit(gdp_per_capita)
gdp_per_capita = aggregate(NY.GDP.PCAP.CD~country, FUN = mean, data = gdp_per_capita)
colnames(gdp_per_capita)[2] = 'GDPpc5y'
gdp_debt = merge(gdp_debt, gdp_per_capita, by = 'country')
gdp_dep_no_outlier = subset(gdp_debt, country != 'Equatorial Guinea')
ggplot(gdp_dep_no_outlier, aes(x = GDPgrowth5prev, y = `2020`)) + geom_point() + geom_smooth(method = 'lm')

#Study the analysis between debt and trade balance 
trade_balance = wb_data('NE.RSB.GNFS.ZS')
trade_balance = trade_balance[3:5]
trade_balance = na.omit(trade_balance)
trade_balance = subset(trade_balance, date > 2014)
trade_balance = aggregate(NE.RSB.GNFS.ZS~country, FUN = mean, 
                          data = trade_balance)
gdp_debt = merge(gdp_debt, trade_balance, by = 'country')
ggplot(gdp_debt, aes(x = NE.RSB.GNFS.ZS, y = difference, label = country)) + 
  geom_vline(xintercept = 0, color = 'red') +
  geom_point(alpha = .7, color = 'grey30') + ggrepel::geom_text_repel(max.overlaps = 13) + ylab('2020 debt/GDP change')+ xlab('% trade balance') + picci + 
  labs(title = 'Debt/GPD ratio and trade balance', subtitle = 'Data show a mixed picture', caption = 'SOURCE: IMF, World Bank') 
ggsave('debt_trade.png', width = 20, height = 15, units = 'cm')

#Relation between debt rise and previous situation
ggplot(gdp_debt, aes(x = `2019`, y = difference, label = country)) + geom_smooth(method = 'lm', color = 'red', alpha = .07, fill = 'black') +
  geom_point(alpha = .7, color = 'grey30') + ggrepel::geom_text_repel(max.overlaps = 8) + ylab('2020 debt/GDP change')+ xlab('Debt/GDP ratio (2019)') + picci + 
  labs(title = 'When it rains it pours', subtitle = 'Highly indebted countries piled up more debt in 2020', caption = 'SOURCE: IMF') 
ggsave('debt_debt.png', width = 20, height = 15, units = 'cm')

