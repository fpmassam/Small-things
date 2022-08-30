library(readr)
library(tidyverse)
library(ggrepel)

#Collect data  on US and UK  defence spending from:
#https://www.usgovernmentspending.com/spending_chart_1900_2022USp_XXs2li011tcn_30f_Defense_Spending_in_20th_Century
#https://www.ukpublicspending.co.uk/spending_chart_1700_2022UKp_XXc1li011tcn_30t_UK_Defence_Spending_Since_1700#copypaste
#Copy and paste them in a text file within RStudio and import it from here:


US_Spending <- read_delim("US Spending", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)
US_Spending <- subset(US_Spending, Year > 1899 & Year < 1946)
US_Spending$County = "United States"
View(US_Spending)
US_Spending = US_Spending %>% select(Year, Inflation, `Defense - Federal percent GDP`, County)
colnames(US_Spending) = c("year", "inflation", "defence", "country")
US_Spending$WW_status[US_Spending$year %in% c(1917, 1918)] <- "Participating in World War I"
US_Spending$WW_status[US_Spending$year %in% seq(1941, 1945, 1)] <- "Participating in World War II"
US_Spending$WW_status[is.na(US_Spending$WW_status)] <- "Not involved in World Wars"


UK_Spending <- read_delim("UK_spending.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)
UK_Spending <- subset(UK_Spending, Year > 1899 & Year < 1946)
UK_Spending$Country = "United Kingdom"
UK_Spending = UK_Spending %>% select(Year, Inflation, `Defence-total percent GDP`, Country)
colnames(UK_Spending) = c("year", "inflation", "defence", "country")
UK_Spending$WW_status[UK_Spending$year %in% seq(1915, 1918, 1)] <- "Participating in World War I"
UK_Spending$WW_status[UK_Spending$year %in% seq(1939, 1945, 1)] <- "Participating in World War II"
UK_Spending$WW_status[is.na(UK_Spending$WW_status)] <- "Not involved in World Wars"


defence = rbind(US_Spending, UK_Spending)

#Write a dataframe with the most important dates (wwi, wwii)

dates_ww = data.frame(date = c(1915, 1917, 1918, 1939, 1941, 1945), 
                      event = c("Start of WWI", "The U.S. enter WWI",
                                "End of WWI","Start of WWII",
                                "The U.S. enter WWII", "End of WWII"),
                      country = "UK")

#Chart about defense spending in the US and the UK (WWI and WWII)
ggplot(defence, aes(x = year, y = defence, group = country)) + 
  geom_col(aes(fill = WW_status)) + 
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = 4, linetype = "dashed") + 
  geom_text(aes(x = 1910, y = 5.5, label = "4% threshold")) + 
  scale_y_continuous(limits = c(0,50), labels = scales::label_number(suffix = '%')) +
  facet_wrap(~country, nrow = 2) +
  labs(title = "Defense budgets for peace?",
       subtitle = "Defense spending (1900-1945, % national GDP)", 
       y = "% of GDP",
       x = '',
       caption = "SOURCE: usgovernmentspending.com, ukpublicspending.co.uk") + picci + 
  theme(legend.title = element_blank())
ggsave('def_budgets.png', width = 20, height = 20, units = 'cm')


#Chart about inflation
ggplot(defence, aes(x = year, y = inflation, group = country)) + 
  geom_col(aes(fill = WW_status)) + 
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::label_number(suffix = '%')) +
  facet_wrap(~country, nrow = 2)  +
  labs(title = "Is inflation a strong enough casus belli?",
       subtitle = "Inflation change (1900-1945, % change)", 
       y = "",
       x = '',
       caption = "SOURCE: usgovernmentspending.com, ukpublicspending.co.uk") + picci + 
  theme(legend.title = element_blank())

ggsave('inflation.png', width = 20, height = 20, units = 'cm')

#Democracy in leading countries, 1900-1920 // 1922-1945 (V-Dem dat)
V-Dem-CY-Core-v12` <- readRDS("~/Documents/Country_Year_V-Dem_Core_R_v12/V-Dem-CY-Core-v12.rds")

demo_average = aggregate(v2x_libdem~year, FUN = mean, data = `V-Dem-CY-Core-v12`)
colnames(demo_average)[2] <- "Demo_avg"

Demo_wwi = `V-Dem-CY-Core-v12` %>% select(year,
                                         country_name, 
                                         historical_date,
                                         v2x_libdem) %>%
  filter(year>1899 & year < 1921) %>% filter(
    country_name %in% c(
      "France", "Germany", "United Kingdom", "United States",
      "Turkey", "Russia", "Austria"
    )
  )

Demo_wwi$WW_stqtus[Demo_wwi$year %in% seq(1914, 1918, 1)] <- "Participating in World War I"
Demo_wwi_it = `V-Dem-CY-Core-v12` %>% select(year,
                                          country_name, 
                                          historical_date,
                                          v2x_libdem) %>%
  filter(year>1899 & year < 1921) %>% filter(
    country_name == "Italy"
    )
Demo_wwi_it$WW_stqtus[Demo_wwi_it$year %in% seq(1915, 1918, 1)] <- "Participating in World War I"

Demo_wwi_us = `V-Dem-CY-Core-v12` %>% select(year,
                                             country_name, 
                                             historical_date,
                                             v2x_libdem) %>%
  filter(year>1899 & year < 1921) %>% filter(
    country_name == "United States of America"
  )
Demo_wwi_us$WW_stqtus[Demo_wwi_us$year %in% seq(1917, 1918, 1)] <- "Participating in World War I"
Demo_wwi = rbind(Demo_wwi, rbind(
  Demo_wwi_it, Demo_wwi_us
))
Demo_wwi$WW_stqtus[is.na(Demo_wwi$WW_stqtus)] <- "Not involved in World Wars"


Demo_wwi = merge(Demo_wwi, demo_average)
Demo_wwi$country_name = gsub("Turkey", "Ottoman Empire", Demo_wwi$country_name)

ggplot(Demo_wwi, aes(x = historical_date, y = v2x_libdem)) +
  geom_col(aes(group = country_name, fill = WW_stqtus)) + 
  scale_fill_brewer(palette = "Set1") + 
  facet_wrap(~country_name, ncol = 2) +
  labs(
    title = "Riding the first wave of democratization?",
    subtitle = "Liberal democracy before and after World War I",
    x ="", y = "",
    caption = "SOURCE: V-Dem (V12) dataset") +
  picci + 
  theme(legend.title = element_blank())
ggsave('demo_wwi.png', width = 20, height = 20, units = 'cm')



Demo_wwii = `V-Dem-CY-Core-v12` %>% select(year,
                                          country_name, 
                                          historical_date,
                                          v2x_libdem) %>%
  filter(year>1922 & year < 1951) %>% filter(
    country_name %in% c(
      "France", "Germany", "United Kingdom")
  )
Demo_wwii$WW_stqtus[Demo_wwii$year %in% seq(1939, 1945, 1)] <- "Participating in World War II"
Demo_wwii_us = `V-Dem-CY-Core-v12` %>% select(year,
                                            country_name, 
                                            historical_date,
                                            v2x_libdem) %>%
  filter(year>1922 & year < 1951) %>% filter(
    country_name %in% c(
      "United States of America", "Japan")
  )
Demo_wwii_us$WW_stqtus[Demo_wwii_us$year %in% seq(1941, 1945, 1)] <- "Participating in World War II"

Demo_wwii_it = `V-Dem-CY-Core-v12` %>% select(year,
                                              country_name, 
                                              historical_date,
                                              v2x_libdem) %>%
  filter(year>1922 & year < 1951) %>% filter(
    country_name %in% c(
      "Italy", "Russia")
  )
Demo_wwii_it$WW_stqtus[Demo_wwii_it$year %in% seq(1940, 1945, 1)] <- "Participating in World War II"

Demo_wwii_cn = `V-Dem-CY-Core-v12` %>% select(year,
                                              country_name, 
                                              historical_date,
                                              v2x_libdem) %>%
  filter(year>1922 & year < 1951) %>% filter(
    country_name == "China"
  )

Demo_wwii_cn$WW_stqtus[Demo_wwii_cn$year %in% seq(1941, 1945, 1)] <- "Participating in World War II"
Demo_wwii_cn$WW_stqtus[Demo_wwii_cn$year %in% seq(1937, 1940, 1)] <- "Pre-WWII conflict with Japan"



Demo_wwii = rbind(Demo_wwii, rbind(Demo_wwii_it, rbind(Demo_wwii_us, Demo_wwii_cn)))
Demo_wwii$WW_stqtus[is.na(Demo_wwii$WW_stqtus)] <- "Not involved in World Wars"


    
Demo_wwii = merge(Demo_wwii, demo_average)
Demo_wwii$country_name = gsub("Russia", "Soviet Union", Demo_wwii$country_name)

ggplot(Demo_wwii, aes(x = historical_date, y = v2x_libdem)) + 
  geom_col(aes(group = country_name, fill = WW_stqtus)) + 
  facet_wrap(~country_name, ncol = 2) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Democracy collapsing in the inter-war period",
    subtitle = "Liberal democracy before and after World War II",
    x ="", y = "", 
    caption = "SOURCE: V-Dem (V12) dataset") +
  picci + 
  theme(legend.title = element_blank())

ggsave('demo_wwii.png', width = 20, height = 20, units = 'cm')

#Military expenditure as share of GDP since 2020

library(readxl)
SIPRI_Milex_data_2000_2021_1_ <- read_excel("SIPRI-Milex-data-2000-2021 (1).xlsx", 
                                            sheet = "Share of GDP")

SIPRI_Milex_data_2000_2021_1_ = na.omit(SIPRI_Milex_data_2000_2021_1_)
SIPRI_Milex_data_2000_2021_1_ =reshape2::melt(SIPRI_Milex_data_2000_2021_1_)
colnames(SIPRI_Milex_data_2000_2021_1_) = c('Country', "year", "share_gdp")
SIPRI_Milex_data_2000_2021_1_$year = as.numeric(as.character(SIPRI_Milex_data_2000_2021_1_$year))

ggplot(SIPRI_Milex_data_2000_2021_1_, aes(x =  year, y = share_gdp)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_area(fill = "SteelBlue3", color = "SteelBlue3", alpha = .7) +
  labs(title = "A politically sensitive arms race",
       subtitle = "Military expenditures as % of GDP (2000-2021)",
       caption = "SOURCE: SIPRI",
       x = "",
       y = ""
       ) +
  facet_wrap(~Country, ncol = 2) + picci
ggsave('contemporary_gdp.png', width = 20, height = 20, units = 'cm')  


abs_spending <- read_excel("SIPRI-Milex-data-2000-2021 (1).xlsx", 
                           sheet = "Constant (2020) USD")
abs_spending = na.omit(abs_spending)
abs_spending = reshape2::melt(abs_spending)
colnames(abs_spending) = c("country", "year", "m_dollars")
abs_spending$billion = abs_spending$m_dollars/1000
abs_spending$year = as.numeric(as.character(abs_spending$year))


ggplot(abs_spending, aes(x =  year, y = billion)) + 
  scale_y_continuous(labels = scales::dollar_format(suffix = "B")) +
  geom_area(fill = "SteelBlue3", color = "SteelBlue3", alpha = .7) +
  labs(title = "How much money for warfare?",
       subtitle = "Military expenditures in 2020 US$ (2000-2021)",
       caption = "SOURCE: SIPRI",
       x = "",
       y = ""
  ) +
  facet_wrap(~country, ncol = 2) + picci

ggsave('abs_exp.png', width = 20, height = 20, units = 'cm')  

#Democracy in 2000, 2021

democracy_20_years =  `V-Dem-CY-Core-v12` %>% select(year,
                               country_name, 
                               historical_date,
                               v2x_libdem) %>%
  filter(year>1999 & year < 2022) %>% filter(
    country_name %in% c("United States of America",
                        "Russia",
                        "China",
                        "France",
                        "United Kingdom",
                        "Japan",
                        "Germany",
                        "Italy"
  ))

ggplot(democracy_20_years, aes(x = historical_date, y = v2x_libdem)) +
  geom_area(fill = "SteelBlue3", color = "SteelBlue3", alpha = .7) +
  facet_wrap(~country_name, ncol = 2) +
  labs(
    title = "Democracy, non-democracies, and the rest",
    subtitle = "Liberal democracy in selected countries (2000-2021)",
    x ="", y = "",
    caption = "SOURCE: V-Dem (V12) dataset") +
  picci + 
  theme(legend.title = element_blank())
ggsave('demo_contemporary.png', width = 20, height = 20, units = 'cm')


#Is trade collapsing? Download data from https://comtrade.un.org/data and visualise here 
comtrade_2_ <- read_csv("comtrade (2).csv")
View(comtrade_2_)

require(zoo)
trade <- comtrade_2_ %>% select(`Period Desc.`, Period, Reporter, `Trade Flow`, `Trade Value (US$)`)
trade$date = lubridate::my(trade$`Period Desc.`)
trade$Billion = trade$`Trade Value (US$)`/1000000000

ggplot(trade, aes(x = date, y = Billion)) + 
  geom_area(color = "SteelBlue3",
            fill = "SteelBlue3",
            alpha = .7) + facet_grid(Reporter~`Trade Flow`) + picci +
  scale_y_continuous(labels = scales::dollar_format(suffix = "B")) +
  labs(title = 'Is global trade holding on after all?',
       subtitle = "Trade flows in billion US$ (2022)",
       x ="", y ="")

ggsave('trqde_2022.png', width = 20, height = 21, units = 'cm')








