library(readr)
library(tidyverse)
require(eurostat)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
require(ggfx)
require(shadowtext)
library(readxl)
library(ggbeeswarm)

# Build a dataset with U.S. GDP and oil product supplies 
GDP <- read_csv("GDP.csv", col_types = cols(DATE = col_date(format = "%Y-%m-%d")))
library(readr)
Supply_US <- read_csv("U.S._Product_Supplied_of_Crude_Oil_and_Petroleum_Products.csv", 
                                                                     skip = 4)
Supply_US_2000 = subset(Supply_US, Year > 1999)
View(U_S_Product_Supplied_of_Crude_Oil_and_Petroleum_Products)


gdp_2000 <- subset(GDP, DATE > '1999-12-01')

gdp_2000$norm_gdp <- (gdp_2000$GDP / gdp_2000$GDP[1])*100

Supply_US_2000 = subset(Supply_US, Year > 1999)
Supply_US_2000$Year <- as.Date(paste(Supply_US_2000$Year, "-01-01", sep=""))
Supply_US_2000$Norm_oil <- (Supply_US_2000$`U.S. Product Supplied of Crude Oil and Petroleum Products Thousand Barrels`/ 
                              Supply_US_2000$`U.S. Product Supplied of Crude Oil and Petroleum Products Thousand Barrels`[[23]])*100


colnames(Supply_US_2000)[1] <- "DATE"

combined_oil_gdp <- merge(gdp_2000, Supply_US_2000, by = "DATE")
combined_oil_gdp = combined_oil_gdp %>% select(DATE, norm_gdp, Norm_oil)
colnames(combined_oil_gdp) = c("Date", "GDP", "Supply of oil and petroleum products")

reference_points = data.frame(Date = c(as.Date("2001-01-01"), 
                                       as.Date("2008-10-01"), 
                                       as.Date("2020-01-01"),
                                       as.Date("2022-01-01")),
                              label = c("September 11", "The 2008 financial crisis", "COVID-19",
                                        "Russia invades Ukraine"))
     
                              



ggplot(combined_oil_gdp, aes(x = Date)) +
  geom_line(aes(y = GDP, color = "GDP"), size = 1) +
  geom_line(aes(y = `Supply of oil and petroleum products`, color = "Supply of oil and petroleum products"), 
            size = 1) + 
  geom_ribbon(aes(ymin = GDP, ymax = `Supply of oil and petroleum products`), alpha = 0.1, 
              fill = 'SteelBlue3') +
  geom_vline(data = reference_points, aes(xintercept = Date), linetype = "dashed", size = .2) +
  geom_shadowtext(data = reference_points, aes(x = Date, y = 155, label = label), 
            size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond', bg.colour='white') +
  geom_text(data = reference_points, aes(x = Date, y = 155, label = label), 
                  size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond') +
  labs(title = "The U.S. economy boomed but oil supply didn't",
       subtitle = "GDP and supply of oil products (100 = year 2000)",
       x = "", 
       y = "", 
       caption = "SOURCE: DaNumbers calculations on EIA, FRED data") + 
  scale_color_brewer(palette = "Set1") + picci() + theme(legend.title = element_blank())
                                              

ggsave('oil_gdp.png', width = 16, height = 12, units = "cm", dpi = 300)

#Opec OIL demand
T47 <- read_excel("T47.xlsx", na = "na", 
                  +     skip = 2, n_max = 76)
Oil_demand_opex = T47
Oil_demand_opex = data.frame(geo_entities = Oil_demand_opex$...1, 
                             Oil_demand_opex[42:64])


Oil_opec_chart = data.frame(Oil_demand_opex[1], as.data.frame(lapply(Oil_demand_opex[2:24], function(x) x / Oil_demand_opex[2]*100)))
colnames(Oil_opec_chart)[2:24] = seq(2000, 2022, 1)
Oil_opec_chart = Oil_opec_chart %>% gather(key = "Year", value = "Oil_demand", -geo_entities)
Oil_opec_chart$Year = as.Date(paste(Oil_opec_chart$Year, "-01-01", sep=""))
Oil_opec_chart = subset(Oil_opec_chart, !(geo_entities == 'Qatar')) 
Oil_opec_chart_details = Oil_opec_chart %>% filter(geo_entities %in% c
                                                   ("Russia", "Germany", "Japan", 
                                                     "Brazil", "China", "India"
                                                     ))
 
 

ggplot(Oil_opec_chart_details, aes(x = Year, y = Oil_demand)) + 
  geom_line(data = , aes(x = Year, y = Oil_demand, group = geo_entities, 
                                               color = geo_entities), size = .75) +
  geom_vline(data = reference_points, aes(xintercept = Date), linetype = "dashed", size = .2) +
  geom_shadowtext(data = reference_points, aes(x = Date, y = 155, label = label), 
                  size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond', bg.colour='white') +
  geom_text(data = reference_points, aes(x = Date, y = 155, label = label), 
            size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond') +
  scale_color_brewer(palette = "Dark2") +
  picci() + theme(legend.title = element_blank()) + labs(title = "Developing economies are driving oil demand",
                                                         subtitle = "Oil demand (100 = year 2000)",
                                                         x = "", 
                                                         y = "", 
                                                         caption = "SOURCE: DaNumbers calculations on OPEC data")


ggsave('oil_OPEC.png', width = 16, height = 12, units = "cm", dpi = 300)



#Number of events per year by type of violence
GEDEvent_v23_1 <- readRDS("~/Documents/Oil Conspiracy Theory/GEDEvent_v23_1.rds")

type_of_violence =as.data.frame(
  table(GEDEvent_v23_1$type_of_violence, GEDEvent_v23_1 $year))
colnames(type_of_violence) = c("type_of_violence", "year", "count")
type_of_violence$type_nat_lan[type_of_violence$type_of_violence == 1] = "State-based conflict"
type_of_violence$type_nat_lan[type_of_violence$type_of_violence == 2] = "Non-state conflict"
type_of_violence$type_nat_lan[type_of_violence$type_of_violence == 3] = "One-sided violence"
type_of_violence$year = as.numeric(as.character(type_of_violence$year))
type_of_violence = subset(type_of_violence, year > 1999)
type_of_violence$year = as.Date(paste(type_of_violence$year, "-01-01", sep=""))


ggplot(type_of_violence) +
  geom_col(stat = "identity", position = "stack", 
           aes(x = year, y = count, fill = type_nat_lan)) +
  geom_vline(data = reference_points, aes(xintercept = Date), linetype = "dashed", size = .2) +
  geom_shadowtext(data = reference_points, aes(x = Date, y = 11000, label = label), 
                  size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond', bg.colour='white') +
  geom_text(data = reference_points, aes(x = Date, y = 11000, label = label), 
            size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond') +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::comma) +
  picci() + theme(legend.title = element_blank()) + labs(title = "States are waging wars - again",
                                                         subtitle = "Number of violent events per year",
                                                         x = "", 
                                                         y = "Number of events", 
                                                         caption = "SOURCE: DaNumbers calculations on UCDP data")


ggsave('war_on_the_rocks.png', width = 16, height = 12, units = "cm", dpi = 300)


#Where conflicts unfold 

world <- map_data("world")

GEDEvent_v23_1 <- readRDS("~/Documents/Oil Conspiracy Theory/GEDEvent_v23_1.rds")
GEDEvent_v23_1 = subset(GEDEvent_v23_1, year > 2000)

GEDEvent_v23_1$type_nat_lan[GEDEvent_v23_1$type_of_violence == 1] = "State-based conflict"
GEDEvent_v23_1$type_nat_lan[GEDEvent_v23_1$type_of_violence == 2] = "Non-state conflict"
GEDEvent_v23_1$type_nat_lan[GEDEvent_v23_1$type_of_violence == 3] = "One-sided violence"
GEDEvent_v23_1$bracket[GEDEvent_v23_1$year < 2006] = "2000-2005"
GEDEvent_v23_1$bracket[GEDEvent_v23_1$year > 2005 & GEDEvent_v23_1$year < 2011] = "2006-2010"
GEDEvent_v23_1$bracket[GEDEvent_v23_1$year > 2010 & GEDEvent_v23_1$year < 2016] = "2011-2015"
GEDEvent_v23_1$bracket[GEDEvent_v23_1$year > 2015 & GEDEvent_v23_1$year < 2021] = "2016-2020"
GEDEvent_v23_1$bracket[GEDEvent_v23_1$year > 2020 & GEDEvent_v23_1$year < 2023] = "2020-2022"

ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), 
               fill = 'SteelBlue3', alpha = .5) + 
geom_point(data = GEDEvent_v23_1, aes(y = latitude, 
                           x = longitude, color = type_nat_lan), 
                                                        size = 1) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~bracket, ncol = 1) + picci() + theme(legend.title = element_blank(), 
                                      panel.grid.major.y = element_blank(),
                                      axis.text.y = element_blank(),
                                      axis.text.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      axis.title.x = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.ticks = element_blank(),
                                      legend.position = 'top') + 
  labs(title = 'The world is falling apart',
       subtitle = 'One point = one event',
       caption = 'SOURCE: DaNumbers calculations on UCDP data')
  

ggsave('world_is_falling_apart.png', width = 16, height = 34, units = "cm", dpi = 300)


#Data on energy consumption -- US 

#https://www.eia.gov/totalenergy/data/browser/?tbl=T01.01#/?f=A&start=1949&end=2019&charted=0-1-2-3-4-5-6-7-8-9-10-11-12-13-14-15&maptype=0&linechart=ELEC.GEN.ALL-US-99.A~PETC.PA1_2.A~PETC.PA2_2.A~PETC.PA3_2.A~PETC.PA4_2.A~PETC.PA5_2.A~PETC.PA6_2.A~PETC.PA7_2.A~PETC.PA8_2.A~PETC.PA9_2.A~PETC.PA10_2.A~PETC.PA11_2.A~PETC.PA12_2.A~PETC.PA13_2.A~PETC.PA14_2.A~PETC.PA15_2.A~PETC.PA16_2.A~PETC.PA17_2.A~PETC.PA18_2.A~PETC.PA19_2.A~PETC.PA20_2.A~PETC.PA21_2.A~PETC.PA22_2.A~PETC.PA23_2.A~PETC.PA24_2.A~PETC.PA25_2.A~PETC.PA26_2.A~PETC.PA27_2.A~PETC.PA28_2.A~PETC.PA29_2.A~PETC.PA30_2.A~PETC.PA31_2.A~PETC.PA32_2.A~PETC.PA33_2.A~PETC.PA34_2.A~PETC.PA35_2.A~PETC.PA36_2.A~PETC.PA37_2.A~PETC.PA38_2.A~PETC.PA39_2.A~PETC.PA40_2.A~PETC.PA41_2.A~PETC.PA42_2.A~PETC.PA43_2.A~PETC.PA44_2.A~PETC.PA45_2.A~PETC.PA46_2.A~PET


energy_production_US <- read_excel("Table_7.2a_Electricity_Net_Generation__Total_(All_Sectors).xlsx", 
              sheet = "Annual Data", na = "Not Available", 
              skip = 10)
View(Table_7_2a_Electricity_Net_Generation__Total__All_Sectors_)


prod_chart = energy_production_US %>% select(
  `Annual Total`,
  `Electricity Net Generation Total (including from sources not shown), All Sectors`,
  `Electricity Net Generation From Petroleum, All Sectors`,
  `Electricity Net Generation From Coal, All Sectors`,
  `Electricity Net Generation From Natural Gas, All Sectors`,
  `Electricity Net Generation From Nuclear Electric Power, All Sectors`,
  `Electricity Net Generation From Solar, All Sectors`,
  `Electricity Net Generation From Wind, All Sectors`,
  `Electricity Net Generation From Conventional Hydroelectric Power, All Sectors`)

prod_chart = reshape2::melt(prod_chart, id.vars = "Annual Total",
                            stringsAsFactors = FALSE)

prod_chart = subset(prod_chart, `Annual Total`> 1999)
prod_chart$labs = as.character(prod_chart$variable)
prod_chart$labs[as.character(prod_chart$labs) == "Electricity Net Generation Total (including from sources not shown), All Sectors"] <- "Total"
prod_chart$labs[as.character(prod_chart$labs) == "Electricity Net Generation From Petroleum, All Sectors"] <- "Oil"
prod_chart$labs[as.character(prod_chart$labs) == "Electricity Net Generation From Coal, All Sectors"] <- "Coal"
prod_chart$labs[as.character(prod_chart$labs) == "Electricity Net Generation From Natural Gas, All Sectors"] <- "Natural Gas"
prod_chart$labs[as.character(prod_chart$labs) == "Electricity Net Generation From Nuclear Electric Power, All Sectors"] <- "Nuclear Power"
prod_chart$labs[as.character(prod_chart$labs) == "Electricity Net Generation From Solar, All Sectors"] <- "Solar"
prod_chart$labs[as.character(prod_chart$labs) == "Electricity Net Generation From Wind, All Sectors"] <- "Wind"
prod_chart$labs[as.character(prod_chart$labs) ==  "Electricity Net Generation From Conventional Hydroelectric Power, All Sectors"] <- "Hydro"

labs = as.factor(prod_chart$labs)
prod_chart$labs = factor(labs, levels = c("Total", "Oil", "Coal", "Natural Gas", "Nuclear Power", "Solar", "Wind", "Hydro"))

prod_chart$value = prod_chart$value / 1000
prod_chart = subset(prod_chart, prod_chart$`Annual Total` >= 2000)
prod_chart$`Annual Total` = as.Date(paste0(prod_chart$`Annual Total`, "-01-01", sep=""))
prod_chart_no_total = subset(prod_chart, prod_chart$labs != "Total")


ggplot(prod_chart_no_total, aes(x =`Annual Total`, y = value)) +
  geom_area(aes(color = labs, 
                fill = labs), alpha = .7, position = 'stack') +
  geom_vline(data = reference_points, aes(xintercept = Date), linetype = "dashed", size = .2) +
  geom_text(data = reference_points, aes(x = Date, y = 1000, label = label), 
            size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond') +
  scale_y_continuous(labels = scales::label_number(
    suffix = " TWh", big.mark = ',')) +
  scale_fill_brewer(palette = 'Set3') +
  scale_color_brewer(palette = 'Set3') + labs(x = "", y = "", title = 'Fossil fuel still producing electricity',
                                              subtitle = "Energy production in Trillion-Watt hour",
                                              caption = "Source: U.S. Energy Information Administration") +
  picci() + theme(legend.title = element_blank())

ggsave('electricity_production+history.png', width = 16, height = 12, units = "cm", dpi = 300)

share = subset(prod_chart, `Annual Total` == max(`Annual Total`))

total = share$value[1]
share = subset(share, labs != "Total")
share$Share = share$value / total
other = data.frame(`Annual Total` = unique(share$`Annual Total`), 
           variable = '', value = total - sum(share$value), labs = 'Other',
           Share = 1-sum(share$Share))
colnames(other) = colnames(share)
share =  rbind(share, other)


ggplot(share, aes(x = reorder(labs, Share), y = Share, fill = labs)) +
  geom_col(aes(y = 1, fill = labs), alpha = .25) +
  geom_col() + picci() +
  scale_fill_brewer(palette = 'Set3') +
  labs(title = "Fossil fuel still producing electricity",
         subtitle = "Share of energy production in U.S., total = 4,230 TWh",
         caption = "Source: U.S. Energy Information Administration (2022)", 
         y = '', x = '') +
  geom_text(aes(label = paste(paste0(labs, ":"), scales::percent(Share))),
            size = 12, family = "EB Garamond", hjust = -.1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_flip() + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
                       legend.position = "none", legend.title = element_blank(), 
                       panel.grid.major.y = element_blank())

ggsave('energy_share.png', width = 16, height = 10, units = "cm", dpi = 300)



#Counterfactual: but also the West...


Absolute_OIL_Demand = data.frame(T47[1], (T47[42:64]))
colnames(Absolute_OIL_Demand) = c("Country", "2000", "2001", "2002", "2003", "2004", "2005", "2006", 
                                  "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
                                  "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")

Absolute_OIL_Demand_countries = Absolute_OIL_Demand %>% filter(Country %in% c
                                                   ("Russia", "Germany", "Japan", 
                                                     "Brazil", "China", "India", "United States",
                                                   'Italy'))
total  = Absolute_OIL_Demand %>% 
  filter(Country == "Total world")
total = reshape2::melt(total, id.vars = "Country", stringsAsFactors = FALSE)
total = total[2:3]

vertical_plot = 
  reshape2::melt(Absolute_OIL_Demand_countries, 
                 id.vars = "Country", stringsAsFactors = FALSE) 

vertical_plot = merge(vertical_plot, total, by = "variable", all.x = TRUE)
vertical_plot = vertical_plot[order(vertical_plot$variable),]
vertical_plot$variable = as.Date(paste0(vertical_plot$variable, "-01-01", sep=""))
vertical_plot$Share = vertical_plot$value.x / vertical_plot$value.y
total$variable = as.Date(paste0(total$variable, "-01-01", sep=""))

ggplot(vertical_plot, aes(x = variable))  + 
  geom_area(data = total, aes(y = value), fill = 'SteelBlue3', color = "white", alpha = .75) +
  geom_area(aes(fill = Country, color = Country, y = value.x), position = 'Stack', alpha = .75) +
  scale_fill_brewer(palette = 'Set3') +
  scale_color_brewer(palette = 'Set3') + 
  geom_text(data = reference_points, aes(x = Date, y = .25, label = label), 
            size = 16, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond') +
  geom_text(aes(x = as.Date("2015-01-01"), y = 65000), label = "Total: world", family = 'EB Garamond', 
                color = 'Black', size = 14, alpha = .85) +
  geom_vline(data = reference_points, aes(xintercept = Date), linetype = "dashed", size = .2) +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(labels = scales::label_number(
    suffix = " Mboe/d", big.mark = ',')) +
  labs(x = "", y = "", title = "A changing energy landscape",
       subtitle = "Share of oil demand by country per year since 2000",
       caption = "Source: DaNumbers on OPEC data") +
  picci() + theme(legend.title = element_blank(), 
                  panel.grid.major.y = element_blank(),
                  axis.ticks.y = element_line(color = "black"))


ggsave('oild_demand_absolute.png', width = 20, height = 16, units = "cm", dpi = 300)


plot_separate = subset(vertical_plot, variable %in% c("2000-01-01", "2022-01-01"))
plot_separate$year[plot_separate$variable == "2000-01-01"] = "2000"
plot_separate$year[plot_separate$variable == "2022-01-01"] = "2022"

ggplot(plot_separate, aes(x = reorder(Country, Share), y = Share, fill = Country)) +
  geom_col(aes(y = 1, fill = Country), alpha = .25) +
  geom_col() + picci() +
  scale_fill_brewer(palette = 'Set3') +
  labs(title = "A decoupling in the making",
       subtitle = "Share of oil demand by selected country in 2000 and 2022",
       caption = "Source: DaNumbers on OPEC data", 
       y = '', x = '') +
  geom_text(aes(label = paste(paste0(Country, ":"), scales::percent(Share, accuracy = 2))),
            size = 12, family = "EB Garamond", hjust = -.1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  facet_wrap(~year, ncol = 2) +
  coord_flip() + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(),
                       legend.position = "none", legend.title = element_blank(), 
                       panel.grid.major.y = element_blank())
ggsave('oil_demand_changes.png', width = 20, height = 10, units = "cm", dpi = 300)

#Europe's getting serious on renewable energy sources

colnames_eu <- read_excel("nrg_ind_peh_page_spreadsheet (1).xlsx", 
                          sheet = "Sheet 1", range = "A11:M12")
energy_eu <- read_excel("nrg_ind_peh_page_spreadsheet (1).xlsx", 
                        sheet = "Sheet 1", na = ":", skip = 11)
colnames(energy_eu)[3:13] <- colnames(colnames_eu[3:13])

eu_energy_production = subset(energy_eu, `GEO (Labels)` == "European Union - 27 countries (from 2020)")
eu_energy_production = reshape2::melt(eu_energy_production, 
                                      id.vars = c("GEO (Labels)", "TIME", "Total"),
                                      stringsAsFactors = FALSE)
eu_energy_production = na.omit(eu_energy_production)
eu_energy_production$TIME = as.Date(paste0(eu_energy_production$TIME, "-01-01", sep=""))
eu_energy_production$Share = eu_energy_production$value / eu_energy_production$Total
eu_energy_production$variable = gsub("Nuclear fuels and other fuels n.e.c.", "Nuclear", eu_energy_production$variable)

ggplot(eu_energy_production, aes(x = TIME, y = Share, fill = variable)) +
  geom_area() + picci() +
  scale_fill_brewer(palette = 'Set3') +
  labs(title = "Europe's getting serious on renewable energy sources",
       subtitle = "Share of renewable energy sources in total energy production",
       caption = "Source: DaNumbers calculations on Eurostat data", 
       y = '', x = '') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_date(expand = c(0,0)) +
  theme(legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave('europe_fossil.png', width = 20, height = 16, units = "cm", dpi = 300)


last_data = subset(eu_energy_production, TIME == max(TIME))

ggplot(last_data, aes(y = reorder(variable, Share))) +
  geom_col(aes(x = 1, fill = variable), alpha = .25) +
  geom_col(aes(x = Share, fill = variable)) + 
  geom_text(aes(x = Share, label = paste(variable, scales::percent(Share, accuracy = 2))),
             size = 12, family = "EB Garamond", hjust = -.1) +
  picci() +
  scale_fill_brewer(palette = 'Set3') +
  labs(title = "Europe's getting serious on renewable energy sources",
       subtitle = "Share of renewable energy sources in total energy production",
       caption = "Source: DaNumbers calculations on Eurostat data (2021)", 
       y = '', x = '') +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme(legend.position = 'none', 
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
ggsave('europe_renewables.png', width = 20, height = 10, units = "cm", dpi = 300)

mean_energy = mean(eu_energy_production$Total)
sd_energy = sd(eu_energy_production$Total)
ggplot(eu_energy_production, aes(x = TIME, y = Total)) +
  geom_line() + picci() +
  scale_fill_brewer(palette = 'Set3') +
  labs(title = "Europe's getting serious on renewable energy sources",
       subtitle = "Share of renewable energy sources in total energy production",
       caption = "Source: DaNumbers calculations on Eurostat data", 
       y = '', x = '') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_date(expand = c(0,0)) +
  theme(legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank())


energy_countries = energy_eu %>% select(`GEO (Labels)`, TIME, `Total`) %>% 
  filter(!(`GEO (Labels)` %in% c("European Union - 27 countries (from 2020)",
             "Euro area – 20 countries (from 2023)",
             'Turkey', 'United Kingdom', "Norway", 'Liechtenstein', 'Iceland', 'Switzerland',
             'Bosnia and Herzegovina', 'Montenegro', 'North Macedonia', 'Serbia', 'Albania',
             "Moldova", "Türkiye","Ukraine",      
             "Kosovo*","Georgia","Special value")))

energy_countries = na.omit(energy_countries)

energ_means = aggregate(Total ~ `GEO (Labels)`, data = energy_countries, FUN = mean)
energy_sd = aggregate(Total ~ `GEO (Labels)`, data = energy_countries, FUN = sd)
energ_means$Para = 'Mean'
energy_sd$Para = 'SD'
energy_means = rbind(energ_means, energy_sd)
energy_means = reshape2::dcast(energy_means, `GEO (Labels)` ~ Para, value.var = "Total")

ggplot() +
  geom_col(data = energy_means, aes(x = reorder(`GEO (Labels)`, Mean), y = (Mean/100), fill = 'Mean'), size = 5, fill = "Grey55") +
  geom_jitter(data =  energy_countries, aes(x = `GEO (Labels)`, Total, y = Total/100), alpha = 1, 
              fill = 'SteelBlue3', shape = 21, size = 2) +
  labs(title = "EU countries produce power predictably",
       subtitle = "Yearly electricity production compared to mean",
       caption = "Source: DaNumbers calculations on Eurostat data", 
       y = '', x = '') +
  scale_y_continuous(labels = scales::label_number(
    suffix = " TWh", big.mark = ',')) +
  coord_flip() + picci() + 
  theme(axis.ticks.y = element_line(),
        panel.grid.major.y = element_blank(), 
        legend.title = element_blank())
 
selected_countries = subset(energy_countries, `GEO (Labels)` %in% c("Germany", "France", "Italy", "Spain", "Poland", "Sweden"))
selected_countries$TIME = as.Date(paste0(selected_countries$TIME, "-01-01", sep=""))


ggplot(selected_countries, aes(x = TIME, y = Total/1000)) +
  geom_area(aes(fill =  `GEO (Labels)`)) +
  scale_fill_brewer(palette = 'Set2') +
  scale_y_continuous(labels = scales::label_number(
    suffix = " TWh", big.mark = ',')) +
  labs(title = "Energy production stable in Europe's largest economies",
       subtitle = "Electric energy production in selected countries (2020-2021)",
       caption = "Source: DaNumbers calculations on Eurostat data", 
       y = '', x = '') + facet_wrap(~`GEO (Labels)`) + picci() +
  scale_x_date(expand = c(0,0)) + theme(legend.position = 'none', 
                                        axis.text.x = element_text(angle = 90, hjust = .1))


ggsave('europe_electicity_production.png', width = 20, height = 16, units = "cm", dpi = 300)
  
  picci() +
  scale_fill_brewer(palette = 'Set3') +
  labs(title = "Europe's getting serious on renewable energy sources",
       subtitle = "Share of renewable energy sources in total energy production",
       caption = "Source: DaNumbers calculations on Eurostat data", 
       y = '', x = '') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_date(expand = c(0,0)) +
  theme(legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank())

  ggsave('europe_electicity_production.png', width = 20, height = 16, units = "cm", dpi = 300)
  
  #Oil PRices 
  
oil_prices = read_csv("Cushing_OK_WTI_Spot_Price_FOB.csv", 
                           col_types = cols(Day = col_date(format = "%m/%d/%Y")), 
                           skip = 4)

oil_prices = oil_prices %>% filter(Day >= "1999-12-31")


ggplot(oil_prices, aes(x = Day, y = `Cushing OK WTI Spot Price FOB  Dollars per Barrel`)) +
  geom_line(color = 'SteelBlue3') +
  geom_line(aes(y = zoo::rollmean(`Cushing OK WTI Spot Price FOB  Dollars per Barrel`, 30, fill = NA, align = "right")), color = 'red') +
  
  picci() +
  labs(title = "Oil markets do not notice the troubles in the Middle East",
       subtitle = "WTI Spot Price (US$/Barrel); Red line = rolling 30-day average",
       caption = "Source: Reuters via U.S. Energy Information Administration (Collected on Jan. 28, 2023)", 
       y = '', x = '') +
  geom_vline(data = reference_points, aes(xintercept = Date), linetype = "dashed", size = .2) +
  geom_shadowtext(data = reference_points, aes(x = Date, y = 50, label = label), 
                  size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond', bg.colour='white') +
  geom_text(data = reference_points, aes(x = Date, y = 50, label = label), 
            size = 10, angle = 90, hjust = 0, vjust = -.5,  family = 'EB Garamond') +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) 
  
  
ggsave('oil_prices.png', width = 20, height = 14, units = "cm", dpi = 300)