install.packages("googlesheets4")
library(googlesheets4)
require(tidyverse)

library(readr)
USFR_federal_costs <- read_csv("USFR_StmtNetCost_20010930_20210930.csv")
View(USFR_federal_costs)


#Collect data from the The Planetary Society's repository 
## If the read_sheet command does not worl, you can copy the original spreadsheet on another Google Sheet
NASA_budget_overall = read_sheet("https://docs.google.com/spreadsheets/d/1NMRYCCRWXwpn3pZU57-Bb0P1Zp3yg2lTTVUzvc5GkIs/edit#gid=670209929")
Inflation_adj = read_sheet("https://docs.google.com/spreadsheets/d/1Gz6kpNZrF1kTiKjAMpYFRqxXSjXC2f3vZ14OWzQZ49U/edit#gid=0")
Major_programs = read_sheet("https://docs.google.com/spreadsheets/d/1NCw-ARZRoSFJY79X7bt7gMsIrzUONLHbNajjrBPCPNo/edit#gid=0")


#Define NASA milestones
NASA_milestones = data.frame(
  year = c(1960, 1969, 1973, 1981, 1998, 2020, 1986, 2003, 2011),
  mission = c("Mercury 7", "Apollo 11", "Skylab", "STS-1", "ISS construction begins", "SpaceX Crew-1", 
              "The Challenger disaster", "The Columbia disaster", "Last Space Shuttle flight")
)


#Visualize the data collected from The Planetary Society
ggplot(Inflation_adj, aes(x = Year, y = `Appropriation (adj dollars)`)) + geom_area(
  alpha = .7, color = "SteelBlue3", fill = "SteelBlue3") + geom_vline(data =
    NASA_milestones, aes(xintercept = year), linetype = "dashed") + 
  geom_text(data = NASA_milestones, aes(x = year, y = 30000, label = mission), angle = 90, 
            nudge_x = -1) +
  
  scale_y_continuous(
      expand = c(0,100), 
      labels = scales::comma
      )  + labs(title = "An Apollo-like mission without an Apollo-sized budget",
                                                     subtitle = "NASA appropriation in million dollars", 
                                                     caption = "SOURCE: The The Planetary Society", 
                                                     x = '', y = '') + picci

ggsave('mnoney.png', width = 20, height = 15, units = 'cm')


growth_percent = Inflation_adj %>% select(Year, `Appropriations year-over-year (YOY) growth`)
growth_percent$is_growing[growth_percent$`Appropriations year-over-year (YOY) growth` > 0] <- 'Grows'
growth_percent$is_growing[growth_percent$`Appropriations year-over-year (YOY) growth`< 0] <- 'Declines'



ggplot(growth_percent, aes(x = Year, y = `Appropriations year-over-year (YOY) growth`)) + 
  geom_col(aes(fill = is_growing)) + 
  geom_vline(data =
                 NASA_milestones, aes(xintercept = year), linetype = "dashed") + 
  geom_text(data = NASA_milestones, aes(x = year, y = .5, label = mission), angle = 90, 
            nudge_x = -1) + 
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") + picci + labs(title = "No recovery after the Apollo program", 
                                                     subtitle = "Yearly % change in NASA appropriation (2021 US$)",
                                                     caption = "SOURCE: The The Planetary Society", x = '', 
                                                     y = '') + theme(legend.position = 'none')
  

ggsave('budget_change.png', width = 20, height = 15, units = 'cm')

#Produce the major program share chart 
major_programs_app = data.frame(year = Major_programs$`Fiscal Year`, 
                               `Human Spaceflight Exploration` = Major_programs$`HSF Exploration (2021 $)`,
                               `Low earth Orbit Operations` = Major_programs$`LEO Space Operations (2021 $)`,
                               `Science Technology Missions Directorate` = Major_programs$`STMD (2021 $)`,
                               `Science Missions Directorate` = Major_programs$`SMD (2021 $)`,
                               Aeronautics = Major_programs$`Aeronautics (2021 $)`,
                               Education = Major_programs$`Education/STEM Outreach (2021 $)`,
                               `Cross agency support` = Major_programs$`Cross Agency Support/CECR (2021 $)`)
major_programs_app = reshape2::melt(major_programs_app, id.var = 'year')
major_programs_app = subset(major_programs_app, year < 2023)
major_programs_app$value[is.na(major_programs_app$value)] <- 0
major_programs_app$Label[major_programs_app$variable == 'Human.Spaceflight.Exploration'] <- "Human Spaceflight Exploration"
major_programs_app$Label[major_programs_app$variable == 'Low.earth.Orbit.Operations'] <- "Low Earth Orbit Operations"
major_programs_app$Label[major_programs_app$variable == 'Science.Technology.Missions.Directorate'] <- "Science Technology Missions Directorate"
major_programs_app$Label[major_programs_app$variable == 'Science.Missions.Directorate'] <- "Science Missions Directorate"
major_programs_app$Label[major_programs_app$variable == 'Aeronautics'] <- "Aeronautics"
major_programs_app$Label[major_programs_app$variable == 'Education'] <- "Education and Outreach (STEM)"  
major_programs_app$Label[major_programs_app$variable == 'Cross.agency.support'] <- "Cross Agency Support"

major_program_total = aggregate(value~year, data = major_programs_app, FUN = sum)
major_programs_app = merge(major_programs_app, major_program_total, by = "year")
major_programs_app$share = major_programs_app$value.x/major_programs_app$value.y
major_programs_app = subset(major_programs_app, share > 0)
major_programs_app = subset(major_programs_app, !(year ==  2007))



ggplot(major_programs_app, aes(x = year, y = share, fill = Label)) + geom_col() +
  picci + geom_text(aes(x = year, y = share, label = paste0(round(share, 2)*100, "%" )), 
                    position = position_stack(vjust = .5)) + 
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette = "Set3") + labs(title = "Major programs, major headaches?",
                                             subtitle = "% of major programs allocation (2007-2022)", 
                                             caption = "SOURCE: DaNumbers calculation on The Planetary Society data", 
                                             x = '', y = '') + 
  theme(legend.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), 
        axis.line.x = element_line())

ggsave('major_programs_share.png', width = 20, height = 16, units = 'cm')




ggplot(major_programs_app, aes(x = year, y = value.x)) + geom_area(
  aes(color = Label, fill = Label), alpha = .8
) + scale_color_brewer(palette = "Set3") + scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~Label, ncol = 2) + labs(title = "From the Earth to the Moon, maybe",
                            subtitle = "Appropriation by major NASA program (US$ million)", 
                            caption = "SOURCE: The Planetary Society", 
                            x = "", y = "") + picci + 
  theme(legend.position = 'none')

ggsave('major_programs.png', width = 20, height = 20, units = 'cm')



ggplot(NASA_budget_overall, aes(x = Year, y = `% of U.S. Spending`)) + 
  geom_area(
    alpha = .7, color = "SteelBlue3", fill = "SteelBlue3") + geom_vline(data =
                                                                          NASA_milestones, aes(xintercept = year), linetype = "dashed") + 
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = NASA_milestones, aes(x = year, y = .02, label = mission), angle = 90, 
            nudge_x = -1) + picci + labs(title = "From hero to zero, a 60-years long decline",
                                         subtitle = "NASA budget as % of federal spending", 
                                         caption = "SOURCE: The Planetary Society ", 
                                         x ="", y = "")
  
ggsave('federal_budget_spending.png', width = 20, height = 15, units = 'cm')
                               

app_request = data.frame(year = NASA_budget_overall$Year,
                         appr_percent = (NASA_budget_overall$Appropriation - NASA_budget_overall$`White House Budget Submission`)/NASA_budget_overall$`White House Budget Submission`)
app_request$sign[app_request$appr_percent>0] <- "b"
app_request$sign[app_request$appr_percent<0] <- "a"

ggplot(app_request, aes(x = year, y = appr_percent)) + 
  geom_col(aes(fill = sign)) + geom_vline(data =
                                                                          NASA_milestones, aes(xintercept = year), linetype = "dashed") + 
  geom_text(data = NASA_milestones, aes(x = year, y = .1, label = mission), angle = 90, 
            nudge_x = -1) + 
  scale_y_continuous(labels = scales::percent) +
  picci + scale_fill_brewer(palette = "Set1") + theme(legend.position = 'none') +
 labs(title = "Is NASA structurally underfunded?",
                                         subtitle = "% difference, appropriation vs. White House submission", 
                                         caption = "SOURCE: DaNumbers calculations on The Planetary Society data", 
                                         x ="", y = "")

ggsave('wishlist_difference.png', width = 20, height = 15, units = 'cm')


