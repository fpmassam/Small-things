require("tidyverse")
library(readr)
library(ggtext)

#Download the PDF table of the share of population by religion from Pew Research 
#Turn it in CSV using Tabula and clean it up in Excel, then run:
tabula_globalReligion_tables <- read_delim("tabula-globalReligion-tables.csv", 
                                           delim = ";", escape_double = FALSE, col_types = cols(POPULATION = col_skip(), 
                                                                                                CHRISTIAN = col_number(), MUSLIM = col_number(), 
                                                                                                UNAFFIL. = col_number(), HINDU = col_number(), 
                                                                                                BUDDHIST = col_number(), RELIGION...8 = col_number(), 
                                                                                                RELIGION...9 = col_number(), JEWISH = col_number()), 
                                           trim_ws = TRUE)
View(tabula_globalReligion_tables)


library(readr)
FSU_Countries <- read_delim("FSU_Countries.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(...2 = col_skip(), 
                                                                                 ...3 = col_skip()), trim_ws = TRUE)
View(FSU_Countries)


tabula_globalReligion_tables %>% filter()

religions_FSU = subset(tabula_globalReligion_tables, COUNTRY %in% FSU_Countries$Country)
religions_FSU = reshape2::melt(religions_FSU, id.var = 'COUNTRY')
religions_FSU$Religion = str_to_title(religions_FSU$variable)


unique(religions_FSU$Religion)
main_affiliation = c("Christian","Muslim","Unaffil.")
religions_FSU=  subset(religions_FSU, Religion %in% main_affiliation)
total_major = aggregate(value ~ COUNTRY, FUN = sum, data = religions_FSU)
total_major$others =100 - total_major$value
total_major = data.frame(COUNTRY = total_major$COUNTRY,
                         Religion = "Other",
                         value = total_major$others)

religions_FSU = religions_FSU %>% select(COUNTRY, value, Religion)
religions_FSU = rbind(total_major, religions_FSU)
religions_FSU = subset(religions_FSU, value > 49)

ggplot(religions_FSU, aes(x = value, y = COUNTRY)) +
  geom_col(aes(x = 100), fill = "grey", alpha = .75) +
  geom_col(aes(fill = Religion)) + scale_fill_brewer(palette = "Set2") +
  geom_text(aes(x = 1, hjust = 0, label = paste(
    paste0(religions_FSU$COUNTRY, ";"),
    paste0(Religion, ":"), paste0(value, "%") 
  )), family = 'EB Garamond', size = 18) +
  scale_y_discrete(limits = rev) + 
  scale_x_continuous(expand = c(0, 1), label = 
                       scales::label_number(suffix = "%")) + 
  labs(title = "Religious divide in the Former Soviet Union",
       subtitle = "% of population by religious affiliation",
       caption = "SOURCE: Pew Research (2012)", 
       x = "", y = "") + picci() + theme(legend.position = 'none',
                                         panel.grid.major.y = element_blank(), 
                                       axis.text.y = element_blank())

ggsave("religions.png", width = 20, height = 16, units = 'cm')


#Check regime events in the countries of the FSU

devtools::install_github("vdeminstitute/vdemdata")
require(vdemdata)
libdem_post_1989 = vdem %>% select(country_name, country_text_id,
                         year, historical_date, v2x_libdem,
                         v2xnp_pres, v2xnp_regcorr,
                         v2x_clphy,
                         v2x_clpol,
                         v2x_clpriv,
                         v2elintim,
                         v2elpeace,
                         v2xed_ed_poed,
                         v2xed_ed_inco,
                         e_pt_coup_attempts,
                         (v2x_regime)) %>% 
  filter (year>1989)

libdem_2022 = vdem %>% select(country_name, country_text_id,
                                   year, historical_date, v2x_libdem,
                                   v2xnp_pres, v2xnp_regcorr,
                                   v2x_clphy,
                                   v2x_clpol,
                                   v2x_clpriv,
                                   v2elintim,
                                   v2elpeace,
                              v2x_regime)%>% 
  filter (year == 2022)

libdem_2022 = replace(libdem_2022,is.na(libdem_2022),0)

for_factor = data.frame(libdem_2022[6:12])

#Perform a bidimensional varimax to study countries worldwide


library(ppcor)
require(psych)
require(ggfortify)
pcor(for_factor)
inds = for_factor
inds_matrix = cor(inds)
KMO(inds_matrix)
scree(inds)
fanone <- fa(r=inds, nfactors = 2, rotate="varimax",fm="pa",
             plot = TRUE)
plot(fanone)


data_fact_chart = data.frame(rownames(head(fanone$loadings)),  
                                       head(fanone$loadings))
colnames(data_fact_chart) <- c("vdem_codes", "Pluralism and civic freedoms",
                               "Order and peace")
codebook = vdemdata::codebook
codebook = subset(codebook, tag %in% data_fact_chart$vdem_codes)
codebook = codebook %>% dplyr::select(name, tag) %>% mutate(
  vdem_codes = tag
)

codebook[2] = NULL
 
data_fact_chart = merge(codebook, data_fact_chart)
codebook[1] = NULL
data_fact_chart = reshape2::melt(data_fact_chart)
data_fact_chart$fill[data_fact_chart$value > 0] = "a"
data_fact_chart$fill[data_fact_chart$value < 0] = "b"

ggplot(data_fact_chart, aes(x = name, y = value)) + 
  geom_col(aes(fill = fill, alpha = abs(value))) + 
  scale_fill_brewer(palette = "Set1", direction = -1) +
  geom_label(aes(label = round(value, 2)), size = 14, family = "EB Garamond",
             alpha = .7) +
  scale_y_continuous(limits = c(-1,1)) +
  facet_wrap(~variable, ncol = 2) + 
  picci() + 
  labs(title = "Individual rights for democracies",
       subtitle = "Loadings of a factor analysis (varimax)",
       caption = "SOURCE: DaNumbers analysis on V-Dem data",
       y = "Loadings", x = "") + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.x.bottom = element_text(size = 36))
  
  
ggsave("factor_vdem.png", width = 20, height = 20, units = 'cm')


fa.diagram(fanone)
head(fanone$scores)

libdem_2022 = data.frame(libdem_2022, fanone$scores)

libdem_2022$`Regime type`[libdem_2022$v2x_regime == 0] <- 'Closed Autocracy'
libdem_2022$`Regime type`[libdem_2022$v2x_regime == 1] <- 'Electoral Autocracy'
libdem_2022$`Regime type`[libdem_2022$v2x_regime == 2] <- 'Electoral Democracy'
libdem_2022$`Regime type`[libdem_2022$v2x_regime == 3] <- 'Liberal Democracy'


libdem_2022_FSU = libdem_2022%>% filter(
  country_name %in% FSU_Countries$Country
)

scatterplot_annotations = 
  data.frame(PA1 = c(-2.2,-2.2,1,1),
             PA2 = c(2,-2, 2, -2),
             lab = c("Closed Regimes",
                     "Unstable Regimes",
                     "Stable Regimes",
                     "Fragile Regimes"))



ggplot(libdem_2022, aes(PA1, PA2,)) + geom_point(alpha = .5, 
                                               shape = 21, aes(fill = `Regime type`),
                                               size = 2) +
  geom_vline(xintercept = 0, linetype = 'dashed', color = "red") +
  geom_hline(yintercept = 0, linetype = 'dashed', color = "red") +
  geom_point(data = libdem_2022_FSU, aes(PA1, PA2, fill = `Regime type`,
                                         size = 16), 
           size = 6, shape = 21) + 
  geom_smooth(se = FALSE, color = "black", linewidth = .5) +
  scale_fill_brewer(palette = "Set3") +
  ggrepel::geom_label_repel(data = libdem_2022_FSU, aes(PA1, PA2,
                                        label = country_name), size = 14,
                            family = "EB Garamond", alpha = .8) + 
  geom_text(data = scatterplot_annotations, aes(x = PA1, y = PA2, 
                                                label = lab), size = 14,
            family = "EB Garamond", alpha = .8, fontface = "bold") +
  labs(title = "A diverse landscape in the former USSR empire",
       subtitle = "Factor analysis - Civic freedoms/Order and peace (2022)",
       caption = "SOURCE: Own calculations on the V-Dem Version 13 dataset (2023)",
       y = "Order and peace",
       x = "Pluralism and civic freedoms") + 
  picci() + theme(
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    legend.spacing.x = unit(2, "mm")
  ) + guides(fill = guide_legend(ncol = 2), title = "")
ggsave("factos.png", width = 20, height = 18, units = 'cm')


#Regimes by quadrant 
stable_democracies = subset(libdem_2022, PA1 > 0 & PA2 >0)
stable_democracies =
  data.frame(
  country_type = "Stable Regimes",
  as.data.frame(table(stable_democracies$`Regime type`)))

fragile_regimes = subset(libdem_2022, PA1 > 0 & PA2 < 0)
fragile_regimes =
  data.frame(
    country_type = "Fragile Regimes",
    as.data.frame(table(fragile_regimes$`Regime type`)))

unstable_regimes = subset(libdem_2022, PA1 < 0 & PA2 < 0)
unstable_regimes =
  data.frame(
    country_type = "Unstable Regimes",
    as.data.frame(table(unstable_regimes$`Regime type`)))

stable_autocracies = subset(libdem_2022, PA1 < 0 & PA2 > 0)
stable_autocracies =
  data.frame(
    country_type = "Closed Regimes",
    as.data.frame(table(stable_autocracies$`Regime type`)))


countries_validation = rbind(stable_autocracies,stable_democracies,
                             unstable_regimes, fragile_regimes)

countries_validation$country_type = factor(countries_validation$country_type,
       levels = c("Closed Regimes","Stable Regimes",
                  "Unstable Regimes", "Fragile Regimes"))


ggplot(countries_validation, aes(x = Freq, y = Var1)) +
  geom_col(aes(fill = Var1)) + 
  scale_fill_brewer(palette = "Set3") +
  facet_wrap( ~country_type) + 
  geom_text(aes(label = Freq, x = 0, hjust = 0),
             family = "EB Garamond", size = 12) + 
  labs(title = "Validating the factor analysis",
       subtitle = "Cross-referecing classification with V-Dem's regime types",
       caption = "SOURCE: Own calculations on the V-Dem Version 13 dataset (2023)",
       x = "", y = ""
       ) + picci() + theme(legend.position = "none",
                           panel.grid.major.y = element_blank(),
                           axis.ticks.y = element_line(),
                           plot.title.position = "plot")
ggsave("validation.png", width = 20, height = 16, units = 'cm')

libdem_post_1989_fsu = subset(libdem_post_1989, country_name %in% FSU_Countries$Country)
libdem_post_1989_fsu = libdem_post_1989_fsu %>%
  dplyr::select(country_name, historical_date, v2xnp_pres, v2xnp_regcorr)

libdem_post_1989_fsu = reshape2::melt(libdem_post_1989_fsu, id.vars = c("country_name",
                                                 "historical_date"))


libdem_post_1989_fsu$Indicator[libdem_post_1989_fsu$variable == "v2xnp_pres"] <- 'Presidentialism'
libdem_post_1989_fsu$Indicator[libdem_post_1989_fsu$variable == "v2xnp_regcorr"] <- 'Corruption'



library(ggh4x)


ggplot(libdem_post_1989_fsu, aes(x = historical_date, y = value)) + 
  geom_line(aes(group = Indicator, color = Indicator)) + 
  scale_y_continuous(n.breaks = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Corruption thrives in personal regimes",
       subtitle = "Comparison between corruption and presidentialism",
       caption = "SOURCE: V-Dem V13 dataset",
       x = "",
       y = "") +
  picci() +  
  theme(legend.title = element_blank(),
        legend.spacing.x = unit(2, "mm")) +
  facet_wrap(~country_name, ncol = 5)
ggsave("corrpution_presidentialism.png", width = 20, height = 18, units = 'cm')


#Load Eurostat's trade balance data 
library(readxl)
trade_eu <- read_excel("ext_lt_maineu_page_spreadsheet.xlsx", 
                       sheet = "Sheet 1", range = "A11:B243", 
                       na = ":")
View(trade_eu)
colnames(trade_eu) = c("country_name", "trade_balance")

#Load UN global trade data, saved on GoogleDocs, prepare data, and calculate trade imbalances

library(googlesheets4)
trade_russia_china = read_sheet("https://docs.google.com/spreadsheets/d/1eDegZWg5VnFD1GsdIYfLvOvhFiHra--JTJsFN5RLe3o/edit#gid=1133850519")
trade_export = subset(trade_russia_china, `Trade Flow` == "X")
trade_export = trade_export %>%
  dplyr::select(Reporter, Partner_1, Value)
colnames(trade_export) = c("Country", "Partner", "Export")

trade_import = subset(trade_russia_china, `Trade Flow` == "M")
trade_import = trade_import %>%
  dplyr::select(Reporter, Partner_1, Value)
colnames(trade_import) = c("Country", "Partner", "Import")

trade_balance = merge(trade_export, trade_import)
trade_balance$Balance = (trade_balance$Export-trade_balance$Import)/1000000000
trade_balance = subset(trade_balance, Country %in% FSU_Countries$Country)
trade_balance$Import = -1*trade_balance$Import
trade_balance = reshape2::melt(trade_balance, id.vars = c("Country",
                                          "Partner",
                                          "Balance"))
trade_balance$value = round(trade_balance$value/1000000000, 1)
trade_balance$Balance = round(trade_balance$Balance, 1)
trade_balance = subset(trade_balance, !(Partner == "China"))

labels_trade = data.frame(
  x = c(-5,5),
  y = c(1, 1),
  lab = c("Import", "Export")
  
)
library(ggtext)
library(glue)

ggplot(trade_balance, aes(y = Country)) + geom_col(
  aes(x = value, fill = variable)) + scale_fill_brewer(palette = "Set1",
                                                       direction = -1) +
  geom_label(aes(x = 0, label = 
                  paste(
                    paste0('Trade balance', ":"),
                    paste0(
                    paste0("$", Balance), "B"
                  ))),size = 14, family = "EB Garamond", alpha = .7) +
  geom_text(data = labels_trade, aes(x = x, y = y, 
                                     label = lab), size = 16, family = "EB Garamond",
            fontface = 'bold') +
  scale_x_continuous(labels = scales::dollar_format(suffix = "B"),
                     limits = c(-8,8), n.breaks = 6) +
  labs(title = "You do not become an autocracy by trading with Russia",
       subtitle = "Trade flows between former USSR countries and Russia (2022)<br>
       <span style = 'font.size:12pt'> *Countries with missing data are not displayed*
       </span>",
       caption = "SOURCE: UN Comtrade <br> Data downloaded on Apr. 10, 2023",
       x = "",
       y = "") +
  picci() + theme(
    plot.subtitle = element_markdown(lineheight = 0),
    plot.caption = element_markdown(lineheight = 0),
    axis.ticks.y = element_line(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.title.position = "plot"
  )
ggsave("trade.png", width = 20, height = 14, units = 'cm')


#Study some socio-cultural variables 
`WVS_Cross-National_Wave_7_Rds_v5_0` <- readRDS("~/Documents/FSU_pol_geography/WVS_Cross-National_Wave_7_Rds_v5_0.rds")

religion = `WVS_Cross-National_Wave_7_Rds_v5_0` %>% 
  dplyr::select(B_COUNTRY_ALPHA, Q170)
religion = labelled::remove_labels(religion)


religion$answer[religion$Q170 == -1] <- "I don't know"
religion$answer[religion$Q170 == -2] <- "Refused to answer"
religion$answer[religion$Q170 == -3] <- "Not applicable"
religion$answer[religion$Q170 == -5] <- "Missing"
religion$answer[religion$Q170 == -4] <- "Not asked"
religion$answer[religion$Q170 == 1] <- "Strongly agree"
religion$answer[religion$Q170 == 2] <- "Agree"
religion$answer[religion$Q170 == 3] <- "Disagree"
religion$answer[religion$Q170 == 4] <- "Strongly disagree"

#Code of answer: -1=don't know; -2=refused to answer; -3=not applicable; -5=other missing). Code -4 is assigned automatically by the WVS Data archive to all cases in a national data-set when one or several questions were excluded.
religion = religion %>% 
  dplyr::select(B_COUNTRY_ALPHA, answer)
religion = as.data.frame(table(religion))
religion = subset(religion, !(answer %in% c("Not asked", "Missing")))
religion_share = split(religion, f = religion$B_COUNTRY_ALPHA)
religion_share = lapply(religion_share, function(x){
  x = x %>% mutate(share = Freq/sum(Freq))
})
religion = do.call(rbind, religion_share)
religion_very_importan = subset(religion, answer == "Strongly agree")
religion_very_importan[2:3] = NULL
colnames(religion_very_importan) = c("country_text_id", "The only acceptable religion is my religion - % strongly agree")
#Extracting and calculating the share of the panel of question Q177 about how acceptable is to take government benefits without being entitled to 


government_benefit = `WVS_Cross-National_Wave_7_Rds_v5_0` %>% 
  dplyr::select(B_COUNTRY_ALPHA, Q177)

share = function(x, q) {
  x = subset(x, !(q %in% c(-4, -5)))
  x = as.data.frame(table(x))
  x = split(x, f = x$B_COUNTRY_ALPHA)
  x = lapply(x, function(df){
    df %>% mutate(share =  as.numeric(Freq)/
                    as.numeric(sum(Freq)))}
    )
  x = do.call(rbind, x)
  return(x)
}

government_benefit = share(government_benefit, q = government_benefit$Q177)
government_benefit = subset(government_benefit,  Q177 == 10)
government_benefit[2:3] = NULL
colnames(government_benefit) = c("country_text_id", "Claiming government benefits to which you are not entitled - % always acceptable")

#Extracting and calculating the share of the panel of question Q200 about how often ppl discuss political matters 

pol_discuss = `WVS_Cross-National_Wave_7_Rds_v5_0` %>% 
  dplyr::select(B_COUNTRY_ALPHA, Q200)
pol_discuss = share(pol_discuss, q = pol_discuss$Q200)
pol_discuss = subset(pol_discuss, Q200 == 1)
pol_discuss[2:3] = NULL
colnames(pol_discuss) = c("country_text_id", "How often discusses political matters with friends? - % frequently")


#Important child qualities: Imagination
kids_imagination = `WVS_Cross-National_Wave_7_Rds_v5_0` %>% 
  dplyr::select(B_COUNTRY_ALPHA, Q11)
kids_imagination = share(kids_imagination, q = kids_imagination$Q11)
kids_imagination = subset(kids_imagination, Q11 == 1)
kids_imagination[2:3] = NULL
colnames(kids_imagination) = c("country_text_id", "Important child qualities: Imagination - % mentions")

#Confidence in elections 
elections_confidence = `WVS_Cross-National_Wave_7_Rds_v5_0` %>% 
  dplyr::select(B_COUNTRY_ALPHA, Q76
)
elections_confidence = share(elections_confidence, 
                             q = elections_confidence$Q76)
elections_confidence = subset(elections_confidence, Q76 == 1)
elections_confidence[2:3] = NULL
colnames(elections_confidence) = c("country_text_id", "Confidence: Election - % a big deal")


#Homosexuality justified 

homosex_just = `WVS_Cross-National_Wave_7_Rds_v5_0` %>% 
  dplyr::select(B_COUNTRY_ALPHA, Q182
  )
homosex_just = share(homosex_just, 
                             q = homosex_just$Q182)
homosex_just = subset(homosex_just, Q182 == 10)
homosex_just[2:3] = NULL
colnames(homosex_just) = c("country_text_id", "Justifiable: Homosexuality- % always")


#Merge dataset 
soc_cap =  merge(religion_very_importan, government_benefit)
soc_cap =  merge(soc_cap, pol_discuss)
soc_cap = merge(soc_cap, kids_imagination)
soc_cap = merge(soc_cap, elections_confidence)
soc_cap = merge(soc_cap, homosex_just)

#factor analysis of social capital 
for_factor_sc = soc_cap[2:7]

pcor(for_factor_sc)
inds_sc = for_factor_sc
inds_matrix_sc = cor(inds_sc)
KMO(inds_matrix_sc)
scree(inds_sc)
fa.diagram(pcone)
pcone <- principal(r=inds_sc, nfactors = 2)
plot(pcone)

scores =data.frame(pcone$scores)
colnames(scores) = c("Social openness", "Political openness")
soc_cap = data.frame(soc_cap, scores)

country_key = libdem_2022 %>% dplyr::select(
  country_name, country_text_id, `Regime type`
  
)

soc_cap[2:7] = NULL
soc_cap = merge(country_key, soc_cap, all = TRUE)
soc_cap = na.omit(soc_cap)



pc_loads = data.frame(
  rownames(pcone$loadings),head(
  pcone$loadings))

colnames(pc_loads) = c("WVS_Q", "Social openness", "Political openness")
pc_loads = reshape2::melt(pc_loads)
pc_loads$fill[pc_loads$value > 0] <- "a"
pc_loads$fill[pc_loads$value < 0] <- "b"

ggplot(pc_loads, aes(y = WVS_Q, x = value)) + 
  geom_col(aes(fill = fill, alpha = abs(value))) + 
  scale_fill_brewer(palette = "Set1", direction = -1) +
  geom_text(aes(label = paste(  
                   paste0(WVS_Q, ':'),
                   round(value, 2)), x = -1), size = 14, family = "EB Garamond",
             alpha = .7, hjust = 0) +
  scale_x_continuous(limits = c(-1,1)) +
  facet_wrap(~variable, ncol = 1) + 
  picci() + theme(legend.position = 'none',
                  axis.ticks.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.x = element_line()
                  ) + 
  labs(title = "Social openness is not political openness",
       subtitle = "Results of a Principal Component Analysis on selected WVS answers",
       caption = "SOURCE: DaNumbers calculations on World Value Suveey (Wave 7) data",
       y = "", x = "Loadings")
                  

ggsave("pc_social_capital.png", width = 20, height = 20, units = "cm")


soc_cap_plot = reshape2::melt(soc_cap, var.id = c("countrt_name",
                                                  "Regime type"))

soc_cap_plot$variable =  gsub("Social.openness", "Social openness",
                              soc_cap_plot$variable)

soc_cap_plot$variable =  gsub("Political.openness", "Political openness",
                              soc_cap_plot$variable)

soc_cap_fsu = subset(soc_cap_plot, country_name %in% 
                       FSU_Countries$Country)

soc_cap_anno = data.frame(
  x = c(-2, 2),
  y = c(1.5,1.5),
  labs = c("Closed societies", "Open societies")
)



ggplot(soc_cap_plot, aes(y = variable, 
                    x = value)) + geom_point(
                      aes(fill = `Regime type`),
                    shape = 21, size = 4, alpha = .7) + 
  geom_point(data = soc_cap_fsu, 
             aes(x = value, 
                 y = variable,
                 fill = `Regime type`), size = 6,
             shape = 21) + 
  geom_vline(xintercept = 0, linetype = 'dashed', color = "red") +
  scale_fill_brewer(palette = "Set3") +
  ggrepel::geom_label_repel(data = soc_cap_fsu, 
                            aes(x = value, 
                                y = variable,
                                label = country_name), size = 12, family = "EB Garamond"
           ) + 
  geom_text(data = soc_cap_anno, aes(x = x, y = y, label = labs),
family = "EB Garamond", size = 16, hjust = 0) +
  picci() + theme(
    legend.title = element_blank(),
    legend.spacing.x = unit(2, "mm")
  ) + 
  guides(fill = guide_legend(ncol = 2), title = "") + labs(title = "Social openness for democracy",
           subtitle = "PCA over World Value Survey data",
           caption = "SOURCE: World Value Survey, Wave 7, v5.0",
           x = "", y = "")
ggsave("factos.png_SC.png", width = 20, height = 12, units = 'cm')





#Institutional factor 

institutional_factors = 
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1RaIhRipUeHMC6x1jgy8Ni2dAtCTLjIArqCWOeAIqGaw/edit#gid=0")

inst_vdem = 
vdemdata::vdem %>% dplyr::select(
  country_name, year, v2x_libdem, v2x_regime
) %>% filter(country_name %in% FSU_Countries$Country)

institutional_factors$plus_2 = institutional_factors$year+2
inst_factors = reshape2::melt(institutional_factors, id.vars = c("country_name",
                                                  "Parliamentary"))

inst_factors_transition = na.omit(inst_factors)
inst_factors_transition[3] = NULL
colnames(inst_factors_transition)[1] = "country_name"


inst_factors_transition = split(
  inst_factors_transition, f = 
  inst_factors_transition$country_name)

inst_factors_transition = lapply(
  inst_factors_transition, function(x){
    x %>% mutate(phases = seq(length(x$value)))
    
  }
)

institutional_factors$minus_2 = institutional_factors$year-2
starting_point = institutional_factors %>% dplyr::select(
  country_name, minus_2
)

starting_point$Parliamentary = 0
starting_point$phases = 0
starting_point = na.omit(starting_point)
colnames(starting_point)[2] = "value"
inst_factors_transition = rbind(
  do.call(rbind, inst_factors_transition),
  starting_point)
colnames(inst_factors_transition)[3] = "year"



inst_factors_transition = merge(inst_factors_transition, inst_vdem)

inst_factors_transition$`Regime type`[inst_factors_transition$v2x_regime == 0] <- 'Closed Autocracy'
inst_factors_transition$`Regime type`[inst_factors_transition$v2x_regime == 1] <- 'Electoral Autocracy'
inst_factors_transition$`Regime type`[inst_factors_transition$v2x_regime == 2] <- 'Electoral Democracy'
inst_factors_transition$`Regime type`[inst_factors_transition$v2x_regime == 3] <- 'Liberal Democracy'
inst_factors_transition = subset(inst_factors_transition, !(country_name == "Kyrgyzstan"))

inst_factors_transition$Phase[inst_factors_transition$phases == 0] <- "Two years before parliamentarism"
inst_factors_transition$Phase[inst_factors_transition$phases == 1] <- "Transition to parliamentarism"
inst_factors_transition$Phase[inst_factors_transition$phases == 2] <- "Two years after parliamentarism"

inst_factors_transition$Phase = ordered(inst_factors_transition$Phase, c("Two years before parliamentarism",
                                         "Transition to parliamentarism",
                                         "Two years after parliamentarism"))

labs_trans = subset(inst_factors_transition, phases == 2)
labs_trans_year = subset(inst_factors_transition, phases == 1)
labs_trans_year$lab = paste(
  paste("Year of transition:", labs_trans_year$year)
) 


ggplot(inst_factors_transition, aes(x = Phase, y = v2x_libdem)) + 
  geom_vline(aes(xintercept = "Transition to parliamentarism")) +
  geom_line(aes(group = country_name,
               color = country_name)) + 
  geom_point(aes(color = `country_name`),size = 3) + 
  geom_text(data = labs_trans, aes(x = 3.05, y = v2x_libdem,
                                   label = country_name,
                                   hjust = 0), 
            size = 12, family = "EB Garamond") +
  ggrepel::geom_label_repel(data = labs_trans_year, aes(x = Phase, y = 
                                           v2x_libdem, 
                                         label = lab,
                                         color = country_name),
             size = 10, family = "EB Garamond", fontface = "bold") +
  scale_color_brewer(palette = "Paired", direction = -1) +
  labs(x = "", y = "Liberal democracy",
       title = "Parliamentarism supports democratic transitions",
       subtitle = "Level of liberal democracy two years before and after transition",
       caption = "SOURCE: DaNumbers research, V-Dem V13 dataset") +
  picci() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(),
        axis.ticks.y = element_line(),
        legend.position = "none")
  
ggsave("transition.png", width = 20, height = 16, units = 'cm')



#Counterfactual on Kyrgyzstan 

kyrg = vdemdata::vdem %>% select(
  country_name, year, historical_date,
  v2x_libdem
) %>% filter(country_name == "Kyrgyzstan")
kyrg$Regime[kyrg$year < 2010] <- "Presidential republic"
kyrg$Regime[kyrg$year > 2009 & kyrg$year < 2021] <- "Parliamentary republic"
kyrg$Regime[kyrg$year >2020] <- "Presidential republic"


ggplot(kyrg, aes(x = historical_date, y = v2x_libdem)) + 
  geom_col(aes(fill = Regime), width = 360) +
  scale_fill_brewer(palette = "Set1", direction = -1) + picci() + 
  theme(legend.title = element_blank()) +
  labs(title = "The curious case of Kyrgyzstan",
       subtitle = "Liberal democracy and institutional changes",
       caption = "SOURCE: V-Dem V-13 dataset (2023)",
       x = "",
       y = "Liberal democracy")

ggsave("kyrg_drama.png", width = 20, height = 16, units = 'cm')

