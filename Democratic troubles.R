require(manifestoR)
require(tidyverse)
mp_setapikey('manifesto_apikey.txt')


#Collect data on local elections
files = list.files()
data_EL = lapply(files, function(x){
  
  read.delim(x, sep =";", dec = ",")
})
data_EL = data.table::rbindlist(data_EL)
data_EL = data_EL %>% mutate(turnout = VOTANTI/ELETTORI)


a = subset(data_EL, TURNO == "1")
b = subset(data_EL, comune == "1")
b$comune[b$comune == '1']  =  b$provincia 
b$provincia = b$regione
b$regione = b$TURNO
b$TURNO = "1"
data_EL = rbind(a,b)

data_EL = data_EL %>% mutate(turnout = 1- (VOTANTI/ELETTORI))
PD = subset(data_EL, LISTA == 'PARTITO DEMOCRATICO')
PD$share =PD$VOTI_LISTA/PD$VOTANTI


Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020$reddito =  Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020$`Reddito imponibile - Ammontare in euro`/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020$`Numero contribuenti`


reddito = Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020 %>% 
  select(`Denominazione Comune`, reddito)

colnames(reddito) = c('comune', 'imponibile')
reddito = merge(reddito, PD, by = 'comune')

ggplot(reddito, aes(x = imponibile, y = share)) + geom_point(
  fill = "SteelBlue", shape = 21, size = 2, alpha = .7
) + geom_smooth(method = 'lm', se = FALSE, color = 'red') + 
  ggrepel::geom_text_repel(aes(label = str_to_title(comune)), max.overlaps = 10) +
  labs(
  title = 'Is the Democratic Party leftist at all?',
  subtitle = 'Municipal elections 2019-2021',
  caption = 'Source: Interior Ministry, Agenzia delle Entrqte',
  y = 'Voting Share, Democratic Party',
  x = 'Taxable income (Imponibile IRPEF)') + 
  scale_x_continuous(labels = scales::label_comma(prefix  = "€")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  picci + theme(axis.ticks.y = element_line(),
                panel.grid.major.y = element_blank())

ggsave('dem_irpef.png', width = 20, height = 16, units = 'cm')

ggplot(reddito, aes(x = imponibile, y = turnout)) + geom_point() + geom_smooth(method = 'lm') + 
  ggrepel::geom_text_repel(aes(label = comune)) +
  labs(
    title = 'Turnout e reddito',
    subtitle = 'Elezioni Comunali 2019-2021',
    caption = 'FONTE: Ministero Interni, Ag. entrate',
    y = 'Astensione',
    x = 'Imponibile IRPEF pro capita') + 
  picci

summary(lm(share~imponibile, data = reddito))


ggplot(reddito, aes(x = turnout, y = share)) + geom_point() + geom_smooth(method = 'lm') + 
  ggrepel::geom_text_repel(aes(label = comune)) +
  labs(
    title = 'Turnout e voto PD',
    subtitle = 'Elezioni Comunali 2019-2021',
    caption = 'FONTE: Ministero Interni, Ag. entrate',
    y = '% PD',
    x = 'Astensione') + 
  picci

#get data from Europe Elects and download Italy
it <- read_csv("~/Downloads/it.csv", col_types = cols(`Polling Firm` = col_skip(), 
                                                           Commissioners = col_skip(), `Fieldwork Start` = col_skip(), 
                                                           Scope = col_skip(), `Sample Size` = col_skip(), 
                                                           `Sample Size Qualification` = col_skip(), 
                                                           Participation = col_skip(), Precision = col_skip(), 
                                                           `Movimento 5 Stelle` = col_skip(), `Partito Democratico` = col_number(), 
                                                           `Lega Nord` = col_number(), `Forza Italia` = col_skip(), 
                                                           `Fratelli d’Italia` = col_number(), 
                                                           `La Sinistra` = col_skip(), `Più Europa` = col_skip(), 
                                                           `Europa Verde` = col_skip()))

comparison = it %>% select(`Fieldwork End`, `Lega Nord`, `Fratelli d’Italia`, `Partito Democratico`)
comparison = reshape2::melt(comparison, id.var = "Fieldwork End")
comparison$share = comparison$value/100
comparison$`Fieldwork End` = as.Date(comparison$`Fieldwork End`)
comparison$color[comparison$variable == "Fratelli d’Italia"] <- "#003366"
comparison$color[comparison$variable == "Lega Nord"] <- "#00FF00"
comparison$color[comparison$variable == "Partito Democratico"] <- "#FF0000"
comparison$party[comparison$variable == "Fratelli d’Italia"] <- "Brothers of Italy"
comparison$party[comparison$variable == "Lega Nord"] <- "Northern League"
comparison$party[comparison$variable == "Partito Democratico"] <- "Democratic Party"
comp_list = split(comparison, f = comparison$`Fieldwork End`)
comp_first = comp_list[[1]]
comp_last = comp_list[[841]]
rm(comp_list)

comp_last$poll = paste(paste0("Last Poll", ':'),
                       paste0(comp_last$value, "%"))
comp_first$poll = paste(paste0("First Poll", ':'),
                       paste0(comp_first$value, "%"))

first_last = rbind(comp_first, comp_last)




ggplot(comparison, aes(x = party, y = share)) + picci +
  labs(title = "Could the Democratic Party do better?",
       subtitle = "Polls between March 14, 2018 and July 23, 2022",
       caption = "SOURCE: Europe Elects", 
       x = '',
       y = '') +
  geom_jitter(fill = comparison$color, shape = 21, alpha = .5) +
  geom_hline(data = first_last, aes(yintercept = share), linetype = 'dashed') +
  geom_point(data = comp_last, aes(x = party, y = share), size = 6, 
              fill = comp_last$color,
              shape = 21) +
  geom_point(data = comp_first, aes(x = party, y = share), size = 6,
              fill = comp_first$color,
              shape = 21) +
  ggrepel::geom_label_repel(data = first_last, aes(y = share, label = poll), alpha = .8, 
                            size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.4))  
ggsave('polls_historical.png', width = 20, height = 16, units = 'cm')


#Study electoral results in national/European elections

elections_national = 
  data.frame(
    election = c(2008, 2013, 2018),
    type = "Lower House",
    share = c(0.3318,0.2543, 0.1876)
  )

elections_EU = 
  data.frame(
    election = c(2009, 2014, 2019),
    type = "European Parliament",
    share = c(0.2612,0.4081,0.2274)
  )

elections_national = rbind(elections_national, elections_EU)

ggplot(elections_national, aes(x = as.factor(election), y = share)) + geom_col(aes(fill = type)) + 
  scale_fill_brewer(palette = "Set1") + picci + scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) + labs(title = "Electoral troubles for the Italian Democrats",
           subtitle = "Share of votes in European and Lower House elections",
           x = "", y ="", caption = 'SOURCE: Interior Ministry') + theme(
             legend.title = element_blank()
           )

ggsave('elections_democrats.png', width = 20, height = 16, units = 'cm')


#Assess the manifesto project data

PD = data.frame(party = c(32440),
                    date = c(201803,201302, 200804))
FDI= data.frame(party = c(32630),
                date = c(201803,201302, 200804))
League= data.frame(party = c(32720),
                date = c(201803,201302, 200804))

my_corpus <- mp_corpus(wanted)
fdi_corpus <- mp_corpus(FDI)
democratic_scale = mp_scale(my_corpus)
fdi_scale = mp_scale(fdi_corpus)
lega_corpus <- mp_corpus(League)
lega_scale = mp_scale(lega_corpus)

democratic_scale$name <- "Democratic Party"
fdi_scale$name <- "Brothers of Italy"
lega_scale$name <- "Northern League"
democratic_scale$year[democratic_scale$date == 200804] <- 2008
democratic_scale$year[democratic_scale$date == 201302] <- 2013
democratic_scale$year[democratic_scale$date == 201803] <- 2018

fdi_scale$year[fdi_scale$date == 200804] <- 2008
fdi_scale$year[fdi_scale$date == 201302] <- 2013
fdi_scale$year[fdi_scale$date == 201803] <- 2018

lega_scale$year[lega_scale$date == 200804] <- 2008
lega_scale$year[lega_scale$date == 201302] <- 2013
lega_scale$year[lega_scale$date == 201803] <- 2018

fdi_scale$color = "#003366"
lega_scale$color = "#00FF00"
democratic_scale$color = "#FF0000"

manifestos = rbind(fdi_scale, lega_scale, democratic_scale)
manifestos_13 = subset(manifestos, year == 2018)

ggplot(manifestos, aes(x = rile, y = as.factor(year))) + 
  geom_vline(xintercept = 0) +
  geom_line() +
  geom_point(fill = manifestos$color,
                                                         shape = 21, size = 4) + 
  ggrepel::geom_label_repel(data = manifestos_13, aes(label = name)) +
  geom_text(y = as.factor(2013), x = -15, aes(label = "Left-leaning positions")) +
  geom_text(y = as.factor(2008), x = 10, aes(label = "Right-leaning positions")) +
  labs(title = "Is there any left or right at all?",
       subtitle = "Negative values represent lef-leaning positions", 
       caption = "SOURCE: Manifesto Project (2021)",
       y = 'Elections year',
       x = 'Lef-right scaling') + picci_h_barplot
ggsave('manifestoes.png', width = 20, height = 8, units = 'cm')


#Where the democratic party wins 
library(readxl)
Elenco_comuni_italiani <- read_excel("Elenco-comuni-italiani.xls")
View(Elenco_comuni_italiani)

PD_20 = subset(reddito, share > 0.1999)
PD_20 <- as.data.frame(table(PD_20$regione))
PD_20$Var1 <- str_to_title(PD_20$Var1)

ggplot(PD_20, aes(y = fct_reorder(Var1, Freq), x = Freq)) + geom_col(
  color = "SteelBlue3", fill = "SteelBlue3"
) + 
  picci_h_barplot + theme(axis.text = element_blank(),
                          axis.ticks.x = element_blank()) + 
  labs(x = '',
       y = '') + geom_text(aes(label = paste(paste0(Var1, ':'),
                                           Freq), x = 0.1), hjust = 0, 
                           family = 'EB Garamond') + 
  labs(title = "A Northern-traction Democratic Party",
       subtitle = "Numer of municipalities where PD got more than 20% of votes",
       caption = "SOURCE: Da Numbers calculation on Interior Ministry data")

ggsave('pd_20_PERCENT.png', width = 20, height = 16, units = 'cm')


Volunteers <- read_csv("DCCV_AVQ_PERSONE_29072022142658088.csv")
Volunteers$Territory = toupper(Volunteers$Territory)
Volunteers$Territory = as.character(Volunteers$Territory)
Volunteers$Territory = trimws(Volunteers$Territory)
Volunteers <- subset(Volunteers, `Data type` == 'free activities in voluntary associations')
Volunteers <- subset(Volunteers, Measure == 'per 100 people with the same characteristics')
Volunteers <- subset(Volunteers, Territory %in% c(
  "Valle d'Aosta / Vallée d'Aoste", 
  "Piemonte",
  "Lombardia",
  "Veneto",
  "Friuli-Venezia Giulia",
  "Liguria",
  "Toscana",
  "Umbria",
  "Emilia-Romagna",
  "Marche",
  "Abruzzo",
  "Campania",
  "Puglia",
  "Lazio",
  "Sicilia",
  "Basilicata",
  "Molise",
  "Calabria",
  "Sicilia",
  "Sardegna"
))

top_5 = subset(Volunteers, 
               Territory %in% c(
                 "Emilia-Romagna",
                 "Piemonte",
                 "Veneto",
                 "Friuli-Venezia Giulia",
                 "Lombardia"
               ))


ggplot(top_5, aes(y = fct_reorder(Territory, Value), 
       x = Value)) + geom_col(fill = "SteelBlue3") +
  geom_text(aes(x = 0.1, 
                 label = paste(paste0(Territory, ':'), paste0(
                   Value, '%'
                 ))
                       ), hjust = 0, family = 'EB Garamond') +
  picci_h_barplot + theme(axis.text = element_blank(),
                          axis.ticks.x = element_blank()) + 
  labs(title = "Italy's hidden treasure",
       subtitle = "Volunteers per 100 people",
       caption = "SOURCE: Istat (2000)",
       x = '', y = '')


ggsave('volunteers_PERCENT.png', width = 20, height = 10, units = 'cm')




