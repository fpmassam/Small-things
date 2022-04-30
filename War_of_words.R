library(tidyverse)
library(syuzhet)
library(cleanNLP)
library(readr)

#Import speeches
speeches_1 <- read_excel("~/Documents/speeches.xlsx")
speeches <- read_delim("~/Documents/speeches.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%m-%d-%Y")), 
                       trim_ws = TRUE)
speeches$Text = speeches_1$Text
View(speeches)
speeches$Text = trimws(speeches$Text)
speeches$Speaker = trimws(speeches$Speaker)
rm(speeches_1)
speeches$Speaker = gsub("Margaret Thathcer", 'Margaret Thatcher', speeches$Speaker)


#Type of speech 
table(speeches$`Type of speech`)
Type_of_speeches = as.data.frame(table(speeches$`Type of speech`))
Type_of_speeches = data.frame('type'= 'Type of speech',
                              Type_of_speeches)

Role = data.frame(type = 'Role of the speaker',
             as.data.frame(table(speeches$Role))
             )
descriptive = rbind.data.frame(Role, Type_of_speeches)

ggplot(descriptive, aes(x = Freq, y = forcats::fct_reorder(Var1, Freq))) +
  scale_x_continuous(label = ~ scales::comma(.x, accuracy = 1)) +
  geom_col(fill = "SteelBlue") + facet_wrap(~type, nrow = 1, scales = "free_y") + picci_h_barplot + 
  labs(title = "War, a presidential business",
       subtitle = "Speeches by type and role of the speaker",
       caption = "SOURCE: DaNumbers research",
       x = "", y ="") + theme(
         plot.title.position = 'plot'
       )

ggsave('description.png', width = 20, height = 16, units = 'cm')


#Do Syuzhet
require(tidyverse)
require(syuzhet)

speeches_s = data.frame(speeches,
                        Labs = paste(speeches$Speaker, 
                                    format(speeches$Date, "%b %d, %Y"), sep = ' - '))
speeches_s = speeches %>% filter(Speaker %in% c("Vladimir Putin",
                                                  "Adolf Hitler",
                                                  "George W. Bush",
                                                "Antonio Salandra"
                                                ),
                                 !(Date == as.Date(	
                                   '2001-09-20')))



get_syuzhet = function(x){
  a = x %>% select(Text, Speaker)
  a = split(a, f = a$Speaker)
  tokens = lapply(a, function(y){
    get_tokens(y$Text)
  } 
                  )
  syuzhet = lapply(tokens, 
                   function(z)
                   {get_sentiment(z, method = 'syuzhet')}
                   )
  syuzhet = lapply(syuzhet, get_dct_transform)
  syuzhet = lapply(syuzhet, as.data.frame)
  syuzhet = lapply(syuzhet, function(df){
    data.frame(narrative_time = seq(1,100),
               df)})
  syuzhet = do.call(rbind, syuzhet)
  syuzhet = data.frame(Speaker = rownames(syuzhet),
                       Styzhet = syuzhet)
  syuzhet$Speaker = gsub('[[:digit:]]+', '', syuzhet$Speaker)
  syuzhet$Speaker = gsub('\\.', '', syuzhet$Speaker)
  rownames(syuzhet) = NULL
  colnames(syuzhet)[2:3] = c('narrative_time', 'Syuzhet')
  return(syuzhet)
}
syuzhet = get_syuzhet(speeches_s)
syuzhet$Sentiment[syuzhet$Syuzhet>0] <- "Positive"
syuzhet$Sentiment[syuzhet$Syuzhet<=0] <- "Negative"

ggplot(syuzhet, aes(x = narrative_time, 
                    y = Syuzhet, group = Speaker)) + 
  geom_area(fill = "SteelBlue3", alpha = .7) + facet_wrap(~Speaker, ncol = 1) + picci + 
  labs(title = "Russian formalism for speech analysis",
       subtitle = "DCT-transformed Syuzhet; values >0 mean a positive sentiment",
       x = "Narrative time",
       caption = "SOURCE: DaNumbers calculations using the Syuzhet R package")

ggsave('syuzhet.png', width = 20, height = 20, units = 'cm')




#Get NRC
nrc <- get_nrc_sentiment(speeches$Text)
negative_positive <- nrc[9:10]
nrc[9:10] = NULL
nrc$total = rowSums(nrc)
nrc = data.frame(Text = speeches$Text,
                 nrc)


nrc_1 = reshape2::melt(nrc, id.vars = c('Text', 'total'))
nrc_1 = merge(nrc_1, speeches, by = 'Text')
nrc_1$Labs = paste(nrc_1$Speaker, 
                              format(nrc_1$Date, "%b %d, %Y"), sep = ' - ')





nrc_1$share = nrc_1$value/nrc_1$total
ggplot(nrc_1, aes(y = fct_reorder(Labs, Date), x = share)) + 
  scale_x_continuous(expand = c(.01,0), labels = scales::percent) +
  scale_fill_brewer(palette = 'Set3') +
  labs(title = 'You need trust to go to war',
       subtitle = "% NRC sentiment values on 19 war declarations",
       caption = "SOURCE: DaNumbers calculations using the Syuzhet R package",
       x = '',
       y = '') + guides(fill = guide_legend(title = '')) +
  geom_col(aes(fill = variable)) + picci_h_barplot + theme(plot.title.position = 'plot')

ggsave('nrc_sentiment.png', width = 20, height = 16, units = 'cm')

#Factor sentiment 

library(ppcor)
require(psych)
require(ggfortify)
pcor(nrc[2:9])
inds = nrc[2:9]
inds_matrix = cor(inds)
KMO(inds_matrix)
fanone <- fa(r=inds, nfactors = 2, rotate="varimax",fm="pa")
fa.diagram(fanone)
head(fanone$scores)
factor_analyisis = data.frame(Speaker = speeches$Speaker, 
                              Date = speeches$Date,
                              fanone$scores)

factor_analyisis$Labs = paste(factor_analyisis$Speaker, 
                              format(factor_analyisis$Date, "%b %d, %Y"), sep = ' - ')
factor_analyisis[1:2] = NULL
colnames(factor_analyisis)[1] = "Rage"
colnames(factor_analyisis)[2] = "Hope"
require(data.table)
factor_melt = data.table(factor_analyisis)
factor_melt = melt.data.table(factor_melt, 
                               id.vars = 'Labs',
                               measure.vars = c("Hope", "Rage"))

ggplot(factor_melt, aes(x = variable, y = fct_reorder(Labs, value))) + 
  geom_point(aes(size = value, fill = variable), shape = 21) + 
  scale_fill_brewer(palette = "Set1") + scale_size_continuous(range = c(.1, 9))  +
  labs(title = "Vladimir Putin's angry dream (and Hitler's nihilism)",
       subtitle = "Factor analysis on NRC sentiment analysis",
       caption = "Source: DaNumbers calculations",
       x = '', y = '') + picci_h_barplot + guides(
         size = guide_legend(title = 'Rage or Hope'),
                             fill = 'none') + 
  theme(plot.title.position = 'plot') + theme(legend.position = 'right')



ggplot(factor_melt, aes(x = value, y = fct_reorder(Labs, value))) + 
  geom_line() +
  geom_point(aes(fill =  variable), shape = 21, size = 4) + 
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Vladimir Putin's angry dream (and Hitler's nihilism)",
       subtitle = "Factor analysis on NRC sentiment analysis",
       caption = "Source: DaNumbers calculations",
       x = '', y = '') + picci + guides(
         fill = guide_legend(title = ' ')) + picci_h_barplot +
  theme(plot.title.position = 'plot') + theme(legend.position = 'right')

ggsave('sentiment_clevelqnd.png', width = 20, height = 16, units = 'cm')

ggplot(factor_analyisis, aes(x = PA1, y = PA2))  + 
  geom_point(shape = 21, fill = "SteelBlue3", size = 2) +
  ggrepel::geom_label_repel(aes(label = 
                                 paste(Speaker, format(Date, "%b %d, %Y"), sep = ' - ')
                                 ), max.overlaps = 2, family = 'EB Garamond') + 
  labs(title = 'How to convince a country to go to war',
       subtitle = "Factor analysis on NRC algorithm data",
       caption = "SOURCE: DaNumbers calculations",
       x = "Rage",
       y = "Hope") + picci

ggsave('sentiment_scatter.png', width = 20, height = 16, units = 'cm')

#Readability score 
require(quanteda.textstats)
require(forcats)
Flesch = data.frame(speeches, textstat_readability(speeches$Text))
Flesch$Labs = paste(Flesch$Speaker, format(Flesch$Date, "%b %d, %Y"), sep = ' - ')
ggplot(Flesch, aes(x = Flesch, y = 
                    fct_reorder(Labs, Flesch))) + geom_col(
                      fill = "SteelBlue3"
                    ) + 
  geom_text(aes(x = 2, label = paste('Flesch', round(Flesch,1), sep = ': '
                                      ), hjust = 0), family = 'EB Garamond') +
  scale_x_continuous(expand = c(0,0)) +
  labs(title = 'Some hard explaining to do',
       subtitle = 'Flesch readability score speeches declaring war',
       caption = 'SOURCE: Calculation from the quanteda.texstqts R package',
       x = '',
       y = '') + picci_h_barplot + theme(axis.text.x = element_blank(),
                                         axis.ticks.x = element_blank(),
                                         plot.title.position ='plot')
ggsave('readability.png', width = 20, height = 16, units = 'cm')


