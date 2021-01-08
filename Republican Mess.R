require('readr')
require('GameTheory')
require('tidyverse')
require('cluster')

trumpists = read_csv('averages.csv')
trump_0 = trumpists %>% filter(congress == 116)
trump_chambers = split(trump_0, f = trump_0$chamber)
trump_chambers[["senate"]] = subset(trump_chambers[["senate"]], last_name != 'McSally')
Senate = trump_chambers[["senate"]]
House = trump_chambers[["house"]]
Senate = Senate %>% filter(last_name != 'Isakson')
House$Distric_full = paste0(House$state,'-', House$district)
House$No_onomyms = paste0(House$last_name, '-', House$Distric_full)
delete_ononym = c(
  'Hill-CA-25',
  'Lewis-GA-5',
  'Cummings-MD-7',
  'Collins-NY-27',
  'Marino-PA-12',
  'Duffy-WI-7')
House = House %>% filter(!(No_onomyms %in% delete_ononym))
House = House %>% filter(party != 'I')
House_clean = House %>% select(last_name, party, state, agree_pct)
House_rep = House_clean %>% filter(party == 'R')
House_dem = House_clean %>% filter(party != 'R')
cluster_rep = kmeans(House_rep$agree_pct, 4)
House_rep$Cluster = cluster_rep[["cluster"]]
House_rep$New_Party[House_rep$Cluster == 1] = 'Leaning Trumpist'
House_rep$New_Party[House_rep$Cluster == 2] = 'Pasdaran Trumpist'
House_rep$New_Party[House_rep$Cluster == 3] = 'Hardcore Trumpist'
House_rep$New_Party[House_rep$Cluster == 4] = 'RINO'
voting_power = rbind(as.data.frame(table(House_rep$New_Party)),
                          as.data.frame(table(House_dem$party)))
voting_power$Var1 = gsub('D', 'Democrats', voting_power$Var1)
house_voting_power_now = ShapleyShubik(((sum(House_now$Freq)/2)+1), House_now$Freq, Names = House_now$Var1)
voting_power_rep = as.data.frame(table(House_rep$New_Party))
republican_control =  ShapleyShubik(((sum(voting_power_rep$Freq)/2)+1), voting_power_rep$Freq, Names = voting_power_rep$Var1)
republicanSS = data.frame(
  Party = rownames(t(republican_control$Results)),
  t(republican_control$Results))
rownames(republicanSS) = NULL
republican_plot = reshape2::melt(republicanSS, id.vars = c('Party', 'Votes'))
republican_plot$variable = gsub('Shapley.Shubik', 'Shapley Shubik', republican_plot$variable)
republican_plot$variable = gsub('Votes....', 'Votes share', republican_plot$variable)
republican_plot$labels = paste(paste0(republican_plot$variable,':'), paste0(100*round(republican_plot$value, 4), '%'))

order_1 = c(
  'Pasdaran Trumpist',
  'Hardcore Trumpist',
  'Leaning Trumpist',
  'RINO'
)


ggplot(republican_plot, aes(x = value, y = variable, fill = factor(Party, levels = order_1))) + geom_col(width = 0.5) + 
  xlab('')+
  ylab('')+
  scale_fill_manual(values = c(
    'tomato4',
    'tomato3',
    'tomato2',
    'tomato1')) + geom_text(aes(label = paste0(100*round(value,2), '%')),  color = 'white', size = 4, position = position_stack(vjust = .5)) +
  labs(title = 'Trump owns the Republican party', subtitle = 'Republicans will have a hard time recovering from Jan. 6', caption = 'SOURCE: FiveThirtyEight', 
       fill = 'Party') + theme_minimal() + theme(legend.position = 'bottom') + scale_x_continuous(
         labels = scales::percent_format(scale = 100))
       


()



