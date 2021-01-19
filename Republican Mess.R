require('readr')
require('GameTheory')
require('tidyverse')
require('cluster')
#Collect data 
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

#Cluster the two parties and calculate voting power
cluster_rep = kmeans(House_rep$agree_pct, 4)
cluster_dem = kmeans(House_dem$agree_pct, 4)
House_dem$Cluster = cluster_dem[["cluster"]]
House_dem$New_Party[House_dem$Cluster == 1] = 'Pasdaran liberal'
House_dem$New_Party[House_dem$Cluster == 2] = 'Liberal '
House_dem$New_Party[House_dem$Cluster == 3] = 'DINO'
House_dem$New_Party[House_dem$Cluster == 4] = 'Hardcore liberal'
voting_power_dem = as.data.frame(table(House_dem$New_Party))
democratic_control =  ShapleyShubik(((sum(voting_power_dem$Freq)/2)+1), voting_power_dem$Freq, Names = voting_power_dem$Var1)
democraticSS = data.frame(
  Party = rownames(t(democratic_control$Results)),
  t(democratic_control$Results))
rownames(democraticSS) = NULL
democratic_plot = reshape2::melt(democraticSS, id.vars = c('Party', 'Votes'))
democratic_plot$variable = gsub('Shapley.Shubik', 'Shapley Shubik', democratic_plot$variable)
democratic_plot$variable = gsub('Votes....', 'Votes share', democratic_plot$variable)
democratic_plot$labels = paste(paste0(democratic_plot$variable,':'), paste0(100*round(democratic_plot$value, 4), '%'))

order_0 = c(
  'DINO',
  'Liberal',
  'Hardcore liberal',
  'Pasdaran liberal'
)

ggplot(democratic_plot, aes(x = value, y = variable, fill = Party)) + geom_col(width = 0.5) + 
  xlab('')+
  ylab('')+
  scale_fill_manual(values = c(
    'steelblue4',
    'steelblue3',
    'steelblue2',
    'steelblue1')) + geom_text(aes(label = paste0(100*round(value,2), '%')),  color = 'white', size = 4, position = position_stack(vjust = .5)) +
  labs(title = 'Who owns the Democratic caucus', subtitle = 'Could hardliner liberals push others to the center?', caption = 'SOURCE: FiveThirtyEight', 
       fill = 'Party') + theme_minimal() + theme(legend.position = 'bottom') + scale_x_continuous(
         labels = scales::percent_format(scale = 100))



#Cluster the entire house
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

#Order only Republicans
order_1 = c(
  'Pasdaran Trumpist',
  'Hardcore Trumpist',
  'Leaning Trumpist',
  'RINO'
)

#Plot only Republicans
ggplot(republican_plot, aes(x = value, y = variable, fill = factor(Party, levels = order_1))) + geom_col(width = 0.5) + 
  xlab('')+
  ylab('')+
  scale_fill_manual(values = c(
    'tomato4',
    'tomato3',
    'tomato2',
    'tomato1')) + geom_text(aes(label = paste0(100*round(value,2), '%')),  color = 'white', size = 4, position = position_stack(vjust = .5)) +
  labs(title = 'Trump owns the Republican congressional caucus', subtitle = 'Republicans will have a hard time getting rid of Trump', caption = 'SOURCE: FiveThirtyEight', 
       fill = 'Party') + theme_minimal() + theme(legend.position = 'bottom') + scale_x_continuous(
         labels = scales::percent_format(scale = 100))

#Cluster the entire House
cluster_house = kmeans(House_clean$agree_pct, 8)
House_clean$Cluster = cluster_house$cluster
House_clean$New_Party[House_clean$Cluster == 1] = 'Pasdaran Trumpist'
House_clean$New_Party[House_clean$Cluster == 8] = 'Hardliner Trumpist'
House_clean$New_Party[House_clean$Cluster == 6] = 'Hardcore Trumpist'
House_clean$New_Party[House_clean$Cluster == 5] = 'Very Strong Trumpist'
House_clean$New_Party[House_clean$Cluster == 3] = 'Strong Trumpist'
House_clean$New_Party[House_clean$Cluster == 2] = 'Trumpist'
House_clean$New_Party[House_clean$Cluster == 7] = 'Leaning Trumpist'
House_clean$New_Party[House_clean$Cluster == 4] = 'Anti-Trump'
#Order all
order_0 = c(
  'Pasdaran Trumpist',
  'Hardliner Trumpist',
  'Hardcore Trumpist',
  'Very Strong Trumpist',
  'Strong Trumpist',
  'Trumpist',
  'Leaning Trumpist',
  'Anti-Trump'
)


voting_power_house = as.data.frame(table(House_clean$New_Party))
house_control =  ShapleyShubik(((sum(voting_power_house$Freq)/2)+1), voting_power_house$Freq, Names = voting_power_house$Var1)
houseSS = data.frame(
  Party = rownames(t(house_control$Results)),
  t(house_control$Results))
rownames(houseSS) = NULL
house_plot = reshape2::melt(houseSS, id.vars = c('Party', 'Votes'))
house_plot$variable = gsub('Shapley.Shubik', 'Shapley Shubik', house_plot$variable)
house_plot$variable = gsub('Votes....', 'Votes share', house_plot$variable)
house_plot$labels = paste(paste0(house_plot$variable,':'), paste0(100*round(house_plot$value, 4), '%'))

#Plot voting power in the House
ggplot(house_plot, aes(x = value, y = variable, fill = factor(Party, levels = order_0))) + geom_col(width = 0.5) + 
  xlab('')+
  ylab('')+
  scale_fill_manual(values = c(
    'tomato4',
    'tomato3',
    'tomato2',
    'tomato1',
    'tomato',
    'orange2',
    'orange1',
    'steelblue')) + geom_text(aes(label = paste0(100*round(value,2), '%')),  color = 'white', size = 4, position = position_stack(vjust = .5)) +
  labs(title = 'Anti-Trumps rule', subtitle = 'Democrats are more compact than Republicans in a polarized House', caption = 'SOURCE: FiveThirtyEight', 
       fill = 'Party') + theme_minimal() + theme(legend.position = 'bottom') + scale_x_continuous(
         labels = scales::percent_format(scale = 100))

#Plot Republicans subsetting from the above clustering
house_rep_1 = subset(House_clean, New_Party != 'Anti-Trump')
voting_power_republican_01 = as.data.frame(table(house_rep_1$New_Party))
republican_01_control =  ShapleyShubik(((sum(voting_power_republican_01$Freq)/2)+1), voting_power_republican_01$Freq, Names = voting_power_republican_01$Var1)
republican_01SS = data.frame(
  Party = rownames(t(republican_01_control$Results)),
  t(republican_01_control$Results))
rownames(republican_01SS) = NULL
republican_01_plot = reshape2::melt(republican_01SS, id.vars = c('Party', 'Votes'))
republican_01_plot$variable = gsub('Shapley.Shubik', 'Shapley Shubik', republican_01_plot$variable)
republican_01_plot$variable = gsub('Votes....', 'Votes share', republican_01_plot$variable)
republican_01_plot$labels = paste(paste0(republican_01_plot$variable,':'), paste0(100*round(republican_01_plot$value, 4), '%'))

#Plot republicans
ggplot(republican_01_plot, aes(x = value, y = variable, fill = factor(Party, levels = order_0))) + geom_col(width = 0.5) + 
  xlab('')+
  ylab('')+
  scale_fill_manual(values = c(
    'tomato4',
    'tomato3',
    'tomato2',
    'tomato1',
    'orange3',
    'orange2',
    'orange1',
    'steelblue')) + geom_text(aes(label = paste0(100*round(value,2), '%')),  color = 'white', size = 4, position = position_stack(vjust = .5)) +
  labs(title = 'Extra Trump, nulla salus', subtitle = "Factionalism will be about relative trumpism", caption = 'SOURCE: FiveThirtyEight', 
       fill = 'Party') + theme_minimal() + theme(legend.position = 'bottom') + scale_x_continuous(
         labels = scales::percent_format(scale = 100))

centers = data.frame(Clusters = rownames(cluster_house$centers), Centers = 
  cluster_house$centers)


centers$Clusters[centers$Clusters == 1] = 'Pasdaran Trumpist'
centers$Clusters[centers$Clusters == 8] = 'Hardliner Trumpist'
centers$Clusters[centers$Clusters == 6] = 'Hardcore Trumpist'
centers$Clusters[centers$Clusters == 5] = 'Very Strong Trumpist'
centers$Clusters[centers$Clusters == 3] = 'Strong Trumpist'
centers$Clusters[centers$Clusters == 2] = 'Trumpist'
centers$Clusters[centers$Clusters == 7] = 'Leaning Trumpist'
centers$Clusters[centers$Clusters == 4] = 'Anti-Trump'

House_clean = merge(House_clean, centers, by.x = 'New_Party', by.y = 'Clusters')


#Plot clusters
ggplot(House_clean, aes(x = factor(New_Party, levels = order_0), y = agree_pct, fill = factor(New_Party, levels = order_0))) + 
  geom_bin2d(alpha = .2) +
  geom_jitter(shape = 21, color = 'black') + 
  xlab('')+
  ylab("538's Loyalty Index")+
  scale_fill_manual(values = c(
    'tomato4',
    'tomato3',
    'tomato2',
    'tomato1',
    'orange3',
    'orange2',
    'orange1',
    'steelblue')) + theme_minimal() + theme(legend.position = 'none') + coord_flip() +
  labs(title = 'Trumpism defines the U.S. House', subtitle = 'The Republican Party a Trump-judged beauty contest', caption = 'SOURCE: FiveThirtyEight')

