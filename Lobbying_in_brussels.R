library(tidyverse)
library(ggalluvial)
library(reshape2)
library(stringr)


#load meps meetings from the https://www.integritywatch.eu/ open data portal then:

library(readr)
X1667429800_IWEU_mepmeetings_01112022 <- read_csv("1667429800_IWEU_mepmeetings_01112022.csv", 
                                                  locale = locale(encoding = "ISO-8859-1"))
View(X1667429800_IWEU_mepmeetings_01112022)

#Measure the meetings by party
lobby_meetings_by_party = as.data.frame(table(X1667429800_IWEU_mepmeetings_01112022$group))
lobby_meetings_by_party$lab = 
  paste(paste0(lobby_meetings_by_party$Var1, ":"), paste(
    formatC(lobby_meetings_by_party$Freq, big.mark = ","), "meetings"
  ))

#Define party colors 
colors = data.frame(
  Var1 = unique(lobby_meetings_by_party$Var1),
  color = c("#1866a5","#87CEEB", "green", "#990000", "#00438e", "yellow", "red")
)

lobby_meetings_by_party = merge(lobby_meetings_by_party, colors)

ggplot(lobby_meetings_by_party, aes(y = 
                                      fct_reorder(Var1, Freq)
                                      , x = Freq)) + 
  geom_col(aes(fill = Var1)) +
  scale_fill_manual(values = unique(lobby_meetings_by_party$color)) +
  geom_label(aes(x = 100, label = lab, hjust = 0), family = 'EB Garamond',
             alpha = .75) +
  picci_h_barplot +
  theme(
    axis.text = element_blank(),
    axis.ticks.x = element_blank(), 
    axis.title = element_blank(), 
    legend.position = "none"
  ) + labs(title = "The Greens report the most meetings with lobbyists",
           subtitle = "Meetings by MEPs since Feb. 2020",
           caption = "SOURCE: DaNumbers calculations on Transparency International data")

ggsave("meetings.png", width = 20, height = 10, units = 'cm')
#Dig into the meetings by party and by lobby 
lobby_meeting_party_lobby = X1667429800_IWEU_mepmeetings_01112022 %>% select(group, lobbyists)


#Try to clean the dataset; if you have the bandwitdth you can correct all the tiny problems of the dataset
lobby = lobby_meeting_party_lobby %>% separate(lobbyists, sep = "\\,", 
                                               into= paste0('lobby',
                                                                    seq(1,50,1)), 
                                               extra = "merge"
)


lobby = reshape2::melt(lobby, id.var = "group")
lobby$value <- gsub("Google.*", "Google", lobby$value)
lobby$value <- gsub(".*Google", "Google", lobby$value)
lobby$value <- gsub("Apple.*", "Apple", lobby$value)
lobby$value <- gsub(".*Apple", "Apple", lobby$value)
lobby$value <- gsub("Microsoft.*", "Microsoft", lobby$value)
lobby$value <- gsub(".*Microsoft", "Microsoft", lobby$value)
lobby$value[lobby$value == "Meta Platforms Ireland Limited and its various subsidiaries (f/k/a META Ireland Limited)"] <- "META"
lobby$value <- gsub("Facebook",
                   "META", lobby$value)
lobby$value <- gsub("META Whistleblower",
                    "Facebook Whistleblower", lobby$value)
lobby$value <- gsub("META.*", "META", lobby$value)
lobby$value <- gsub(".*META", "META", lobby$value)
lobby$value <- gsub("Amazon.*", "Amazon", lobby$value)
lobby$value <- gsub(".*Amazon", "Amazon", lobby$value)
lobby$value = tolower(lobby$value)
lobby$value = trimws(lobby$value)
lobby$number = 1
lobby = aggregate(number~value+group, FUN = sum, data = lobby)
lobby$value <- gsub(".*transparency international", "transparency international", lobby$value)
lobby$value <- gsub("transparency international.*", "transparency international", lobby$value)
lobby$value <- gsub(".*cefic", "cefic", lobby$value)
lobby$value <- gsub("cefic.*", "cefic", lobby$value)
lobby = aggregate(number~value+group, FUN = sum, data = lobby)
lobby$value <- gsub("airbus.*", "airbus", lobby$value)
lobby$value <- gsub(".*airbus", "airbus", lobby$value)
lobby$value <- gsub("deepmind.*", "deepmind", lobby$value)
lobby = aggregate(number~value+group, FUN = sum, data = lobby)
lobby$value <- gsub("huawei.*", "huawei", lobby$value)
lobby$value <- gsub(".*huawei", "huawei", lobby$value)
lobby$value <- gsub(".*tesla", "tesla", lobby$value)
lobby$value <- gsub("tesla.*", "tesla", lobby$value)
lobby$value <- gsub(".*jp morgan", "jp morgan", lobby$value)
lobby$value <- gsub("jp morgan.*", "jp morgan", lobby$value)
lobby = aggregate(number~value+group, FUN = sum, data = lobby)
lobby$value <- gsub(".*amcham", "amcham", lobby$value)
lobby$value <- gsub("amcham.*", "amcham", lobby$value)
lobby$value <- gsub("bnp.*", "bnp paribas", lobby$value)
lobby$value <- gsub("*.bnp", "bnp paribas", lobby$value)
lobby = aggregate(number~value+group, FUN = sum, data = lobby)
lobby$value <- gsub("*.shell", "shell", lobby$value)
lobby$value <- gsub("shell.*", "shell", lobby$value)
lobby$value <- gsub("*.ig metall", "ig metall", lobby$value)
lobby$value <- gsub("ig metall.*", "ig metall", lobby$value)
lobby$value <- gsub("*.wwf", "wwf", lobby$value)
lobby$value <- gsub("wwf.*", "wwf", lobby$value)
lobby = aggregate(number~value+group, FUN = sum, data = lobby)
lobby$value <- gsub("*.dgb", "dgb", lobby$value)
lobby$value <- gsub("dgb.*", "dgb", lobby$value)
lobby = aggregate(number~value+group, FUN = sum, data = lobby)

party_colors =
  data.frame(group = party_colors, color = c(
    "#87CEEB", "green", "yellow", "red"
  )

lobby = subset(lobby, number > 30)
lobby = merge(lobby, party_colors)

lobby$value = str_to_title(lobby$value)
lobby$value[ lobby$value == "Wwf"] <- "WWF"
lobby$data = paste(paste0(lobby$value, ":"), lobby$number)

#Visualize the dataset with a alluval chart and save the chart 

ggplot(lobby, aes(y = number, axis1 = group, axis2 = data, fill = group)) + 
  geom_alluvium() + scale_fill_manual(values = unique(lobby$color)) + 
  geom_stratum(color = "white") +
  geom_text(aes(y = 820, x = 1, label = "Group", family = 'EB Garamond'), size = 5) +
  geom_text(aes(y = 820, x = 2, label = "Lobby", family = 'EB Garamond'), size = 5) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), color = 'black', fill = "white",
             family = 'EB Garamond', alpha = .75) + picci + theme(
               axis.title = element_blank(),
               axis.title.x = element_blank(),
               axis.text = element_blank(),
               axis.ticks = element_blank(),
               axis.ticks.x = element_blank(),
               panel.grid = element_blank(),
               panel.grid.major.y = element_blank(),
               legend.position = "none"
             ) + 
  labs(
               title = "Trsansparency, what is it good for?",
               subtitle = "Lobbies with more than 30 meetings with MEPs since Feb. 2020",
               caption = "SOURCE: DaNumbers calculations on Transparency International data"
             )


ggsave("good_world.png", width = 20, height = 16, units = 'cm')


#Meeting by roles
meetings_role = X1667429800_IWEU_mepmeetings_01112022 %>% select(
  group, role)

meetings_role = as.data.frame(table(meetings_role))
meetings_role = merge(meetings_role, colors, by.x = "group", by.y = "Var1")
meetings_role = split(meetings_role, f = meetings_role$group)
meetings_role = lapply(
  meetings_role, function(df) {
    df %>% mutate(percent = Freq/sum(Freq))
    
  }
)
meetings_role = do.call(rbind, meetings_role)
roles = meetings_role %>% filter(role %in%
                           c("Member","Committee chair", "Rapporteur",
                             "Shadow rapporteur"
                         ))
ggplot(meetings_role, aes(y = group, x = percent)) +
  geom_col(aes(x = 1, fill = group), alpha = .25) +
  geom_col(aes(x = percent, fill = group)) +
  scale_fill_manual(values = unique(meetings_role$color)) +
  geom_vline(xintercept = 0.5, linetype = 'dashed') +
  geom_text(aes(label = paste0(round(percent,2)*100, "%" ), hjust = 0), family = 'EB Garamond' ,
            size = 5) +
  scale_x_continuous(expand = c(0,0)) +
  facet_wrap(~role, ncol = 2) + 
  picci +
  labs(title = "Average members meeting with lobbyists",
           subtitle = "% of meetings by role and by political group",
           caption = "SOURCE: DaNumbers calculations on Transparency International data") +
  theme(legend.position = "none", 
        axis.title = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("meetings_share.png", width = 20, height = 16, units = 'cm')

# install and load the xml2 package
install.packages("xml2")
library(xml2)

#Read the trasparency registry and parse the latest available xml version 

persons = read_xml("full_acc_pers_export.xml") %>% as_list()
persons_tb = tibble::as_tibble(persons) %>% unnest_longer('ListOfAccreditedPerson')
persons_tb <-persons_tb %>% unnest_wider('ListOfAccreditedPerson')

persons_tb = data.frame(
                        firstName = unlist(persons_tb$firstName),
                        lastName = unlist(persons_tb$lastName),
                        orgIdentificationCode = unlist(persons_tb$orgIdentificationCode),
                        orgName = unlist(persons_tb$orgName),
                        accreditationStartDate = unlist(persons_tb$accreditationStartDate),
                        accreditationEndDate = unlist(persons_tb$accreditationStartDate)
                        
                        )

#ORGs by registerd persons 
org_ppl = as.data.frame(table(persons_tb$orgName))
org_tb = tibble::as_tibble(persons) %>% unnest_longer('ListOfAccreditedPerson')



#Organisations
orgs = read_xml("full_export_new.xml") %>% as_list()
org_tb = tibble::as_tibble(orgs) %>% unnest_longer('ListOfIRPublicDetail')
org_tb <-org_tb %>% unnest_wider('ListOfIRPublicDetail')
org_tb$identificationCode  = unnest_wider(org_tb$financialData)

#Get financials for the last closed year and extract the grants 
financials = org_tb %>% select(identificationCode, financialData)
financials = subset(financials, !(identificationCode == "NULL"))
financials = financials %>% unnest_longer(col = "financialData")
financials = financials %>% unnest_wider(col = "financialData")
financials = financials %>% unnest_longer(col = "closedYear")
financials = financials %>% unnest_wider(col = "closedYear", names_repair = "minimal")
financials = financials %>% unnest_longer(col = "grants", names_repair = "minimal")
financials = financials %>% unnest_wider(col = "grants", names_repair = "minimal")
complimentary_notes = 
  financials %>% select(
    identificationCode, 
    `...1`, 
    financialData_id
  ) %>% filter(financialData_id == "complementaryInformation")
complimentary_notes$identificationCode = unlist(complimentary_notes$identificationCode)
colnames(complimentary_notes)[2] <- "complementaryInformation"
complimentary_notes[3] = NULL

newOrg = financials %>% select(
  identificationCode,
  `...1`,
  financialData_id) %>% filter(financialData_id == "newOrganisation")
colnames(newOrg)[2] = "newOrganisation"
newOrg[3] <- NULL
newOrg$identificationCode = unlist(newOrg$identificationCode)
org_desc = merge(newOrg, complimentary_notes, all.x = TRUE)

closed_year = subset(financials, financialData_id == "closedYear")
closed_year_key = closed_year[1:4]
closed_year_key[2] = NULL
closed_year_key$identificationCode = unlist(closed_year_key$identificationCode)
closed_year_key$startDate = unlist(closed_year_key$startDate)
closed_year_key$endDate = unlist(closed_year_key$endDate)

closed_year_grant = subset(closed_year, grants_id == "grant")
closed_year_grant = closed_year_grant %>% select(
  identificationCode, startDate, endDate, source, amount
)
closed_year_grant$identificationCode = unlist(closed_year_grant$identificationCode)
closed_year_grant$startDate = unlist(closed_year_grant$startDate)
closed_year_grant$endDate = unlist(closed_year_grant$endDate)
closed_year_grant$source = unlist(closed_year_grant$source)
closed_year_grant$amount = unlist(closed_year_grant$amount)
colnames(closed_year_grant)[4:5] = c("grant_source", "grant_amount")
closed_year_grant$grant_amount = as.numeric(closed_year_grant$grant_amount)

closed_year_amount = subset(closed_year, is.na(grants_id))
closed_year_amount = closed_year_amount %>% unnest_longer("fundingSources")
closed_year_amount = closed_year_amount %>% unnest_longer("totalBudget")
closed_year_amount = closed_year_amount %>% select(
  identificationCode, 
  startDate,
  endDate,
  fundingSources,
  totalBudget_id,
  fundingSources_id,
  totalBudget
  
)
closed_year_amount = na.omit(closed_year_amount)
closed_year_amount$startDate = unlist(closed_year_amount$startDate)
closed_year_amount$endDate = unlist(closed_year_amount$endDate)
closed_year_amount$identificationCode = unlist(closed_year_amount$identificationCode)
closed_year_amount$fundingSources = unlist(closed_year_amount$fundingSources)
closed_year_amount$totalBudget = unlist(closed_year_amount$totalBudget)

#Here is the moment I realise that the Transparency register does not show the share of contributions

closed_year_amount = closed_year_amount %>% select(identificationCode,  
                                                   startDate,
                                                   endDate,
                                                   totalBudget)

closed_year_amount = unique(closed_year_amount)
closed_year_money = merge(closed_year_amount, closed_year_grant, all.y = TRUE,
                          all.x = TRUE)

closed_year_money = merge(closed_year_money, complimentary_notes, all.x = TRUE)
closed_year_money = merge(closed_year_key, closed_year_money, all.x = TRUE)
closed_year_money = unique(closed_year_money)


closed_years_costs = closed_year %>% select(identificationCode, costs)
closed_years_costs = closed_years_costs %>% unnest_longer("costs")
closed_years_costs = closed_years_costs %>% unnest_wider("costs")
closed_years_costs$identificationCode = unlist(closed_years_costs$identificationCode)


closed_years_costs = na.omit(closed_years_costs)
closed_years_costs$max= unlist(closed_years_costs$max)
closed_years_costs = closed_years_costs %>% unnest_wider(
  "min"
)

colnames(closed_years_costs)[2:3] = c('min', 'max')
closed_years_costs[4] = NULL

closed_year_money = merge(closed_year_money, closed_years_costs, all.x = TRUE, 
                          all.y = TRUE)

closed_year_money = unique(closed_year_money)


#Try to identify costs for lobbying 
costs = merge(names, closed_years_costs)
costs = merge(interest, costs)
costs = unique(costs)

costs$min[is.na(costs$min)] <- 0

min_cost = aggregate(as.numeric(min)~registrationCategory, data = costs, FUN = mean)
min_cost$cat = 'Minimum'
colnames(min_cost)[2] = "value"
max_cost = aggregate(as.numeric(max)~registrationCategory, data = costs, FUN = mean)
max_cost$cat = 'Maximum'
colnames(max_cost)[2] = "value"
cost_cat = rbind(min_cost, max_cost)
cost_cat$value = cost_cat$value/1000 

ggplot(cost_cat, aes(x = value, y = registrationCategory)) + 
  geom_line(aes(group = registrationCategory)) +
  geom_point(aes(fill = cat), shape = 21, size = 4) +
  scale_x_continuous(labels = scales::dollar_format(
    prefix = "€", suffix = "K"
  )) +
  scale_fill_brewer(palette = "Set1") + picci + 
  labs(title = "Lobbying, how much do you cost?",
       subtitle = "Mean cost bracket by category",
       caption = "SOURCE: DaNumbers calculations on EU open data",
       x = "",
       y = "") +
  theme(panel.grid.major.y = element_blank(),
        legend.title = element_blank())

ggsave("costs.png", width = 20, height = 16, units = 'cm')

#Data on the organzations EU affairs address 

place= org_tb %>% select(identificationCode, EUOffice)
place = place %>% unnest_wider("EUOffice")
place = place %>% unnest_longer("identificationCode")
place = place %>% unnest_longer("postCode")
place = place %>% unnest_longer("city")
place = place %>% unnest_longer(c("country", "phone"))
place$address = unlist(place$address)
place = place[1:5]
place = na.omit(place)
place = unique(place)


eu_office_base = as.data.frame(table(place$country))


#Data on the organzations origins


origin = org_tb %>% select(identificationCode, headOffice)
origin = origin %>% unnest_wider("headOffice")
origin = origin %>% unnest_longer(c(
  "identificationCode",
  "address",
  "postCode",
  "city",
  "country")) %>% unnest_wider("phone")
origin = origin[1:5]
orign = na.omit(origin)
base = as.data.frame(table(origin$country))

#Interest represented and chart by interest type

interest = org_tb %>% select(identificationCode, registrationCategory)
interest = interest %>% unnest_longer(
                                      "registrationCategory")
interest = interest %>% unnest_longer("identificationCode")
interest_table = as.data.frame(table(interest$registrationCategory))
interest_table$share =round(interest_table$Freq/sum(interest_table$Freq),3)*100
interest_table$lab = paste(
  paste0(interest_table$Var1,":"),
    format(interest_table$Freq, big.mark = ",")
  )
  
interest_table$share_lab = paste0(interest_table$share, "%")
interest_table$share_lab = paste0("(", interest_table$share_lab)
interest_table$share_lab = paste0(interest_table$share_lab, ")")

interest_table$lab = paste(interest_table$lab, interest_table$share_lab)




ggplot(interest_table, aes(y = fct_reorder(Var1, Freq), x = Freq)) + 
  geom_col(fill = "SteelBlue3") + 
  geom_text(aes(x = 10, label = lab), family = "EB Garamond", hjust = 0) + picci + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.ticks.x = element_blank()) + 
  labs(title = "NGOs rule the Brussels bubble",
       subtitle = "Organizations by type: number and % on total",
       caption = "SOURCE: EU open data portal")


ggsave("organizations.png", width = 20, height = 10, units = 'cm')


#compare grants and budgets
simple_grants = grants %>% select(identificationCode, amount) %>% 
  mutate(source = "Grant")
simple_amount = closed_year_amount %>% select(identificationCode, 
                                              totalBudget) %>%
  mutate(source = "Budget")
simple_amount$totalBudget = as.numeric(simple_amount$totalBudget )
colnames(simple_amount)[2] = "amount"
simple_amount$amount = as.numeric(simple_amount$amount)

simple_amount = rbind(simple_grants, simple_amount)
names = org_tb %>% select(identificationCode, name)
names = names %>% unnest_longer("identificationCode")
names = names %>% unnest_longer("name")
names = names %>% unnest_longer("name")
names = na.omit(names)
names = unique(names)
simple_amount = merge(names, simple_amount)

average = aggregate(amount~source, data = simple_amount, FUN = mean)
sd = aggregate(amount~source, data = simple_amount, FUN = sd)
colnames(sd)[2] = 'sd'
average = merge(average, sd)


ggplot(average, aes(x = amount, y = source)) + geom_errorbar(
  aes(xmin = (amount-sd), xmax = (amount+sd))) + geom_jitter(
    data = simple_amount, aes(x = amount, y = source)
  ) + scale_x_continuous(limits = c(0, 500000000))


#People organizations and other stuff 



colnames(persons_tb)[3] <- "identificationCode"
table_ppl = as.data.frame(table(persons_tb$identificationCode))
colnames(table_ppl)[1] <- "identificationCode"
interest = na.omit(interest)

 
table_ppl = merge(table_ppl, names, by ="identificationCode", all.y = TRUE)
table_ppl = merge(table_ppl, interest, by ="identificationCode")

table_ppl$y = runif(length(table_ppl$identificationCode),
                    min = 0, max = 200000)

top_10_numbers = top_n(table_ppl, 10, Freq)
top_10_numbers$lab = paste(
  paste0(top_10_numbers$name, ":"), top_10_numbers$Freq
)


ggplot(table_ppl,(aes(y = y, x = Freq))) + 
  geom_point(aes(fill = registrationCategory,
                  ), position = position_jitter(seed = 1),
              shape = 21, alpha = .75, size = 3) +
  ggrepel::geom_label_repel(data = top_10_numbers, aes(label = lab),
                            position = position_jitter(seed = 1),
                            family = "EB Garamond", alpha = .75
                            ) +
  scale_fill_brewer(palette = "Paired") +
  picci + labs(title = "An army of influencers",
               subtitle = "Number of registered people in the EU Transparency Register",
               caption = "SOURCE: EU open data portal",
               x = "Number of people in the Transparency Register",
               y = "") + 
  guides(fill=guide_legend(ncol=2)) +
  theme(
                 axis.text.y = element_blank(),
                 panel.grid.major.y = element_blank(), 
                 panel.grid.major.x = element_line(),
               ) + theme(legend.position = "bottom",
                         legend.title = element_blank())

ggsave("army_influencers.png", width = 20, height = 16, units = 'cm') 


#Diagram on how XML files code 
data_sample =
  data.frame(
    nodes = 
      c("Document",
        "Document",
        "Document",
        "Document",
        "Document",
        "Identification",
        "Identification",
        "Finance",
        "Finance", 
        "Finance",
        "Name: Lobby X",
        "id: 11111111111-11",
        "Grant: € 0",
        "Budget: €0", 
        "Total costs (min max)"
        ),
    pos_x = c("a","a","a","a","a",
              "b","b","b","b","b",
              "c", "c", "c","c","c"),
    pos_y = c(3,3,3,3,3,
              4,4,2,2,2, 
              4.5,3.5,3,2,1),
    group_1 = c(
      "a1", "a1","b1", "b1", "b1",
      "a1","a2","b1","b2","b3",
                "a1","a2",
                "b1","b2","b3"),
    color = c("a","a","b", "b","b",
              "a","a","b", "b","b",
              "a","a","b", "b","b"))


ggplot(data_sample, aes(x = pos_x, y = pos_y)) +  geom_line(
    aes(group = group_1, color = color)) + geom_label(
      aes(label = nodes), size = 4,
      family = "EB Garamond") + scale_color_brewer(palette = "Set1") + 
  picci +  theme(axis.text = element_blank(),
                 axis.ticks.x = element_blank(),
                 panel.grid.major.y = element_blank(),
                 axis.title = element_blank(),
                 legend.position = 'none') + 
  labs(title = "Anatomy of an XML hideout",
       subtitle = "Representation of how lobbying data is presented",
       caption = "SOURCE: DaNumbers original")

ggsave("taxonomy_sample.png", width = 20, height = 10, units = 'cm')


#Finance and stuff 
amount_simple = aggregate(amount~name+source, FUN = sum, 
                          data =  simple_amount)
top_10_amounts = top_n(amount_simple, 10, amount)
top_10_amounts$billion = round(top_10_amounts$amount/1000000000, 1)
top_10_amounts$lab = 
  paste(paste0(
    top_10_amounts$name, ":",
    paste0("€", paste0(top_10_amounts$billion, "B"))
  ))

ggplot(top_10_amounts, aes(x = billion, y = fct_reorder(
  name, billion
))) + geom_col(aes(fill = source)) + 
  scale_fill_brewer(palette = "Set1") + 
  geom_text(aes(x = 0.1, label = lab), 
            hjust = 0, family = "EB Garamond") +
  labs(title = "Sorting out an accounting mess",
       subtitle = "Top ten of recieved grants and budget for the past year",
       caption = "SOURCE: DaNumbers elaboration on EU open data portal data", 
       x = "",
       y = "") + 
  picci + theme(
    axis.text = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks.x = element_blank(), 
    legend.title = element_blank()
  )


ggsave("grants_budgets.png", width = 20, height = 16, units = 'cm')
