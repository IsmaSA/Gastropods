setwd("C:/Users/isma-/OneDrive/Escritorio/Gastropod")
#df <- read_excel("InvaCost_database_v4.1.xlsx")
df <- read.csv2("Invacost.csv")

head(df)
unique(df$Species)

### Cleaning steps
# Eliminating data with no information on starting and ending years
df <- df[-which(is.na(df$Probable_starting_year_adjusted)), ]
df <- df[-which(is.na(df$Probable_ending_year_adjusted)), ]

# Eliminating data with no usable cost value
df <- df[-which(is.na(df$Cost_estimate_per_year_2017_USD_exchange_rate)), ]

expanded <- expandYearlyCosts(df, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")

max(expanded$Impact_year)

expanded<-expanded %>% filter(Impact_year <= "2020")
expanded<-expanded %>% filter(Impact_year >= "1966")             
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))

expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil) # 3.9388
nrow(expanded)

c<- expanded %>% group_by(Species,Environment) %>% summarize(costs= sum(cost_bil),
                                                        n=n())
c
#Subsetting
expanded <- expanded[expanded$Implementation %in% c("Observed"),]
#expanded <- expanded[expanded$Implementation %in% c("Potential"),]
sum(expanded$cost_bil) #3.79  /////////////   0.14
sort(expanded_r$Probable_starting_year_adjusted, decreasing = F)
global.raw.all <- summarizeCosts(expanded,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1966,
                                 maximum.year = 2020,
                                 year.breaks = seq(1966, 2020, by = 5))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1966,
                                 maximum.year = 2020,
                                 year.breaks = seq(1986, 2020, by = 5))

res <- computeAvgTotCost(expanded,
                         min.year = 1966,
                         max.year = 2020) # Excluding data after 2020 (e.g. planned budgets)
res


res<- computeAvgTotCost(
  expanded,
  cost.column = "cost_bil",
  year.column = "Impact_year",
  min.year = 1966,
  max.year = 2020)



#World map
ddf = read.table(text="
country value
'Norway' 94
'Finland' 94
'Sweden' 94
'Vietnam' 94
'Ecuador' 94
'China' 94
'India' 94
'South Africa' 94
'Australia' 94
'Spain' 94
'Japan' 94
'United Kingdom' 94
'Denmark' 94
'Cuba' 94
'Thailand' 94", header=TRUE)

pal <- colorRampPalette(brewer.pal(7, 'Reds'))(length(ddf$value))
pal <- pal[with(ddf, findInterval(value, sort(unique(value))))]
plot(newmap,col=col,
     bg="lightblue",border="grey60",
     asp = 1
)


min(df$Applicable_year)


expanded1 <- expanded[expanded$Method_reliability %in% c("High"),]
nrow(expanded1)
table(expanded$)

#Geographic cost

geo<- expanded%>% group_by(Geographic_region) %>% summarise(coste=sum(cost_bil),
                                                            entries=n())
geo




##### Plot 2
head(expanded)

a<- expanded %>% group_by(Family, Geographic_region,Environment) %>% summarise(coste =sum(cost))

aa<- a %>% filter(Environment=="Aquatic")
ab<- a %>% filter(Environment=="Terrestrial")

ggplot(aa, aes(x=Family,y= coste,fill = factor(Geographic_region)))+
 geom_bar(position = "dodge",stat = "identity", width=0.5)+
 scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x))+
  theme_classic()+scale_fill_brewer(palette="Dark2")  + annotation_logticks() +
  scale_y_continuous(trans='log10')


ggplot(ab, aes(x=Family,y= coste,fill = factor(Geographic_region)))+
  geom_bar(position = "dodge",stat = "identity", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x))+
  theme_classic()+scale_fill_brewer(palette="Dark2")  + annotation_logticks()
p1+p2

#### Tyoe of cost

a<- expanded %>% group_by(Family, Type_of_cost_merged,Environment) %>% summarise(coste =sum(cost))

aa<- a %>% filter(Environment=="Aquatic")
ab<- a %>% filter(Environment=="Terrestrial")

ggplot(aa, aes(x=Family,y= coste,fill = factor(Type_of_cost_merged)))+
  geom_bar(position = "dodge",stat = "identity", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x))+
  theme_classic()+scale_fill_brewer(palette="Dark2")  + annotation_logticks() +
  scale_y_continuous(trans='log10')


ggplot(ab, aes(x=Family,y= coste,fill = factor(Type_of_cost_merged)))+
  geom_bar(position = "dodge",stat = "identity", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x))+
  theme_classic()+scale_fill_brewer(palette="Dark2")  + annotation_logticks()


### Plot 3:

a<- expanded %>% group_by(Species,Environment, Impacted_sector) %>% summarise(coste =sum(cost_bil))
unique(a$Impacted_sector)
a$Impacted_sector[a$Impacted_sector== "Authorities-Stakeholders/Fishery"] <- "Diverse"
a$Impacted_sector[a$Impacted_sector== "Agriculture/Environment/Fishery"] <- "Diverse"
a$Impacted_sector[a$Impacted_sector== "Environment/Public and social welfare"] <- "Diverse"


aa<- a %>% filter(Environment=="Aquatic")
ab<- a %>% filter(Environment=="Terrestrial")


ggplot(aa, aes(Species, coste)) + geom_bar(stat="identity", 
            aes(fill = Impacted_sector)) + theme_classic()+
  scale_y_break(c(0.5, 3.4))
unique(aa$Species)

ggplot(ab, aes(Species, coste)) + geom_bar(stat="identity", 
                   aes(fill = Impacted_sector)) + theme_classic()



#heps fake

c<- expanded %>% group_by(Species) %>% summarise(Coste=sum(cost_bil))

ggplot(c, aes(Species,Coste)) +  geom_point( size=3, color="black") + 
  geom_segment(aes(xend=Species, yend=0))+ theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))



#Plot over time ----

expanded1<- expanded %>% filter(Environment=="Aquatic")
expanded2<- expanded %>% filter(Environment=="Terrestrial")

#Subsetting
expanded_obs <- expanded2[expanded2$Implementation %in% c("Observed"),]
sum(expanded_obs$cost_bil)

global.raw.all <- summarizeCosts(expanded2,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1966,
                                 maximum.year = 2020,
                                 year.breaks = seq(1965, 2020, by = 10))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded_obs,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1966,
                                 maximum.year = 2020,
                                 year.breaks = seq(1965, 2020, by = 10))
global.raw.obs$average.cost.per.period$middle.years <- global.raw.obs$average.cost.per.period$initial_year +
  (global.raw.obs$average.cost.per.period$final_year -
     global.raw.obs$average.cost.per.period$initial_year) / 2
global.raw.all$average.cost.per.period$middle.years <- global.raw.all$average.cost.per.period$initial_year +
  (global.raw.all$average.cost.per.period$final_year -
     global.raw.all$average.cost.per.period$initial_year) / 2
all.costs <- rbind(data.frame(global.raw.obs$average.cost.per.period,
                              cost.type = "Observed"),
                   data.frame(global.raw.all$average.cost.per.period,
                              cost.type = "All"))
all.costs.per.year <- rbind(data.frame(global.raw.obs$cost.per.year,
                                       cost.type = "Observed"),
                            data.frame(global.raw.all$cost.per.year,
                                       cost.type = "All"))
svg("fish_cum_all_vs_obs.svg",width=15,height=8)
ggplot(all.costs) +
  ylab(paste0("Annual cost in US$ millions")) +
  # Points
  geom_point(aes_string(x = "middle.years",
                        y = "annual_cost",
                        col = "cost.type"),
             shape = 15) +
  # Lines between points
  geom_line(aes_string(x = "middle.years",
                       y = "annual_cost",
                       col = "cost.type"),
            linetype = 2) +
  # Horizontal bars (year span)
  geom_segment(aes_string(x = "initial_year",
                          xend = "final_year",
                          y = "annual_cost",
                          yend = "annual_cost",
                          col = "cost.type")) +
  geom_point(data = all.costs.per.year,
             aes(x = year, y = cost,
                 size = number_estimates,
                 col = cost.type),
             alpha = .6) +
  xlab("Year") +
  scale_x_continuous(breaks = global.raw.obs$year.breaks) +
  scale_size_continuous(name = "Number of estimates\nper year",
                        breaks = c(2, 4, 8, 12)) +
  scale_color_brewer(name = "Cost estimations",
                     palette = "Dark2") + # Minimal theme
  scale_y_log10(breaks = 10^(-15:15), # y axis in log 10 with pretty labels
                labels = scales::comma) +
  annotation_logticks(sides = "l")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
dev.off()