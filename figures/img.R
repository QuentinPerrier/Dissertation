library(xlsx); library(ggplot2); library(dplyr); library(tidyr)


#1 LCOE Bar plot
#2 Demand graph
#3 Onshore historical
#4 Nuclear historical construction
#5 DNTE Share of nuclear


###############
# LCOE Bar plot
###############

LCOE <- read.xlsx("img.xlsx",sheetName="LCOE_barplot")

ggplot(LCOE,aes(x=reorder(Technology,LCOE),y=LCOE,fill=reorder(Source,LCOE))) + 
  geom_bar(stat="identity",position = "dodge") + theme_bw() 

ggplot(LCOE,aes(x=reorder(Technology,LCOE),y=LCOE,fill=reorder(Source,LCOE))) + 
  geom_bar(stat="identity",position = "dodge") + theme_bw() 

ggplot(LCOE,aes(x=reorder(Source,LCOE),y=LCOE)) + #,fill=reorder(Source,LCOE)
  geom_bar(stat="identity",position = "dodge") + theme_bw() +
  facet_wrap(~Technology,scales="free_x") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text=element_text(size=12),
        strip.text = element_text(size=15)) +
  labs(x="",y="LCOE (€/MWh)")

#ggsave("LCOE_barplot.png",width=18,height=18,units=("cm"))


################
# Demand graph
################

D <- read.xlsx("img.xlsx",sheetName="Demand") %>% 
  select(Year,DEC,DIV,EFF,SOB) %>%
  mutate(Year=as.numeric(substring(Year,2))) %>%
  gather(Scenario,Demand,2:5) %>%
  mutate(Demand=Demand/10^6)


for (i in 1:nrow(D)) {
  if (D$Scenario[i]=="SOB") D$Scenario[i] <- "D1 (SOB)"
  if (D$Scenario[i]=="EFF") D$Scenario[i] <- "D2 (EFF)"
  if (D$Scenario[i]=="DIV") D$Scenario[i] <- "D3 (DIV)"
  if (D$Scenario[i]=="DEC") D$Scenario[i] <- "D4 (DEC)"
}
names(D)[2] <- "Demand"
names(D)[3] <- "Value"

ggplot(D,aes(x=Year,y=Value,group=Demand,colour=Demand))+geom_line(aes(linetype=Demand),size=1.8)+
  theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
                     legend.text=element_text(size=16),legend.title=element_text(size=16)) +
  labs(y="Demand (TWh)") +  theme(legend.title=element_blank(),legend.position="bottom")
  
#ggsave("Demand.png")


####################
# Onshore historical
####################

Y <- read.csv2("../data/Enerdata_Wind_capacities.csv") %>% 
  mutate(Investment=(Value-lag(Value,k=1))/1000) %>%
  filter(Year>1999) %>%
  filter(ISO.code %in% c("FR","DE","DK","IT","SP","GB","NL"))

Onshore <- Y %>% filter(Item.code=="eeomwon") %>%
  ggplot(aes(x=Year,y=Investment,group=ISO.code,colour=ISO.code)) + 
  geom_line() + theme_bw() + labs(y="Investment (GW)"); 
Onshore
ggsave("../img/Plot_onshore_deployment.png");

Z <- Y %>% filter(Item.code=="eeomwon" & ISO.code=="DE")

#######################
# Nuclear construction
#######################

D <- read.csv2("../data/NuclearPlants.csv")

y0 <- min(Y$Year)
yf <- max(Y$Year)
df <- data.frame(Year=y0:yf,Construction=0)

Annual_construction <- function(y) {
  R <- D %>% filter(Year==y) %>% .$Pcn..MW. %>% sum()
  return(R)
}

df <- D %>% rowwise() %>%
  mutate(Historical=Annual_construction(Year)/1000)
M <- df %>% filter(Year %in% 1980:1990) %>% .$Historical %>% mean()
df$Average <- M
df <- df %>% gather(Scenario,Capacity,Historical:Average) %>% 
  mutate(Scenario = factor(Scenario)) %>% 
  mutate(Scenario=factor(Scenario,labels=c("Average 1980-1990","Historical")))

P <- df %>% ggplot(aes(x=Year,y=Capacity,group=Scenario,colour=Scenario)) + geom_line() +theme_bw()+ labs(y="Annual Construction (GW)"); P
ggsave("../img/Plot_Historical_nuclear_construction.png")


##################
# 5 DNTE shares of nuclear
##################

df <- read.xlsx("img.xlsx",sheetName="NukeShare") 

for (i in 1:length(names(df))) {
  if (names(df)[i]=="SOB") names(df)[i] <- "S1 (SOB)"
  if (names(df)[i]=="EFF") names(df)[i] <- "S2 (EFF)"
  if (names(df)[i]=="DIV") names(df)[i] <- "S3 (DIV)"
  if (names(df)[i]=="DEC") names(df)[i] <- "S4 (DEC)"
}
g <- df %>% gather(Scenario,Share,2:5) %>% mutate(Share=Share*100) %>%
  ggplot(aes(x=Year,y=Share,colour=Scenario)) + geom_line(aes(linetype=Scenario),size=2) + 
  theme_bw() + theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
                     legend.text=element_text(size=16),legend.title=element_text(size=16)) + 
  labs(y="Nuclear share in the power mix (%)") +  theme(legend.title=element_blank(),legend.position="bottom")
g
#ggsave("DnteShares.png",g)

