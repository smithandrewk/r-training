library(readr)
library(ggplot2)

df <- read_csv("/Users/andrew/R_training/VSRR_Provisional_Drug_Overdose_Death_Counts.csv")

View(df)
unique(df$State)
unique(df$Year)
unique(df$Month)
unique(df$Indicator)

sc <- df[df$State == "SC",]

total <- sc[sc$Indicator == "Number of Deaths",]
od <- sc[sc$Indicator == "Number of Drug Overdose Deaths",]

# ggplot(data=total,mapping=aes(x=Month,y=`Data Value`)) +
#  geom_point() +
#  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

# tempermental date function
# as.Date("2015-April-01",format="%Y-%B-%d")

total$date <- as.Date(with(total,paste(Year,Month,"01",sep="-")),format="%Y-%B-%d")
od$date <- as.Date(with(od,paste(Year,Month,"01",sep="-")),format="%Y-%B-%d")

ggplot(data=total,mapping=aes(x=date,y=`Data Value`)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

ggplot(data=od,mapping=aes(x=date,y=`Data Value`)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

proportion <- sc[sc$Indicator == "Number of Drug Overdose Deaths",]
proportion$`Data Value` <- od$`Data Value` / total$`Data Value`
proportion$Indicator <- "Overdose Death Proportion"
proportion$date <- as.Date(with(proportion,paste(Year,Month,"01",sep="-")),format="%Y-%B-%d")
ggplot(data=proportion,mapping=aes(x=date,y=`Data Value`)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))


total <- df[df$Indicator == "Number of Deaths",]
total$date <- as.Date(with(total,paste(Year,Month,"01",sep="-")),format="%Y-%B-%d")
total_aggregation <- aggregate(`Data Value` ~ date,FUN=sum,data=total)
ggplot(data=total_aggregation,mapping=aes(x=date,y=`Data Value`)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

od <- df[df$Indicator == "Number of Drug Overdose Deaths",]
od$date <- as.Date(with(od,paste(Year,Month,"01",sep="-")),format="%Y-%B-%d")
od_aggregation <- aggregate(`Data Value` ~ date,FUN=sum,data=od)
ggplot(data=od_aggregation,mapping=aes(x=date,y=`Data Value`)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

proportion <- df[df$Indicator == "Number of Drug Overdose Deaths",]
proportion$date <- as.Date(with(proportion,paste(Year,Month,"01",sep="-")),format="%Y-%B-%d")
proportion_aggregation <- aggregate(`Data Value` ~ date,FUN=sum,data=proportion)
proportion_aggregation$`Data Value` <- od_aggregation$`Data Value` / total_aggregation$`Data Value`

ggplot(data=proportion_aggregation,mapping=aes(x=date,y=`Data Value`)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

###########################
state <- aggregate(`Data Value` ~ State,FUN=sum,data=od)
colnames(state)[1] <- "state"

library(usmap)
party_colors <- c("#44637F", "#F8A65D") 

plot_usmap(data = state, values = "Data Value") + 
  scale_fill_binned(low = "#FDC079", high = "#44637F") +
  theme(legend.position = "right")

















