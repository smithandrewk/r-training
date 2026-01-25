library(readxl)
data <- read_excel("data.xlsx")
View(data)

# how clean is the data?
# what are the data types?
# unnecessary columns?
# data cleaning, standardize factors for columns
closed_completed = "Yes"

df <- data[c("Interaction Date","Region","Setting Name","Setting Type",'Initiation of Interaction')]

library(ggplot2)
ggplot(df,aes(x=Region)) + geom_bar()

colnames(df)[3] <- "Setting"
colnames(df)[1] <- "Interaction_Date"
colnames(df)[4] <- "Setting_Type"
colnames(df)[5] <- "Initiation_of_Interaction"

ggplot(df,aes(x=Setting)) + geom_bar()
#  Has our proactive outreach increased over time?
#  Try to plot interaction date over time, first guess is histogram
ggplot(df,aes(x=Interaction_Date)) + geom_histogram(bins=50)

#  Has it increased overtime by setting type or specific setting name?
ggplot(df,aes(x=Interaction_Date,color=Setting_Type)) + geom_histogram(bins=50)

ggplot(df,aes(x=Interaction_Date,color=Setting_Type)) + geom_histogram(bins=50)
ggplot(df,aes(x=Interaction_Date,color=Setting_Type)) + geom_histogram(bins=50)
ggplot(df,aes(x=Interaction_Date,color=Setting_Type)) + geom_density(adjust=.6)
ggplot(df,aes(x=Interaction_Date,color=Setting_Type,fill=Setting_Type)) + geom_histogram(bins=50,position='fill')


#  Display the total number of proactive and reactive.
table(data['Initiation of Interaction'])
ggplot(df,aes(x=Initiation_of_Interaction)) + geom_bar()

make.names(tolower(colnames(df)))

replace(".","_")
#  Which setting names had the most number of proactive and reactive interactions?
df %>% group_by(Initiation_of_Interaction) %>% summarise(total=sum(Setting)) # similar to pivot table in excel


#  What are the public health benefits/interventions?

#  Has outreach effected outbreaks over time?
