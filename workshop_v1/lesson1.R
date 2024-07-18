library(readr)

df <- read_csv("R_training/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_State_Timeseries.csv")
View(df)
table(df$state)

sc <- df[df$state == "SC",]

hist(sc$hospital_onset_covid)

plot(sc$hospital_onset_covid)

# ggplot2
library(ggplot2)
ggplot(data=sc,mapping=aes(x=date,y=hospital_onset_covid)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

# remove the 12 AM thing
sc$date
library(tidyr)
sc <- separate(data = sc, col = date, into = c("date", "time"), sep=" ")
View(sc)

ggplot(data=sc,mapping=aes(x=date,y=hospital_onset_covid)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

# TODO: rotate x axis labels

# normalizing to validate visualization
# check coverage
hist(sc$hospital_onset_covid_coverage)

# outlier removal via inspection
sc <- sc[sc$hospital_onset_covid_coverage > 60,]

hist(sc$hospital_onset_covid_coverage)

ggplot(data=sc,mapping=aes(x=date,y=hospital_onset_covid)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

# homework
# cast as date, plot time series
sc$date <- as.Date(sc$date , format = "%m/%d/%Y")

ggplot(data=sc,mapping=aes(x=date,y=hospital_onset_covid)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

southeast <- df[df$state %in% c("SC","NC","KY","FL"),]
southeast <- southeast[southeast$hospital_onset_covid_coverage > 60,]
southeast <- separate(data = southeast, col = date, into = c("date", "time"), sep=" ")
southeast$date <- as.Date(southeast$date , format = "%m/%d/%Y")

ggplot(data=southeast,mapping=aes(x=date,y=hospital_onset_covid,color=state)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))

southeast$ratio <- southeast$hospital_onset_covid / southeast$hospital_onset_covid_coverage

ggplot(data=southeast,mapping=aes(x=date,y=ratio,color=state)) +
  geom_point() +
  theme(aspect.ratio = 9/16,axis.text=element_text(size=10))
