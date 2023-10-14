library(readr)
library(dplyr)

# cleaning data 
data <- read_csv("life expectancy.csv")

# remove corruption file, which has the most NAs 
rm_corruption <- data[-c(12)]

# rename all columns 
df <- rm_corruption %>% 
  rename( "country" = "Country Name",
          "code" = "Country Code",
          "region" = "Region", 
          "inc" = "IncomeGroup",
          "year" = "Year",
          "lifespan" = "Life Expectancy World Bank",
          "starving" = "Prevelance of Undernourishment",
          "health" = "Health Expenditure %",
          "edu" = "Education Expenditure %",
          "unemp" = "Unemployment",
          "sanitation" = "Sanitation",
          "injury" = "Injuries",
          "comm" = "Communicable",
          "noncomm" = "NonCommunicable"
  )

View(df)

library(ggplot2)
library(RColorBrewer)
world0=map_data("world")

world=world0%>%rename("country"=region)%>%select(-subregion)%>%mutate(country=ifelse(country=="USA","United States",country))

datWorld=full_join(df,world,by="country")%>%select(long,lat,group,order,country,region,lifespan,injury,comm,noncomm,year)%>%filter(year==2019)

# LIFESPAN PLOT 
ggplot(datWorld,aes(x=long,y=lat,group=group,fill=lifespan))+geom_polygon(color="white")+scale_fill_distiller(palette="Spectral",direction=1)+coord_quickmap()+theme_classic()+theme_void()+labs(title = "Lifespan by Country", fill = "Total DALYs")+theme(plot.title = element_text(hjust = 0.5))


datWorld2=datWorld%>%filter(year==2019)%>%mutate(totDaly=((injury+comm+noncomm)))
View(datWorld2)

# DALYS PLOT 
ggplot(datWorld2,aes(x=long,y=lat,group=group,fill=totDaly))+geom_polygon(color="white")+scale_fill_distiller(palette="Spectral",direction=-1)+coord_quickmap()+theme_classic()+theme_void()+labs(title = "Total DALYs by Country (Injury, Comm, Noncomm)", fill = "Total DALYs")+theme(plot.title = element_text(hjust = 0.5))


