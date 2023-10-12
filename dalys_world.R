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
world0=map_data("world")
#ggplot(world,aes(x=long,y=lat,group=group))+geom_polygon(color="white",fill="tomato")+coord_quickmap()
world=world0%>%rename("country"=region)%>%select(-subregion)%>%mutate(country=ifelse(country=="USA","United States",country))

#y=any(world$country == "United States")

#z=df%>%arrange(desc(country))

#datWorld=left_join(df,world,by="country")%>%select(long,lat,group,order,country,region,lifespan,noncomm)

datWorld=full_join(df,world,by="country")%>%select(long,lat,group,order,country,region,lifespan,injury,comm,noncomm)

library(RColorBrewer)

ggplot(datWorld,aes(x=long,y=lat,group=group,fill=lifespan))+geom_polygon(color="white")+scale_fill_distiller(palette="Spectral",direction=1)+coord_quickmap()+theme_classic()+theme_void()

datWorld2=datWorld%>%mutate(totDaly=injury+comm+noncomm)
View(datWorld2)

ggplot(datWorld2,aes(x=long,y=lat,group=group,fill=totDaly))+geom_polygon(color="white")+scale_fill_distiller(palette="Spectral",direction=-1)+coord_quickmap()+theme_classic()+theme_void()


