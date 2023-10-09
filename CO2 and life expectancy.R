# DS 2003 project
# Answer the question: relationship between CO2 and life expectancy 
# CO2 = carbon dioxide emissions by kilotons. These come from the burning of fossil fuels and the making of cement
# Life expectancy = number of years that a newborn baby will live 


# load libraries 
library(ggplot2)
library(dplyr)
library(ggwordcloud)


# cleaning the data
data <- read.csv("/Users/daphnepfoser/DS2003project/life expectancy.csv") 

# remove the corruption file, since it has all the NAs 
rm_corruption <- data[-c(12)]

# rename all the columns 
df <- rm_corruption %>% 
  rename( "country" = "Country.Name",
          "code" = "Country.Code",
          "region" = "Region", 
          "inc" = "IncomeGroup",
          "year" = "Year",
          "lifespan" = "Life.Expectancy.World.Bank",
          "starving" = "Prevelance.of.Undernourishment",
          "health" = "Health.Expenditure..",
          "edu" = "Education.Expenditure..",
          "unemp" = "Unemployment",
          "sanitation" = "Sanitation",
          "injury" = "Injuries",
          "comm" = "Communicable",
          "noncomm" = "NonCommunicable"
  )


# CO2 and lifespan graph:GRAPH 
ggplot(df,aes(x=CO2, y=lifespan, fill=CO2, size=lifespan))+geom_point(shape =21)+scale_fill_viridis_c(option = "turbo")+
labs(title = "CO2 Emissions and Lifespan Relationship")+theme(plot.title = element_text(hjust = 0.5))

# Average lifespan:DESCRIPTIVE STATISTICS - center of the data
df%>% summarise(average_life = mean(lifespan, na.rm = TRUE))
# average lifespan is 69.74
df%>%summarise(median_life=median(lifespan, na.rm=TRUE))
# median lifespan is 72.16 = 
# We observed that regions with higher co2 emissions do not regularly have lifespans of less than 72 years 
# and also regions with lower co2 emissions do not regularly have lifespans of more than 72 years. 

# CO2 emissions :DESCRIPTIVE STATISTICS - center of the data
df%>%summarise(average_co2_emissions=mean(CO2, na.rm=TRUE))
# average co2 emissions is 157492.4 


# Highest CO2 emissions and life span:
highest_CO2<-df%>%filter(CO2>=5.0e+06)
# choose only the country, lifespan, and co2 emissions columns: 
Emissions_and_health<-df%>%select(country,lifespan,CO2)

# make a world cloud to see the co2 emissions by country:
# Because I used the df that has multiple sets for each country to make the CO2_emissions
# I Had to combine the total emissions for each country: 
CO2_combined <- Emissions_and_health %>%
  group_by(country) %>%
  summarize(CO2_total = sum(CO2, na.rm = TRUE))

# making the word cloud:
ggplot(CO2_combined, aes(label = country, size =CO2_total, colour=CO2_total)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 32)
# size of each country's name is proportional to the CO2 emissions. 


# can't say that the highest the emissions the lower the lifespan. 
# Both US and China have high emissions and lifespan(larger than the average). This could be because both countries
# have gone through the industrialization process, they both rely heavily on coal for energy production which emitts lots of CO2
# and both have lot's of cars on their roads. For the life span,they both have advanced healthcare services, education about health, and clean water and food 



# Lowest CO2 emissions and lifespan:
lowest_CO2<-df%>%filter(CO2<=2.5e+06) 

# can't say that the lowest the emissions the higher the lifespan (low emissions and lifespan ex)
# There are some countries like Zimbabwe that have a lifespan of 44 and CO2 of 13,900 which actually is the dot 


# BUT....... 
# we can see that the highest life span is at age 84 when the CO2 emissions are 2,500,000 

# SOMEHOW SAY THAT THE AVERAGE LIFESPAN IS 70. 
# we would like to think that that higher CO2 emissions would lead to a lower lifespan, but as we look at the graph
# we can see that this is not the case. One example of this would be the red dots with 50,000,000 to 100,000,000 emissions 
# that are the United States and China. These two countries also have higher life spans of 73-78. 

# Another logical conclusion is that lower emissions would lead to higher lifespans but as we can see on the graph
# there are a lot of points that are below the average lifespan who are in the 0 emissions column. This means that not
# all countries who have lower emissions will have a higher lifespan. An example of this is Zimbabwe that
# has a lifespan of 44 and CO2 of 13,900 This could be because of other factors: dirty water, no food, and no medical facilities and doctors. 
