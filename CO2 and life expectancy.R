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
df_rename <- rm_corruption %>% 
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

df <- df_rename %>%
  group_by(region,year) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# CO2 and lifespan graph:GRAPH 
ggplot(df,aes(x=CO2, y=lifespan, fill=CO2, size=lifespan))+geom_point(shape =21)+scale_fill_viridis_c(option = "turbo")+
labs(title = "CO2 Emissions and Lifespan Relationship")+theme(plot.title = element_text(hjust = 0.5))

# Average lifespan:OVERALL 
df_rename%>% summarise(average_life = mean(lifespan, na.rm = TRUE))
# average lifespan is 69.74 years 

df_rename%>%summarise(average_co2_emissions=mean(CO2, na.rm=TRUE))
# average co2 emissions is 157,492 kilotons 



# Highest CO2 emissions and life span:
highest_CO2<-df_rename%>%filter(CO2>=5.0e+06)
# choose only the country, lifespan, and co2 emissions columns: 
Emissions_and_health<-df_rename%>%select(country,lifespan,CO2)

# average co2  emissions for US: 
US_data<-df_rename[df_rename$country=="United States",]
US_data%>%summarise(US_co2=mean(CO2, na.rm = TRUE))
# average co2 emissions is 5,306,051 kilotons

# average lifespan for US:
US_avg_lifespan<-mean(US_data$lifespan)
# average lifespan for US: 78


# co2 emissions by country wordcloud:
# Because I used the df that has multiple sets for each country to make the CO2_emissions
# I had to combine the total emissions for each country: 
CO2_combined <- Emissions_and_health %>%
  group_by(country) %>%
  summarize(CO2_total = sum(CO2, na.rm = TRUE))

# making the word cloud:
ggplot(CO2_combined, aes(label = country, size =CO2_total, colour=CO2_total)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 32)+ labs(title = "C02 Emissions by Country ")+
  theme(plot.title = element_text(hjust = 0.5))
# size of each country's name is proportional to the CO2 emissions. 


# Lowest CO2 emissions and lifespan:
lowest_CO2<-df_rename%>%filter(CO2<=2.5e+06) 

# average co2 emissions of Zambia:
Zambia_data<-df_rename[df_rename$country=="Zambia",]
Zambia_data%>%summarise(ZMB_co2= mean(CO2, na.rm=TRUE))
# average co2 emissions of Zimbabwe: 3,651  kilotons 

# average lifespan for Zambia:
ZMB_avg_lifespan<-mean(Zambia_data$lifespan)
# average lifespan for Zambia is 55  


