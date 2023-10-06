# DS 2003 project
# Answer the question: relationship between CO2 and life expectancy 
# CO2 = emissions stemming from the burning of fossil fuels and the making of cement 
# Life expectancy = number of years that a newborn baby will live 



# load libraries 
library(ggplot2)
library(dplyr)


# TODO: 
# 1. descriptive statistics to summarize variables: tables and data visualization 
# 2. at least 1 one graph to show answer and can have additional if required - DONE 

 
# CO2 and lifespan graph:GRAPH 
ggplot(df, aes(x=CO2, y=lifespan, fill=CO2, size=lifespan))+geom_point(shape =21)+scale_fill_viridis_c(option = "turbo")
+labs(title = "Relationship Between CO2 Emissions and Lifespan")

# Average lifespan:DESCRIPTIVE STATISTICS 
df %>% summarise(average_life = mean(lifespan, na.rm = TRUE))
# average lifespan is 69.74


# DO THE HIGHEST CO2 EMISSIONS AND LIFE SPAN
highest_CO2<-df%>%filter(CO2>=5.0e+06) 



# can't say that the highest the emissions the lower the lifespan. 
# Both US and China have high emissions and lifespan(larger than the average). This could be because both countries
# have gone through the industrialization process, they both rely heavily on coal for energy production which emitts lots of CO2
# and both have lot's of cars on their roads. For the life span,they both have advanced healthcare services, education about health, and clean water and food 




# DO THE LOWEST CO2 EMISSIONS AND LIFE SPAN 
lowest_CO2<-df%>%filter(CO2<=2.5e+06) 

# can't say that the lowest the emissions the higher the lifespan (low emissions and lifespn ex)
# There are some countries like Sierra Leone that have a lifespan of 40 and CO2 of 440 which actually is the dot 


# BUT....... 
# we can see that the highest life span is at age 84 when the CO2 emissions are 2,500,000 

# SOMEHOW SAY THAT THE AVERAGE LIFESPAN IS 70. 
# we would like to think that that higher CO2 emissions would lead to a lower lifespan, but as we look at the graph
# we can see that this is not the case. One example of this would be the red dots with 50,000,000 to 100,000,000 emissions 
# that are the United States and China. These two countries also have higher life spans of 73-78. 

# Another logical conclusion is that lower emissions would lead to higher lifespans but as we can see on the graph
# there are a lot of points that are below the average lifespan who are in the 0 emissions column. This means that not
# all countries who have lower emissions will have a higher lifespan. An example of this is Sierra Leone that
# has a lifespan of 40 and CO2 of 440. This could be because of other factors: dirty water, no food, and no medical facilities and doctors. 
