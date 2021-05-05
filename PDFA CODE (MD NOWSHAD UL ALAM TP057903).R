
#load data file 
car_description = read.csv(file ="C:/Users/Nawshad/Documents/4. data.csv")
view(car_description)

#library needed
library(plyr) 
library(tidyverse)  
library(ggplot2)

library(dplyr)

library(ggthemes)

library(mice)

#total missing value.

md.pattern(car_description)

#to check missing Value
missing=function(data,variable)
{
  count=sum(is.na(data[variable]))
  print(paste("there is",count,"missing value",names(data[variable])))
}
missing(car_description,29)


#summary of data file
summary(car_description)




#which type of transmission type is suitable for environment?
#to see that we need co2 emmission,nitrogent emission and noisy level of the transmission type?

#mean of noise level
meanNoise<-mean(car_description$noise_level,na.rm = TRUE)

#mean of co emmision
meanCoEmmission<-mean(car_description$noise_level,na.rm = TRUE)

#to see records in electricity
environmentFriendly <- car_description %>% 
  select(transmission_type,co_emissions,co2,noise_level,nox_emissions,fuel_type) %>%
  filter(fuel_type == "Electricity") %>% view()


#number of transmission type and which is greater.
totalNumberOfTransmission = car_description %>%select(transmission_type)%>% count()
print(paste("Number of records in transmission","= ",totalNumberOfTransmission))


totalNumberOfManual = car_description %>%select(transmission_type)%>% 
  filter(transmission_type =="Manual")%>%count()

print(paste("Number of records on Manual"," = ",totalNumberOfManual))


#cleaning and filling missing data with most repited value and mean



environmentFriendly <- car_description %>% 
  select(transmission_type,co_emissions,co2,noise_level,nox_emissions,fuel_type) %>%
  mutate(transmission_type = if_else(transmission_type == "","Manual",
  recode(transmission_type,Manual = "Manual",Automatic= "Automatic")))%>% 
  
  mutate(transmission_type = if_else(transmission_type == "N/A","Electric Vehicle",
  recode(transmission_type,Manual = "MANUAL",Automatic= "AUTOMATIC"))) %>%
  
  mutate(co_emissions = if_else(fuel_type == "Electricity",0,
  replace_na(co_emissions,meanCoEmmission)))%>%
  
  mutate(nox_emissions = replace_na(nox_emissions,0))%>%
  mutate(noise_level = replace_na(noise_level,meanNoise))



View(environmentFriendly)
  
  

  #noise level in graph
  environmentFriendly %>% 
    ggplot(aes(x = transmission_type,y = noise_level,)) +
    geom_bar(
      stat="identity",
      width=0.5,
      color ="blue")
  
  
  #co2 emmission and noise level 
  
  
  environmentFriendly %>% 
    ggplot(aes(noise_level,co2)) + 
    geom_point(aes(col = transmission_type)) +
    theme_minimal()
  
  
  #notrogen oxide emmission
  p = ggplot(environmentFriendly, aes(x = transmission_type,
    y = nox_emissions, fill = transmission_type)) +
    geom_bar(stat="identity") + 
    theme_minimal()
  p

  
  #co emmission
  q = ggplot(environmentFriendly, aes(x = transmission_type,
                                      y = co_emissions, fill = transmission_type)) +
    geom_bar(stat="identity") + 
    theme_minimal()
  q


  
  
  #Which type of car is less costly
  #to fill the missing value with mean
  meanOfFuelCost6000miles = mean(car_description$fuel_cost_6000_miles,na.rm = TRUE)
  meanOfFuelCost12000miles = mean(car_description$fuel_cost_12000_miles,na.rm = TRUE)
  
  
  
  #cleaning
  costOfCar <- car_description %>% 
    select(transmission_type,fuel_cost_12000_miles,fuel_cost_6000_miles,
           engine_capacity,fuel_type) %>%
    
    mutate(fuel_cost_6000_miles = replace_na(fuel_cost_6000_miles,
    meanOfFuelCost6000miles)) %>%
    
    
    mutate(fuel_cost_12000_miles = replace_na(fuel_cost_12000_miles,
    meanOfFuelCost12000miles)) %>%
    
    mutate(engine_capacity = replace_na(engine_capacity,
    mean(car_description$engine_capacity,na.rm = TRUE))) %>%
    
    mutate(transmission_type = if_else(transmission_type == "","Manual",
    recode(transmission_type,Manual = "Manual",Automatic= "Automatic")))%>% 
    mutate(transmission_type = if_else(transmission_type == "N/A","Electric Vehicle",
    recode(transmission_type,Manual = "MANUAL",Automatic= "AUTOMATIC"))) %>% view()
  
  
  
  #cost in 12000mile
  
  costOfCar %>%
    ggplot(aes(fuel_cost_12000_miles, fill =fuel_type )) + 
    geom_histogram(bins = 10)
  #cost in 12000mile saperately
  costOfCar %>%
    ggplot(aes(fuel_cost_12000_miles, fill =fuel_type )) + 
    facet_wrap(~transmission_type)+
    geom_histogram(bins = 10)
  
  
  #cost in 6000mile
  
  costOfCar %>%
    ggplot(aes(fuel_cost_6000_miles, fill =fuel_type )) + 
    geom_histogram(bins = 10)
  #cost in 6000mile saperately
  costOfCar %>%
    ggplot(aes(fuel_cost_6000_miles, fill =fuel_type )) + 
    facet_wrap(~transmission_type)+
    geom_histogram(bins = 10)
  
  
  
  
  
  #which year has more carboon emission?
  #to see this we need to know year, co_emmision and co2
  
  Carbon_Emission_in_Year <- car_description %>% 
    select(year,co2,co_emissions,nox_emissions,fuel_type) %>%
    mutate(co_emissions = if_else(fuel_type == "Electricity",0,
    replace_na(co_emissions,meanCoEmmission)))
  
  
  #CO2 emissions in grammes per kilometre (g/km)."
  
  #description": "Carbon monoxide emissions in milligrammes per kilometre (mg/km)
  
  
  Carbon_Emission_in_Year  %>% 
    ggplot(aes(year,fill = co_emissions)) + 
    geom_bar() + 
    labs(title = "Carbion Monoxide emissions in milligramms per kilometre in Year",
         width=0.5,
         x = "year", 
         y = "co_emmision")  +
    theme_minimal()
  
  Carbon_Emission_in_Year %>% 
    ggplot(aes(year,fill = co2)) + 
    geom_bar() + 
    labs(title = "Bar chart for co_emmision in grammes per kilometre (g/km) year",
         width=0.5,
         x = "number of year", 
         y = "co2 emmision")  +
    theme_minimal()
  

  
  
  #which type of fuel most of the car use and which fuel demand in increasing?
  
  
  
  fuelDemanByYear <- car_description %>% 
    select(year,fuel_type)
  
  
  fuelDemanByYear  %>%
    ggplot(aes(fill = fuel_type,year)) +
    geom_histogram(bins = 30) + facet_wrap(~fuel_type) +
    labs(title = "",
         x = "Number of years", 
         y = "number of car use fuel")  +
    theme_minimal()
  
  
  fuelDemanByYear  %>%
    ggplot(aes(fill = fuel_type,year)) +
    geom_histogram(bins = 30) + 
    labs(title = "",
         x = "Number of years", 
         y = "number of car use fuel")  +
    theme_minimal()
  
  
  totalNumberOfFuel = car_description %>%select(fuel_type)%>% count()
  print(paste("Number of records","= ",totalNumberOfFuel))
  
  
  totalNumberOfPetrolFuel = car_description %>%select(fuel_type)%>% 
    filter(fuel_type == "Petrol")%>%count()
  print(paste("Number of records on petrol","= ",totalNumberOfPetrolFuel))
  
  
  
  #########################################################
  #in which month of 2012 cost of fuel was high in driving per mile
  
  summary(car_description$transmission_type)
  
  FuelCostIn2012 <- car_description %>% 
    select(year,fuel_cost_12000_miles) %>%
    filter(year == "2012") %>% 
    mutate(fuel_cost_12000_miles = 
             replace_na(fuel_cost_12000_miles,meanOfFuel_cost_1200_miles)) %>%
    mutate(costPerMile = fuel_cost_12000_miles/12000)
  view(FuelCostIn2012)
  
  
  
  
  view(FuelCostIn2012)
  
  ts.plot(CostPerMileIn2012, ylab = "costperMile",xlab = "Number of months")
  CostPerMileIn2012 = ts(data = FuelCostIn2012$costPerMile,start = 1,end = 12)
  
  
  
  ##########################################
  
  #Which transmission type has more engine capacity and also emits more co2?
  
  engine_Capacity_With_No_Value<- car_description %>% 
    select(engine_capacity,transmission_type,co2,fuel_type)%>%
    filter(fuel_type == "Electricity") %>% view()
  
  
  
  transmissionTypeOnEngine<- car_description %>% 
    select(engine_capacity,transmission_type,co2) %>%
    mutate(engine_capacity = replace_na(engine_capacity,0))%>%  
    mutate(transmission_type = if_else(transmission_type == "","Manual",                                                                                            recode(transmission_type,Manual = "Manual",Automatic= "Automatic")))%>% 
    mutate(transmission_type = if_else(transmission_type == "N/A","Electric Vehicle",
    recode(transmission_type,Manual = "MANUAL",Automatic= "AUTOMATIC")))
  
  
  #visualization
  
  transmissionTypeOnEngine %>% 
    ggplot(aes(engine_capacity,co2)) + 
    geom_jitter(aes(col = transmission_type)) +
    facet_wrap(~transmission_type) +
    theme_minimal()
  
  
  ggplot(data = transmissionTypeOnEngine, 
         mapping = aes(x = engine_capacity, y = transmission_type)) +
    geom_boxplot()
  
  
  
  
  #Relationship between year and euro standard
  ggplot(data=car_description, aes(x=euro_standard, y=year)) +
    geom_line(linetype="dashed", color="blue", size=1.2)+
    geom_point(color="red", size=3)
  
  
  
  
  
  
  






















