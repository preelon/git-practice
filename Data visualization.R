library(tidyverse)
library(lubridate)
case_data <- read_csv("data/training_case_data_long.csv")

#filtering out presumed cases in Amhara region
amhara_presumed_data<-case_data %>%
  filter(region=="Amhara", data_type=="presumed")

#using the ggplot() function and putting arguments
ggplot(data = amhara_presumed_data, mapping = aes(x=period,y=count)) +
  geom_point() #when i make the x axis just period-the graph appears same to z PDF
              #but in the PDG just year period was used-figure out how


#adding transparency
ggplot(data = amhara_presumed_data,mapping = aes(x=year(period),y=count)) +
  geom_point(alpha= 0.1) #alpha value less 1 gives transparency,helps to emphasize the important data

#adding color to the graph
ggplot(data = amhara_presumed_data,mapping = aes(x=period,y=count)) +
  geom_point(color="blue",alpha= 0.1)

#adding colors to specific woreda
ggplot(data = amhara_presumed_data,mapping = aes(x=period,y=count)) +
  geom_point(aes(color=woreda) ,alpha= 0.1) +
  theme(legend.position = "none")

#confirmed cases in abergele
Abergele_conf <- case_data %>%
  filter(woreda=="Abergele", data_type == "confirmed")

#scatter plot of Abergele_conf
ggplot(data = Abergele_conf, mapping = aes(x= period, y= count)) +
  geom_point()

#line graph of Abergele_conf
ggplot(data = Abergele_conf, mapping = aes(x= period, y= count)) +
  geom_line(color="purple", size=2) +
  geom_point(col="black", size=3)+
  aes(ymin=100)

#addin axis label and graph title
ggplot(data = Abergele_conf, mapping = aes(x= period, y= count)) +
  geom_line(color= "purple", size = 2) +
  aes(ymin=0)+
  geom_point(col="black", size=3)+
  labs(x="Year", y="confirmed malaria cases",
  title="confirmed malaria cases in Abergele Woreda from 2018 to 2022")

#Creating a table of all cases in Abergele woreda
Abergele_conf_chw<-case_data %>%
  filter(woreda=="Abergele", data_type%in% c("confirmed", "presumed"))
  
#creating a line graph for confirmed and presumed cases
ggplot(data=Abergele_conf_chw, mapping = aes(x= period, y= count)) +
  geom_line(aes(color=data_type))

#making the above graph look better
ggplot(data=Abergele_conf_chw, mapping = aes(x= period, y= count)) +
  geom_line(aes(color=data_type), size=1.2) +
  scale_color_manual(values = c("confirmed"="dodgerblue3","presumed"="olivedrab3"))+
  labs(x="year", y="confirmed & presumed malaria cases",
       title = "Confirmed and presumed malaria cases in Abergele (2018 - 2021)")

# confirmed cases over time in Addi Arekay woreda?
#3rd PDF Q-1a
Addi_Arekay_conf <-case_data %>%
  filter(woreda=="Addi Arekay", data_type=="confirmed")

  ggplot(data= Addi_Arekay_conf, mapping= aes(x= period, y= count)) +
  geom_line(color="darkblue", size= 1.2) +
    geom_point(color="red")+
  labs(x = "year", y = "confirmed malaria case", 
       title = "confirmed malaria cases in Addi Arekay woreda-2018-2022")
  
# confirmed and presumed cases over time in Addi Arekay woreda
# 3rd PDF Q-1b& 1c
  Addi_Arekay_conf_presumed <- case_data %>%
    filter(woreda=="Addi Arekay", data_type%in% c("confirmed", "presumed"))
 
   ggplot(data= Addi_Arekay_conf_presumed, mapping= aes(x=period, y=count)) +
    geom_line(aes(color=data_type), size=1) +
    labs(x="year", y="malaria cases",
         title = "confirmed and presumed malaria cases in Addi Arekay woreda (2018-2022)") +
    scale_color_manual(values = c("confirmed"="purple", "presumed"= "red"))

#creating a table for confirmed cases in Amhara region in jan 2020
Amhara_conf_2020 <- case_data %>%
  filter(region=="Amhara", period==ymd("2020-01-01"), data_type=="confirmed")

#plotting the above table with a filter of count >100
ggplot(data=filter(Amhara_conf_2020,count>100), mapping = aes(x= woreda, y= count)) +
  geom_col(color= "yellowgreen", size= 1.2) +
  labs(x="woreda", y="confirmed cases", 
       title = "confirmed cases in Amhara region", subtitle = "Jan, 2020")

#fliping the axises of the above graph
ggplot(data=filter(Amhara_conf_2020, count>100), mapping = aes(x= woreda, y= count)) +
  geom_col(color= "yellowgreen", size= 1.2) +
  labs(x="woreda", y="confirmed cases", 
       title = "confirmed cases in Amhara region", subtitle = "Jan, 2020") +
  coord_flip()

#reordering the woredas based on the number of confirmed cases
ggplot(data=filter(Amhara_conf_2020, count>100), mapping = aes(x= reorder(woreda, count), y= count)) +
  geom_col(color= "blue", size= 1.2) +
  labs(x="woreda", y="confirmed cases", 
       title = "confirmed cases in Amhara region", subtitle = "Jan, 2020") +
  coord_flip()

#data set for both confirmed and presumed cases in Amhara in jan, 2020
Amhara_conf_presumed_jan_2020 <- case_data %>%
  filter(region=="Amhara",count>100, data_type%in% c("confirmed", "presumed"), period==ymd("2020-01-01")) %>%
  group_by(woreda) %>%
  mutate(total_count=sum(count)) %>%
  ungroup()

#stack bar chart for the above data set
ggplot(data=Amhara_conf_presumed_jan_2020, aes(x= reorder(woreda, total_count), y=count, fill = data_type)) +
  geom_col(aes(color=data_type), size= 2) +
  scale_color_manual(values = c("confirmed"="tomato", "presumed"="darkgreen"))+
  labs(x="woreda", y="malaria cases", title="confirmed cases in health facilities", subtitle = "Jan.2020") +
  coord_flip() +
theme_classic()  #is order function working??-worked with mutating as written above
#saving the plot- why is it not working
ggsave(filename = "plot/amhara-cases-jan-2020.pdf", plot = stacked_bar_plot)

#confirmed cases in Somali region in Nov 2019
#3rd PDF, Q2A
Somali_conf_nov2019<-case_data %>%
  filter(region=="Somali", data_type=="confirmed", period==ymd("2019-11-1"))

ggplot(data = Somali_conf_nov2019, mapping = aes(x= reorder(woreda,count), y=count)) +
         geom_col() +
  coord_flip()+
  theme_classic()

#stacked bar plot of confirmed and presumed cases in Somali in nov 2019
#3rd PDF, Q2B
Somali_cases_2019 <- case_data %>%
  filter(region=="Somali", data_type %in% c("confirmed", "presumed"), period==ymd("2019-11-1"))

#column chart for 3rd PDF, Q2B
  ggplot(data=Somali_cases_2019, mapping = aes(x=reorder(woreda, count),y= count, fill = data_type )) +
  geom_col(aes(color=data_type)) +
  scale_color_manual(values = c("confirmed"="slateblue3", "presumed"= "violet")) +
  labs(x="woreda", y="malaria cases", 
       title = "malaria cases in Somali region", subtitle = "Nov. 2019" ) +
  theme_classic() +
  coord_flip()

#confirmed cases in Alfa woreda in 2020
#3rd PDF, Q2C
Alfa_conf_2020 <- case_data %>%
  filter(woreda=="Alfa", data_type=="confirmed", year(period)==2020)%>%
  group_by(region, woreda,month=month(period, label=TRUE))%>%
  summarise(total_confirmed=sum(count,na.rm = T))

ggplot(data=Alfa_conf_2020, mapping = aes(x= month, y=total_confirmed)) +
  geom_col()+
  labs(x="month", y="confirmed cases", 
       title = "confirmed cases in Alfa woreda", subtitle = "2020")


#presumed cases from each region each year
presumed_cases_each_region <- case_data %>%
  filter(data_type=="presumed") %>%
  group_by(region, period) %>%
  summarise(total_presumed=sum(count, na.rm = T))

#visualization for presumed cases from each region
ggplot(data=presumed_cases_each_region, mapping=aes(x=period, y=total_presumed))+
  geom_line(aes(color=region), size=1) +
  geom_point()+
  labs(x="Date", y="total presumed cases", title = "total Presumed cases in health facilities") +
  theme_classic()

#this doesn't work for me, the difference from above is the scale_colour_viridis_d part
ggplot(data=presumed_cases_each_region, mapping=aes(x=period, y=total_presumed))+
  geom_line() +
  geom_point()+
  labs(x="Date", y="total presumed cases", title = "total Presumed cases in health facilities") +
  scale_colour_viridis_d("region")+
  theme_classic()

#3rd PDF, Question 3A 
#cummulative montly cases for each region
cummulative_montly<- case_data %>%
  group_by(region, period) %>%
  summarise(cummulative_montly_case= sum(count,na.rm = T))

ggplot(data=cummulative_montly, mapping=aes(x=period, y=cummulative_montly_case))+
  geom_line(aes(color=region), size=1)+
  labs(x="Month", y="total malaria cases", title = "Cummulative mntly malaria cases in each region") +
  theme_classic()


#facet wrapping the presumed cases in each region
ggplot(data = presumed_cases_each_region,
       mapping = aes(x = period, y = total_presumed, color= region)) +
  facet_wrap(vars(region)) + #it helps to see splitted graphs based on the given criteria
  geom_line() +
  geom_point() +
  labs(y = "Total presumed cases", x = "Date",
       title = "Total presumed cases in health facilities") +
  scale_color_viridis_d("region") +
  theme_classic()

#3rd PDF 4th question--keep trying...
#annual totals for each woredas in amhara region 2018-2021
annual_total_amhara <- case_data %>%
  filter(year(period)==c(2018:2021), region=="Amhara", count>500) %>%
  group_by(woreda, data_type,year=year(period)) %>%
  summarize(annual_total = sum(count, na.rm = TRUE))

# Create a stacked bar plot with facets for each woreda in Amhara
ggplot(data = annual_total_amhara, mapping =aes (x = year, y = annual_total, fill = data_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Annual Total Malaria Cases", 
       title = "Annual Malaria Cases by Woreda", 
       subtitle = "Stacked by Case Type (2018-2021)") +
  scale_fill_manual(values = c("confirmed" = "orange", "presumed" = "purple")) +
  facet_wrap(~woreda, scales = "free")# Create a facet for each woreda
  coord_flip()+ 
  theme_classic() 
 
#to save the plot
  ggsave(filename = "plot/stack.tiff",
         width = 9, height = 9, compression = "lzw")
  
ggplot(Hudet_confirmed_2020, aes(x = confirmed)) +
    geom_histogram(binwidth = 5)