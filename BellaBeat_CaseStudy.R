#Prepare the data
install.packages("rmarkdown")
install.packages("ggplot2")
library(readr)
library(dplyr)
library(ggplot2)
library(knitr)
library(lubridate)

setwd("/Users/tusharrao/Documents/CaseStudy_BellaBeat/Fitabase_Data_4.12.16-5.12.16")
daily_activity <- read.csv("dailyActivity_merged.csv")
weight_loss <- read.csv("weightLogInfo_merged.csv")
daily_intensity <- read.csv("dailyIntensities_merged.csv")
daily_calorie <- read.csv("dailyCalories_merged.csv")
daily_steps <- read.csv("dailySteps_merged.csv")
daily_sleep <- read.csv("sleepDay_merged.csv")

head(daily_activity)
head(daily_sleep)

#Checking for NA 
sum(is.na(daily_activity)) 
sum(is.na(weight_loss)) # 65 NA available
sum(is.na(daily_intensity))
sum(is.na(daily_calorie))
sum(is.na(daily_steps))
sum(is.na(daily_sleep))

#Finding unique values
unique(daily_activity$Id) # 33 unique users also n_distinct(daily_activity$Id)
unique(weight_loss$Id)  # 8 unique users
unique(daily_intensity$Id) # 33 unique users
unique(daily_calorie$Id) # 33 unique users
unique(daily_steps$Id) # 33 unique users
unique(daily_sleep$Id) # 24 unique users



# Checking for the average 
daily_activity %>% 
  select(TotalSteps, TotalDistance, Calories, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, SedentaryMinutes) %>% 
  summary() # We can see that the average steps taken by the given users are 7638 and the average calorie is 2304

#Checking if all the users are in daily activity from other table

daily_activity_2 <- daily_activity %>% 
  select(Id, ActivityDate, Calories)
sql_check_calorie <- sqldf('Select * from daily_activity_2 Intersect Select * from daily_calorie')
nrow(daily_activity)
nrow(sql_check_calorie)

daily_activity_3 <- daily_activity %>% 
  select(Id, ActivityDate, TotalSteps)
sql_check_steps <- sqldf('Select * from daily_activity_3 intersect select * from daily_steps')
nrow(daily_activity)
nrow(sql_check_steps)

# looks like we can only use the daily activity table from now on instead of calories and TotalSteps table


  ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories, color = Id)) + geom_point()  + stat_smooth(method = lm)+ labs(title = 'The Relationship between Total Steps Taken and Calories burnt') 


ggplot(data=daily_activity) + geom_jitter(mapping = aes(x = TotalSteps, y = Calories)) + 
  geom_point(mapping = aes(x = TotalSteps, y = Calories, color = Id))
                                                                         


# Relationship between total minutes asleep and total time in bed

ggplot(data = daily_sleep, aes(x = TotalTimeInBed, y = TotalMinutesAsleep, color = Id)) + geom_point() + stat_smooth(method = lm)

# We can see that this data nicely monitors the sleep cycle which consumers may like

#Combining the data (daily_activity and daily sleep)
#keeping only the common ID and date

# inorder to combine activity and sllep we need to change the column name so that there are two column with same name so that its easy to merge

daily_activity <- daily_activity %>% 
  rename(Date = ActivityDate) %>% 
  mutate(Date = as_date(Date, format = "%m/%d/%Y"))

daily_sleep <- daily_sleep %>% 
  rename(Date = SleepDay) %>% 
  mutate(Date = as_date(Date, format = "%m/%d/%Y %I:%M:%S %p" , tz = Sys.timezone()))

daily_data <- merge(daily_activity, daily_sleep, by = c("Id", "Date"))
glimpse(daily_data)

n_distinct(daily_data$Id)

#Cobining all the Id regardless if they are present in the other data

combined_data <- merge(daily_activity, daily_sleep, by = c("Id", "Date"), all = TRUE)
head(combined_data)
n_distinct(combined_data$Id)
  
  combined_data$weekday <- weekdays(combined_data$Date)
  combined_data$weekday <- factor(combined_data$weekday , levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  head(combined_data)

combined_data$totalminutes <- combined_data$VeryActiveMinutes + combined_data$FairlyActiveMinutes + combined_data$LightlyActiveMinutes + combined_data$SedentaryMinutes
head(combined_data)

ggplot(data = combined_data, aes(x = weekday, fill = weekday)) + geom_bar(stat = "count")


ggplot(data=combined_data, aes(x=weekday, y=SedentaryMinutes, fill=weekday))+ 
  geom_bar(stat="identity", fill="steelblue")+
  labs(title="Less Sedentary Minutes on Saturday", y="Sedentary Minutes")

ggplot(data=combined_data, aes(x=weekday, y=TotalMinutesAsleep, fill=weekday))+ 
  geom_bar(stat="identity")+
  labs(title="Total Minutes Asleep During the Week", y="Total Minutes Asleep")
  
  ggplot(data=combined_data, aes(x=weekday, y=TotalSteps, fill=weekday))+ 
    geom_bar(stat="identity")+
    labs(title="More Steps on Saturday", y="Total Steps")
  
  
  
  active_minutes_vs_steps <- ggplot(data = combined_data) + 
    geom_point(mapping=aes(x=TotalSteps, y=FairlyActiveMinutes), color = "maroon", alpha = 1/3) +
    geom_smooth(method = loess,formula =y ~ x, mapping=aes(x=TotalSteps, y=FairlyActiveMinutes, color=FairlyActiveMinutes), color = "maroon", se = FALSE) +
    
    geom_point(mapping=aes(x=TotalSteps, y=VeryActiveMinutes), color = "forestgreen", alpha = 1/3) +
    geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalSteps, y=VeryActiveMinutes, color=VeryActiveMinutes), color = "forestgreen", se = FALSE) +
    
    geom_point(mapping=aes(x=TotalSteps, y=LightlyActiveMinutes), color = "orange", alpha = 1/3) +
    geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalSteps, y=LightlyActiveMinutes, color=LightlyActiveMinutes), color = "orange", se = FALSE) +
    
    geom_point(mapping=aes(x=TotalSteps, y=SedentaryMinutes), color = "steelblue", alpha = 1/3) +
    geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalSteps, y=SedentaryMinutes, color=SedentaryMinutes), color = "steelblue", se = FALSE) +
    
    annotate("text", x=35000, y=150, label="Very Active", color="black", size=3)+
    annotate("text", x=35000, y=50, label="Fairly Active", color="black", size=3)+
    annotate("text", x=35000, y=1350, label="Sedentary", color="black", size=3)+
    annotate("text", x=35000, y=380, label="Lightly  Active", color="black", size=3)+
    labs(x = "Total Steps", y = "Active Minutes", title="Steps vs Active Minutes")
  
  active_minutes_vs_steps
  
  

# lets see how the calories are affected by various exercise

install.packages("ggpubr")
install.packages("ggarrange")
library("ggarrange")
library("ggpubr")

require(gridExtra)


p1 = ggplot(data = combined_data)  + geom_point(mapping = aes(x = totalminutes, y = Calories), color = "black")+
  labs(title = "TotalMinutes Vs Calories")

p2 = ggplot(data = combined_data)  + geom_point(mapping = aes(x = TotalSteps, y = Calories), color = 'blue')+
  labs(title = "TotalSteps Vs Calories")

p3 = ggplot(data = combined_data)  + geom_point(mapping = aes(x = TotalDistance, y = Calories), color = "grey")+
  labs(title = "TotalDistance Vs Calories")

grid.arrange(p1, p2, p3, ncol=3, nrow=1)



head(combined_data)




#plotting calories for every step taken
combined_data %>% 
  select (TotalSteps, Calories) %>% 
  summary()


ggplot(data = combined_data) + geom_point(mapping = aes(x = TotalSteps, y = Calories, color = TotalSteps)) +
  scale_color_gradientn(colors = "rainbow"(6))+
  geom_hline(yintercept = 2306 , color = "red", size =1)+
  geom_vline(xintercept = 7638, color = "blue", size = 1) +
  geom_text(aes(x = 10000, y = 2100, label = "Mean"), color = "black", size=5) + 
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold")) +
  labs(
    x = 'Steps taken',
    y = 'Calories burned',
    title = 'Calories burned for every step taken')
  

#Plot Activity

Data_Ind <- combined_data %>%
  summarise(Sum_VAM = sum(VeryActiveMinutes/1148807*100), 
            Sum_FAM = sum(FairlyActiveMinutes/1148807*100), 
            Sum_LAM = sum(LightlyActiveMinutes/1148807*100), 
            Sum_SEM = sum(SedentaryMinutes/1148807*100),
            Sum_TOTAL=sum(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes)) %>% 
  round(digits = 2)

slices <- c(Data_Ind$Sum_VAM, Data_Ind$Sum_FAM, Data_Ind$Sum_LAM, Data_Ind$Sum_SEM)
lbls <- c("Very Active Min", "Fairly Active Min", "Lightly Active Min", "Sedentary Min")
pie(slices,
    labels=paste(lbls, slices, sep=" ", "%"),
    col = rainbow(6),
    main="Pie Chart - % of Activity in Minutes")









