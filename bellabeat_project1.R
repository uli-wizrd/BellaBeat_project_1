# Firstly we'll see our original tables to check the amount of records

library('tidyverse')
library('ggplot2')
library('dplyr')
library('janitor')
library('RColorBrewer')
library('ggExtra')
library('ggpubr')
library('skimr')

# We'll process every table individually to create amazing visualizations 
# We'll start by seeing how frequently smart device users from our database
# use their devices by looking into the daily_average_data table.

data_path1 <- file.choose()
dad_table <- read.csv(data_path1, header = T, sep = ",", dec = ".")
colnames(dad_table) # Column names of our table

# since we don't need all of our data  we'll trim some of the columns

trim_dad_table <- dad_table %>%
  select(day_count,avg_sm,avg_vam,avg_fam,avg_lam) %>%
  rename(average_sedentary_minutes = avg_sm,
         average_very_active_minutes = avg_vam,
         average_lighltly_active_minutes = avg_lam,
         average_fairly_active_minutes = avg_fam)
  

# Now with the following functions we can see the structure our database

head(trim_dad_table) # First six records
tail(trim_dad_table) # Last six records
summary(trim_dad_table) # Some stats of our data

# Now that we know the structure of our data we start to obtain some insights
# lets make some groups and a new table for our graphs

condensed_dad_table <- trim_dad_table %>%
  group_by(day_count) %>%
  summarise(mean_sedentary_minutes = as.integer(round(mean(average_sedentary_minutes),0)), 
            mean_very_active_minutes= as.integer(round(mean(average_very_active_minutes),0)),
            mean_lightly_active_minutes = as.integer(round(mean(average_lighltly_active_minutes),0)),
            mean_fairly_active_minutes = as.integer(round(mean(average_fairly_active_minutes),0)))

# Now for another insight we'll create a tibble

user_dist <- dad_table %>%
  select(day_count) %>%
  count(day_count) %>%
  arrange(day_count)

# We add our labels to our data frame

user_dist$days = c(as.character(user_dist$day_count))

# Now we make our first visualization

viz_1 <- ggplot(data=user_dist, aes(x=day_count, y=n, fill=days))+
  geom_bar(stat="identity") +
  theme_light()+
  labs(title="Days users registered activity and how many of them did",
       x="Days", y="Users", 
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       21 (63%) out of 33 users tracked their data for 31 days")+
  scale_x_continuous(breaks=seq(3,32,1))

viz_1

# So most users tracked their data for 31 days
# Another way to see this is through a pie chart

viz_2 <- ggplot(user_dist, aes(x="",y=n, fill=days))+
  geom_col(color="black") +
  coord_polar(theta="y")+
  theme_void() +
  labs(title="Days tracked (external labels) by users and how many users (internal labels)",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Labels inside the chart correspond to the amount of users.
       21 (63%) out of 33 users tracked their activity during 31 days")+
  geom_label(aes(label = n), color="ivory",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE,label.size = 0.25) +
  guides(fill = guide_legend(title = "Days"))+
  scale_fill_brewer(palette="Paired")

viz_2

# Let's take a look at how each group of users
#(those that wore the device often and those that didn't)
# behaves in terms of their activity
# To achieve this we'll use our condensed table


viz_3a <- ggplot()+
  geom_point(data= condensed_dad_table, aes(x=day_count, y=mean_sedentary_minutes,color="coral1"),
             shape = 4, size = 3,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Days",caption="Data from: FitBit Fitness Tracker on Kaggle.")+
  theme_light()+ ggtitle("User workout behavior")+
  scale_x_continuous(breaks=seq(0,100,10))+
  guides(color=guide_legend("Behavior"))+
scale_color_manual(labels = c("Sedentary"),values=c("coral1"))
margin_viz_3a <- ggMarginal(viz_3a,type="density",margins="x")
margin_viz_3a
  
viz_3b <- ggplot()+
  geom_point(data= condensed_dad_table, aes(x=day_count, y=mean_fairly_active_minutes,color="dodgerblue1"),
             shape = 17, size = 3,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Days",caption="Data from: FitBit Fitness Tracker on Kaggle.")+
  theme_light()+ ggtitle("User workout behavior")+
  scale_x_continuous(breaks=seq(0,100,10))+
  guides(color=guide_legend("Behavior"))+
  scale_color_manual(labels = c("Fairly active"),values=c("dodgerblue1"))
margin_viz_3b <- ggMarginal(viz_3b,type="density",margins="x")
margin_viz_3b

viz_3c <- ggplot()+
  geom_point(data= condensed_dad_table, aes(x=day_count, y=mean_lightly_active_minutes,color="maroon3"),
             shape = 19, size = 3,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Days",caption="Data from: FitBit Fitness Tracker on Kaggle.")+
  theme_light()+ ggtitle("User workout behavior")+
  scale_x_continuous(breaks=seq(0,100,10))+
  guides(color=guide_legend("Behavior"))+
  scale_color_manual(labels = c("Lightly active"),values=c("maroon3"))
margin_viz_3c <- ggMarginal(viz_3c,type="density",margins="x")
margin_viz_3c

viz_3d <- ggplot()+
  geom_point(data= condensed_dad_table, aes(x=day_count, y=mean_very_active_minutes,color="seagreen3"),
             shape = 15, size = 3,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Days",caption="Data from: FitBit Fitness Tracker on Kaggle.")+
  theme_light()+ ggtitle("User workout behavior")+
  scale_x_continuous(breaks=seq(0,100,10))+
  guides(color=guide_legend("Behavior"))+
  scale_color_manual(labels = c("Very active"),values=c("seagreen3"))
margin_viz_3d <- ggMarginal(viz_3d,type="density",margins="x")
margin_viz_3d

# To see everything and get an insight we can do the following

plot_arr1 <- ggarrange(margin_viz_3a, margin_viz_3b, margin_viz_3c, margin_viz_3d,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
plot_arr1


# Since we know most of the users kept track of their data for 31 days
# we'll look at this group closer to get to know them better
# we will work with the  daily_average_activity table


data_path2 <- file.choose()

daat_table <- read.csv(data_path2, header = T, sep = ",", dec = ".")

# With the following functions we can see the structure our database

colnames(daat_table) # Column names of our table
head(daat_table) # First six records
tail(daat_table) # Last six records
summary(daat_table) # Some stats of our data

# After getting a preview of our table, we'll create a copy to work with

trim_daat_table <- daat_table %>% select(avg_total_steps,total_distance,
                                         avg_vam,avg_fam, avg_lam, avg_sm,
                                         avg_calories)

# We'll look at how different metrics relate to each other in this user group
# First steps and calories

viz_4a <- ggplot() + 
  geom_point(data= trim_daat_table, aes(x=avg_total_steps,
            y=avg_calories,color="sienna2"),
                                  shape = 15, size = 2,
                                  stroke = 1, alpha=0.6)+
  labs(y= "Average calories", x = "Average steps",
  caption="Data from: FitBit Fitness Tracker on Kaggle.
       Curves on both edges of the plot show the data distribution.
       Most users burn on average between 2250 and 1750 calories.
  Most users walk on average between 6000 and 9000 steps")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = 'gray97', color = 'slategrey'),
        panel.grid.major=element_line(colour="slategrey"),
        panel.grid.minor=element_line(colour="slategrey"))+
  ggtitle("How calories and steps relate")+
  guides(color=guide_legend("Symbols"))+
  scale_color_manual(labels = c("Users"),values=c("sienna2"))

margin_viz_4a <- ggMarginal(viz_4a,type="density", color="lightsalmon2")
margin_viz_4a

# Secondly we'll look at calories and the type of activity
# sedentary states and calories

viz_4b <- ggplot() + 
  geom_point(data= trim_daat_table, aes(x=avg_calories,
                                        y=avg_sm,color="steelblue1"),
             shape = 15, size = 2,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Average calories",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Curves on both edges of the plot show the data distribution.")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = 'gray97', color = 'slategrey'),
        panel.grid.major=element_line(colour="slategrey"),
        panel.grid.minor=element_line(colour="slategrey"))+
  ggtitle("How calories and sedentary states relate")+
  guides(color=guide_legend("Symbols"))+
  scale_color_manual(labels = c("Sedentary users"),values=c("steelblue1"))

margin_viz_4b <- ggMarginal(viz_4b,type="density", color="slategrey")
margin_viz_4b

# fairly active states and calories

viz_4c <- ggplot() + 
  geom_point(data= trim_daat_table, aes(x=avg_calories,
                                        y=avg_fam,color="plum3"),
             shape = 15, size = 2,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Average calories",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Curves on both edges of the plot show the data distribution.")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = 'gray97', color = 'slategrey'),
        panel.grid.major=element_line(colour="slategrey"),
        panel.grid.minor=element_line(colour="slategrey"))+
  ggtitle("How calories and fairly active states relate")+
  guides(color=guide_legend("Symbols"))+
  scale_color_manual(labels = c("Fairly active users"),values=c("plum3"))

margin_viz_4c <- ggMarginal(viz_4c,type="density", color="slategrey")
margin_viz_4c

# lightly active states and calories

viz_4d <- ggplot() + 
  geom_point(data= trim_daat_table, aes(x=avg_calories,
                                        y=avg_lam,color="olivedrab2"),
             shape = 15, size = 2,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Average calories",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Curves on both edges of the plot show the data distribution.")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = 'gray97', color = 'slategrey'),
        panel.grid.major=element_line(colour="slategrey"),
        panel.grid.minor=element_line(colour="slategrey"))+
  ggtitle("How calories and lightly active states relate")+
  guides(color=guide_legend("Symbols"))+
  scale_color_manual(labels = c("Lightly active users"),values=c("olivedrab2"))

margin_viz_4d <- ggMarginal(viz_4d,type="density", color="slategrey")
margin_viz_4d

# very active states and calories

viz_4e <- ggplot() + 
  geom_point(data= trim_daat_table, aes(x=avg_calories,
                                        y=avg_vam,color="tomato3"),
             shape = 15, size = 2,
             stroke = 1, alpha=0.6)+
  labs(y= "Average minutes", x = "Average calories",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Curves on both edges of the plot show the data distribution.")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = 'gray97', color = 'slategrey'),
                                panel.grid.major=element_line(colour="slategrey"),
                                        panel.grid.minor=element_line(colour="slategrey"))+
  ggtitle("How calories and very active states relate")+
  guides(color=guide_legend("Symbols"))+
  scale_color_manual(labels = c("Very active users"),values=c("tomato3"))

margin_viz_4e <- ggMarginal(viz_4e,type="density", color="slategrey")
margin_viz_4e


# Now we see all of them together

plot_arr2 <- ggarrange(margin_viz_4b, margin_viz_4c, margin_viz_4d, margin_viz_4e,
                       labels = c("", "", "", ""),
                       ncol = 2, nrow = 2)
plot_arr2

# To finish with this table we can take a look at the average values for each 
# kind of physical activity on our user group
# we'll create the data frame for this

avg_daat_table <- data.frame(Active_type = c("Lightly","Fairly","Very"),
                             Average_minutes = c(mean(daat_table$avg_fam),
                                         mean(daat_table$avg_lam),
                                         mean(daat_table$avg_vam))
                             ) %>% arrange(Average_minutes)

# Now we'll validate these results through the following calculation

daat_table%>%summarise(lightly = mean(avg_lam), very = mean(avg_vam),
                       fairly = mean(avg_fam))

# Now that we have validated these results we'll create our visualization
# The following visualization informs us about the kind of activities our users prefer

viz_5 <- ggplot(data=avg_daat_table, aes(x=Active_type, y=Average_minutes, fill=Active_type))+
  geom_bar(stat="identity") +
  theme_light()+ guides(fill = guide_legend(title = "Activity type"))+
  labs(title="User average activity preference", x = "Activity type", y="Average minutes", 
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Users that wore the device for 31 days, spent on average 194 minutes on light activities.") 

viz_5

# We can now move on to the next table 
# We'll look into weight log info

data_path3 <- file.choose()

wli_table <- read.csv(data_path3, header = T, sep = ",", dec = ".")

# With the following functions we can see the structure our database

colnames(wli_table) # Column names of our table
head(wli_table) # First six records
tail(wli_table) # Last six records
summary(wli_table) # Some stats of our data

# After getting a preview of our table, we'll create a copy to work with

trim_wli_table <- wli_table %>% select(user_id,date_registered,is_manual_report) 

# For the following viz we'll make a new small table

log_info <- trim_wli_table %>% group_by(user_id) %>% count(is_manual_report)

log_info

# Since we have unique user_id's we can simply add another column with simpler
# values to identify each user

log_info$n_user_id = c(1:8)

# now we can delete that user column

log_info <- log_info %>% as.data.frame(log_info) %>% select(-user_id)

# From this table what we are most interested in knowing is:
# How many of each kind of report we have

viz_6 <- ggplot(data=log_info) +
  geom_bar(mapping=aes(x=is_manual_report, fill= is_manual_report)) +
  theme_light()+ guides(fill = guide_legend(title = "Symbols"))+
  labs(title="How users like logging their info", x="Was it reported manually?",
       y="Registered entries",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Most users logg their info manually.")
viz_6

# Now we can look at two more tables to get another perspective
# first we'll see how the amount of hours influences the calories consumed

data_path4 <- file.choose()

hcalorie_table <- read.csv(data_path4, header = T, sep = ",", dec = ".")

# With the following functions we can see the structure our database

colnames(hcalorie_table) # Column names of our table
head(hcalorie_table) # First six records
tail(hcalorie_table) # Last six records

# We'll start by simplifying our user id's

hcalorie_table$Id = c(1:33)

hcalorie_table <- hcalorie_table %>% select(-user_id)

summary(hcalorie_table) # Some stats of our data

# Now a quick visualization


viz_7 <- ggplot() + 
  geom_point(data= hcalorie_table, aes(x=hours_registered,
                                        y=calories,color="hotpink3"),
             shape = 15, size = 2,
             stroke = 1, alpha=0.6)+
  labs(y= "Calories burned", x = "Hours registered",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Curves on both edges of the plot show the data distribution.")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = 'gray97', color = 'slategrey'),
        panel.grid.major=element_line(colour="slategrey"),
        panel.grid.minor=element_line(colour="slategrey"))+
  ggtitle("How hours and burned calories relate")+
  guides(color=guide_legend("Symbols"))+
  scale_color_manual(labels = c("Users"),values=c("hotpink3"))

margin_viz_7 <- ggMarginal(viz_7,type="density", color="slategrey")
margin_viz_7

# Now let's look at hourly intensities

data_path5 <- file.choose()

hintensities_table <- read.csv(data_path5, header = T, sep = ",", dec = ".")

# With the following functions we can see the structure our database

colnames(hintensities_table) # Column names of our table
head(hintensities_table) # First six records
tail(hintensities_table) # Last six records

# We'll start by simplifying our user id's

hintensities_table$Id = c(1:33)

hintensities_table <- hintensities_table %>% select(-user_id)

summary(hintensities_table) # Some stats of our data

# Now a quick visualization


viz_8 <- ggplot() + 
  geom_point(data= hintensities_table, aes(x=hours_registered,
                                       y=total_intensity,color="orange2"),
             shape = 15, size = 2,
             stroke = 1, alpha=0.6)+
  labs(y= "Total intensity", x = "Hours registered",
       caption="Data from: FitBit Fitness Tracker on Kaggle.
       Curves on both edges of the plot show the data distribution.")+
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = 'gray97', color = 'slategrey'),
        panel.grid.major=element_line(colour="slategrey"),
        panel.grid.minor=element_line(colour="slategrey"))+
  ggtitle("How hours and workout intensity relate")+
  guides(color=guide_legend("Symbols"))+
  scale_color_manual(labels = c("Users"),values=c("orange2"))

margin_viz_8 <- ggMarginal(viz_8,type="density", color="slategrey")
margin_viz_8

# Now to finish we can look at our largest database
# From this table we are interested in seeing how intensities vary by the minute 
# In our users, in this case we'll only look at users that registered activity often


data_path6 <- file.choose()

minute_intensity_table <- read.csv(data_path6, header = T, sep = ",", dec = ".")

# With the following functions we can see the structure our database

colnames(minute_intensity_table) # Column names of our table
head(minute_intensity_table) # First six records
tail(minute_intensity_table) # Last six records

# we can look a little deeper at our data with the following piece of code
# this will let us know the type of our data and if it's missing values

skim_without_charts(minute_intensity_table)

# We can see similar information with

summary(minute_intensity_table)

# Now we can clean things a little

minute_intensity_table <- minute_intensity_table %>%
  select(Id,ActivityMinute,Intensity) %>% clean_names()

# now we just look at our table 
head(minute_intensity_table)

# if we didn't have the functions that the string cleaning packages provide
# we would have to perform some loop stuff (not pretty things)

# We can now select the users we will work with through the following vector

qty_hrs_usr <- minute_intensity_table %>% select(id,activity_minute) %>%
  group_by(id) %>% count(activity_minute,name="value_occurrance")

qty_hrs_usr2 <- qty_hrs_usr %>% select(id,value_occurrance) %>%
  group_by(id) %>% count(value_occurrance,name="total_minutes")

# one last level of abstraction

qty_hrs_usr3 <- qty_hrs_usr2 %>% select(total_minutes) %>%
  group_by(total_minutes) %>% count(total_minutes,name="total_minute_occurrance")

# Now we want to know how many minutes on average are our users active

mean(qty_hrs_usr3$total_minutes)

# Now we'll only look at data that has a non zero values

cmts_no_ceros <- minute_intensity_table[-row(minute_intensity_table)[minute_intensity_table == 0],]

# Now we can separate some of our columns for further analysis

cmts_sep1 <- separate(cmts_no_ceros,activity_minute,into=c("date","time","am_pm"),sep=' ')

# Now we'll separate our data one more time

cmts_sep1 <- separate(cmts_sep1,time,into=c("hour","minute"),sep=':')
cmts_sep1 <- transform(cmts_sep1,hour = as.numeric(hour))

# We'll unite some columns to create our viz

#cmts_sep1 <- unite(cmts_sep1,col='hr_am_pm',c("hour","am_pm"),sep=' ')
  

# With the previous table we can know at what time of the day our users 
# prefer to workout, so now let's visualize this

viz_9 <- ggplot(data=cmts_sep1) + geom_bar(mapping=aes(x=intensity,fill=am_pm))+
  theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~hour) +
  theme_light() +
  labs(title="Total user activity intensity by the minute",
                       x="Activity intensity", y="Total activity occurrance", 
                       caption="Data from: FitBit Fitness Tracker on Kaggle.
                       The grey headers are the hours in each part of the day.
       Most users perform light activities during the afternoon.")+
  scale_x_continuous(breaks=seq(0,12,1)) +
  guides(fill = guide_legend(title = "Part of the day"))
viz_9

# Now we'll validate our results

validation_table <- cmts_sep1 %>% select(hour,intensity,am_pm) %>%
  group_by(hour,intensity,am_pm) %>% count(intensity,name = "occurrences")

# Now we have enough data and insights to start making decisions without looking
# at all of our data.