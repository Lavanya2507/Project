library(tidyverse)

## To read csv file and concatenated into one  data frame
csv_files <- list.files(path = "L:/Google_Bike_Share_Navigate_Speedy_Successs/Data/", recursive = TRUE, full.names=TRUE)
cyclistic_merged <- do.call(rbind, lapply(csv_files, read.csv))
head(cyclistic_merged)

## data cleaning through by removing duplicates rows.
cyclistic_no_dups <- cyclistic_merged[!duplicated(cyclistic_merged$ride_id), ]
print(paste("Removed", nrow(cyclistic_merged) - nrow(cyclistic_no_dups), "duplicated rows"))
cyclistic_no_dups$started_at <- as.POSIXlt(cyclistic_no_dups$started_at, "%Y-%m-%d %H:%M:%S")
cyclistic_no_dups$ended_at <- as.POSIXlt(cyclistic_no_dups$ended_at, "%Y-%m-%d %H:%M:%S")

##To calculate total time in ride_time in minutes
cyclistic_no_dups <- cyclistic_no_dups %>%
  mutate(ride_time_m = as.numeric(cyclistic_no_dups$ended_at - cyclistic_no_dups$started_at) / 60)
summary(cyclistic_no_dups$ride_time_m)

## To calculate separate the year and the month in one month
cyclistic_no_dups <- cyclistic_no_dups %>%
  mutate(year_month = paste(strftime(cyclistic_no_dups$started_at, "%Y"),
                            "-",
                            strftime(cyclistic_no_dups$started_at, "%m"),
                            paste("(",strftime(cyclistic_no_dups$started_at, "%b"), ")", sep="")))
unique(cyclistic_no_dups$year_month)

## To calculate weekday travel in the week
cyclistic_no_dups <- cyclistic_no_dups %>%
  mutate(weekday = paste(strftime(cyclistic_no_dups$ended_at, "%u"), "-", strftime(cyclistic_no_dups$ended_at, "%a")))
unique(cyclistic_no_dups$weekday)
##To calculate start hour of the day
cyclistic_no_dups <- cyclistic_no_dups %>%
  mutate(start_hour = strftime(cyclistic_no_dups$ended_at, "%H"))
unique(cyclistic_no_dups$start_hour)

##To save the file after cleaning
cyclistic_no_dups %>%
  write.csv("cyclistic_clean.csv")
##This function helps to resize the plot and we can rename the variable in the future
fig <- function(width, heigth){options(repr.plot.width = width, repr.plot.height = heigth)}
cyclistic <- cyclistic_no_dups
head(cyclistic)
##To generate summary of dataset
#summary(cyclistic)
##To calculate how much data casual vs members
cyclistic %>% 
  group_by(member_casual) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100)
##To generate data distribution between casuals and members through ggplot
fig(16,8)
ggplot(cyclistic, aes(member_casual, fill=member_casual)) +
  geom_bar() +
  labs(x="Casuals x Members", title="Chart 01 - Casuals x Members distribution")+
  scale_fill_brewer(palette="Dark2")
ggsave("chart_01.png")


##To calculate how much data distributed between casuals and members by month
cyclistic %>%
  group_by(year_month) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            'Member x Casual Perc Difer' = members_p - casual_p)

##To calculate ggplot data distributed by month
cyclistic %>%
  ggplot(aes(year_month, fill=member_casual)) +
  geom_bar() +
  labs(x="Month", title="Chart 02 - Distribution by month") +
  coord_flip()+
  scale_fill_brewer(palette=" paired")
ggsave("chart_02.png")



##To calculate the data with climate of chicago
chicago_mean_temp <- c(-3.2, -1.2, 4.4, 10.5, 16.6, 22.2, 24.8, 23.9, 19.9, 12.9, 5.8, -0.3)
month <- c("001 - Jan","002 - Feb","003 - Mar","004 - Apr","005 - May","006 - Jun","007 - Jul","008 - Aug","009 - Sep","010 - Oct","011 - Nov","012 - Dec")

data.frame(month, chicago_mean_temp) %>%
  ggplot(aes(x=month, y=chicago_mean_temp, fill=chicago_mean_temp)) +
  labs(x="Month", y="Mean temperature", title="Chart 02.5 - Mean temperature for Chicago (2021)") +
  geom_col()
ggsave("chart 02.5.png")


##To calculate data distributed by weekday
cyclistic %>%
  group_by(weekday) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            'Member x Casual Perc Difer' = members_p - casual_p)


##To calculate ggplot data distributed by weekday
ggplot(cyclistic, aes(weekday, fill=member_casual)) +
  geom_bar() +
  labs(x="Weekday", title="Chart 03 - Distribution by weekday") +
  coord_flip()
ggsave("chart_03.png")

##To calculate hours of day
cyclistic %>%
  group_by(start_hour) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            'member_casual_perc_difer' = members_p - casual_p)

##To calculate ggplot with hours of day
cyclistic %>%
  ggplot(aes(start_hour, fill=member_casual)) +
  labs(x="Hour of the day", title="Chart 04 - Distribution by hour of the day") +
  scale_fill_brewer(palette="Dark2") +
  geom_bar()
ggsave("chart_04.png")
##To calculate data distribution by hours of the day divided by weekday
cyclistic %>%
  ggplot(aes(start_hour, fill=member_casual)) +
  geom_bar() +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Hour of the day", title="Chart 05 - Distribution by hour of the day divided by weekday") +
  facet_wrap(~ weekday)
ggsave("chart_05.png")


##To generate chart with two configuration difference between mid-day and weekday
cyclistic %>%
  mutate(type_of_weekday = ifelse(weekday == '6 - Sat' | weekday == '7 - Sun',
                                  'weekend',
                                  'midweek')) %>%
  ggplot(aes(start_hour, fill=member_casual)) +
  labs(x="Hour of the day", title="Chart 06 - Distribution by hour of the day in the midweek") + 
  scale_fill_brewer(palette="Dark2") +
  geom_bar() +
  facet_wrap(~ type_of_weekday)
ggsave("chart_06.png")
##To calculate rideable_type between casuals and members
cyclistic %>%
  group_by(rideable_type) %>% 
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            'members_p' = (sum(member_casual == "member") / length(ride_id)) * 100,
            'casual_p' = (sum(member_casual == "casual") / length(ride_id)) * 100,
            'member_casual_perc_difer' = members_p - casual_p)


##To calculate rideable_type through ggplot between casuals and members
ggplot(cyclistic, aes(rideable_type, fill=member_casual)) +
  labs(x="Rideable type", title="Chart 07 - Distribution of types of bikes") +
  geom_bar() +
  coord_flip()
ggsave("chart_07.png")


##To summarize data with ride_time in minutes
summary(cyclistic$ride_time_m)
ventiles = quantile(cyclistic$ride_time_m, seq(0, 1, by=0.05))
ventiles
cyclistic_without_outliners <- cyclistic %>% 
  filter(ride_time_m > as.numeric(ventiles['5%'])) %>%
  filter(ride_time_m < as.numeric(ventiles['80%']))
print(paste("Removed", nrow(cyclistic) - nrow(cyclistic_without_outliners), "rows as outliners" ))


##To calculate subplots based on casuals and members based on data and summarize data
cyclistic_without_outliners %>% 
  group_by(member_casual) %>% 
  summarise(mean = mean(ride_time_m),
            'first_quarter' = as.numeric(quantile(ride_time_m, .25)),
            'median' = median(ride_time_m),
            'third_quarter' = as.numeric(quantile(ride_time_m, .75)),
            'IR' = third_quarter - first_quarter)


##To calculate ggplot distribution on ride_timing between casuals and members
ggplot(cyclistic_without_outliners, aes(x=member_casual, y=ride_time_m, fill=member_casual)) +
  labs(x="Member x Casual", y="Riding time", title="Chart 08 - Distribution of Riding time for Casual x Member") +
  geom_boxplot()+
  scale_fill_brewer(palette="Dark2")
ggsave("chart_08.png")


##To calculate ggplot with weekday
ggplot(cyclistic_without_outliners, aes(x=weekday, y=ride_time_m, fill=member_casual)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2")+
  facet_wrap(~ member_casual) +
  labs(x="Weekday", y="Riding time", title="Chart 09 - Distribution of Riding time for day of the week")
ggsave("chart_09.png")


##To calculate ggplot with rideable_type
ggplot(cyclistic_without_outliners, aes(x=rideable_type, y=ride_time_m, fill=member_casual)) +
  geom_boxplot() +
  scale_fill_brewer(palette="Dark2")+
  facet_wrap(~ member_casual) +
  labs(x="Rideable type", y="Riding time", title="Chart 10 - Distribution of Riding time for rideable type") +
  coord_flip()
ggsave("chart_10.png")
