# I wanted to categorize location points as either daytime points or nighttime
# points, but I realized sunrise and sunset might not be good indiators of "day"
# and "night" for my species. They could be active before sunrise, while it's
# still dark, or they might wait until it's broad daylight, several hours after
# sunrise. 

# This code helps visualize when an individual "wakes up" relative to
# sunrise and "goes to bed" relative to sunset. Then it classifies each point
# based on sunrise/set times for the day on which the point was taken.

rm(list = ls())   # Clear the workspace.
gc()              # Perform a garbage collection.

# Set the working directory.
setwd('C:/dir')

# Load some libraries.
library('ggplot2')
library('lubridate')
# library('extrafont')
# I have strong opinions about fonts.
library('suncalc')
library('dplyr')

# Import and prep data

df <- 
  read.csv('points.csv',
           stringsAsFactors=FALSE)

head(df)

# My data has a column called "date" which contains date and time for each
# location point. This is going to be confusing later because I need a column
# containing the date only. So I'll rename the "date" column.

colnames(df)[colnames(df)=='date'] <- 'time'

# Also convert the time column to an actual time format. Keep an eye on
# timezones throughout this process. Timezones are hard.

df$time <- ymd_hms(df$time, tz='America/Vancouver')

# Now to actually make a date column.

df$date <- date(df$time)

# I can use the library 'suncalc' to calculate sunrise and sunset times for each
# date. This method calculates sunrise for each specific location, as well. For
# my data set, the points are very close together so it doesn't really matter.

# suncalc can also calculate a lot of different metrics beside simple sunrise
# and sunset, like different degrees of twilight, noon and midnight, etc. I
# only want sunrise and sunset, though.

df <-  getSunlightTimes(data=df, keep=c('sunrise', 'sunset'), 
                        tz='America/Vancouver')

# Ok, so I have the sunrise times. Now I need to know how long before or after
# sunrise and sunset each location point was taken.

df$diff.rise <- as.numeric(difftime(df$time, df$sunrise, units='hours'))
df$diff.set <- as.numeric(difftime(df$time, df$sunset, units='hours'))

# Now I'm gonna graph the activity levels relative to sunrise.
# I'm using speed as a metric of activity. Another possible metric (I haven't
# tried this myself) could be distance between consecutive points.

ggplot(data=df) +
  geom_point(aes(x=diff.rise, y=speed)) +
  scale_x_continuous() +
  geom_vline(xintercept=0, linetype='dashed') +
  labs(x='Hours after sunrise', y='Activity (Speed in km/h)') +
  # theme(text=element_text(family='Lato')) +
  theme_classic() +

# And also relative to sunset.

ggplot(data=df) +
  geom_point(aes(x=diff.set, y=speed)) +
  scale_x_continuous() +
  geom_vline(xintercept=0, linetype='dashed') +
  labs(x='Hours before sunset', y='Activity (Speed in km/h)') +
  # theme(text=element_text(family='Lato')) +
  theme_classic() +

# For my data, activity falls very neatly between sunrise and sunset, so I'll
# classify points as being either "daytime" or "nighttime" activity.

df$t.period <- case_when(
  df$diff.rise >= 0 & df$diff.set <= 0 ~ 'day',
  TRUE ~ 'night'
)

# Plot again with colors, to see if the points were categorized correctly.
# Note there's overlap at the tails. That's OK.

ggplot(data=df) +
  geom_point(aes(x=diff.rise, y=speed, color=t.period)) +
  scale_x_continuous() +
  geom_vline(xintercept=0, linetype='dashed') +
  labs(x='Hours after sunrise', y='Activity (Speed in km/h)') +
  # theme(text=element_text(family='Lato')) +
  theme_classic()

ggplot(data=df) +
  geom_point(aes(x=diff.set, y=speed, color=t.period)) +
  scale_x_continuous() +
  geom_vline(xintercept=0, linetype='dashed') +
  labs(x='Hours before sunset', y='Activity (Speed in km/h)') +
  # theme(text=element_text(family='Lato')) +
  theme_classic()
