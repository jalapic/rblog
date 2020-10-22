
## Download Data

library(RCurl)
library(readxl)

# for Windows, mode = 'wb'
download.file("https://dshs.texas.gov/coronavirus/TexasCOVID19DailyCountyCaseCountData.xlsx", "temp.xlsx", mode = "wb")
tmp <- read_xlsx("temp.xlsx")


## Cleaning Data

# data is a mess
tmp[1:5, 1:6]

# column names are a mess
colnames(tmp)

#do not need row 1.
tmp <- tmp[-1,]

tmp[1:5, 1:6]


# first row is now the column names. from excel sheet
as.character(tmp[1,1:2])


# 15th July and 17th July have stars on them
as.character(tmp[1,131:135])



# remove all stars
# keep last 5 characters

stringr::str_sub(tmp[1,2],-5)

colnames(tmp) <- stringr::str_sub(gsub("\\*", "", tmp[1,]),-5)

# get rid of row 1 again
tmp <- tmp[-1,]
tmp[1:5, 1:6]

dim(tmp)

# have habit of writing notes at end of excel sheets
tmp[250:265, 1:5]

# Zavala is actually the last county.  All rows after that are notes or totals, or subtotals.

which(tmp[,1]=="Zavala") #254

#only keep Anderson to Zavala rows
# tmp <- tmp[1:254,]

df <- tmp[1:which(tmp[,1]=="Zavala"),]
df[1:5,1:6]

## Go from wide to long
ncol(tmp)
df[1:4, 226:230]

library(tidyverse)

df.long <- df %>% pivot_longer(2:ncol(df), names_to = "date")

colnames(df.long)[1]<-"county"

df.long$date <- as.Date(df.long$date, format = "%m-%d")

df.long$value <- as.numeric(df.long$value)


### Plotting

# sum of all cases in Texas:

library(scales)

df.long %>%
  group_by(date) %>%
  summarise(total = sum(value)) %>%
  ggplot(aes(x=date, y = total)) + 
  geom_line(color="#123abc", lwd=1) +
  theme_classic() +
  scale_y_continuous(labels = comma_format()) +
  ylab("Total Cases") +
  xlab("") +
  ggtitle("Number of Covid Cases in Texas 2020") +
  theme(axis.title = element_text(size=16))


### Make an animation - 

# let's pick the five highest counties.

df.long %>%
  group_by(county) %>%
  summarise(total = sum(value)) %>%
  arrange(-total)

# we can automatically grab these like this:

df.long %>%
  group_by(county) %>%
  summarise(total = sum(value)) %>%
  arrange(-total) %>%
  .$county %>%
  head(5) -> my_counties

my_counties

#only keep data with these

df.long %>%
  filter(county %in% my_counties) -> df.x

head(df.x)

library(gganimate)
#library(av) # if saving output as video
#library(gifski) # if saving output as gif

# May take a few seconds
ggplot(df.x, aes(x=date, y=value, color=county)) +
  geom_line(lwd=1) +
  geom_point() +
  scale_color_manual(values=c("red", "black", "purple", "blue","orange"))+
  ggtitle("Cumulative Covid Cases \n    for Selected Texas Counties") +
  theme_classic() +
  ylab("Cumulative Covid Cases") +
  xlab("")+
  theme(
    axis.title = element_text(size=16),
    axis.text = element_text(size=15),
    plot.title = element_text(size=20)
    )+
  transition_reveal(date) 

# Save at gif:
anim_save("covid.gif")
