########################################
### Wild card search for all UCSBees ###
########################################

# libraries
library(tidyverse)
library(dplyr)

# download data
#this is all of the hymenoptera data from CCBER gbif site
bees <- read.csv(file = "beedata17jan2020.csv", sep = "\t")
bees
#use wild cards to only see UCSBees lines within 
#all of the hymenoptera
attach(bees)
newdata <- bees[ which(bees$recordedBy=='UCSBees Survey'),]
newdata

#figure out how to use the wildcard
attach(bees)
newdata <- bees[ which(bees$recordedBy==str_detect(bees,'UCSBees*')),]
newdata
#use bee

newdata <- filter(bees, str_detect(recordedBy), pattern = "UCSBees")

?str_detect
newdata
head(newdata)
#to check what is in the recordedBy column
unique(bees$recordedBy)

#to clear environment
rm(list=ls())


# visualizing data
locality_month_count <- ggplot(bees, aes(x = month)) +
  geom_bar() +
  facet_wrap(~locality)
locality_month_count
### google how to make the months not alphabetical--> name to month

### same process as above
bees2 <- read.csv(file = "my_project/data/apisdatanov21new.csv")
bees2

### now trying to look at all bee data
### using new csv that has all ucsbees survey data
#set working directory to my project data
setwd("~/my_project/data/")
# download data
bees <- read.csv(file = "hymenoptera_data_nov21_1.csv")
bees

#test by using a summary
summary(bees$family)

# visualizing data with all bees now
locality_month_count <- ggplot(bees, aes(x = Month)) +
  geom_bar() +
  facet_wrap(~locality)
locality_month_count

#need to differentiate between taxa
locality_month_count_family <- ggplot(bees, aes(x = month)) +
  geom_bar(family) +
  facet_wrap(~locality)
locality_month_count_family

ggplot(bees, aes(x = Month)) + 
  geom_bar() + facet_wrap(~locality)

#trying to change month numbers to month name
# using month.abb
bees <- transform(bees, Month = month.abb[month])
bees
summary(bees$Month)
#change x-axis from month by alpha to month by time
Month = factor(Month, levels=c("Oct", "Nov", "Dec", 
                            "Jan", "Feb", "Mar", 
                            "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep"))
##that didn't work

#visualize bees divided by family on a specific site per month
locality_family <- ggplot(bees, aes(x = family)) + geom_bar() +
  facet_wrap(~locality)
locality_family

#try to visualize all on one plot, different locality on x axis
# and the count on y axis, with color coordinated family in key
count_persite_byfamily <- ggplot(bees, aes(x = locality,)) + 
                                 geom_bar(aes(fill=family))
count_persite_byfamily
#with the x axis rotated
count_persite_byfamily <- ggplot(bees, aes(x = locality,)) + 
  geom_bar(aes(fill=family)) + theme(axis.text.x = 
                                       element_text(angle = 90))
count_persite_byfamily
# visualize by genus instead of just family
count_persite_bygenus <- ggplot(bees, aes(x = locality,)) + 
  geom_bar(aes(fill=genus))
count_persite_bygenus
#Visualize by genus plus now split so each site has its own plot
count_persite_bygenus_wrap <- ggplot(bees, aes(x=family)) +
  geom_bar(aes(fill=genus)) + facet_wrap(~locality)
count_persite_bygenus_wrap
#with the x axis labels rotated 90 degrees
count_persite_bygenus_wrap <- ggplot(bees, aes(x=family)) +
  geom_bar(aes(fill=genus)) + facet_wrap(~locality) + 
  theme(axis.text.x = element_text(angle = 90))
#visualize counts of apis mellifera vs all others at all sites
# maybe apis_vs_others_bysite <- ggplot(bees, aes(x=locality)) ??
#create a way to differentiate between apis mellifera and all others
#in the original data sheet, add a column for native/introduced
#or some other comparable title
#and then fill in this column with "native" or "introduced" for all
#then plot with facet_wrapped(~locality) and month on x axis

#basic scatter plot, 

pb_vol2 <-
  ggplot(pb, aes(x = elevation, y = volume_cm3, colour = trout, fill = trout)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Volume (cm3)") +
  xlab("Elevation (m)") +
  theme(legend.position = "none", axis.text.x=element_text(angle = -90, hjust = 0))

#mutate for new column with if apis = 1 and if not apis = 0
#then group by time and group by locality, then
#use summarize(counts) for 0s and 1s
#graph with time and count, color = apis
#above may not want lm bc that is linear


summary(bees$species)
