################################################################################
# Parse VT District 1 Fish Data
# Source: Vermont Fish and Wildlife
# Location: District 1 (Springfield)
# Name: Daniel J. Hocking
# Start Date: 20 June 2016
# Prep: Exported all tables from MS Access to csv
################################################################################

########## Load packages ##########
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)


######## Set directories ##########

# data directory relative to the working directory
dir_data <- "Data/Fish_Data_Raw/VT_Raw/District_1"

# output directory
dir_out <- "Data/Fish_Data_Processed/VT/District_1"

if(!exists(file.path(getwd(), dir_out))) dir.create(file.path(getwd(), dir_out), recursive = TRUE)


######### Load data #########

list.files(dir_data)

df_fishes <- read.csv(file.path(dir_data, "tblFullFishList.csv"), header = TRUE, stringsAsFactors = FALSE)
df_sites <- read.csv(file.path(dir_data, "tblSiteNameCoordinates.csv"), header = TRUE, stringsAsFactors = FALSE)
df_fish <- read.csv(file.path(dir_data, "tblSurveyData.csv"), header = TRUE, stringsAsFactors = FALSE)
df_survey <- read.csv(file.path(dir_data, "tblSurveyEvent.csv"), header = TRUE, stringsAsFactors = FALSE)
df_gis <- read.csv(file.path(dir_data, "tblGISLink.csv"), header = TRUE, stringsAsFactors = FALSE)

######### Clean data #########

str(df_fishes)
str(df_sites)
str(df_survey)
str(df_fish)

summary(df_fish)
unique(df_fish$Fish.Code)
unique(df_fish$Run.Number)

# unify fish names and run numbers (passes)
df_fish <- df_fish %>%
  dplyr::mutate(Fish.Code = ifelse(Fish.Code == "bkt", "BKT", Fish.Code),
                Fish.Code = ifelse(Fish.Code == "BKT ", "BKT", Fish.Code),
                Fish.Code = ifelse(Fish.Code == "bnt", "BNT", Fish.Code),
                Fish.Code = ifelse(Fish.Code == "rbt", "RBT", Fish.Code),
                Run.Number = ifelse(Run.Number == "2 2 2", "2", Run.Number),
                Run.Number = ifelse(Run.Number == "", "NA", Run.Number),
                Run.Number = ifelse(Run.Number == "22", "2", Run.Number),
                Run.Number = ifelse(Run.Number == "2 2", "2", Run.Number),
                Run.Number = as.integer(Run.Number)
  )

summary(df_fish)
unique(df_fish$Fish.Code)
unique(df_fish$Run.Number)

summary(df_sites)


######### Join data ##########

df_VT1 <- df_fish %>%
  dplyr::left_join(df_survey) %>%
  dplyr::left_join(df_sites) %>%
  dplyr::filter(!is.na(Survey.Date)) %>%
  dplyr::mutate(date = ymd(as.Date(Survey.Date)))

str(df_VT1)
summary(df_VT1)

dplyr::filter(df_VT1, is.na(Run.Number))

######### Expand passes ##########

# looks like number of passes will either be the max where any trout was caught or it will be in the notes of df_survey. This will have to be translated by hand into something useable for determining the number of passes because it seems like for "Multiple Run Removal" Survey.Type there can be 2, 3, or 4 passes.

df_VT1 <- df_VT1 %>% 
  dplyr::mutate(Run.Number = ifelse(is.na(Run.Number & Survey.Type == "Single Run Removal"), 1, Run.Number)) %>%
  dplyr::mutate(visit = paste0(SiteName, "_", Survey.Date),
                year = year(date))

foo <- filter(df_VT1, Survey.Event.ID == 376) %>%
  dplyr::select(Survey.Data.ID, Survey.Event.ID, Fish.Code, Total.Length, Run.Number)
foo
# only 1 individual with a missing pass number. Assume that individual wasn't captured so it just goes into the detection or error
              
              
foo <- df_VT1 %>%
  group_by(visit) %>%
  dplyr::summarise(pass_max = max(Run.Number))
  
foo

# Add stage for each species and year

# bkt
ggplot(group_by(dplyr::filter(df_VT1, Fish.Code == "BKT"), year)) + geom_freqpoly(aes(length, colour = SiteName)) + xlim(10, 110) + facet_wrap(~year)

# check size of yoy
ggplot(data = dplyr::filter(df_VT1, Fish.Code == "BKT"), aes(year)) + geom_histogram(aes(Total.Length), binwidth = 1)  + facet_wrap(~year) + xlim(0, 250)

df_VT1 <- dplyr::mutate(df_VT1, site_year = paste0(SiteName, "_", year))
length(unique(df_VT1$site_year))

# 1308 unique site-year combos. Many likely have too few fish for a useful histogram so if fewer than 100 fish, just give an NA then apply the mean for the year. Only output histograms with sufficient fish.
# do the entry in a csv file for ease then import

########## do for BKT ###########
df_bkt <- df_VT1 %>%
  dplyr::group_by(site_year, SiteName, year) %>%
  dplyr::filter(Fish.Code == "BKT") %>%
  dplyr::summarise(count = n())
                   
hist(df_bkt$count, breaks = 100)
length(df_bkt$count[which(df_bkt$count > 25)])
length(df_bkt$count[which(df_bkt$count > 50)])
length(df_bkt$count[which(df_bkt$count > 75)])
length(df_bkt$count[which(df_bkt$count > 100)])

# check how sparse the histogram is for different counts
for(i in 1:3){
foo <- df_bkt[which(df_bkt$count == 25), ]
bar <- df_VT1 %>% dplyr::filter(site_year == foo$site_year[i] & Fish.Code == "BKT")
g <- ggplot(bar, aes(Total.Length)) + geom_histogram(binwidth = 5) + ggtitle(paste0("25 individuals [", i, "]"))
print(g)

foo <- df_bkt[which(df_bkt$count == 50), ]
bar <- df_VT1 %>% dplyr::filter(site_year == foo$site_year[i] & Fish.Code == "BKT")
g <- ggplot(bar, aes(Total.Length)) + geom_histogram(binwidth = 5) + ggtitle(paste0("50 individuals [", i, "]"))
print(g)

foo <- df_bkt[which(df_bkt$count >= 75 & df_bkt$count <= 80), ]
bar <- df_VT1 %>% dplyr::filter(site_year == foo$site_year[i] & Fish.Code == "BKT")
g <- ggplot(bar, aes(Total.Length)) + geom_histogram(binwidth = 5) + ggtitle(paste0("75 individuals [", i, "]"))
print(g)

foo <- df_bkt[which(df_bkt$count >= 100 & df_bkt$count <= 110), ]
bar <- df_VT1 %>% dplyr::filter(site_year == foo$site_year[i] & Fish.Code == "BKT")
g <- ggplot(bar, aes(Total.Length)) + geom_histogram(binwidth = 5) + ggtitle(paste0("100 individuals [", i, "]"))
print(g)
} # use 75 individuals as the minimum, otherwise use the mean for the year if available, else use the site mean, else use the overall mean

foo <- df_bkt[which(df_bkt$count >= 75), ]

if(!exists(file.path(getwd(), dir_out, "/Output/Figures/YOY/BKT"))) dir.create(file.path(getwd(), dir_out, "/Output/Figures/YOY/BKT"), recursive = T)

# DON'T OVERWRITE
if(!exists(file.path(getwd(), dir_out, "/Output/Figures/YOY/BKT/yoy_assignment_bkt.csv"))) write.csv(foo, file = file.path(getwd(), dir_out, "/Output/Figures/YOY/BKT/yoy_assignment_bkt.csv"), row.names = F)

# check for outliers
temp <- df_VT1 %>% dplyr::filter(Fish.Code == "BKT")
summary(temp$Total.Length)
hist(temp$Total.Length, breaks = 100)

# export histograms
for(i in 1:length(unique(foo$site_year))) {
  bar <- df_VT1 %>% dplyr::filter(site_year == foo$site_year[i] & Fish.Code == "BKT")
  g <- ggplot(bar, aes(Total.Length)) + geom_histogram(binwidth = 5) + ggtitle(paste0("BKT ", foo$site_year[i])) + xlim(50, 175) + geom_density(aes(y = 5*..count..)) 
  ggsave(filename = paste0(dir_out, "/Output/Figures/YOY/BKT/", foo$site_year[i], ".png"))
}

########## do for BNT ###########
df_bnt <- df_VT1 %>%
  dplyr::group_by(site_year) %>%
  dplyr::filter(Fish.Code == "BNT") %>%
  dplyr::summarise(count = n())

hist(df_bnt$count, breaks = 100)

foo <- df_bnt[which(df_bnt$count >= 75), ]

if(!exists(file.path(getwd(), dir_out, "/Output/Figures/YOY/BNT"))) dir.create(file.path(getwd(), dir_out, "/Output/Figures/YOY/BNT"), recursive = T)

# dont overwrite
if(!exists(file.path(getwd(), dir_out, "/Output/Figures/YOY/BNT/yoy_assignment_bnt.csv"))) write.csv(foo, file = file.path(getwd(), dir_out, "/Output/Figures/YOY/BNT/yoy_assignment_bnt.csv"), row.names = F)

# export histograms
for(i in 1:length(unique(foo$site_year))) {
  bar <- df_VT1 %>% dplyr::filter(site_year == foo$site_year[i] & Fish.Code == "BNT")
  g <- ggplot(bar, aes(Total.Length)) + geom_histogram(binwidth = 5) + ggtitle(paste0("BNT ", foo$site_year[i])) + xlim(50, 200) + geom_density(aes(y = 5*..count..)) 
  ggsave(filename = paste0(dir_out, "/Output/Figures/YOY/BNT/", foo$site_year[i], ".png"))
}

########## do for BNT ###########
df_rbt <- df_VT1 %>%
  dplyr::group_by(site_year) %>%
  dplyr::filter(Fish.Code == "RBT") %>%
  dplyr::summarise(count = n())

hist(df_rbt$count, breaks = 100)

foo <- df_rbt[which(df_rbt$count >= 15), ]

if(!exists(file.path(getwd(), dir_out, "/Output/Figures/YOY/RBT"))) dir.create(file.path(getwd(), dir_out, "/Output/Figures/YOY/RBT"), recursive = T)

# dont overwrite - Doesn't work, does overwrite - WTF!
if(!exists(file.path(getwd(), dir_out, "Output/Figures/YOY/RBT/yoy_assignment_rbt.csv"))) write.csv(foo, file = file.path(getwd(), dir_out, "Output/Figures/YOY/RBT/yoy_assignment_rbt.csv"), row.names = F)

# export histograms
for(i in 1:length(unique(foo$site_year))) {
  bar <- df_VT1 %>% dplyr::filter(site_year == foo$site_year[i] & Fish.Code == "RBT")
  g <- ggplot(bar, aes(Total.Length)) + geom_histogram(binwidth = 5) + ggtitle(paste0("RBT ", foo$site_year[i])) + xlim(50, 200) + geom_density(aes(y = 5*..count..)) 
  ggsave(filename = paste0(dir_out, "/Output/Figures/YOY/RBT/", foo$site_year[i], ".png"))
}
###########

# import trout stage cutoffs
yoy_bkt <- read.csv(file = file.path(getwd(), dir_out, "/Output/Figures/YOY/BKT/yoy_assignment_bkt2.csv"), header = T)
yoy_bnt <- read.csv(file = file.path(getwd(), dir_out, "/Output/Figures/YOY/BNT/yoy_assignment_bnt2.csv"), header = T)
yoy_rbt <- read.csv(file = file.path(getwd(), dir_out, "/Output/Figures/YOY/RBT/yoy_assignment_rbt2.csv"), header = T)

# join to df
df_VT1 <- df_VT1 %>%
  left_join(yoy_bkt) %>%
  left_join(yoy_bnt) %>%
  left_join(yoy_rbt)

str(df_VT1)

# replace max yoy with mean by species (could do mean by site and/or year but would get very messy as most sites and years don't have much replication and it varies by site & year & survey date)
df_VT1 <- df_VT1 %>%
  dplyr::mutate(yoy_max_bkt = ifelse(is.na(yoy_max_bkt), mean(yoy_max_bkt, na.rm = T), yoy_max_bkt),
                yoy_max_bnt = ifelse(is.na(yoy_max_bnt), mean(yoy_max_bnt, na.rm = T), yoy_max_bnt),
                yoy_max_rbt = ifelse(is.na(yoy_max_rbt), mean(yoy_max_rbt, na.rm = T), yoy_max_rbt))

# Assign stages
df_VT1 <- df_VT1 %>%
  dplyr::mutate(stage = ifelse(Fish.Code == "BKT" & Total.Length>= yoy_max_bkt, "adult", ifelse(Fish.Code == "BNT" & Total.Length>= yoy_max_bnt, "adult", ifelse(Fish.Code == "RBT" & Total.Length>= yoy_max_rbt, "adult", "yoy"))))


# Summarize to counts
df_sum <- df_VT1 %>%
  dplyr::group_by(SiteName, date, year, visit, Run.Number, Fish.Code, stage) %>%
  dplyr::summarise(count = n(),
                   length = mean(Station.Length),
                   width = mean(Average.Station.Width)) %>%
  dplyr::mutate(area = length*width)

#??? IS THERE A STANDARD SURVEY LENGTH WHEN NOT RECORDED??? #

# check if there's any variation in survey length when sites are repeatedly visited
df_station_var <- df_VT1 %>%
  dplyr::group_by(SiteName) %>%
  dplyr::summarise(length_mean = mean(Station.Length, na.rm = T),
                   length_min = min(Station.Length, na.rm = T),
                   length_max = max(Station.Length, na.rm = T),
                   length_sd = sd(Station.Length, na.rm = T))

df_station_na <- df_survey %>%
  dplyr::group_by(SiteName) %>%
  dplyr::filter(is.na(Station.Length)) %>%
  dplyr::summarise(events_na = n())

df_station_events<- df_survey %>%
  dplyr::group_by(SiteName) %>%
  #dplyr::filter(!is.na(Station.Length)) %>%
  dplyr::summarise(events = n())
                   
head(df_station_var, 20)
summary(df_station_var)

df_survey_lengths <- df_station_events %>%
  left_join(df_station_na) %>%
  left_join(df_station_var)  %>%
  dplyr::mutate(events_na = ifelse(is.na(events_na), 0, events_na),
                pct_missing = events_na/events*100)
  
df_survey_lengths
summary(df_survey_lengths)

# export
write.csv(df_survey_lengths, file = paste0(dir_data, "/Output_QAQC/station_variability.csv"))


# expand the grid grouping by site 
foo <- tidyr::complete(df_VT1, c(visit, Run.Number), Fish.Code, fill = list(count = 0))


df_sum <- df_bkt %>%
  dplyr::group_by(location_id, date, pass, species, stage) %>% # need to classify YOY
  dplyr::summarise(count = n(),
                   width_ft = mean(width),
                   area_ft2 = mean(area))

df_sum <- dplyr::arrange(ungroup(df_sum), location_id, date, stage, pass)
head(df_sum, 20)
summary(df_sum)
dim(df_sum)

# how to handle NA in length/stage?
dplyr::filter(df_bkt, is.na(length))

# it seems like NA represent zero captures for that species on that pass. I can throw these out then use tidyr::complete (expand.grid) to set these as 0 for all life age/stages.

df_sum <- df_sum %>%
  dplyr::filter(!is.na(stage)) %>%
  dplyr::mutate(visit = paste0(location_id, "_", as.character(date)))

# need to expand to all passes for all loction-dates-species
# use new tidyr wrapper for expand.grid
df_sum2 <- tidyr::complete(df_sum, c(visit), pass, stage, fill = list(count = 0)) %>%
  dplyr::arrange(location_id, date, species, stage, pass) %>%
  dplyr::mutate(year = year(date),
                month = month(date))
head(df_sum2, 20)
dim(df_sum2)
summary(df_sum2)

# check that the size I want
n_site_visits <- length(unique(df_sum$visit))
n_pass <- 3
n_stage <- 2
n_species <- 1

# assume ALWAYS have 3 passes per visit
n_rows_expect <- n_site_visits * n_pass * n_stage * n_species

n_rows_expect == dim(df_sum2)[1]


# if any fish or pass info is NA should the whole visit be excluded? - myabe if more than 10% of fish can't be assigned to a stage or if more than 10(5?)% of fish can't be assigned to a pass, throw out the site-visit.











