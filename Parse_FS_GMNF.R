# library(xlsx) - causes fatal errors with R?
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Preprocessing outside of R
# Unlocked 3 files
# Used bash script to replace all spaces in file names with underscore
# for f in *\ *; do mv "$f" "${f// /_}"; done
# Then added a prefix to prevent numbers at start (not shure how to do this in 1 step)
# for f in *; do mv "$f" "fs_$f"; done;

# data directory relative to the working directory
data_dir <- "Data/Fish_Data_Raw/Forest_Service_GMNF/Fish_Data"

# output directory
out_dir <- "Data/Fish_Data_Processed/Forest_Service_GMNF"

if(!exists(file.path(getwd(), out_dir))) dir.create(file.path(getwd(), out_dir))

# get list of file names
data_files <- list.files(data_dir)
n_files <- length(data_files)

# If filename has set structure then import - might not be necessary

# Import files with xlsx package
df_list <- list()
foo <- NULL
cat("List of files with unmatched headers not imported\n", file = file.path(out_dir, "error_files.txt"))
pb <- txtProgressBar(min = 0, max = 1, style = 3)
j = 0
for(i in 1:n_files) {
  setTxtProgressBar(pb = pb, value = i/n_files)
  foo[i] <- str_split(data_files[i], "\\.")[[1]][1]
  df_list[[i]] <- read_excel(file.path(data_dir, data_files[i]), sheet = 1)
  
  if(!all(c("STREAM", "SITE", "PASS", "SPECIES") %in% names(df_list[[i]])) | all(is.na(df_list[[i]]$DATE))) {
    #print(paste0("Header names do not match. Removing file ", data_files[i]))
    #j <- j + 1
    df_list[[i]] <- NULL
    # output list of filenames that couldn't be imported
    cat(paste0(data_files[i], "\n"), file = file.path(out_dir, "error_files.txt"), append = TRUE)
  } else {
    
    #df_list[[i]] <- bar
    # remove extra rows
    df_list[[i]] <- df_list[[i]][rowSums(is.na(df_list[[i]])) != ncol(df_list[[i]]), ]
    
    # Sometimes only top row of data entered for things that don't change within a sample: need to carry it down. For example, date is only on the top row below the header in some files. Therefore, if date == NA, replace with the one from row 2 if not NA
    # This assumes that only one site and date per data sheet
    # carry date
    if("DATE" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$DATE), "DATE"] <- df_list[[i]][1, "DATE"]
    # carry stream
    if("STREAM" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$STREAM), "STREAM"] <- df_list[[i]][1, "STREAM"]
    # carry watershed
    if("WATERSHED" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$WATERSHED), "WATERSHED"] <- df_list[[i]][1, "WATERSHED"]
    # carry site
    if("SITE" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$SITE), "SITE"] <- df_list[[i]][1, "SITE"]
    # carry elevation
    if("ELEVATION" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$ELEVATION), "ELEVATION"] <- df_list[[i]][1, "ELEVATION"]
    # carry width
    if("WIDTH" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$WIDTH), "WIDTH"] <- df_list[[i]][1, "WIDTH"]
    # carry area
    if("AREA" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$AREA), "AREA"] <- df_list[[i]][1, "AREA"]
    # carry air temp
    if("AIRTEMP" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$AIRTEMP), "AIRTEMP"] <- df_list[[i]][1, "AIRTEMP"]
    # carry water temp
    if("WATERTEMP" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$WATERTEMP), "WATERTEMP"] <- df_list[[i]][1, "WATERTEMP"]
    # carry conductivity
    if("CONDUCTIVITY" %in% names(df_list[[i]])) df_list[[i]][is.na(df_list[[i]]$CONDUCTIVITY), "CONDUCTIVITY"] <- df_list[[i]][1, "CONDUCTIVITY"]
    
    
    # Clean Dates
    df_list[[i]]$DATE <- as.character(df_list[[i]]$DATE)
    df_list[[i]]$DATE <- parse_date_time(df_list[[i]]$DATE, c("ymd", "mdy"))
    
    # append file name
    df_list[[i]]$file_name <- data_files[i]
    df_list[[i]] <- as.data.frame(unclass(df_list[[i]]))
  }
}
# packages cause R to R when do summary or otherwise manipulate the df
#detach("package:xlsx", unload = TRUE)
#detach("package:xlsxjars", unload = TRUE)
#detach("package:rJava", unload = TRUE)

# check dates and identify which files have problems before combining
file_names <- NULL
date_class <- NULL
file_date <- NULL
for(i in 1:length(df_list)) {
  if(is.null(df_list[[i]])) {
    file_names[i] <- NA
    date_class[i] <- NA
    file_date[i] <- NA
  } else {
  file_names[i] <- as.character(df_list[[i]]$file_name[1])
  date_class[i] <- class(df_list[[i]]$DATE)[1]
  file_date[i] <- as.character(df_list[[i]]$DATE[1])
}
}
df_dates <- data.frame(file_names, date_class, file_date)
df_dates

# combine
names(df_list) <- foo
df_fish <- bind_rows(df_list) # try with dplyr unless types don't match
# if dplyr doesn't work use data.table
if(!exists("df_fish")) {
  library(data.table)
  df_fish <- rbindlist(df_list, use.names = TRUE, fill = TRUE)
  df_fish <- as.data.frame(unclass(df_fish))
 # detach("package:data.table", unload=TRUE) # detaching & unloading crashes R when do summary()
}

df_fish <- as.data.frame(unclass(df_fish))

saveRDS(df_fish, paste0(file.path(out_dir, "Combined_Data.RData")))

#-------------cleaning------------

# Remove extra columns
keepers <- c("STREAM", "WATERSHED", "SITE", "ELEVATION", "DATE", "WIDTH", "AREA", "AIRTEMP", "WATERTEMP", "CONDUCTIVITY", "PASS", "SPECIES", "LENGTH", "WEIGHT", "AGECLASS", "file_name")

df_fish <- df_fish %>%
  dplyr::select(one_of(keepers))

# rename
df_fish <- df_fish %>%
  dplyr::rename(stream = STREAM, watershed = WATERSHED, site = SITE, elevation = ELEVATION, date = DATE, width = WIDTH, area = AREA, airtemp = AIRTEMP, watertemp = WATERTEMP, conductivity = CONDUCTIVITY, pass = PASS, species = SPECIES, length = LENGTH, weight = WEIGHT, ageclass = AGECLASS)

str(df_fish)
summary(df_fish)

# add unique ids?

# clean length
df_fish$length <- as.character(df_fish$length)
df_fish[which(df_fish$length == "NONE"), "length"] <- NA
df_fish$length <- as.numeric(df_fish$length)
summary(df_fish$length)

# clean weight
unique(df_fish$weight)
df_fish[which(df_fish$weight == "<1" | df_fish$weight == "UNK" | df_fish$weight == " " | df_fish$weight == "N/A" | df_fish$weight == "na"), "weight"] <- NA
df_fish$weight <- as.numeric(df_fish$weight)
summary(df_fish$weight)

# parse file name and if contains "MS" to add to file under treatment otherwise add "Control"

unique(df_fish$stream)
unique(df_fish$site)
dplyr::filter(df_fish, site == "MS3")

df_fish <- mutate(df_fish, site = as.character(site))
df_fish <- df_fish %>%
  dplyr::mutate(site = ifelse(site == "MS-1", "MS1", site),
                site = ifelse(site == "MS-2", "MS2", site),
                site = ifelse(site == "CTRL", "Control", site),
                site = ifelse(site == "control", "Control", site),
                site = ifelse(site == "contol", "Control", site),
                site = ifelse(site == "Contol", "Control", site),
                site = ifelse(site == "CONTROL", "Control", site),
                site = ifelse(site == "Lower MS1", "MS1", site),
                site = ifelse(site == "Monitor", "MS1", site),
                site = ifelse(site == "MS1 ", "MS1", site),
                site = ifelse(site == "MS upper ", "MS1", site),
                site = ifelse(site == "MS upper", "MS1", site),
                site = ifelse(site == "MS ", "MS", site),
                site = ifelse(site == "Lower MS (MS2)", "MS2", site),
                site = ifelse(site == "Upper MS (MS1)", "MS1", site),
                site = ifelse(site == "Upper MS(MS1)", "MS1", site)
                )
unique(df_fish$site)

df_fish$site <- gsub(" ", "_", x = df_fish$site)
unique(df_fish$site)

# clean stream names
str_brook_matches <- c(" Bk.", " Bk", " Brk", " Brook", " brook", " BROOK", " bk", " BK", " BK.")
replacement_str <- rep("", times = length(str_brook_matches))
names(replacement_str) <- str_brook_matches
df_fish$stream <- str_replace_all(df_fish$stream, pattern = replacement_str)
unique(df_fish$stream)

df_fish2 <- df_fish
df_fish2$stream <- gsub("Chittenden Control", "Chittenden", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub(", ", " ", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Michigan Branch Tweed River", "Michigan", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Michigan Branch", "Michigan", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Michigan", "Michigan Branch Tweed River", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Brnch.", "Branch", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Br.", "Branch", x = df_fish2$stream, fixed = T)
unique(df_fish2$stream)

df_fish2$stream <- gsub("Robbins Branch Post TS Irene", "Robbins", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Robbins Branch", "Robbins", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Ck.", "Creek", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("So.", "South", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("No.", "North", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("N.", "North ", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Mt.", "Mount", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Mt ", "Mount ", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("M.", "Mount", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("R.", "River", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("SouthBranch", "South Branch", x = df_fish2$stream, fixed = T)
unique(df_fish2$stream)

df_fish2$stream <- gsub("(Feller)", "_Feller", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Middlebury River", "Middlebury", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Branch of", "Branch", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Branch.", "Branch", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub(" Mainstem", "", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub(" mainstem", "", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub(", Mainstem", "", x = df_fish2$stream, fixed = T)
df_fish2$stream <- gsub("Steammill", "Steam Mill", x = df_fish2$stream, fixed = T)
df_fish2$stream <- str_trim(df_fish2$stream, side = "right")
unique(df_fish2$stream)

# identify when additional passes were completed without any captures and save for later use
empty_passes <- dplyr::filter(df_fish2, grepl("[[:punct:]]{2}", stream))
saveRDS(empty_passes, file = file.path(out_dir, "Empty_Passes.RData"))

df_fish2 <- dplyr::filter(df_fish2, !grepl("[[:punct:]]{2}", stream))

# replace spaces with underscores in stream names
df_fish2$stream <- gsub(" ", "_", x = df_fish2$stream)

# combine stream names and site names to make unique location_id
df_fish2$location_id <- paste0(df_fish2$stream, "_", df_fish2$site)
unique(df_fish2$location_id)

# combine with location information
df_locations <- read.table("Data/Fish_Data_Raw/Forest_Service_GMNF/locations_cleaned.csv", header = T, sep = ",", stringsAsFactors = FALSE)

head(df_locations)

df_locations$stream <- gsub("Ck.", "Creek", x = df_locations$stream)
df_locations$stream <- gsub("So.", "South", x = df_locations$stream)
df_locations$stream <- gsub("No.", "North", x = df_locations$stream)
df_locations$stream <- gsub("Mt.", "Mount", x = df_locations$stream)
df_locations$stream <- gsub(" Brook", "", x = df_locations$stream)
df_locations$stream <- gsub("Robbins Branch", "Robbins", x = df_locations$stream)
df_locations$stream <- gsub("Middlebury River", "Middlebury", x = df_locations$stream)

df_locations$site <- gsub("MS_Lower_MS1", "MS1", x = df_locations$site)
df_locations$site <- gsub("MS_Upper_MS2", "MS2", x = df_locations$site)
df_locations$site <- gsub("MS_Lower", "MS2", x = df_locations$site)
df_locations$site <- gsub("MS_Upper", "MS1", x = df_locations$site)
df_locations$site <- gsub("Granville_Bowl_Mill", "Granville", x = df_locations$site)
df_locations$site <- gsub("MS1_Control", "Control", x = df_locations$site)
df_locations$site <- gsub("MS2_MS1", "MS2", x = df_locations$site)
df_locations$site <- gsub("Contol", "Control", x = df_locations$site)

df_locations$stream <- gsub(" ", "_", x = df_locations$stream)

unique(df_locations$stream)

df_locations$location_id <- paste0(df_locations$stream, "_", df_locations$site)
unique(df_locations$location_id)

length(unique(df_locations$location_id))
length(unique(df_fish2$location_id))
unique(df_fish2$location_id)

fish_locations <- sort(unique(df_fish2$location_id))
locations <- sort(unique(df_locations$location_id))

fish_locations[!(fish_locations %in% locations)]

write.table(fish_locations, file = "Data/Fish_Data_Processed/Forest_Service_GMNF/unique_fish_locations.txt")
write.table(locations, file = "Data/Fish_Data_Processed/Forest_Service_GMNF/unique_locations.txt")

# fix so locations and fish locations match
df_fish2$location_id <- gsub("Goshen_control", "Goshen_East_Control", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Baker_MS1", "Baker_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Bingo_MS2", "Bingo_MS3", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Blue_Bank_MS1", "Blue_Bank_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Blue_Bank_MS", "Blue_Bank_MS1", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Bluebank_MS1", "Blue_Bank_MS1", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Branch_Pond_MS1", "Branch_Pond_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Brandon_MS1", "Brandon_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Burnt_Meadow_MS1", "Burnt_Meadow_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Chittenden_MS1", "Chittenden_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Clark_MS1", "Clark_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Clark_MS", "Clark_MS1", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Deerfield_River_MS1", "Deerfield_River_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Farnum_MS1", "Farnum_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Farnum_MS", "Farnum_MS1", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Flood_MS1", "Flood_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Goshen_Control", "Goshen_East_Control", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Griffith_MS1", "Griffith_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Hale_MS1", "Hale_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Hewitt_MS1", "Hewitt_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Lamb_MS1", "Lamb_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Mill_MS1", "Mill_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("North_Branch_Middlebury_MS1", "North_Branch_Middlebury_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Patterson_MS1", "Patterson_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Sparks_MS1", "Sparks_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Townsend_MS1", "Townsend_MS", x = df_fish2$location_id, fixed = T)
df_fish2$location_id <- gsub("Yaw_Pond_MS1", "Yaw_Pond_MS", x = df_fish2$location_id, fixed = T)

df_fish2 <- df_fish2 %>%
  dplyr::mutate(location_id = ifelse(location_id == "Utley_MS", "Utley_MS1", location_id),
                location_id = ifelse(location_id == "Steam_Mill_MS", "Steam_Mill_MS1", location_id),
                location_id = ifelse(location_id == "Jones_MS", "Jones_MS_Control", location_id),
                location_id = ifelse(location_id == "Jones_Control", "Jones_MS_Control", location_id),
                location_id = ifelse(location_id == "Jones_MS1", "Jones_MS_Control", location_id),
                year = year(date),
                month = month(date))
                

fish_locations <- sort(unique(df_fish2$location_id))
locations <- sort(unique(df_locations$location_id))

fish_locations[!(fish_locations %in% locations)]

# merge individual capture data with locations
df_ind <- left_join(dplyr::select(df_fish2, -elevation), dplyr::select(df_locations, featureid, projection, location_id, huc12, latitude, longitude))
str(df_ind)

# reorder columns
df_ind <- df_ind %>%
  dplyr::select(location_id, featureid, latitude, longitude, projection, huc12, watershed, stream, site, date, airtemp, watertemp, conductivity, width, area, pass, species, length, weight, ageclass, file_name)
summary(df_ind)

# convert all factors to characters
i <- sapply(df_ind, is.factor)
df_ind[i] <- lapply(df_ind[i], as.character)
df_ind$huc12 <- as.character(df_ind$huc12)
df_ind$pass <- as.integer(df_ind$pass)

# save combined, cleaned data
write.csv(df_ind, file = paste0(file.path(out_dir, "Combined_Individual_Data.csv")), row.names = FALSE)
saveRDS(df_ind, file = file.path(out_dir, "GMNF_Individual.RData"))

# make visit table
df_visit <- df_ind %>%
  group_by(location_id, date, file_name) %>%
  dplyr::summarise(width_ft = mean(width, na.rm = T),
                   area_ft2 = mean(area, na.rm = T),
                   conductivity = mean(as.numeric(conductivity), na.rm = T)) %>%
  dplyr::mutate(visit = paste(location_id, "_", date))


# make location table
df_location <- df_ind %>%
  group_by(location_id, featureid, huc12, projection) %>%
  dplyr::summarise(latitude = mean(latitude, na.rm = T),
                   longitude = mean(longitude, na.rm = T))
dim(df_location)


# for kyle
foo <- df_visit %>%
  ungroup() %>%
  group_by(location_id) %>%
  mutate(year=year(date)) %>%
  dplyr::filter(year > 2000)

foo <- as.data.frame(unique(foo$location_id))
names(foo) <- "location_id"

kyle <- left_join(foo, df_location) %>%
  dplyr::filter(!is.na(latitude))

str(kyle)
write.table(kyle, file = file.path(out_dir, "sample_nodes.csv"), sep = ",", row.names = F)



