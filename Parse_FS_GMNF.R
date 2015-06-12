# library(xlsx) - causes fatal errors with R?
library(readxl)
library(stringr)
library(dplyr)

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

for(i in 1:n_files) {
  setTxtProgressBar(pb = pb, value = i/n_files)
  foo[i] <- str_split(data_files[i], "\\.")[[1]][1]
  df_list[[i]] <- read_excel(file.path(data_dir, data_files[i]), sheet = 1)

  if(!all(c("STREAM", "SITE", "PASS", "SPECIES") %in% names(df_list[[i]]))) {
    #print(paste0("Header names do not match. Removing file ", data_files[i]))
    
    df_list[[i]] <- NULL
    # output list of filenames that couldn't be imported
    cat(paste0(data_files[i], "\n"), file = file.path(out_dir, "error_files.txt"), append = TRUE)
  } else {
    
    # remove extra rows
    df_list[[i]] <- df_list[[i]][rowSums(is.na(df_list[[i]])) != ncol(df_list[[i]]), ]
    
    # Sometimes only top row of data entered for things that don't change within a sample: need to carry it down. For example, date is only on the top row below the header in some files. Therefore, if date == NA, replace with the one from row 2 if not NA
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
    
    # append file name
    df_list[[i]]$file_name <- data_files[i]
    
  }
}
# packages cause R to R when do summary or otherwise manipulate the df
#detach("package:xlsx", unload = TRUE)
#detach("package:xlsxjars", unload = TRUE)
#detach("package:rJava", unload = TRUE)

# combine
names(df_list) <- foo
df_fish <- bind_rows(df_list) # try with dplyr unless types don't match
# if dplyr doesn't work use data.table
if(!exists("df_fish")) {
  library(data.table)
  df_fish <- rbindlist(df_list, use.names = TRUE, fill = TRUE)
  detach("package:data.table", unload=TRUE)
}

df_fish <- as.data.frame(unclass(df_fish))

saveRDS(df_fish, paste0(file.path(out_dir, "Combined_Data.RData")))

summary(df_fish)
str(df_fish)

#-------------cleaning------------

# Remove extra columns
keepers <- c("STREAM", "WATERSHED", "SITE", "ELEVATION", "DATE", "WIDTH", "AREA", "AIRTEMP", "WATERTEMP", "CONDUCTIVITY", "PASS", "SPECIES", "LENGTH", "WEIGHT", "AGECLASS", "file_name")

df_fish <- df_fish %>%
  dplyr::select(one_of(keepers))

# rename
df_fish <- df_fish %>%
  dplyr::rename(stream = STREAM, watershed = WATERSHED, site = SITE, elevation = ELEVATION, date = DATE, width = WIDTH, area = AREA, airtemp = AIRTEMP, watertemp = WATERTEMP, conductivity = CONDUCTIVITY, pass = PASS, species = SPECIES, length = LENGTH, weight = WEIGHT, ageclass = AGECLASS)

summary(df_fish)
str(df_fish)

# add unique ids?

# clean length
df_fish$length <- as.character(df_fish$length)
df_fish[which(df_fish$length == "NONE"), "length"] <- NA
df_fish$length <- as.numeric(df_fish$length)
summary(df_fish$length)

# parse file name and if contains "MS" to add to file under treatment otherwise add "Control"

#

# combine with location information

# dates
library(lubridate)
df_fish$datel <- ymd(df_fish$date)

df_sum <- df_fish %>%
  dplyr::group_by(stream, site, date, pass, species) %>% # need to classify YOY
  dplyr::summarise(count = n())


