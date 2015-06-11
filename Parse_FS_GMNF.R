library(xlsx)
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
  df_list[[i]] <- read.xlsx(file.path(data_dir, data_files[i]), sheetIndex = 1, stringsAsFactors = FALSE)
  
  all(c("STREAM", "SITE", "PASS", "SPECIES", "TEST") %in% names(df_list[[i]]))
  
  if(!all(c("STREAM", "SITE", "PASS", "SPECIES") %in% names(df_list[[i]]))) {
    #print(paste0("Header names do not match. Removing file ", data_files[i]))
    
    df_list[[i]] <- NULL
    # output list of filenames that couldn't be imported
    cat(paste0(data_files[i], "\n"), file = file.path(out_dir, "error_files.txt"), append = TRUE)
  } else {
    
    # remove extra rows
    df_list[[i]] <- df_list[[i]][rowSums(is.na(df_list[[i]])) != ncol(df_list[[i]]), ]
    
    # date only on the top row below the header in some files. Therefore, if date == NA, replace with the one from row 2 if not NA
    df_list[[i]][is.na(df_list[[i]]$DATE), "DATE"] <- df_list[[i]][1, "DATE"]
    
  }
}

# combine
names(df_list) <- foo
df_fish <- bind_rows(df_list)


saveRDS(df_fish, paste0(file.path(out_dir, "Combined_Data.RData")))

df_fish <- rbindlist(df_list, use.names = TRUE, fill = TRUE)

df2 <- df_fish[ , 1:15]


# parse file name and if contains "MS" to add to file under treatment otherwise add "Control"


# combine with location information
