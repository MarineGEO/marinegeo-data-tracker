library(fs)
library(here)
library(tidyverse)
library(purrr)

# set paths for the data folders
l0_data_path <- "../MarineGEO-data-L0"
l1_data_path <- "../MarineGEO-data-L1"

{
if(!fs::dir_exists(l0_data_path)){
  stop("Directory for level zero data does not exist. Check path.")
}
if(!fs::dir_exists(l1_data_path)){
  stop("Directory for level one data does not exist. Check path.")
}  

# level 0 data processing
dirsL0 <- tibble(path =list.dirs(l0_data_path)) %>% # lists all the dirs in the folder
  filter(!grepl("NETWORK", path))
leng <- dirsL0 %>% mutate(number=str_count(path,pattern="/")) %>% pull(number) %>% max() # max number of slashes
df_dirsL0 <- dirsL0 %>% separate(path, into=c(paste0("folder",0:leng+1)), sep="/", remove=FALSE) # split df into columns by forword slash
level0_df <- df_dirsL0 %>% 
  select(path, root=folder2, site=folder3, year=folder4, project=folder5) %>% # select and rename columns of interest
  filter(!is.na(year), !is.na(project)) %>% # filter out null values
  mutate(level=0)  %>% # add the data level
  distinct(site, year, project, .keep_all = TRUE) # remove any duplicate rows

# level 1 data processing
dirsL1 <- tibble(path =list.dirs(l1_data_path)) %>% # lists all the dirs in the folder
  filter(!grepl(".git", path))
leng <- dirsL1 %>% mutate(number=str_count(path,pattern="/")) %>% pull(number) %>% max() # max number of slashes
df_dirsL1 <- dirsL1 %>% separate(path, into=c(paste0("folder",0:leng+1)), sep="/", remove=FALSE) # split df into columns by forword slash
level1_df <- df_dirsL1 %>% 
  select(path, root=folder2, project=folder3, year_site=folder4) %>% # select and rename columns of interest
  separate(year_site, into=c('year', 'site'), sep="_") %>% 
  filter(!is.na(year), !is.na(project)) %>% # filter out null values
  mutate(level=1)  %>% # add the data level
  distinct(site, year, project, .keep_all = TRUE) # remove any duplicate rows

level1_df

# save to disk as a csv
write_csv(level0_df, here("data/data-status-level-zero.csv"))
write_csv(level1_df, here("data/data-status-level-one.csv"))
}




