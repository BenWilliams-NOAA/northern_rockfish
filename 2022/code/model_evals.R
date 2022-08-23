library(groundfishr)
library(gfdata)
# github check
# globals ----
year = 2022
model_name = "nr"
dat_name = "goa_nr_2020"

# inital setup 
# setup(2022)
# accepted_model(2020, "m18.2b", 2022)

# create folders for different models to explore
# dir.create(here::here(year, "m18.2b_2022"))
# dir.create(here::here(year, "m18.2b_length"))
# dir.create(here::here(year, "srv_wt_25"))
# dir.create(here::here(year, "srv_wt_50"))
# dir.create(here::here(year, "srv_wt_75"))
# dir.create(here::here(year, "srv_wt_1"))
# 
# dir.create(here::here(year, "vast_wt_25"))
# dir.create(here::here(year, "vast_wt_50"))
# dir.create(here::here(year, "vast_wt_75"))
# dir.create(here::here(year, "vast_wt_1"))
# file.create(here::here(year, "README.MD")) # document different models 

# base files needed to run a model
# fls <- c("mat.dat", "nr.tpl", "goa_nr_2020.ctl", "goa_nr_2020.dat")
# 
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "m18.2b_length"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "srv_wt_25"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "srv_wt_50"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "srv_wt_75"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "srv_wt_1"))
# 
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "vast_wt_25"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "vast_wt_50"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "vast_wt_75"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "vast_wt_1"))
# file.copy(here::here(year, "base", fls),
#           here::here(year, "srv_sensitivity", "vast_wt_1_var"))

# compile all the models
# R2admb::setup_admb()

folders <- c("db_wt_25", "db_wt_50", "db_wt_75", "db_wt_1", "srv_wt_25","srv_wt_1", "srv_wt_50", "srv_wt_75")

# for(i in 1:length(folders)){
  # setwd(here::here(year, "srv_sensitivity", folders[i]))
  # # R2admb::compile_admb("nr")
  # R2admb::run_admb("nr", verbose = TRUE)
# }

# setwd(here::here())

vroom::vroom(here::here(2020, "data", "user_input", "VAST_estimates2.csv")) %>%
  dplyr::rename_all(tolower) -> dat

df <- data.frame()
for(i in 1:length(folders)){
  # read in rep 
  id = gsub("wt_", "", folders[i])

  REP <- readLines(here::here(year, "srv_sensitivity", folders[i],  paste0(model_name, ".rep")))
  
  pred = REP[grep("Survey Biomass",REP)[1]:(grep("Survey Biomass",REP)[2]-2)][3]
  pred = strsplit(pred," ")
  pred = subset(pred[[1]], pred[[1]]!="")
  pred = as.numeric(pred[2:length(pred)])
  df %>% 
    dplyr::bind_rows(
    data.frame(pred = pred,
               id = id, 
               year = dat$year)) -> df

}
df %>% 
  ggplot2::ggplot(ggplot2::aes(year, pred, color = id)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() +
  ggplot2::expand_limits(y = 0) +
  ggplot2::ggtitle("Survey biomass") +
  funcr::theme_report()


df2 <- data.frame()
for(i in 1:length(folders)){
  # read in rep and ctl files
  
  id = gsub("wt", "", folders[i])
  REP <- readLines(here::here(year, "srv_sensitivity", folders[i],  paste0(model_name, ".rep")))
  
  REP[grep("SpBiom", REP)] %>% 
    strsplit( " ") %>% 
    unlist() %>% 
    as.numeric() %>% 
    as.data.frame() %>% 
    tidyr::drop_na() %>% 
    dplyr::rename(sb = ".") -> pred
  
  df2 %>% 
    dplyr::bind_rows(
      data.frame(pred = pred$sb,
                 id = id,
                 year =  1961:2020)) -> df2
  
}

df2 %>% 
  ggplot2::ggplot(ggplot2::aes(year, pred, color = id)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_point() +
  ggplot2::expand_limits(y = 0) +
  funcr::theme_report() +
  ggplot2::ggtitle("Spawning biomass")
  

df3 <- data.frame()

for(i in 1:length(folders)){
  # read in rep and ctl files
  
  id = gsub("wt", "", folders[i])
  
  REP <- readLines(here::here(year, "srv_sensitivity", folders[i],  paste0(model_name, ".rep")))
  
  pred = REP[grep("Tot_biom",REP)][1]
  pred = strsplit(pred," ")
  pred = subset(pred[[1]], pred[[1]]!="")
  pred = as.numeric(pred[2:length(pred)])
  
  yr = REP[grep("Year",REP)][1]
  yr = strsplit(yr," ")
  yr = subset(yr[[1]], yr[[1]]!="")
  yr = as.numeric(yr[2:length(yr)])
  df3 %>% 
    dplyr::bind_rows(
      data.frame(pred = pred,
                 id = id, 
                 year = yr)) -> df3
  
}
df3 %>% 
  ggplot2::ggplot(ggplot2::aes(year, pred, color = id)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line() +
  ggplot2::expand_limits(y = 0) +
  ggplot2::ggtitle("Total biomass") +
  funcr::theme_report()

