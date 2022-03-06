### Testing cleaning of travel times for 1 month

library(ggplot2)
library(reshape2)
library(dplyr)
library(chron)

######### Read in camera and link data that is common to all time periods:

camera_locs1 <- read.csv("BCCANPR21_cameraIDandLocation.csv", 
                     stringsAsFactors = F)

site_group1 <- camera_locs1[,c("VAEid", "cam_loc")]


# now combine camera location numbers with the distance matrix data 
cameras3 <- read.csv("BCCANPR21_DistMatrix_withGROUP.csv", 
                     stringsAsFactors = F)

camera3b <- merge(cameras3, site_group1, 
                  by.x = "start_site", by.y = "VAEid",
                  all.x=T)

camera3b <- rename(camera3b, start_cam_loc = cam_loc)

camera3b <- merge(camera3b, site_group1, 
                  by.x = "end_site", by.y = "VAEid",
                  all.x=T)

camera3b <- rename(camera3b, end_cam_loc = cam_loc)

camera3b$link <- paste0(camera3b$start_cam_loc, "_TO_", camera3b$end_cam_loc)

cameras4 <- unique(camera3b[,c("start_cam_loc", "end_cam_loc", "link",
                               "travelDistance", "travelDuration")])

# thresholds for each 'link'
cameras4$init_thresh <- 60*60
cameras4$init_thresh[cameras4$travelDuration > (60*60/5)] <- 5*cameras4$travelDuration[cameras4$travelDuration > (60*60/5)]

# separate rule if == 0 => no 'possible' for these, just OK if < 15 mins and reject otherwise
cameras4$init_thresh[cameras4$travelDuration == 0] <- 0

cameras4$accept_thresh <- 15*60
cameras4$accept_thresh[cameras4$travelDuration > (60*10)] <- 1.5*cameras4$travelDuration[cameras4$travelDuration > (60*10)]

cameras4_thresh <- cameras4[,c("link","init_thresh","accept_thresh", "travelDuration")]


##########################################################################################
# sample of 20 links for day to day comparison:
freq_links <- c("36_TO_33", "33_TO_35", "2_TO_10", "38_TO_40", "40_TO_38", 
                  "13_TO_2", "33_TO_30", "33_TO_31", "25_TO_26", "33_TO_25", 
                "31_TO_33", "19_TO_21", "21_TO_19", "32_TO_33", 
                "26_TO_25", "26_TO_33", "36_TO_19", "19_TO_35", 
                "8_TO_19", "37_TO_13")






files_in_wd2 <- list.files()
files_in_wd2 <- files_in_wd2[grepl("_2019-", files_in_wd2)]

cleaning_info1 <- data.frame(matrix(ncol = 0, nrow = length(files_in_wd2)))


j=1

for (j in 1:length(files_in_wd2)){
  
  file1 <- files_in_wd2[j]
  cleaning_info1$file[j] <- file1
  
  full_data <- read.csv(file1, header=T, sep = ",", stringsAsFactors=F)
  
  full_data <- full_data[with(full_data, order(Date, Time)), ]  
  
  full_data$Date <- as.character(full_data$Date)
  full_data$Time <- as.character(full_data$Time)
  
  full_data$DateTime <- paste(full_data$Date, full_data$Time, sep = " ")
  full_data$DateTime <- strptime(full_data$DateTime,"%Y-%m-%d %H:%M:%S")
  full_data$DateTime <- as.POSIXct(full_data$DateTime)
  
  
  ###########################################################################################
  # link together observations for the same number plate which follow one another
  
  full_data2 <- full_data %>%
    group_by(VRM) %>%
    mutate(Prev_VAEid = lag(VAEid),
           Prev_DateTime = lag(DateTime),
           Prev_Vehicle.Id = lag(VRM)) 
  
  
  full_data2$ttime_sec <- full_data2$DateTime - full_data2$Prev_DateTime
  
  full_data2$ttime_sec <- as.numeric(full_data2$ttime_sec, unit = "secs")
  
  # what to have as upper limit? - Just put as 12 hours to separate out trips made on different days etc.
  #############################################################################
  full_data_fin <- filter(full_data2, ttime_sec < (60*60*12))
  full_data_fin <- filter(full_data_fin, is.na(Prev_Vehicle.Id)==F)
  
  

  ####  add in additional site info
  ANPR_data1 <- rename(full_data_fin, group = site)

  ANPR_data1 <- merge(ANPR_data1, site_group1, 
                      by.x = "Prev_VAEid", by.y = "VAEid",
                      all.x=T)

  ANPR_data1 <- rename(ANPR_data1, Prev_site = cam_loc)
  
  ANPR_data1 <- merge(ANPR_data1, site_group1, 
                      by.x = "VAEid", by.y = "VAEid",
                      all.x=T)
  
  ANPR_data1 <- rename(ANPR_data1, site = cam_loc)
  
  ANPR_data1$link <- paste0(ANPR_data1$Prev_site, "_TO_", ANPR_data1$site)
  
  print(nrow(ANPR_data1))
  
  cleaning_info1$obs_read[j] <- nrow(ANPR_data1)
  
  
  
  #####################################################
  #### Exclude multiple reads (note that 'site' here is a camera set location)
  ANPR_data5 <- subset(ANPR_data1, !(site == Prev_site & ttime_sec < 60))
  
  print(nrow(ANPR_data5))
  cleaning_info1$obs_after_multread[j] <- nrow(ANPR_data5)
  
  
  
  ##############################################################
  ### Look at ttimes relative to Bing estimate
  
  #####################################################################
  ###### STEP 1 - Take out crazy times:
  ANPR_data5$hour <- substr(ANPR_data5$Time, 1,2)
  
  ANPR_data5 <- merge(ANPR_data5, cameras4_thresh,
                      by.x= "link", by.y = "link")
  
  ANPR_data5$flag <- "Reject"
  ANPR_data5$flag[ANPR_data5$ttime_sec < ANPR_data5$init_thresh] <- "Possible"
  ANPR_data5$flag[ANPR_data5$ttime_sec < ANPR_data5$accept_thresh] <- "OK"
  
  ftable(ANPR_data5$flag)
  
  ANPR_data5 <- subset(ANPR_data5, flag != "Reject")
  
  print(nrow(ANPR_data5))
  cleaning_info1$obs_after_init_thresh[j] <- nrow(ANPR_data5)
  cleaning_info1$accept_thresh[j] <- nrow(ANPR_data5[ANPR_data5$flag == "OK",])
  cleaning_info1$poss_thresh[j] <- nrow(ANPR_data5[ANPR_data5$flag == "Possible",])
  
  
  #####################################################################
  ###### STEP 2 - Compare to vehicles doing same link at same time:
  
  #### look at up to 3 cars before and after the current one
  ANPR_data5 <- arrange(ANPR_data5, DateTime)
  
  cars_around <- ANPR_data5 %>% 
    group_by(link) %>%
    mutate(Prev_car_time = lag(ttime_sec),
           Prev_DateTime = lag(DateTime),
           Prev2_car_time = lag(ttime_sec, n=2),
           Prev2_DateTime = lag(DateTime, n=2),
           Prev3_car_time = lag(ttime_sec, n=3),
           Prev3_DateTime = lag(DateTime, n=3),
           Next_car_time = lead(ttime_sec),
           Next_DateTime = lead(DateTime),
           Next2_car_time = lead(ttime_sec, n=2),
           Next2_DateTime = lead(DateTime, n=2),
           Next3_car_time = lead(ttime_sec, n=3),
           Next3_DateTime = lead(DateTime, n=3))  
    
  cars_around3 <- subset(cars_around, flag == "Possible")
  cars_around3 <- subset(cars_around3, !is.na(Prev_car_time))
  
  cars_around3$DateTime <- strptime(cars_around3$DateTime,"%Y-%m-%d %H:%M:%S")
  cars_around3$DateTime <- as.POSIXct(cars_around3$DateTime)
  
  
  # summary of 3 vehicles before and after
  cars_around3$num_cars <- apply(cars_around3[,c("Prev_car_time", "Prev2_car_time", "Prev3_car_time", 
                                                 "Next_car_time", "Next2_car_time", "Next3_car_time")], 1, function(x) length(which(!is.na(x))))
  
  cars_around3$av_others <- rowMeans(cars_around3[,c("Prev_car_time", "Prev2_car_time", "Prev3_car_time", 
                                                    "Next_car_time", "Next2_car_time", "Next3_car_time")], na.rm = T)
    
 
  cars_around3$flag[(cars_around3$num_cars >= 3) & (cars_around3$ttime_sec < 1.5*cars_around3$av_others)] <- "OK 2"
  
  
  # Update main dataset:
  cars_around3 <- rename(cars_around3, updated_flag = flag)
  
  ANPR_data5 <- merge(ANPR_data5, cars_around3[,c("link", "VRM", "Date", "Time", "ttime_sec","updated_flag")],
                      by = c("link", "VRM", "Date", "Time", "ttime_sec"),
                      all.x=T)
  
  ANPR_data5$updated_flag[is.na(ANPR_data5$updated_flag)] <- ANPR_data5$flag[is.na(ANPR_data5$updated_flag)]
  
  ftable(ANPR_data5$updated_flag)
  
  cleaning_info1$accept_CarsAround[j] <- nrow(ANPR_data5[ANPR_data5$updated_flag == "OK 2",])
  
  
  
  
  
  
  
  
  #####################################################################
  ###### STEP 3 - Compare to 75th %-ile for link and TOD:
  
  #################################################################
  #### Look at whether likely based on hour of day:
  
  summ_data4 <- ANPR_data5 %>% 
    group_by(link, hour) %>% 
    summarise(num_obs = length(ttime_sec),
              seventyfive_perc = quantile(ttime_sec, 0.75))
  
  possibles2 <- subset(ANPR_data5, updated_flag == "Possible")
  
  possibles2 <- merge(possibles2, summ_data4,
                      by = c("link","hour"), all.x=T)
  
  # delete flag col so it doesn't cause confusion later
  possibles2$flag <- NULL
  
  # accept as long as not in top 25% of OK and possibles on this link at this hour of the day
  possibles2$updated_flag[possibles2$num_obs >= 25 &
                            possibles2$ttime_sec <= possibles2$seventyfive_perc] <- "OK 3"
  
  
  # Update main dataset:
  possibles2 <- rename(possibles2, updated_flag2 = updated_flag)
  
  ANPR_data5 <- merge(ANPR_data5, possibles2[,c("link", "VRM", "Date", "Time", "ttime_sec","updated_flag2")],
                      by = c("link", "VRM", "Date", "Time", "ttime_sec"),
                      all.x=T)
  
  ANPR_data5$updated_flag2[is.na(ANPR_data5$updated_flag2)] <- ANPR_data5$updated_flag[is.na(ANPR_data5$updated_flag2)]
  
  ftable(ANPR_data5$updated_flag2)
  
  cleaning_info1$accept_HourlyTimes[j] <- nrow(ANPR_data5[ANPR_data5$updated_flag2 == "OK 3",])
  
  
  
  
  
  
  
  
  #########################################################################################
  ###### STEP 4 - Compare to 75th %-ile of link travel times x adjustment for date/TOD :
  
  summ_data5_link <- ANPR_data5 %>% 
    group_by(link) %>% 
    summarise(link_75perc = quantile(ttime_sec, 0.75))
  
  
  summ_data5_dayslink <- ANPR_data5 %>% 
    group_by(link, Date) %>% 
    summarise(av_time = mean(ttime_sec))
  
  
  summ_data5_hourlink <- ANPR_data5 %>% 
    group_by(link, hour) %>% 
    summarise(av_time = mean(ttime_sec))
  
  ###########################################################################################
  # use sample of 20 links freq used links (i.e. lots of data) to calc. adjustment factors
  
  #   HOUR factor
  summ_data5_hourlink_freq <- subset(summ_data5_hourlink, link %in% freq_links)
  
  freq_links_hourly <- summ_data5_hourlink_freq %>% 
    group_by(hour) %>% 
    summarise(av_time_comb = sum(av_time),
              num_links = length(av_time))
  
  av_all_hrs <- sum(freq_links_hourly$av_time_comb)/nrow(freq_links_hourly)
  
  freq_links_hourly$hour_factor <- freq_links_hourly$av_time_comb/av_all_hrs
  cleaning_info1$hour_factor[j] <- "calc"
    
  
  if (sum(freq_links_hourly$num_links) < 24*20){
    
    freq_links_hourly$hour_factor <- 1
    cleaning_info1$hour_factor[j] <- "not calc"
    
  } 
  
  
  #   DAY factor
  summ_data5_dayslink_freq <- subset(summ_data5_dayslink, link %in% freq_links)
  
  freq_links_daily <- summ_data5_dayslink_freq %>% 
    group_by(Date) %>% 
    summarise(av_time_comb = sum(av_time),
              num_links = length(av_time))
  
  av_all_days <- sum(freq_links_daily$av_time_comb)/nrow(freq_links_daily)
  
  freq_links_daily$day_factor <- freq_links_daily$av_time_comb/av_all_days
  cleaning_info1$day_factor[j] <- "calc"
  
  if (sum(freq_links_daily$num_links) < 20*nrow(freq_links_daily)){
    
    freq_links_daily$day_factor <- 1
    cleaning_info1$day_factor[j] <- "not calc"
  } 
  
  
  #############################################################################
  
  ANPR_data5 <- merge(ANPR_data5, summ_data5_link[, c("link", "link_75perc")], 
                      by = "link", all.x = T)
  ANPR_data5 <- merge(ANPR_data5, freq_links_hourly[,c("hour", "hour_factor")], 
                      by = "hour", all.x = T)
  ANPR_data5 <- merge(ANPR_data5, freq_links_daily[,c("Date", "day_factor")], 
                      by = "Date", all.x = T)
  
  
  ANPR_data5$updated_flag3 <- ANPR_data5$updated_flag2
  
  ANPR_data5$updated_flag3[(ANPR_data5$updated_flag2 == "Possible") & 
                             (ANPR_data5$ttime_sec < (ANPR_data5$link_75perc*ANPR_data5$hour_factor))] <- "OK 4"
  ANPR_data5$updated_flag3[(ANPR_data5$updated_flag2 == "Possible") & 
                             (ANPR_data5$ttime_sec < (ANPR_data5$link_75perc*ANPR_data5$day_factor))] <- "OK 4"
  
  ftable(ANPR_data5$updated_flag3)
  cleaning_info1$accept_DayTimeFactor[j] <- nrow(ANPR_data5[ANPR_data5$updated_flag3 == "OK 4",])
  
  
  # remain_poss <- subset(ANPR_data5, updated_flag3 == "Possible")
  
  ANPR_data6 <- subset(ANPR_data5, updated_flag3 != "Possible")
  
  keep2 <- c("VRM", "Prev_DateTime", "DateTime", "ttime_sec",
             "link", "Prev_site", "site", "Prev_VAEid", "VAEid")
  
  ANPR_data6 <- ANPR_data6[,keep2]
  
  print(nrow(ANPR_data6))
  cleaning_info1$final_obs[j] <- nrow(ANPR_data6)
  
  
  
  
  write_file1 <- gsub(".csv", "_cleaned_links.csv", file1)

  write.csv(ANPR_data6, file=write_file1, row.names = F)
  
  print(file1)
  
  rm(full_data, full_data2, full_data_fin, ANPR_data1,
     ANPR_data5, cars_around, cars_around3, summ_data4,
     possibles2, summ_data5_link, summ_data5_dayslink, summ_data5_hourlink, 
     freq_links_hourly, freq_links_daily, ANPR_data6, write_file1)
}


write.csv(cleaning_info1, 
          file="BCCANPR21_link_cleaning_metadata_2019v1.csv", row.names = F)












