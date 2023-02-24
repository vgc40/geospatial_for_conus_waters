### Flow Analysis

#Finding USGS gages within 10 km up/downstream of each site. Then, if a date is provided in the site meta data, will pull the daily discharge on that date.


gages <- getGages(df = sites)


#Getting the 20th and 80th percentile flows (starting as early as 1980 water year if available, ending in 2020) and daily discharge on the date of sampling for sites with nearby USGS gages.


parameterCd <- "00060" #DAILY DISCHARGE CODE
startDate <- "1979-10-01"
endDate <- "2022-09-30"
discharge_usgs <- readNWISdv(gages$site_no, parameterCd, startDate, endDate) %>%
  rename(CFS=4) %>% 
  dplyr::filter(X_00060_00003_cd == "A" | X_00060_00003_cd == "A e" |
                  X_00060_00003_cd == "A R" | X_00060_00003_cd == "A [4]" |
                  X_00060_00003_cd == "A <"| X_00060_00003_cd == "A >") %>% # only data that has been approved by USGS
  dplyr::mutate(month=lubridate::month(Date)) %>% 
  dplyr::mutate(year=lubridate::year(Date)) %>%
  dplyr::mutate(wyear=as.numeric(ifelse(month>9, year+1, year))) %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(flow_record=ifelse((min(wyear) - max(wyear) == 0), paste0(min(wyear)),paste0(min(wyear), ' - ', max(wyear))),
                percentile_20 = quantile(CFS, 0.20, na.rm=F),
                percentile_80 = quantile(CFS, 0.80, na.rm=F)) %>%
  mutate(gage_id = paste0('USGS-', site_no)) %>%
  inner_join(gages, by = "site_no") %>%
  left_join(sites, ., by = c("site", "date" = "Date")) %>%
  ungroup() %>%
  mutate(flowlink=paste0('https://waterdata.usgs.gov/usa/nwis/dv?referred_module=sw&site_no=',site_no)) %>%
  distinct()

