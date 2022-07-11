# Data for paper

library(tidyverse)
library(lubridate)
library(splines)
library(caTools)




df.plot<-read_csv("data/dfplot_seed0_26Nov.csv")

weather<-read_csv("data/dly532/dly532.csv")
masks<-read_csv("data/masks.csv")

googleni<-read.csv("data/2020_GB_Region_Mobility_Report.csv")
googleni1<-read.csv("data/2021_GB_Region_Mobility_Report.csv")
googleroi<-read_csv("data/googleroi.csv")

# df.plot is a replicable draw including predicted contact ratios from the epidemiological model described in
# Kamiya et al (2022) "Estimating time-dependent infectious contact: a multi-strain epidemiological model of SARS-CoV-2 on the island of Ireland"
# at doi: https://doi.org/10.1101/2022.03.25.22272942


# Data on proportion of people wearing masks are from
# https://www.gov.ie/en/collection/6b4401-view-the-amarach-public-opinion-survey/ for Republic of Ireland and
# https://www.nisra.gov.uk/publications/nisra-coronavirus-covid-19-opinion-survey-previous-results for Northern Ireland.
# also includes data on standing apart in queue, washing hands, using sanitiser, sitting apart for Rof I

# Weather data:
#•	Copyright Met Éireann
#•	Source www.met.ie
#•	This data is published under a Creative Commons Attribution 4.0 International (CC BY 4.0). https://creativecommons.org/licenses/by/4.0/
#•	Met Éireann does not accept any liability whatsoever for any error or omission in the data, their availability, or for any loss or damage arising from their use.
#•	This material has been modified from the original 


# google mobility data from https://www.google.com/covid19/mobility/ Ireland and UK data
# six mobility streams from 27/02/2020 - 28/02/2021

# from epidemiological model predictions
# select date, predicted contact ratio (c), predicted hospitalisations, observed hospitalisations, and jurisdiction (NI/ROI)

df.plot<-df.plot %>% dplyr::select(plotted_date, median_c,lower_c, upper_c, median_hosp_adm, lower_hosp_adm, upper_hosp_adm, area, hosp_adm)

df.plot<-df.plot %>%
  mutate(plotted_date = as_date(ymd(plotted_date))) %>%
  mutate(hosp = ifelse(is.na(hosp_adm), 0, hosp_adm))

# column for each variable for NI and Republic separately 
df.plot1<-df.plot  %>% dplyr::select (! hosp_adm) %>%
  pivot_wider(names_from = area, values_from = c(median_c, upper_c, lower_c,
                                                 median_hosp_adm, upper_hosp_adm, lower_hosp_adm, hosp) )

# 7 day running means for observed hospitalisations
df.plot1<-df.plot1 %>%
  mutate(hosp7dayROI = runmean(hosp_ROI,  k = 7)) %>%
  mutate(hosp7dayNI = runmean(hosp_NI,  k = 7))


## google mobility data

## for NI (2020 and 2021 separately)

# select NI subregions
googleni<-filter(googleni, iso_3166_2_code=="GB-ANN"
                 |iso_3166_2_code=="GB-AND"
                 |iso_3166_2_code=="GB-ABC"
                 |iso_3166_2_code=="GB-BFS"
                 |iso_3166_2_code=="GB-CCG"
                 |iso_3166_2_code=="GB-DRS"
                 |iso_3166_2_code=="GB-FMO"
                 |iso_3166_2_code=="GB-LBC"
                 |iso_3166_2_code=="GB-MEA"
                 |iso_3166_2_code=="GB-MUL"
                 |iso_3166_2_code=="GB-NMD")
googleni<-googleni %>%
  dplyr::select(sub_region_1, date:residential_percent_change_from_baseline)

## changes as proportions
googleni<-googleni %>%
  mutate(plotted_date = as_date(ymd(date))) %>%
  mutate(retail_and_recreation_percent_change_from_baseline = (100+retail_and_recreation_percent_change_from_baseline)/100) %>%
  mutate(grocery_and_pharmacy_percent_change_from_baseline = (100+grocery_and_pharmacy_percent_change_from_baseline)/100) %>%
  mutate(parks_percent_change_from_baseline = (100+parks_percent_change_from_baseline)/100) %>%
  mutate(transit_stations_percent_change_from_baseline = (100+transit_stations_percent_change_from_baseline)/100) %>%
  mutate(workplaces_percent_change_from_baseline = (100+workplaces_percent_change_from_baseline)/100) %>%
  mutate(residential_percent_change_from_baseline = (100+residential_percent_change_from_baseline)/100) 

googleni<-googleni %>%
  filter(plotted_date>as_date(ymd("2020/02/26")))

## 2021 data


googleni1<-filter(googleni1, iso_3166_2_code=="GB-ANN"
                  |iso_3166_2_code=="GB-AND"
                  |iso_3166_2_code=="GB-ABC"
                  |iso_3166_2_code=="GB-BFS"
                  |iso_3166_2_code=="GB-CCG"
                  |iso_3166_2_code=="GB-DRS"
                  |iso_3166_2_code=="GB-FMO"
                  |iso_3166_2_code=="GB-LBC"
                  |iso_3166_2_code=="GB-MEA"
                  |iso_3166_2_code=="GB-MUL"
                  |iso_3166_2_code=="GB-NMD")
googleni1<-googleni1 %>%
  dplyr::select(sub_region_1, date:residential_percent_change_from_baseline)

googleni1<-googleni1 %>%
  mutate(plotted_date = as_date(ymd(date))) %>%
  mutate(retail_and_recreation_percent_change_from_baseline = (100+retail_and_recreation_percent_change_from_baseline)/100) %>%
  mutate(grocery_and_pharmacy_percent_change_from_baseline = (100+grocery_and_pharmacy_percent_change_from_baseline)/100) %>%
  mutate(parks_percent_change_from_baseline = (100+parks_percent_change_from_baseline)/100) %>%
  mutate(transit_stations_percent_change_from_baseline = (100+transit_stations_percent_change_from_baseline)/100) %>%
  mutate(workplaces_percent_change_from_baseline = (100+workplaces_percent_change_from_baseline)/100) %>%
  mutate(residential_percent_change_from_baseline = (100+residential_percent_change_from_baseline)/100) 

googleni1<-googleni1 %>%
  filter(plotted_date<as_date(ymd("2021/03/01")))

## combine 2020 and 2021
googleni<-rbind(googleni, googleni1)

## for weighted averages giving more weight to high population areas
## populations from 
## from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
## (mid 2020 data)

sub_region_1<-names(table(googleni$sub_region_1))
nipop<-as.data.frame(sub_region_1)
nipop$pop<-c(143756, 162056, 217232, 342560, 144943, 151109, 117337, 146452, 139443, 148953, 181669)
nipop$p<-(nipop$pop)/sum(nipop$pop)

googleni<-left_join(googleni, nipop, by = "sub_region_1")


## calculate weights for each date & each transportation type
## to deal with missing values (ie too small to record), just want to weight the areas on dates where they are included
## calculate sum of p*!is.na(mobility variable) by date (across subregions)
googleni<-dplyr::select(googleni, -date) %>%
  group_by(plotted_date) %>%
  mutate(sumpnotnares = sum(p*as.numeric(!is.na(residential_percent_change_from_baseline)))) %>%
  mutate(sumpnotnaret = sum(p*as.numeric(!is.na(retail_and_recreation_percent_change_from_baseline)))) %>%
  mutate(sumpnotnawork = sum(p*as.numeric(!is.na(workplaces_percent_change_from_baseline)))) %>%
  mutate(sumpnotnagroc = sum(p*as.numeric(!is.na(grocery_and_pharmacy_percent_change_from_baseline)))) %>%
  mutate(sumpnotnapark = sum(p*as.numeric(!is.na(parks_percent_change_from_baseline)))) %>%
  mutate(sumpnotnatrans = sum(p*as.numeric(!is.na(transit_stations_percent_change_from_baseline)))) %>%
  mutate(pres = p/sumpnotnares) %>%
  mutate(pret = p/sumpnotnaret)%>%
  mutate(pwork = p/sumpnotnawork)%>%
  mutate(pgroc = p/sumpnotnagroc)%>%
  mutate(ppark =  p/sumpnotnapark)%>%
  mutate(ptrans = p/sumpnotnatrans)

googleni<-dplyr::select(googleni, c(sub_region_1,plotted_date, 
                                    retail_and_recreation_percent_change_from_baseline: residential_percent_change_from_baseline,        
                                      pres:ptrans))  

googleni<-googleni %>%
  mutate(wresidential_percent_change_from_baseline_NI = pres*residential_percent_change_from_baseline) %>%
  mutate(wretail_and_recreation_percent_change_from_baseline_NI = pret*retail_and_recreation_percent_change_from_baseline) %>%
  mutate(wworkplaces_percent_change_from_baseline_NI = pwork*workplaces_percent_change_from_baseline) %>%
  mutate(wgrocery_and_pharmacy_percent_change_from_baseline_NI = pgroc*grocery_and_pharmacy_percent_change_from_baseline) %>%
  mutate(wparks_percent_change_from_baseline_NI = ppark*parks_percent_change_from_baseline) %>%
  mutate(wtransit_stations_percent_change_from_baseline_NI = ptrans*transit_stations_percent_change_from_baseline) 

# for all NI
googleniall <- dplyr::select(googleni, c(plotted_date,wresidential_percent_change_from_baseline_NI:wtransit_stations_percent_change_from_baseline_NI )) %>% 
  group_by(plotted_date) %>%
  summarise(across(everything(), sum, na.rm=TRUE))

googleniall<-googleniall %>%
  mutate(wres7dayNI = runmean(wresidential_percent_change_from_baseline_NI, k = 7)) %>%
  mutate(wret7dayNI = runmean(wretail_and_recreation_percent_change_from_baseline_NI, k = 7)) %>%
  mutate(wwork7dayNI = runmean(wworkplaces_percent_change_from_baseline_NI, k = 7)) %>%
  mutate(wgroc7dayNI = runmean(wgrocery_and_pharmacy_percent_change_from_baseline_NI, k = 7)) %>%
  mutate(wparks_percent_change_from_baseline_NI = ifelse(wparks_percent_change_from_baseline_NI>0.00001, wparks_percent_change_from_baseline_NI, NA))%>%
  mutate(wpark7dayNI= runmean(wparks_percent_change_from_baseline_NI, k = 7)) %>%
  mutate(wtran7dayNI = runmean(wtransit_stations_percent_change_from_baseline_NI, k = 7)) %>%
  mutate(googlewtNI = rowMeans(dplyr::select(.,c("wret7dayNI", "wwork7dayNI", "wtran7dayNI")) )) %>%
  mutate(googlewtNI2 = rowMeans(dplyr::select(.,c("wret7dayNI", "wwork7dayNI", "wtran7dayNI", "wgroc7dayNI")) ))

# for ROI


googleroi<-googleroi  %>% dplyr::select(-area) %>%
  rename(residential_percent_change_from_baseline_ROI=residential_percent_change_from_baseline,
         retail_and_recreation_percent_change_from_baseline_ROI = retail_and_recreation_percent_change_from_baseline,
         workplaces_percent_change_from_baseline_ROI = workplaces_percent_change_from_baseline,
         grocery_and_pharmacy_percent_change_from_baseline_ROI = grocery_and_pharmacy_percent_change_from_baseline,
         parks_percent_change_from_baseline_ROI = parks_percent_change_from_baseline,
         transit_stations_percent_change_from_baseline_ROI = transit_stations_percent_change_from_baseline)

## 7 day running means and means over the 3 columns transit, work, retail
googleroi<-googleroi %>%
  mutate(res7dayI = runmean(residential_percent_change_from_baseline_ROI, k = 7)) %>%
  mutate(ret7dayI = runmean(retail_and_recreation_percent_change_from_baseline_ROI, k = 7)) %>%
  mutate(work7dayI = runmean(workplaces_percent_change_from_baseline_ROI, k = 7)) %>%
  mutate(groc7dayI = runmean(grocery_and_pharmacy_percent_change_from_baseline_ROI, k = 7)) %>%
  mutate(park7dayI = runmean(parks_percent_change_from_baseline_ROI, k = 7)) %>%
  mutate(tran7dayI = runmean(transit_stations_percent_change_from_baseline_ROI, k = 7)) %>%
  mutate(google1ROI = rowMeans(dplyr::select(.,c("ret7dayI", "work7dayI", "tran7dayI")) )) %>%
  mutate(google2ROI = rowMeans(dplyr::select(.,c("ret7dayI", "work7dayI", "tran7dayI", "groc7dayI")) )) 

google<-left_join(googleroi, googleniall, by = "plotted_date")

#write.csv(google, "googleallIreland.csv")

# merge epidemiological model predictions with mobility data, mask data and weather data
# new data set is called daydata

daydata<-left_join(df.plot1, google, by = "plotted_date")


masks<-masks %>%
  mutate(plotted_date = as_date(dmy(plotted_date))) 


daydata<-left_join(daydata, masks, by = "plotted_date")


weather<-weather %>%
  mutate(year = as.numeric(substr(weather$date, 8, 9)))

weather<-filter(weather, year==20|year==21) %>%
  mutate(plotted_date=as_date(dmy(date)))%>%
  mutate(maxtemp_7day = runmean(maxtp, k = 7)) %>%
  mutate(mintemp_7day = runmean(mintp, k = 7)) %>%
  mutate(medtemp = rowMeans(dplyr::select(., c("maxtemp_7day", "mintemp_7day")))) %>%
  mutate(summer = ifelse(medtemp >10.5 & plotted_date<as_date(dmy("30/09/2020")),1, 0))


daydata<-left_join(daydata, weather, by = "plotted_date")

# dates of lockdowns
daydata<-daydata %>%
  mutate(lockdowni = ifelse(plotted_date>as_date(ymd("2020/03/28")) & plotted_date<as_date(ymd("2020/05/18"))|
                              plotted_date>as_date(ymd("2020/10/22")) & plotted_date<as_date(ymd("2020/12/01"))|
                              plotted_date>as_date(ymd("2020/12/31")) & plotted_date<as_date(ymd("2021/02/28")), 1, 0)) %>%
  mutate(lockdownni = ifelse(plotted_date>as_date(ymd("2020/03/28")) & plotted_date<as_date(ymd("2020/05/18"))|
                               plotted_date>as_date(ymd("2020/10/16")) & plotted_date<as_date(ymd("2020/12/11"))|
                               plotted_date>as_date(ymd("2020/12/26")) & plotted_date<as_date(ymd("2021/02/28")), 1, 0)) 
 



# dates of mask mandates
daydata<-daydata %>%
  mutate(mask_transI = ifelse(plotted_date>as_date(dmy("13/07/2020")), 1, 0)) %>%
  mutate(mask_public = ifelse(plotted_date>as_date(dmy("10/08/2020")), 1, 0)) %>%
  mutate(mask_transNI = ifelse(plotted_date>as_date(dmy("10/07/2020")), 1, 0)) %>%
  mutate(mask_cdc = ifelse(plotted_date>as_date(dmy("03/04/2020")), 1, 0)) %>%
  mutate(mask_ecdc = ifelse(plotted_date>as_date(dmy("08/04/2020")), 1, 0)) %>%
  mutate(mask_who = ifelse(plotted_date>as_date(dmy("05/06/2020")), 1, 0)) 

# for middle of each week

daydata<-daydata %>%
  mutate(day = seq(1, length(plotted_date), 1)) %>%
  mutate(day1 = round((day/7-floor(day/7))*7)) 



# smooth for mask proportion and proportion reporting other NPI variables for Republic of Ireland 
# 



mask_mod<-glm(mask_perc ~ bs(plotted_date, df=6),family = binomial, data = daydata)
daydata$mask_pred<-predict(mask_mod, daydata, type = "response")


apart_mod<-glm(apart ~ bs(plotted_date, df=5),family = binomial, data = daydata)
daydata$apart_pred<-predict(apart_mod, daydata, type = "response")

queue_mod<-glm(queue ~ bs(plotted_date, df=5),family = binomial, data = daydata)
daydata$queue_pred<-predict(queue_mod, daydata, type = "response")

hands_mod<-glm(hands ~ bs(plotted_date, df=5),family = binomial, data = daydata)
daydata$hands_pred<-predict(hands_mod, daydata, type = "response")

san_mod<-glm(sanitiser ~ bs(plotted_date, df=5),family = binomial, data = daydata)
daydata$san_pred<-predict(san_mod, daydata, type = "response")


# smooth for mask proportion Northern Ireland 

mask_mod_ni<-glm(mask_perc_ni ~ bs(plotted_date, df=4),family = binomial, data = daydata)
daydata$mask_pred_ni<-predict(mask_mod_ni, daydata, type = "response")






# restricting to end of 2020 as vaccinations are starting after then 
# and mid week data for predictors in regression models

weekdata<-filter(daydata, day1==4 & day>4 & plotted_date<as_date(ymd("2020/12/27")) ) %>%
  dplyr::select(plotted_date, median_c_NI, median_c_ROI, google1ROI, googlewtNI, mask_pred_ni, 
                mask_pred, summer) %>%
  rename(contact_NI = median_c_NI, contact_ROI = median_c_ROI, google_ROI = google1ROI, google_NI = googlewtNI,
         mask_NI = mask_pred_ni, mask_ROI = mask_pred) %>% 
  pivot_longer(cols = ends_with("NI") | ends_with("ROI"), names_to = c(".value", "jur"), names_pattern = "(.*)_(.*)") %>%
  mutate(winter = 1-summer)


# for NW variances
ni<-filter(weekdata, jur=="NI")
roi<-filter(weekdata, jur=="ROI")




# Republic of Ireland to adjust for other changes

rois<-filter(daydata, day1==4 & day>4 & plotted_date<as_date(ymd("2020/12/27")) ) %>%
  dplyr::select(plotted_date, apart_pred, queue_pred, hands_pred, san_pred) %>%
  rename(apart = apart_pred, queue = queue_pred, hands = hands_pred, sanit = san_pred)


rois<-left_join(roi, rois, by = "plotted_date")
  




