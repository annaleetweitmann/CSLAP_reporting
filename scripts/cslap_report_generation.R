########################################################################
###Automating CSLAP reports
###Use this script to run RMD script to produce CSLAP reports
########################################################################

###Read in packages
library(Hmisc)
library(dplyr)
library(rmarkdown)
library(tidyr)
library(purrr)
library(remotes)
library(ggpattern)
library(EnvStats)
library(stringr)
library(varhandle)
library(grid)
library(gridExtra)
library(webshot)
library(kableExtra)
library(magick)
library(lubridate)
library(BBmisc)
library(gt)

###Read in data
setwd("C:/Users/amtweitm/OneDrive - New York State Office of Information Technology Services/Documents/R/CSLAP_reporting/data")

source("old_database/Reading.LMAS.Data.R")

#Make sure records are distinct
data<-data %>% distinct()
setwd("C:/Users/amtweitm/OneDrive - New York State Office of Information Technology Services/Documents/R/CSLAP_reporting")
lake<-data
lake$SAMPLE_DATE<-as.Date(lake$SAMPLE_DATE,format="%Y-%m-%d")
lake<-lake %>% 
  mutate(year=substring(SAMPLE_DATE,1,4))
lake<-lake %>% 
  select(LAKE_ID,SAMPLE_ID,SAMPLE_NAME,LOCATION_ID,DATA_PROVIDER,SAMPLE_DATE,TIME, LAKE_PROJ_CODE,
         Characteristic.Name,Result.Value,Result.Unit,Result.Sample.Fraction,Depth,
         WATER,LocationName,Y_Coordinate,X_Coordinate,INFO_TYPE,Waterbody_Classification,PWLID, LAB_NAME_CODE,
         year,ACRES,County,PWS,Beaches,REMARK,STATUS,QUANTITATION_LIMIT,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS)
lake<-lake[!is.na(lake$SAMPLE_ID),]
# lakes<-unique(lake[c('LAKE_ID','WATER')])
# lakes<-lakes[!is.na(lakes$LAKE_ID),]

lakes_onesite <- read.csv("C:/Users/amtweitm/OneDrive - New York State Office of Information Technology Services/Documents/R/CSLAP_reporting/data/one_site_lake_list_2020.csv")
lakes_multisites <- read.csv("C:/Users/amtweitm/OneDrive - New York State Office of Information Technology Services/Documents/R/CSLAP_reporting/data/multi_site_lake_list_2020.csv")
silver_lake <- read.csv("C:/Users/amtweitm/OneDrive - New York State Office of Information Technology Services/Documents/R/CSLAP_reporting/data/silver_lake.csv")

###For loop that runs RMarkdown for every lake with CSLAP data
###Change output directory to folder for year
###change report year


report_year <- "2020" 

#nlakes <- nrow(lakes)
for(i in 1:nrow(lakes_onesite)){
  habs_lake1 <- lake %>% 
    filter(LAKE_ID == lakes_onesite[i,2])
  lake1<-lake %>% 
    filter(LAKE_ID == lakes_onesite[i,2]) %>% 
    filter(DATA_PROVIDER == "CSL")
  titles<-lake1$WATER[1]
  ids<-lake1$LAKE_ID[1]
  multisite_var <- 0 
  rmarkdown::render('scripts/cslap_report_markdown.Rmd',
                    output_file =  paste(report_year, "_", "report_", titles,"(",ids,")", ".html", sep=''), 
                    output_dir = 'reports/2020')
}

for(i in 1:nrow(lakes_multisites)){
  habs_lake1 <- lake %>% 
    filter(LAKE_ID == lakes_onesite[i,2])
  lake1<-lake %>% 
    filter(LAKE_ID == lakes_onesite[i,2]) %>% 
    filter(DATA_PROVIDER == "CSL") %>% 
    filter(LOCATION_ID == lakes_onesite[i,1])
  titles<-lakes_multisites$NAME[i]
  ids<-lakes_multisites$LAKE_ID[i]
  loc_id <- lakes_multisites$LOCATION_ID[i]
  multisite_var <- 1
  rmarkdown::render('scripts/cslap_report_markdown.Rmd',
                    output_file =  paste(report_year, "_", "report_", titles,"(",ids,")", ".html", sep=''), 
                    output_dir = 'reports/2020')
}

for(i in 1:nrow(silver_lake)){
habs_lake1 <- lake %>% 
  dplyr::filter(year < "2020") %>% 
  dplyr::filter(LAKE_ID == silver_lake$Lake_ID[i])
  
if(silver_lake$Lake_ID[i] == "0904SIL0351"){
  list_of_2020_samps <- c("20-117-01", "20-117-02", "20-117-03", "20-117-04", "20-117-05", "20-117-06", "20-117-07" )
} else if (silver_lake$Lake_ID[i] == "0403SIL0115"){
  list_of_2020_samps <- c("20-25-01", "20-25-02", "20-25-03", "20-25-04", "20-25-05", "20-25-06", "20-25-07", "20-25-08", 
                          "20-25-11", "20-25-12", "20-25-13", "20-25-14", "20-25-15", "20-25-16", "20-25-17", "20-25-18")
}
  
lake1 <- lake %>% 
  dplyr::filter(DATA_PROVIDER == "CSL") %>% 
  dplyr::filter(SAMPLE_NAME %in% list_of_2020_samps | LAKE_ID == silver_lake$Lake_ID[1] & year < as.numeric("2020")) 

titles<- "Silver Lake"
ids<- silver_lake$Lake_ID[i]
multisite_var <- 0 
rmarkdown::render('scripts/cslap_report_markdown.Rmd',
                  output_file =  paste(report_year, "_", "report_", titles,"(",ids,")", ".html", sep=''), 
                  output_dir = 'reports/2020')

}







