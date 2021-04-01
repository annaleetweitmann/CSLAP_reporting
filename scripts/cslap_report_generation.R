########################################################################
###Automating CSLAP reports
###Use this script to run RMD script to produce CSLAP reports
########################################################################

###Read in packages
library(dplyr)
library(rmarkdown)
library(tidyr)
library(purrr)
library(remotes)
library(ggpattern)

#test
###Read in data
setwd("C:/Users/amtweitm/OneDrive - New York State Office of Information Technology Services/Documents/R/Current")
lake<-read.csv("from.other.files/lake.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
class<-read.csv("from.other.files/Lake.Master.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
class<-class %>% rename(LAKE_ID=LakeID) %>% select(LAKE_ID,PWLID,Waterbody_Classification,M_BAS_NAME,M_BASIN_ID,BasinSub,ACRES,AREA,LEN) %>% distinct() 
lake<-merge(lake,class,by=c('LAKE_ID'),all.x=TRUE)
rm(class)

county<-read.csv("from.other.files/Location-Clean-10-29-2019.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
county<-county %>% rename(LAKE_ID=LakeID,LOCATION_ID=LocationID) %>% select(LOCATION_ID,County,DEC.Region) %>% distinct()
location<-read.csv("from.other.files/location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
location<-merge(location,county,by="LOCATION_ID",all.x=TRUE)
rm(county)

profiles<-read.csv("DEPTH.PROFILE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
results<-read.csv("Test.Results.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sample<-read.csv("Sample.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
sample<-sample %>% mutate(LOCATION_ID=gsub("\\n","",LOCATION_ID),LAKE_ID=gsub("\\n","",LAKE_ID))
habs<-read.csv("HABstatus.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#Merge data tables
data<-merge(sample,results,by=c('SAMPLE_ID','SAMPLE_NAME','INFO_TYPE'),all.x=TRUE)
data<-data %>% filter(!is.na(Characteristic.Name))
data2<-merge(sample,profiles,by=c("SAMPLE_ID",'INFO_TYPE'),all.x=TRUE)
data2<-data2 %>% filter(!is.na(Characteristic.Name))
data<-merge(data,data2,by=c("LAKE_ID","LOCATION_ID","SAMPLE_ID","SAMPLE_NAME","DATA_PROVIDER","INFO_TYPE","SAMPLE_DATE",
                            "TIME","START_DEPTH","END_DEPTH","ESF.","UFI.","REMARK","BLOOM_LOC",
                            "WIND_DIR","WIND_INT","EXTENT","BLOOM_DESC","EQUIPMENT_DESC","EQUIPMENT_TYPE","LAKE_PROJ_CODE",
                            "SAMPLE_METHOD","PERCENT","DEPTH_UNIT","Characteristic.Name","Result.Value","Result.Unit"),all=TRUE)
rm(data2)
data<-merge(data,sample,by=c("LAKE_ID","LOCATION_ID","SAMPLE_ID","SAMPLE_NAME","DATA_PROVIDER","INFO_TYPE","SAMPLE_DATE",
                             "TIME","START_DEPTH","END_DEPTH","ESF.","UFI.","REMARK","BLOOM_LOC",
                             "WIND_DIR","WIND_INT","EXTENT","BLOOM_DESC","EQUIPMENT_DESC","EQUIPMENT_TYPE","LAKE_PROJ_CODE",
                             "SAMPLE_METHOD","PERCENT","DEPTH_UNIT"),all=TRUE)
data<-merge(data,lake,by=c('LAKE_ID'),all=TRUE)
data<-merge(data,location,by=c('LOCATION_ID','LAKE_ID'),all = TRUE)
data<-merge(data,habs, by=c('SAMPLE_ID'),all.x = TRUE)
data<-data %>%
  mutate(SAMPLE_DATE=as.Date(SAMPLE_DATE,format="%Y-%m-%d")) %>% 
  filter(INFO_TYPE!="MP") %>% 
  filter(!is.na(LAKE_ID))

#remove individual data tables in working environment
rm(list = c('lake','profiles','location','results','sample','habs'))

#Make sure records are distinct
data<-data %>% distinct()
setwd("C:/Users/amtweitm/OneDrive - New York State Office of Information Technology Services/Documents/R/CSLAP_reporting")
lake<-data
lake <- lake %>% 
  filter(DATA_PROVIDER == "CSL")
lake$SAMPLE_DATE<-as.Date(lake$SAMPLE_DATE,format="%Y-%m-%d")
lake<-lake %>% 
  mutate(year=substring(SAMPLE_DATE,1,4))
lake<-lake %>% 
  select(LAKE_ID,SAMPLE_ID,SAMPLE_NAME,LOCATION_ID,DATA_PROVIDER,SAMPLE_DATE,TIME, LAKE_PROJ_CODE,
         Characteristic.Name,Result.Value,Result.Unit,Result.Sample.Fraction,Depth,
         WATER,LocationName,Y_Coordinate,X_Coordinate,INFO_TYPE,Waterbody_Classification,PWLID, LAB_NAME_CODE,
         year,ACRES,County,PWS,Beaches,REMARK,STATUS,QUANTITATION_LIMIT,VALIDATOR_QUALIFIERS,INTERPRETED_QUALIFIERS)
lake<-lake[!is.na(lake$SAMPLE_ID),]
lakes<-unique(lake[c('LAKE_ID','WATER')])
lakes<-lakes[!is.na(lakes$LAKE_ID),]

###For loop that runs RMarkdown for every lake with CSLAP data
###Change output directory to folder for year

nlakes <- nrow(lakes)
for(i in 1:nlakes){
  report_year <- "2018" 
  lake1<-lake %>% 
    filter(LAKE_ID == lakes[i,1])
  titles<-lake1$WATER[1]
  ids<-lake1$LAKE_ID[1]
  rmarkdown::render('scripts/cslap_report_markdown.Rmd',
                    output_file =  paste("report_", titles,"(",ids,")", ".html", sep=''), 
                    output_dir = 'reports/')
}



