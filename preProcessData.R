library(data.table)
library(lubridate)
library(stringr)
library(readxl)

setwd("D:\\CS5346\\novel-corona-virus-2019-dataset\\")
covid_19_data=fread("covid_19_data.csv")
COVID19_line_list_data=fread("COVID19_line_list_data.csv")
COVID19_open_line_list=fread("COVID19_open_line_list.csv")

time_series_covid_19_confirmed=fread("time_series_covid_19_confirmed.csv")
time_series_covid_19_deaths=fread("time_series_covid_19_deaths.csv")
time_series_covid_19_recovered=fread("time_series_covid_19_recovered.csv")
mapData=COVID19_open_line_list[(!is.na(latitude) & !is.na(longitude) ),c("latitude","longitude","sex","age","date_confirmation")]

mapData[sex=="male" | sex=="Male" ,sex:="1"]
mapData[sex=="female" | sex=="Female"  ,sex:="2"]
mapData[sex=="N/A" | sex=="" ,sex:="3"]

mapData[stri_length(age)>2 & age=="N/A" , new_age:="3"]
mapData[stri_length(age)>2,age:=substr(age,1,2)]
mapData[!is.na(age),age:=as.numeric(age)]
mapData[is.na(age) , new_age:="3"]


mapData[age>=30 ,new_age:="1"]
mapData[age<=30 ,new_age:="2"]
mapData[age=="" ,new_age:="3"]
mapData[date_confirmation=="" ,date_confirmation:="NA"]

fwrite(mapData,file="D:\\CS5346\\novel-corona-virus-2019-dataset\\mapData.csv",sep=',',col.names = TRUE,row.names = FALSE)

covid_19_data=setDT(covid_19_data)[,.SD[which.max(`Confirmed`)],keyby=`Country/Region`]
names(covid_19_data)[names(covid_19_data) == "Country/Region"] <- "country"
names(covid_19_data)[names(covid_19_data) == "Confirmed"] <- "confirmed"
names(covid_19_data)[names(covid_19_data) == "Deaths"] <- "deaths"
names(covid_19_data)[names(covid_19_data) == "Recovered"] <- "recovered"
names(covid_19_data)[names(covid_19_data) == "Last Update"] <- "last_update"

covid_19_country=covid_19_data[,c("country","confirmed","deaths","recovered","last_update")]
covid_19_country[,recovery_percentage:=round((recovered/confirmed)*100)]
covid_19_country[,death_percentage:=round((deaths/confirmed)*100)]
covid_19_country[,country:=tolower(country)]

life_expectancy=fread("life_expectancy_rate.csv")
life_expectancy=setDT(life_expectancy)[,.SD[which.max(Year)],keyby=Country]
life_expectancy=life_expectancy[,c(1,3,4,5)]
colnames(life_expectancy)=c("country","status","life_expectancy","adult_mortality")
life_expectancy[,country:=tolower(country)]
life_expectancy[country=="china",country:="mainland china"]
life_expectancy[country=="united kingdom of great britain and northern ireland",country:="uk"]
life_expectancy[country=="united states of america",country:="us"]





covid_with_life_expectancy=merge(covid_19_country,life_expectancy,by="country")

fwrite(covid_with_life_expectancy,file="D:\\CS5346\\novel-corona-virus-2019-dataset\\covid_with_life_expectancy.csv",sep=',',col.names = TRUE,row.names = FALSE)
time_series_covid_confirmed=covid_19_data[,c("last_update","confirmed","country")]
time_series_covid_confirmed[,name:="confirmed"]
names(time_series_covid_confirmed)[names(time_series_covid_confirmed) == "confirmed"] <- "count"
time_series_covid_deaths=covid_19_data[,c("last_update","deaths","country")]
time_series_covid_deaths[,name:="deaths"]
names(time_series_covid_deaths)[names(time_series_covid_deaths) == "deaths"] <- "count"

time_series_covid_recovered=covid_19_data[,c("last_update","recovered","country")]
time_series_covid_recovered[,name:="recovered"]
names(time_series_covid_recovered)[names(time_series_covid_recovered) == "recovered"] <- "count"

time_series=rbind(time_series_covid_confirmed,time_series_covid_deaths,time_series_covid_recovered)
time_series[,last_update:=substr(last_update,1,10),]
time_series_total=time_series[,list(total_count=sum(count)),by=list(country,last_update,name)]
time_series_total=time_series_total[total_count>1]

time_series_covid_19_confirmed_reshaped=melt(setDT(time_series_covid_19_confirmed), id.vars = c("Province/State","Country/Region","Lat","Long"), variable.name = "date")[,c(2,5,6)]
colnames(time_series_covid_19_confirmed_reshaped)=c("country","date","confirmed")


time_series_covid_19_recovered_reshaped=melt(setDT(time_series_covid_19_recovered), id.vars = c("Province/State","Country/Region","Lat","Long"), variable.name = "date")[,c(2,5,6)]
colnames(time_series_covid_19_recovered_reshaped)=c("country","date","recovered")


china=time_series_covid_19_confirmed_reshaped[country=="China"]
china_confirmed=china[,sum(confirmed),by=list(date)]
china_confirmed[,new_date:=paste0("20",lapply(china_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][3]),"0",lapply(china_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][1]),"",lapply(china_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][2]))]
china=time_series_covid_19_recovered_reshaped[country=="China"]
china_recovered=china[,sum(recovered),by=list(date)]
china_recovered[,new_date:=paste0("20",lapply(china_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][3]),"0",lapply(china_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][1]),"",lapply(china_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][2]))]


italy=time_series_covid_19_confirmed_reshaped[country=="Italy"]
italy_confirmed=italy[,sum(confirmed),by=list(date)]
italy_confirmed[,new_date:=paste0("20",lapply(italy_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][3]),"0",lapply(italy_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][1]),"",lapply(italy_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][2]))]
italy=time_series_covid_19_recovered_reshaped[country=="Italy"]
italy_recovered=italy[,sum(recovered),by=list(date)]
italy_recovered[,new_date:=paste0("20",lapply(italy_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][3]),"0",lapply(italy_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][1]),"",lapply(italy_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][2]))]


korea=time_series_covid_19_confirmed_reshaped[country=="Korea, South"]
korea_confirmed=korea[,sum(confirmed),by=list(date)]
korea_confirmed[,new_date:=paste0("20",lapply(korea_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][3]),"0",lapply(korea_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][1]),"",lapply(korea_confirmed$date, function(x) strsplit(as.character(x), "/")[[1]][2]))]
korea=time_series_covid_19_recovered_reshaped[country=="Korea, South"]
korea_recovered=korea[,sum(recovered),by=list(date)]
korea_recovered[,new_date:=paste0("20",lapply(korea_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][3]),"0",lapply(korea_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][1]),"",lapply(korea_recovered$date, function(x) strsplit(as.character(x), "/")[[1]][2]))]

com_confimed=merge(merge(china_confirmed,italy_confirmed,by="new_date"),korea_confirmed,by="new_date")
com_recovered=merge(merge(china_recovered,italy_recovered,by="new_date"),korea_confirmed,by="new_date")

com=merge(com_confimed,com_recovered,by="new_date")
com=com[,c(1,3,5,7,9,11,13)]
com[,new_date:=ymd(new_date)]
com=com[order(new_date)]
com[,new_date:=as.character(new_date)]
com[,new_date:=str_replace_all(new_date,"-","")]
com[,`V1.x`:=as.double(`V1.x`)]
com[,`V1.y`:=as.double(`V1.y`)]
com[,`V1`:=as.double(`V1`)]
#colnames(com)=c("")
fwrite(com,file="D:\\CS5346\\novel-corona-virus-2019-dataset\\china_confirmed.csv",sep=',',col.names = FALSE,row.names = FALSE)


sars=read_excel("sars_final.xlsx")
sars=data.table(sars)
sars=sars[,c(2,3)]
covid_19=fread("covid_19_data.csv")
covid_cases=covid_19[,sum(Confirmed),by=list(ObservationDate)]
sars[,date:=seq( as.Date('2020-01-01'),as.Date('2020-04-05'), by = '1 day')]
covid_cases[,date:=seq( as.Date('2020-01-01'),as.Date('2020-02-23'), by = '1 day')]
sars=sars[,c(3,2)]
covid_cases=covid_cases[,c(3,2)]
combined=merge(covid_cases,sars,by="date")
combined[,date:=as.character(date)]
combined[,date:=str_replace_all(date,"-","")]
fwrite(combined,file="D:\\CS5346\\novel-corona-virus-2019-dataset\\sars_covid_comaprison.csv",sep=',',col.names = FALSE,row.names = FALSE)
covid_overall=covid_19[,list(confirmed=sum(Confirmed),recovered=sum(Recovered),deaths=sum(Deaths)),by=list(ObservationDate)]
covid_overall[,date:=strptime(ObservationDate,"%m/%d/%Y")]
covid_overall=covid_overall[,c(5,2,3,4)]
covid_overall[,date:=as.character(date)]
covid_overall[,date:=str_replace_all(date,"-","")]
fwrite(covid_overall,file="D:\\CS5346\\novel-corona-virus-2019-dataset\\covid_overall.csv",sep=',',col.names = FALSE,row.names = FALSE)
