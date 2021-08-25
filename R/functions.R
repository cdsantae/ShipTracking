suppressPackageStartupMessages({
  library(geosphere)
  library(geojsonio)
  library(fst)
  library(dplyr)
})

data <- read.fst("R/ships.fst")

ggc <- function(n,a=1) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100,alpha = a)[1:n]
}

long_travel <- function(data,ship_name){
  data %>%
    filter(SHIPNAME==ship_name) %>%
    mutate(lat_f = c(LAT[-1],last(LAT)),
           lon_f = c(LON[-1],last(LON)),
           AvSp=mean(SPEED,na.rm=T),
           TpSp=max(SPEED,na.rm=T),
           end = c(DATETIME[-1],last(DATETIME)+1)) %>%
    rename(lat_i=LAT,lon_i=LON) %>%
    rowwise() %>%
    mutate(dist=distVincentyEllipsoid(c(lon_i,lat_i),
                                      c(lon_f,lat_f))) %>%
    ungroup() %>%
    filter(dist == max(dist)) %>%
    select(SHIPNAME,ship_type,SHIP_ID,lat_i,lon_i,lat_f,lon_f,
           FLAG,WIDTH,LENGTH,DWT,dist,AvSp,TpSp,start=DATETIME,end) %>%
    filter(end == max(end))
}

bbox <- function(data){
  data %>%
    summarise(lon1=min(lon_i),lat1=min(lat_i),
              lon2=max(lon_i),lat2=max(lat_i))
}

traffic_sail <- function(data){
  data %>%
    mutate(end = c(DATETIME[-1],last(DATETIME)+1),
           lat_f = c(LAT[-1],last(LAT)),
           lon_f = c(LON[-1],last(LAT)),
           color=ggc(length(unique(SHIPNAME)))[factor(SHIPNAME)]) %>%
    rename(lat_i=LAT,lon_i=LON,start=DATETIME) %>%
    rowwise() %>%
    mutate(dist=distVincentyEllipsoid(c(lon_i,lat_i),
                                      c(lon_f,lat_f))) %>%
    ungroup
}

ltime_resume <- function(data){
  data %>%
    group_by(SHIPNAME,is_parked) %>%
    summarise(tot=sum(dist),
              AS=mean(SPEED),
              TS=max(SPEED),
              lgt=mean(LENGTH,na.rm=T),
              wdt=mean(WIDTH,na.rm=T),
              dwt=mean(DWT,na.rm=T)) %>%
    mutate_at(vars(dwt),~replace(.,is.nan(.),0)) %>%
    ungroup() %>%
    group_by(SHIPNAME) %>%
    summarise(total=round(sum(tot),2),
              AvSp=round(sum(AS),4),
              TopS=sum(TS),
              length=mean(lgt,na.rm=T),
              width=mean(wdt,na.rm=T),
              DWT=sum(dwt,na.rm=T)) %>%
    ungroup() %>%
    select(`Ship Name`=SHIPNAME,
           `Total Distance Sailed (m)`=total,
           `Average Speed (kn)`=AvSp,
           `Top Speed (kn)`=TopS,
           length,width,
           `Deadweight (ton)`=DWT)
}

