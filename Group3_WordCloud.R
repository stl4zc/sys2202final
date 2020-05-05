#LOAD NECESSARY PACKAGES
library(rtweet)
library(jsonlite)
library(readr)
library(httr)
library(twitteR)
library(tidyverse)
library(rgdal)
library(leaflet)
library(slam) 
library(wordcloud2)
library(wordcloud)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(gdata)

#FOR SAMPLE DATA
#read sample data (json) and convert to data frame
#data <- read_json("sampledata.json", simplifyDataFrame = TRUE, flatten = TRUE)
#extract nested trend data table and nested location data table
#trends <- data[[1]][[1]]
#locations <- data[[4]][[1]]
#clean trends data, removing excess columns
#trendsclean = subset(trends, select = -c(url, promoted_content, query))
#cleaning NA tweet volumes, if given
#for(i in seq_along(trendsclean$tweet_volume)) {
#  if(is.na(trendsclean$tweet_volume[i])) {
#    trendsclean$tweet_volume[i]=0
#  }
#}
#trendsclean

#CREATING THE COVID MAP
#us_states <- read.csv("states.csv")
#p <- ggplot(data = us_states,
#            mapping = aes(x = long, y = lat,
#                          group = group, fill = Cases))
#p + geom_polygon(color = "black", size = 0.1) + guides() + 
#  scale_fill_gradientn(colours=rev(terrain.colors(10))) +
#  theme(panel.background = element_blank(), axis.title = element_blank(),
#        axis.text.x = element_blank(), axis.text.y = element_blank(),
#        axis.ticks.x = element_blank(),axis.ticks.y = element_blank()) 

#FOR STREAMED API DATA 
#developer app api keys
consumer_key <- "H3aoaS1dHoilgwRIs36psKsJr"
consumer_secret <- "4Hdo7O7Kq1CchW7qqUsaycZXjpLvzRFeDcQTKFTW07uRBqPjlx"
access_token <- "2746996907-KzFrsOQI7Dg2orWhrdI47EMo0MURN4unNU5pjaq"
access_secret <- "kfKmB4fwR3lLJvbSkNn12LpQkyjzlSz7AuKHEEQELXHyN"

#twitter authentication token
token <- create_token(
  app = "SYS 2202 Twitter Word Cloud",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv = TRUE
)

#get top 50 trends per location (world, USA, and all 50 states) with tweet volume, and for loops to clean NA values
world <- select(get_trends(woeid = 1),trend,tweet_volume)
world_sum = 0
world_count = 0
for(i in seq_along(world$tweet_volume)) {
  if(!is.na(world$tweet_volume[i])){
    world_sum = world_sum + world$tweet_volume[i]
    world_count = world_count + 1 }}
world_avg = world_sum/world_count
for(i in seq_along(world$tweet_volume)) {
  if(is.na(world$tweet_volume[i])) {
    world$tweet_volume[i]=world_avg }}
usa <- select(get_trends(woeid = 23424977),trend,tweet_volume)
usa_sum = 0
usa_count = 0
for(i in seq_along(usa$tweet_volume)) {
  if(!is.na(usa$tweet_volume[i])){
    usa_sum = usa_sum + usa$tweet_volume[i]
    usa_count = usa_count + 1 }}
usa_avg = usa_sum/usa_count
for(i in seq_along(usa$tweet_volume)) {
  if(is.na(usa$tweet_volume[i])) {
    usa$tweet_volume[i]=usa_avg }}
Alabama <- select(get_trends(lat = 32.806671, lng = -86.791130),trend,tweet_volume)
al_sum = 0
al_count = 0
for(i in seq_along(Alabama$tweet_volume)) {
  if(!is.na(Alabama$tweet_volume[i])){
    al_sum = al_sum + Alabama$tweet_volume[i]
    al_count = al_count + 1 }}
al_avg = al_sum/al_count
for(i in seq_along(Alabama$tweet_volume)) {
  if(is.na(Alabama$tweet_volume[i])) {
    Alabama$tweet_volume[i]=al_avg }}
Alaska <- select(get_trends(lat = 61.370716, lng = -152.404419),trend,tweet_volume)
ak_sum = 0
ak_count = 0
for(i in seq_along(Alaska$tweet_volume)) {
  if(!is.na(Alaska$tweet_volume[i])){
    ak_sum = ak_sum + Alaska$tweet_volume[i]
    ak_count = ak_count + 1 }}
ak_avg = ak_sum/ak_count
for(i in seq_along(Alaska$tweet_volume)) {
  if(is.na(Alaska$tweet_volume[i])) {
    Alaska$tweet_volume[i]=ak_avg }}
Arizona <- select(get_trends(lat = 33.729759, lng = -111.431221),trend,tweet_volume)
ar_sum = 0
ar_count = 0
for(i in seq_along(Arizona$tweet_volume)) {
  if(!is.na(Arizona$tweet_volume[i])){
    ar_sum = ar_sum + Arizona$tweet_volume[i]
    ar_count = ar_count + 1 }}
ar_avg = ar_sum/ar_count
for(i in seq_along(Arizona$tweet_volume)) {
  if(is.na(Arizona$tweet_volume[i])) {
    Arizona$tweet_volume[i]=ar_avg }}
Arkansas <- select(get_trends(lat = 34.969704, lng = -92.373123),trend,tweet_volume)
ak_sum = 0
ak_count = 0
for(i in seq_along(Arkansas$tweet_volume)) {
  if(!is.na(Arkansas$tweet_volume[i])){
    ak_sum = ak_sum + Arkansas$tweet_volume[i]
    ak_count = ak_count + 1 }}
ak_avg = ak_sum/ak_count
for(i in seq_along(Arkansas$tweet_volume)) {
  if(is.na(Arkansas$tweet_volume[i])) {
    Arkansas$tweet_volume[i]=ak_avg }}
California <- select(get_trends(lat = 36.116203, lng = -119.681564),trend,tweet_volume)
ca_sum = 0
ca_count = 0
for(i in seq_along(California$tweet_volume)) {
  if(!is.na(California$tweet_volume[i])){
    ca_sum = ca_sum + California$tweet_volume[i]
    ca_count = ca_count + 1 }}
ca_avg = ca_sum/ca_count
for(i in seq_along(California$tweet_volume)) {
  if(is.na(California$tweet_volume[i])) {
    California$tweet_volume[i]= ca_avg }}
Colorado <- select(get_trends(lat = 39.059811, lng = -105.311104),trend,tweet_volume)
co_sum = 0
co_count = 0
for(i in seq_along(Colorado$tweet_volume)) {
  if(!is.na(Colorado$tweet_volume[i])){
    co_sum = co_sum + Colorado$tweet_volume[i]
    co_count = co_count + 1 }}
co_avg = co_sum/co_count
for(i in seq_along(Colorado$tweet_volume)) {
  if(is.na(Colorado$tweet_volume[i])) {
    Colorado$tweet_volume[i]=co_avg }}
Connecticut <- select(get_trends(lat = 41.597782, lng = -72.755371),trend,tweet_volume)
ct_sum = 0
ct_count = 0
for(i in seq_along(Connecticut$tweet_volume)) {
  if(!is.na(Connecticut$tweet_volume[i])){
    ct_sum = ct_sum + Connecticut$tweet_volume[i]
    ct_count = ct_count + 1 }}
ct_avg = ct_sum/ct_count
for(i in seq_along(Connecticut$tweet_volume)) {
  if(is.na(Connecticut$tweet_volume[i])) {
    Connecticut$tweet_volume[i]=ct_avg}}
Delaware <- select(get_trends(lat = 39.318523, lng = -75.507141),trend,tweet_volume)
de_sum = 0
de_count = 0
for(i in seq_along(Delaware$tweet_volume)) {
  if(!is.na(Delaware$tweet_volume[i])){
    de_sum = de_sum + Delaware$tweet_volume[i]
    de_count = de_count + 1 }}
de_avg = de_sum/de_count
for(i in seq_along(Delaware$tweet_volume)) {
  if(is.na(Delaware$tweet_volume[i])) {
    Delaware$tweet_volume[i]=de_avg }}
District_of_Columbia <- select(get_trends(lat = 38.897438, lng = -77.026817),trend,tweet_volume)
dc_sum = 0
dc_count = 0
for(i in seq_along(District_of_Columbia$tweet_volume)) {
  if(!is.na(District_of_Columbia$tweet_volume[i])){
    dc_sum = dc_sum + District_of_Columbia$tweet_volume[i]
    dc_count = dc_count + 1 }}
dc_avg = dc_sum/dc_count
for(i in seq_along(District_of_Columbia$tweet_volume)) {
  if(is.na(District_of_Columbia$tweet_volume[i])) {
    District_of_Columbia$tweet_volume[i]=dc_avg }}
Florida <- select(get_trends(lat = 27.766279, lng = -81.686783),trend,tweet_volume)
fl_sum = 0
fl_count = 0
for(i in seq_along(Florida$tweet_volume)) {
  if(!is.na(Florida$tweet_volume[i])){
    fl_sum = fl_sum + Florida$tweet_volume[i]
    fl_count = fl_count + 1 }}
fl_avg = fl_sum/fl_count
for(i in seq_along(Florida$tweet_volume)) {
  if(is.na(Florida$tweet_volume[i])) {
    Florida$tweet_volume[i]=fl_avg }}
Georgia <- select(get_trends(lat = 33.040619, lng = -83.643074),trend,tweet_volume)
ga_sum = 0
ga_count = 0
for(i in seq_along(Georgia$tweet_volume)) {
  if(!is.na(Georgia$tweet_volume[i])){
    ga_sum = ga_sum + Georgia$tweet_volume[i]
    ga_count = ga_count + 1 }}
ga_avg = ga_sum/ga_count
for(i in seq_along(Georgia$tweet_volume)) {
  if(is.na(Georgia$tweet_volume[i])) {
    Georgia$tweet_volume[i]=ga_avg }}
Hawaii <- select(get_trends(lat = 21.094318, lng = -157.498337),trend,tweet_volume)
hi_sum = 0
hi_count = 0
for(i in seq_along(Hawaii$tweet_volume)) {
  if(!is.na(Hawaii$tweet_volume[i])){
    hi_sum = hi_sum + Hawaii$tweet_volume[i]
    hi_count = hi_count + 1 }}
hi_avg = hi_sum/hi_count
for(i in seq_along(Hawaii$tweet_volume)) {
  if(is.na(Hawaii$tweet_volume[i])) {
    Hawaii$tweet_volume[i]=hi_avg }}
Idaho <- select(get_trends(lat = 44.240459, lng = -114.478828),trend,tweet_volume)
id_sum = 0
id_count = 0
for(i in seq_along(Idaho$tweet_volume)) {
  if(!is.na(Idaho$tweet_volume[i])){
    id_sum = id_sum + Idaho$tweet_volume[i]
    id_count = id_count + 1 }}
id_avg = id_sum/id_count
for(i in seq_along(Idaho$tweet_volume)) {
  if(is.na(Idaho$tweet_volume[i])) {
    Idaho$tweet_volume[i]=id_avg }}
Illinois <- select(get_trends(lat = 40.349457, lng = -88.986137),trend,tweet_volume)
il_sum = 0
il_count = 0
for(i in seq_along(Illinois$tweet_volume)) {
  if(!is.na(Illinois$tweet_volume[i])){
    il_sum = il_sum + Illinois$tweet_volume[i]
    il_count = il_count + 1 }}
il_avg = il_sum/il_count
for(i in seq_along(Illinois$tweet_volume)) {
  if(is.na(Illinois$tweet_volume[i])) {
    Illinois$tweet_volume[i]=il_avg }}
Indiana <- select(get_trends(lat = 39.849426, lng = -86.258278),trend,tweet_volume)
in_sum = 0
in_count = 0
for(i in seq_along(Indiana$tweet_volume)) {
  if(!is.na(Indiana$tweet_volume[i])){
    in_sum = in_sum + Indiana$tweet_volume[i]
    in_count = in_count + 1 }}
in_avg = in_sum/in_count
for(i in seq_along(Indiana$tweet_volume)) {
  if(is.na(Indiana$tweet_volume[i])) {
    Indiana$tweet_volume[i]=in_avg }}
Iowa <- select(get_trends(lat = 42.011539, lng = -93.210526),trend,tweet_volume)
ia_sum = 0
ia_count = 0
for(i in seq_along(Iowa$tweet_volume)) {
  if(!is.na(Iowa$tweet_volume[i])){
    ia_sum = ia_sum + Iowa$tweet_volume[i]
    ia_count = ia_count + 1 }}
ia_avg = ia_sum/ia_count
for(i in seq_along(Iowa$tweet_volume)) {
  if(is.na(Iowa$tweet_volume[i])) {
    Iowa$tweet_volume[i]=ia_avg }}
Kansas <- select(get_trends(lat = 38.526600, lng = -96.726486),trend,tweet_volume)
ks_sum = 0
ks_count = 0
for(i in seq_along(Kansas$tweet_volume)) {
  if(!is.na(Kansas$tweet_volume[i])){
    ks_sum = ks_sum + Kansas$tweet_volume[i]
    ks_count = ks_count + 1 }}
ks_avg = ks_sum/ks_count
for(i in seq_along(Kansas$tweet_volume)) {
  if(is.na(Kansas$tweet_volume[i])) {
    Kansas$tweet_volume[i]=ks_avg }}
Kentucky <- select(get_trends(lat = 37.668140, lng = -84.670067),trend,tweet_volume)
ky_sum = 0
ky_count = 0
for(i in seq_along(Kentucky$tweet_volume)) {
  if(!is.na(Kentucky$tweet_volume[i])){
    ky_sum = ky_sum + Kentucky$tweet_volume[i]
    ky_count = ky_count + 1 }}
ky_avg = ky_sum/ky_count
for(i in seq_along(Kentucky$tweet_volume)) {
  if(is.na(Kentucky$tweet_volume[i])) {
    Kentucky$tweet_volume[i]=ky_avg }}
Louisiana <- select(get_trends(lat = 31.169546, lng = -91.867805),trend,tweet_volume)
la_sum = 0
la_count = 0
for(i in seq_along(Louisiana$tweet_volume)) {
  if(!is.na(Louisiana$tweet_volume[i])){
    la_sum = la_sum + Louisiana$tweet_volume[i]
    la_count = la_count + 1 }}
la_avg = la_sum/la_count
for(i in seq_along(Louisiana$tweet_volume)) {
  if(is.na(Louisiana$tweet_volume[i])) {
    Louisiana$tweet_volume[i]=la_avg }}
Maine <- select(get_trends(lat = 44.693947, lng = -69.381927),trend,tweet_volume)
me_sum = 0
me_count = 0
for(i in seq_along(Maine$tweet_volume)) {
  if(!is.na(Maine$tweet_volume[i])){
    me_sum = me_sum + Maine$tweet_volume[i]
    me_count = me_count + 1 }}
me_avg = me_sum/me_count
for(i in seq_along(Maine$tweet_volume)) {
  if(is.na(Maine$tweet_volume[i])) {
    Maine$tweet_volume[i]=me_avg }}
Maryland <- select(get_trends(lat = 39.063946, lng = -76.802101),trend,tweet_volume)
md_sum = 0
md_count = 0
for(i in seq_along(Maryland$tweet_volume)) {
  if(!is.na(Maryland$tweet_volume[i])){
    md_sum = md_sum + Maryland$tweet_volume[i]
    md_count = md_count + 1 }}
md_avg = md_sum/md_count
for(i in seq_along(Maryland$tweet_volume)) {
  if(is.na(Maryland$tweet_volume[i])) {
    Maryland$tweet_volume[i]=md_avg }}
Massachusetts <- select(get_trends(lat = 42.230171, lng = -71.530106),trend,tweet_volume)
ma_sum = 0
ma_count = 0
for(i in seq_along(Massachusetts$tweet_volume)) {
  if(!is.na(Massachusetts$tweet_volume[i])){
    ma_sum = ma_sum + Massachusetts$tweet_volume[i]
    ma_count = ma_count + 1 }}
ma_avg = ma_sum/ma_count
for(i in seq_along(Massachusetts$tweet_volume)) {
  if(is.na(Massachusetts$tweet_volume[i])) {
    Massachusetts$tweet_volume[i]=ma_avg }}
Michigan <- select(get_trends(lat = 43.326618, lng = -84.536095),trend,tweet_volume)
mi_sum = 0
mi_count = 0
for(i in seq_along(Michigan$tweet_volume)) {
  if(!is.na(Michigan$tweet_volume[i])){
    mi_sum = mi_sum + Michigan$tweet_volume[i]
    mi_count = mi_count + 1 }}
mi_avg = mi_sum/mi_count
for(i in seq_along(Michigan$tweet_volume)) {
  if(is.na(Michigan$tweet_volume[i])) {
    Michigan$tweet_volume[i]=mi_avg }}
Minnesota <- select(get_trends(lat = 45.694454, lng = -93.900192),trend,tweet_volume)
mn_sum = 0
mn_count = 0
for(i in seq_along(Minnesota$tweet_volume)) {
  if(!is.na(Minnesota$tweet_volume[i])){
    mn_sum = mn_sum + Minnesota$tweet_volume[i]
    mn_count = mn_count + 1 }}
mn_avg = mn_sum/mn_count
for(i in seq_along(Minnesota$tweet_volume)) {
  if(is.na(Minnesota$tweet_volume[i])) {
    Minnesota$tweet_volume[i]=mn_avg }}
Mississippi <- select(get_trends(lat = 32.741646, lng = -89.678696),trend,tweet_volume)
ms_sum = 0
ms_count = 0
for(i in seq_along(Mississippi$tweet_volume)) {
  if(!is.na(Mississippi$tweet_volume[i])){
    ms_sum = ms_sum + Mississippi$tweet_volume[i]
    ms_count = ms_count + 1 }}
ms_avg = ms_sum/ms_count
for(i in seq_along(Mississippi$tweet_volume)) {
  if(is.na(Mississippi$tweet_volume[i])) {
    Mississippi$tweet_volume[i]=ms_avg }}
Missouri <- select(get_trends(lat = 38.456085, lng = -92.288368),trend,tweet_volume)
mo_sum = 0
mo_count = 0
for(i in seq_along(Missouri$tweet_volume)) {
  if(is.na(Missouri$tweet_volume[i])) {
    Missouri$tweet_volume[i]=10000 }}
Montana <- select(get_trends(lat = 46.921925, lng = -110.454353),trend,tweet_volume)
mt_sum = 0
mt_count = 0
for(i in seq_along(Montana$tweet_volume)) {
  if(!is.na(Montana$tweet_volume[i])){
    mt_sum = mt_sum + Montana$tweet_volume[i]
    mt_count = mt_count + 1 }}
mt_avg = mt_sum/mt_count
for(i in seq_along(Montana$tweet_volume)) {
  if(is.na(Montana$tweet_volume[i])) {
    Montana$tweet_volume[i]=mt_avg }}
Nebraska <- select(get_trends(lat = 41.125370, lng = -98.268082),trend,tweet_volume)
ne_sum = 0
ne_count = 0
for(i in seq_along(Nebraska$tweet_volume)) {
  if(!is.na(Nebraska$tweet_volume[i])){
    ne_sum = ne_sum + Nebraska$tweet_volume[i]
    ne_count = ne_count + 1 }}
ne_avg = ne_sum/ne_count
for(i in seq_along(Nebraska$tweet_volume)) {
  if(is.na(Nebraska$tweet_volume[i])) {
    Nebraska$tweet_volume[i]=ne_avg }}
Nevada <- select(get_trends(lat = 38.313515, lng = -117.055374),trend,tweet_volume)
nv_sum = 0
nv_count = 0
for(i in seq_along(Nevada$tweet_volume)) {
  if(!is.na(Nevada$tweet_volume[i])){
    nv_sum = nv_sum + Nevada$tweet_volume[i]
    nv_count = nv_count + 1 }}
nv_avg = nv_sum/nv_count
for(i in seq_along(Nevada$tweet_volume)) {
  if(is.na(Nevada$tweet_volume[i])) {
    Nevada$tweet_volume[i]=nv_avg }}
New_Hampshire <- select(get_trends(lat = 43.452492, lng = -71.563896),trend,tweet_volume)
nh_sum = 0
nh_count = 0
for(i in seq_along(New_Hampshire$tweet_volume)) {
  if(!is.na(New_Hampshire$tweet_volume[i])){
    nh_sum = nh_sum + New_Hampshire$tweet_volume[i]
    nh_count = nh_count + 1 }}
nh_avg = nh_sum/nh_count
for(i in seq_along(New_Hampshire$tweet_volume)) {
  if(is.na(New_Hampshire$tweet_volume[i])) {
    New_Hampshire$tweet_volume[i]=nh_avg }}
New_Jersey <- select(get_trends(lat = 40.298904, lng = -74.521011),trend,tweet_volume)
nj_sum = 0
nj_count = 0
for(i in seq_along(New_Jersey$tweet_volume)) {
  if(!is.na(New_Jersey$tweet_volume[i])){
    nj_sum = nj_sum + New_Jersey$tweet_volume[i]
    nj_count = nj_count + 1 }}
nj_avg = nj_sum/nj_count
for(i in seq_along(New_Jersey$tweet_volume)) {
  if(is.na(New_Jersey$tweet_volume[i])) {
    New_Jersey$tweet_volume[i]=nj_avg }}
New_Mexico <- select(get_trends(lat = 34.840515, lng = -106.248482),trend,tweet_volume)
nm_sum = 0
nm_count = 0
for(i in seq_along(New_Mexico$tweet_volume)) {
  if(!is.na(New_Mexico$tweet_volume[i])){
    nm_sum = nm_sum + New_Mexico$tweet_volume[i]
    nm_count = nm_count + 1 }}
nm_avg = nm_sum/nm_count
for(i in seq_along(New_Mexico$tweet_volume)) {
  if(is.na(New_Mexico$tweet_volume[i])) {
    New_Mexico$tweet_volume[i]=nm_avg }}
New_York <- select(get_trends(lat = 42.165726, lng = -74.948051),trend,tweet_volume)
ny_sum = 0
ny_count = 0
for(i in seq_along(New_York$tweet_volume)) {
  if(!is.na(New_York$tweet_volume[i])){
    ny_sum = ny_sum + New_York$tweet_volume[i]
    ny_count = ny_count + 1 }}
ny_avg = ny_sum/ny_count
for(i in seq_along(New_York$tweet_volume)) {
  if(is.na(New_York$tweet_volume[i])) {
    New_York$tweet_volume[i]=ny_avg }}
North_Carolina <- select(get_trends(lat = 35.630066, lng = -79.806419),trend,tweet_volume)
nc_sum = 0
nc_count = 0
for(i in seq_along(North_Carolina$tweet_volume)) {
  if(!is.na(North_Carolina$tweet_volume[i])){
    nc_sum = nc_sum + North_Carolina$tweet_volume[i]
    nc_count = nc_count + 1 }}
nc_avg = nc_sum/nc_count
for(i in seq_along(North_Carolina$tweet_volume)) {
  if(is.na(North_Carolina$tweet_volume[i])) {
    North_Carolina$tweet_volume[i]=nc_avg }}
North_Dakota <- select(get_trends(lat = 47.528912, lng = -99.784012),trend,tweet_volume)
nd_sum = 0
nd_count = 0
for(i in seq_along(North_Dakota$tweet_volume)) {
  if(!is.na(North_Dakota$tweet_volume[i])){
    nd_sum = nd_sum + North_Dakota$tweet_volume[i]
    nd_count = nd_count + 1 }}
nd_avg = nd_sum/nd_count
for(i in seq_along(North_Dakota$tweet_volume)) {
  if(is.na(North_Dakota$tweet_volume[i])) {
    North_Dakota$tweet_volume[i]=nd_avg }}
Ohio <- select(get_trends(lat = 40.388783, lng = -82.764915),trend,tweet_volume)
oh_sum = 0
oh_count = 0
for(i in seq_along(Ohio$tweet_volume)) {
  if(!is.na(Ohio$tweet_volume[i])){
    oh_sum = oh_sum + Ohio$tweet_volume[i]
    oh_count = oh_count + 1 }}
oh_avg = oh_sum/oh_count
for(i in seq_along(Ohio$tweet_volume)) {
  if(is.na(Ohio$tweet_volume[i])) {
    Ohio$tweet_volume[i]=oh_avg }}
Oklahoma <- select(get_trends(lat = 35.565342, lng = -96.928917),trend,tweet_volume)
ok_sum = 0
ok_count = 0
for(i in seq_along(Oklahoma$tweet_volume)) {
  if(!is.na(Oklahoma$tweet_volume[i])){
    ok_sum = ok_sum + Oklahoma$tweet_volume[i]
    ok_count = ok_count + 1 }}
ok_avg = ok_sum/ok_count
for(i in seq_along(Oklahoma$tweet_volume)) {
  if(is.na(Oklahoma$tweet_volume[i])) {
    Oklahoma$tweet_volume[i]=ok_avg }}
Oregon <- select(get_trends(lat = 44.572021, lng = -122.070938),trend,tweet_volume)
or_sum = 0
or_count = 0
for(i in seq_along(Oregon$tweet_volume)) {
  if(!is.na(Oregon$tweet_volume[i])){
    or_sum = or_sum + Oregon$tweet_volume[i]
    or_count = or_count + 1 }}
or_avg = or_sum/or_count
for(i in seq_along(Oregon$tweet_volume)) {
  if(is.na(Oregon$tweet_volume[i])) {
    Oregon$tweet_volume[i]=or_avg }}
Pennsylvania <- select(get_trends(lat = 40.590752, lng = -77.209755),trend,tweet_volume)
pa_sum = 0
pa_count = 0
for(i in seq_along(Pennsylvania$tweet_volume)) {
  if(!is.na(Pennsylvania$tweet_volume[i])){
    pa_sum = pa_sum + Pennsylvania$tweet_volume[i]
    pa_count = pa_count + 1 }}
pa_avg = pa_sum/pa_count
for(i in seq_along(Pennsylvania$tweet_volume)) {
  if(is.na(Pennsylvania$tweet_volume[i])) {
    Pennsylvania$tweet_volume[i]=pa_avg }}
Rhode_Island <- select(get_trends(lat = 41.680893, lng = -71.511780),trend,tweet_volume)
ri_sum = 0
ri_count = 0
for(i in seq_along(Rhode_Island$tweet_volume)) {
  if(!is.na(Rhode_Island$tweet_volume[i])){
    ri_sum = ri_sum + Rhode_Island$tweet_volume[i]
    ri_count = ri_count + 1 }}
ri_avg = ri_sum/ri_count
for(i in seq_along(Rhode_Island$tweet_volume)) {
  if(is.na(Rhode_Island$tweet_volume[i])) {
    Rhode_Island$tweet_volume[i]=ri_avg }}
South_Carolina <- select(get_trends(lat = 33.856892, lng = -80.945007),trend,tweet_volume)
sc_sum = 0
sc_count = 0
for(i in seq_along(South_Carolina$tweet_volume)) {
  if(!is.na(South_Carolina$tweet_volume[i])){
    sc_sum = sc_sum + South_Carolina$tweet_volume[i]
    sc_count = sc_count + 1 }}
sc_avg = sc_sum/sc_count
for(i in seq_along(South_Carolina$tweet_volume)) {
  if(is.na(South_Carolina$tweet_volume[i])) {
    South_Carolina$tweet_volume[i]=sc_avg }}
South_Dakota <- select(get_trends(lat = 44.299782, lng = -99.438828),trend,tweet_volume)
sd_sum = 0
sd_count = 0
for(i in seq_along(South_Dakota$tweet_volume)) {
  if(!is.na(South_Dakota$tweet_volume[i])){
    sd_sum = sd_sum + South_Dakota$tweet_volume[i]
    sd_count = sd_count + 1 }}
sd_avg = sd_sum/sd_count
for(i in seq_along(South_Dakota$tweet_volume)) {
  if(is.na(South_Dakota$tweet_volume[i])) {
    South_Dakota$tweet_volume[i]=sd_avg }}
Tennessee <- select(get_trends(lat = 35.747845, lng = -86.692345),trend,tweet_volume)
tn_sum = 0
tn_count = 0
for(i in seq_along(Tennessee$tweet_volume)) {
  if(!is.na(Tennessee$tweet_volume[i])){
    tn_sum = tn_sum + Tennessee$tweet_volume[i]
    tn_count = tn_count + 1 }}
tn_avg = tn_sum/tn_count
for(i in seq_along(Tennessee$tweet_volume)) {
  if(is.na(Tennessee$tweet_volume[i])) {
    Tennessee$tweet_volume[i]=tn_avg }}
Texas <- select(get_trends(lat = 31.054487, lng = -97.563461),trend,tweet_volume)
tx_sum = 0
tx_count = 0
for(i in seq_along(Texas$tweet_volume)) {
  if(!is.na(Texas$tweet_volume[i])){
    tx_sum = tx_sum + Texas$tweet_volume[i]
    tx_count = tx_count + 1 }}
tx_avg = tx_sum/tx_count
for(i in seq_along(Texas$tweet_volume)) {
  if(is.na(Texas$tweet_volume[i])) {
    Texas$tweet_volume[i]=tx_avg }}
Utah <- select(get_trends(lat = 40.150032, lng = -111.862434),trend,tweet_volume)
ut_sum = 0
ut_count = 0
for(i in seq_along(Utah$tweet_volume)) {
  if(!is.na(Utah$tweet_volume[i])){
    ut_sum = ut_sum + Utah$tweet_volume[i]
    ut_count = ut_count + 1 }}
ut_avg = ut_sum/ut_count
for(i in seq_along(Utah$tweet_volume)) {
  if(is.na(Utah$tweet_volume[i])) {
    Utah$tweet_volume[i]=ut_avg }}
Vermont <- select(get_trends(lat = 44.045876, lng = -72.710686),trend,tweet_volume)
vt_sum = 0
vt_count = 0
for(i in seq_along(Vermont$tweet_volume)) {
  if(!is.na(Vermont$tweet_volume[i])){
    vt_sum = vt_sum + Vermont$tweet_volume[i]
    vt_count = vt_count + 1 }}
vt_avg = vt_sum/vt_count
for(i in seq_along(Vermont$tweet_volume)) {
  if(is.na(Vermont$tweet_volume[i])) {
    Vermont$tweet_volume[i]=vt_avg }}
Virginia <- select(get_trends(lat = 37.769337, lng = -78.169968),trend,tweet_volume)
va_sum = 0
va_count = 0
for(i in seq_along(Virginia$tweet_volume)) {
  if(!is.na(Virginia$tweet_volume[i])){
    va_sum = va_sum + Virginia$tweet_volume[i]
    va_count = va_count + 1 }}
va_avg = va_sum/va_count
for(i in seq_along(Virginia$tweet_volume)) {
  if(is.na(Virginia$tweet_volume[i])) {
    Virginia$tweet_volume[i]=va_avg }}
Washington <- select(get_trends(lat = 47.400902, lng = -121.490494),trend,tweet_volume)
wa_sum = 0
wa_count = 0
for(i in seq_along(Washington$tweet_volume)) {
  if(!is.na(Washington$tweet_volume[i])){
    wa_sum = wa_sum + Washington$tweet_volume[i]
    wa_count = wa_count + 1 }}
wa_avg = wa_sum/wa_count
for(i in seq_along(Washington$tweet_volume)) {
  if(is.na(Washington$tweet_volume[i])) {
    Washington$tweet_volume[i]=wa_avg }}
West_Virginia <- select(get_trends(lat = 38.491226, lng = -80.954453),trend,tweet_volume)
wv_sum = 0
wv_count = 0
for(i in seq_along(West_Virginia$tweet_volume)) {
  if(!is.na(West_Virginia$tweet_volume[i])){
    wv_sum = wv_sum + West_Virginia$tweet_volume[i]
    wv_count = wv_count + 1 }}
wv_avg = wv_sum/wv_count
for(i in seq_along(West_Virginia$tweet_volume)) {
  if(is.na(West_Virginia$tweet_volume[i])) {
    West_Virginia$tweet_volume[i]=wv_avg }}
Wisconsin <- select(get_trends(lat = 44.268543, lng = -89.616508),trend,tweet_volume)
wi_sum = 0
wi_count = 0
for(i in seq_along(Wisconsin$tweet_volume)) {
  if(!is.na(Wisconsin$tweet_volume[i])){
    wi_sum = wi_sum + Wisconsin$tweet_volume[i]
    wi_count = wi_count + 1 }}
wi_avg = wi_sum/wi_count
for(i in seq_along(Wisconsin$tweet_volume)) {
  if(is.na(Wisconsin$tweet_volume[i])) {
    Wisconsin$tweet_volume[i]=wi_avg }}
Wyoming <- select(get_trends(lat = 42.755966, lng = -107.302490),trend,tweet_volume)
wy_sum = 0
wy_count = 0
for(i in seq_along(Wyoming$tweet_volume)) {
  if(!is.na(Wyoming$tweet_volume[i])){
    wy_sum = wy_sum + Wyoming$tweet_volume[i]
    wy_count = wy_count + 1 }}
wy_avg = wy_sum/wy_count
for(i in seq_along(Wyoming$tweet_volume)) {
  if(is.na(Wyoming$tweet_volume[i])) {
    Wyoming$tweet_volume[i]=wy_avg }}

#integrated dataset for all 50 states (plus Washington DC)
data <- combine(Alabama,Alaska,Arizona,Arkansas,California,Colorado,Connecticut,District_of_Columbia,Delaware,Florida,Georgia,Hawaii,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,
                Louisiana,Maine,Maryland,Massachusetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New_Hampshire,New_Jersey,New_Mexico,New_York,North_Carolina,North_Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode_Island,South_Carolina,South_Dakota,Tennessee,Texas,
                Utah,Vermont,Virginia,Washington,West_Virginia,Wisconsin,Wyoming)
#change "source" column to "state"
colnames(data) <- c("trend", "tweet_volume", "state")

#WORD CLOUD
#create UI as a fluid page and set theme to cerulean
ui <- fluidPage(theme = shinytheme("cerulean"),
                #create title panel with twitter bird logo and subtitle to describe the application
                titlePanel(h1(div(img(src="twitter_PNG.png", height = 80, width = 80), "Top Twitter Trends"), 
                              h4("Live wordclouds showing Twitter trends from around the world, the USA, and all 50 states."), 
                              h5(Sys.time()))),
                #create interactive slider bar to display the number of trends (from 0-50)
                fluidRow(column(5,sliderInput(inputId='n', label='Number of Trends',value=200, min=0, max=50))),
                tabsetPanel(
                  #world wordcloud panel
                  tabPanel(
                    "Around the World", wordcloud2Output(outputId = "WorldTrending")
                  ),
                  #usa wordcloud panel
                  tabPanel(
                    "USA", wordcloud2Output(outputId = "USATrending")
                  ),
                  #50 states wordcloud panel
                  tabPanel(
                    "50 States", 
                    #creates the dropdown menu for all states
                    selectInput(inputId = 'State', label=NULL,
                                choices = c('select a state' = '','Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','District of Columbia'='District_of_Columbia','Delaware','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine',
                                            'Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire'='New_Hampshire','New Jersey'='New_Jersey','New Mexico'='New_Mexico','New York'='New_York','North Carolina'='North_Carolina','North Dakota'='North_Dakota',
                                            'Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island'='Rhode_Island','South Carolina'='South_Carolina','South Dakota'='South_Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia'='West_Virginia','Wisconsin','Wyoming')),
                    wordcloud2Output(outputId = "StatesTrending"),
                  ),
                  #panel for COVID-19 map
                  tabPanel("COVID-19 Map", h5("This map of COVID-19 cases by state is provided to encourage analysis of top trends related to COVID vs. the presence of the virus in each state."), (div(img(src="COVID.png"))))
                ))
#creation of server 
server <- function(input, output){
  #make reactive
  hashtag <-reactive({
    req(input$number)
    paste("#",input$number)
  })
  tab <- reactive({ 
    req(input$State)
    get(input$State)
  })
  #assign output and link outputs to slider 
  output$WorldTrending<-renderWordcloud2({wordcloud2(world[1:input$n,])})
  output$USATrending<-renderWordcloud2({wordcloud2(usa[1:input$n,])})
  output$StatesTrending<-renderWordcloud2({wordcloud2(tab()[1:input$n,])})
}
shinyApp(ui=ui,server = server)
