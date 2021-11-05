p# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#Total rank each of the key Conservation element variables
# 5 categories - Negligible (1), Low(2), Medium,(3) High(4), Very High(5)

#function that takes a row and a category and returns a total for that category and row
classSumFn <- function(df, j) {
  mutate(df, !!Conservation_Class[[j]] := rowSums(across(all_of(ElementsToRank), ~ .== j,)))
  }

#what EcoLandElementsNhaclass to base ranking on
wshd_3<-readRDS('tmp/wshd_3')

ElementsToRank<-c("OG_haclass","CH_haclass","KBA_haclass")
wshd_4<-wshd_3 %>%
  #st_drop_geometry() %>%
  select(aqua_id,all_of(ElementsToRank))

for(i in 1:5) {
  wshd_4 <- classSumFn(wshd_4, i)
}


#Calculate overall rank following NatureServe Threat ranking method
# VHigh: VHigh>=1  or High>=2 or High==1 & Medium>=2
# High: High==1 or Medium>=3 or Medium==2 & Low==2 or Medium==1 & Low>=3
# Medium Medium==1 or Low>=4
# Low Low>0 & Low<=3
# Negligible 0

#Set 1 - skews towards High and VHigh - check Provincial output
wshd_5<- wshd_4 %>%
  mutate(Rank=case_when(
    (Low>0 & Low<=3) & Medium==0 & High==0 & VHigh==0 ~ 'Low',
    (Low>=0 & Medium>0 & Medium<=2 & High==0 & VHigh==0) | (Low>=4 & Medium==2 & High==0 & VHigh==0) ~ 'Medium',
    (Low>=0 & Medium>=0 & High==1 & VHigh==0) | (Low>=0 & Medium>=3 & High==0 & VHigh==0) |
      (Low==2 & Medium==2 & High==0 & VHigh==0) | (Low>=3 & Medium ==1 & High==0 & VHigh==0) ~ 'High',
    (Low>=0 & Medium>=0 & High>=0 & VHigh>=1) | (Low>=0 & Medium>=2 & High==1 & VHigh==0) |
      (Low>=0 & Medium>=0 & High>=2 & VHigh==0) ~ 'VHigh',
    (Low==0 & Medium==0 & High==0 & VHigh==0 ~ 'Negligible' )
  ))

unique(wshd_5$Rank)
C_ClassM <- c('Negligible','Low','Medium','High','VHigh')
barplot(table(wshd_5$Rank))

wshd_6 <- wshd_5 %>%
  dplyr::select(aqua_id, all_of(Conservation_Class), Rank) %>%
  left_join(wshd_3, by='aqua_id')

write_sf(wshd_6, file.path(spatialOutDir,"wshd_6.gpkg"))

#OG and CH
wshd_8<- wshd_4 %>%
  dplyr::filter(OG_haclass %in% c(3,4,5) & CH_haclass %in% c(3,4,5))

wshd_9<-wshd_8 %>%
  st_drop_geometry()
write_sf(wshd_8, file.path(spatialOutDir,"wshd_8.gpkg"))

wshd_6 <- wshd_5 %>%
  dplyr::select(aqua_id, all_of(Conservation_Class), Rank) %>%
  left_join(wshd_3, by='aqua_id')
ElementsToRank<-c("OG_haclass","CH_haclass","KBA_haclass")


  mutate(Rank=case_when(
    (Low>0 & Low<=3) & Medium==0 & High==0 & VHigh==0 ~ 'Low',
    (Low>=0 & Medium>0 & Medium<=2 & High==0 & VHigh==0) | (Low>=4 & Medium==2 & High==0 & VHigh==0) ~ 'Medium',
    (Low>=0 & Medium>=0 & High==1 & VHigh==0) | (Low>=0 & Medium>=3 & High==0 & VHigh==0) |
      (Low==2 & Medium==2 & High==0 & VHigh==0) | (Low>=3 & Medium ==1 & High==0 & VHigh==0) ~ 'High',
    (Low>=0 & Medium>=0 & High>=0 & VHigh>=1) | (Low>=0 & Medium>=2 & High==1 & VHigh==0) |
      (Low>=0 & Medium>=0 & High>=2 & VHigh==0) ~ 'VHigh',
    (Low==0 & Medium==0 & High==0 & VHigh==0 ~ 'Negligible' )
  ))

unique(wshd_5$Rank)
C_ClassM <- c('Negligible','Low','Medium','High','VHigh')
barplot(table(wshd_5$Rank))

wshd_6 <- wshd_5 %>%
  dplyr::select(aqua_id, all_of(Conservation_Class), Rank) %>%
  left_join(wshd_3, by='aqua_id')

write_sf(wshd_6, file.path(spatialOutDir,"wshd_6.gpkg"))









# To much VHigh
# VHigh: VHigh>=1  or High>=3(was 2) or High==1 & Medium>=3(was 2)
# High: High==1 or Medium>=3 or Medium==2 & Low==2 or Medium==1 & Low>=3
# Medium Medium==1 or Low>=4
# Low Low>0 & Low<=3
# Negligible 0

#Set 2
wshd_3<- wshd_2 %>%
  mutate(Rank=case_when(
    (Low>0 & Low<=3) & Medium==0 & High==0 & VHigh==0 ~ 'Low',
    (Low>=0 & Medium>0 & Medium<=2 & High==0 & VHigh==0) | (Low>=4 & Medium==2 & High==0 & VHigh==0) ~ 'Medium',
    (Low>=0 & Medium>=0 & High==1 & VHigh==0) | (Low>=0 & Medium>=3 & High==0 & VHigh==0) |
      (Low==2 & Medium==2 & High==0 & VHigh==0) | (Low>=3 & Medium ==1 & High==0 & VHigh==0) |
      (Low>=0 & Medium>=3 & High==1 & VHigh==0) | (Low>=0 & Medium>=0 & High>=2 & VHigh==0) ~ 'High',
    (Low>=0 & Medium>=0 & High>=0 & VHigh>=1) ~ 'VHigh',
    (Low==0 & Medium==0 & High==0 & VHigh==0 ~ 'Negligible' )
  ))


###############
#Other code
Threat_O <- Threat_O %>%
  mutate(Threat_Class =
           case_when(
             Low>0 & Low<4 & Medium==0 & High==0 ~ 'Low',
             (Low>3 & Medium==0 & High==0) | (Low<=3 & Medium==1 & High==0 ) ~ 'Medium',
             (Low>=0 & Medium<2 & High==1 ) | (Low>=0 & Medium>2 & High==0) | (Low>=2 & Medium==2 & High==0) |
               (Low>2 & Medium==1 & High==0)  ~ 'High',
             (Low>=0 & Medium>=0 & High>1) | (Low>=0 & Medium>1 & High==1) ~ 'VHigh',
             Low==0 & Medium==0 & High==0 ~ 'Negligible'
           )) %>%
  left_join(Threat_LUT, by='Threat_Class')


#######
#Trying to use lapply, ends up being more complicated
wshd_3<-wshd_2
wshd_3<-lapply(rep(1:5), function(i){
    wshd_2 %>%
    mutate(!!C_Class[[i]] := rowSums(across(CElements_names, ~ .== i,))) %>%
    dplyr::select(aqua_id,!!C_Class[[i]])
    #distinct(aqua_id,!!C_Class[[i]])
}) %>%
 as.data.frame() %>%
 dplyr::select(aqua_id,all_of(C_Class))


requs<-data.frame(seq(1, length(CElements), by=1), CElements, CElements_names)

  df<-lapply(requs[,1], function(i) RequireFn(SampleStrata, i))
  ScoreCard_1<-ldply(df,data.frame)

rowSums((. == 1))))
  #mutate(Low=classSumFn(.), across(CElements_names))
  #mutate(across(CElements_names ,classSumFn(.)))
#mutate(Low=classSumFn(.), across(OGclass, ~ .))

df %>% mutate(across(CElements_names, classSumFn(df)))


gdf %>% mutate(n = rnorm(1), across(v1:v2, ~ .x + n))
mutate(across(v1:v2, ~ .x + rnorm(1)))


  mutate(across(c(Sepal.Length, Sepal.Width), round))


#mutate(Low=classSumFn(.))
#mutate(Low=rowSums((. == 1), na.rm=TRUE)) # as a function and pass in 1 to 5 or seperate statements?


   mutate_at(CElements_names, ~ fun(.))

data %>% mutate_at(vars, myoperation)


  mutate(across(CElements_names),

         mutate_at(vars(Year), list(~parse_number(.)))%>%


  filter(OGclass==1)

gather(df, Species, Value, matches("Species")) %>%
  group_by(Month) %>% summarise(z = sum(Value))


  select(which(., OGclass==1))
wshd_1 %>%
  rowwise() %>%
  select(which(wshd_1$OGclass == 1))

  select_if(function(col) == 1)

y <- x %>% select_if(function(col) is.numeric(col) |
                       all(col == .$V4) |
                       all(col == .$V5))

select(c('OGclass','CHclass') == 1)




#For each watershed assign the number in each threat_class
wshd_2<-wshd_1 %>%
  st_drop_geometry() %>%
  rowwise(aqua_id) %>%
  mutate(Low=)
mutate(Low=sum(across(all_of(CElements_names))))

a <- c(1:10)
b <- c(2:11)
c <- c(3:12)

tibble(a, b, c) %>%
  modify(~ ifelse(. == 3, 4,
                  (ifelse(. == 2, 9, .))
  ))

tibble(a, b, c) %>%
  modify_if(~ if) %>%

ifelse(. == 3, 4,
                  (ifelse(. == 2, 9, .))
  ))




# A tibble: 10 x 3



wshd_2<- wshd_1 %>%
  mutate(Rank=case_when(
    Low>0 & Low<4 & Medium==0 & High==0 ~ 'Low',
    (Low>3 & Medium==0 & High==0) | (Low<=3 & Medium==1 & High==0 ) ~ 'Medium',
    (Low>=0 & Medium<2 & High==1 ) | (Low>=0 & Medium>2 & High==0) | (Low>=2 & Medium==2 & High==0) |
      (Low>2 & Medium==1 & High==0)  ~ 'High',
    (Low>=0 & Medium>=0 & High>1) | (Low>=0 & Medium>1 & High==1) ~ 'VHigh',
    Low==0 & Medium==0 & High==0 ~ 'Negligible' )





count(across(contains("color")), sort = TRUE)

mutate(Low=sum(c_across(all_of(CElements_names))))

rf %>% mutate(total = sum(c_across(w:z)))

df <- tibble(id = 1:4, w = runif(4), x = runif(4), y = runif(4), z = runif(4))
df %>%
  rowwise() %>%
  mutate(
    sum = sum(c_across(c(x,z)))  )

mutate(Low=rowSums(across(where(.==1))))

  mutate(Low = (OGclass == 1 + CHclass==1))

  total = rowSums(across(where(is.numeric))))


Threat_O <- Threat_O %>%
  mutate(Threat_Class =
           case_when(
             Low>0 & Low<4 & Medium==0 & High==0 ~ 'Low',
             (Low>3 & Medium==0 & High==0) | (Low<=3 & Medium==1 & High==0 ) ~ 'Medium',
             (Low>=0 & Medium<2 & High==1 ) | (Low>=0 & Medium>2 & High==0) | (Low>=2 & Medium==2 & High==0) |
               (Low>2 & Medium==1 & High==0)  ~ 'High',
             (Low>=0 & Medium>=0 & High>1) | (Low>=0 & Medium>1 & High==1) ~ 'VHigh',
             Low==0 & Medium==0 & High==0 ~ 'Negligible'
           )) %>%
  left_join(Threat_LUT, by='Threat_Class')




CElement_names

#do as a function

NormQ<-quantile(wshd_1$OG_Ha)
m<- c(NormQ[2],NormQ[3],NormQ[4],NormQ[5])

hist(wshd_1$OG_Ha,
     main = "Distribution of Human Footprint",
     xlab = "Score", ylab = "Frequency",
     col = "springgreen")

OG_LUT<-data.frame(wshd_1$aqua_id,
    OG_Ha=wshd_1$OG_Ha,Cclass=findInterval(wshd_1$OG_Ha, unique(quantile(wshd_1$OG_Ha,
    prob = seq(0, 1, 1/num_bins)))))




#Classify intersected Connectivity watersheds
NormQ<-quantile(aqua_sf$ConnectHa)
#make a classification table based on quantiles
m<- c(NormQ[2],NormQ[3],NormQ[4],NormQ[5])

aqua_sf$ConnectHaClass <- cut(aqua_sf$ConnectHa,
                              breaks=m,
                              labels=c("low","moderate","high"))

NormQ<-quantile(aqua_sf$ConnectSM3Ha)
#make a classification table based on quantiles
m<- c(NormQ[2],NormQ[3],NormQ[4],NormQ[5])

aqua_sf$ConnectSM3HaClass <- cut(aqua_sf$ConnectSM3Ha,
                                 breaks=m,
                                 labels=c("low","moderate","high"))

aqua_sf_ng<-aqua_sf %>%
  st_drop_geometry() %>%
  dplyr::select(ASSESSMENT_UNIT_SOURCE_ID,ConnectHa,ConnectHaClass, ConnectSM3Ha, ConnectSM3HaClass)
View(aqua_sf_ng)

write_sf(aqua_sf, file.path(spatialOutDir,"aqua_sf.gpkg"), overwrite=TRUE)

#m <- c(0,1,1, 1,1.25,2, 1.25,1.5,3, 1.5,1.75,4, 1.75,2,5, 2,9999,6)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
norm_cum_curr <- reclassify(normalized_cum_currmap, rclmat)
writeRaster(norm_cum_curr, filename=file.path(spatialOutDir,'norm_cum_curr'), format="GTiff", overwrite=TRUE)

Intact70_pts <-st_intersection(aquapt, Intactcls70)
saveRDS(Intact70_pts,'tmp/Intact70_pts')
#write_sf(Intact70_pts, file.path(spatialOutDir,"Intact70_pts.gpkg"))

Intact70_pts<-readRDS('tmp/Intact70_pts') %>%
  st_drop_geometry() %>%
  dplyr::select(ASSESSMENT_UNIT_SOURCE_ID,OGCmt_IntactV=value, OGCmt_IntactC=label)

#Subset the aqua by the pts in OG commitee intact
Overlap_AUs <- Intact70_pts$ASSESSMENT_UNIT_SOURCE_ID

aqua_sf_intact <- aqua_sf %>%
  left_join(Intact70_pts )

saveRDS( aqua_sf_CU, 'tmp/aqua_sf_CU')
write_sf(aqua_sf_CU, file.path(spatialOutDir,"aqua_sf_CU.gpkg"))

#Mask the Connectivity layer
#Make the connectivity into a vector - smooth?
connect <- read_stars(file.path(spatialOutDir,'connect.tif')) %>%
  st_as_sf(merge=TRUE,na.rm=TRUE,use_integer=TRUE)
connectM$areaHa <- as.single(st_area(connectM)/1000000)

#Eiliminate small polygons
connectMe2<-connectM %>%
  dplyr::filter(areHa>2)
write_sf(connectMe2, file.path(spatialOutDir,"connectMe2.gpkg"), overwrite=TRUE)

connectMe10<-connectM %>%
  dplyr::filter(areHa>10) %>%
  st_transform(3005) %>%
  st_buffer(dist=0)
write_sf(connectMe10, file.path(spatialOutDir,"connectMe10.gpkg"), overwrite=TRUE)

#mask with intact
corridor<-connectMe10 %>%
  st_difference(aqua_intact_mask)

saveRDS(corridor,file='tmp/corridor')
write_sf(corridor, file.path(spatialOutDir,"corridor.gpkg"), overwrite=TRUE)



#Class total disturbance
aqua_sf_intact <- aqua_sf_intact %>%
  mutate(TotDisturb=round(DISTRB_HUMAN_CRNT_20YR_HA+DISTRB_HUMAN_HIST_HA)/ASSESSMENT_UNIT_AREA_HA*100,2) %>%
  mutate(DisturbCat=cut(TotDisturb, breaks=c(-Inf, 10, 15, 20, 25, 30, 50, 90, Inf)))

write_sf(aqua_sf_intact, file.path(spatialOutDir,"aqua_sf_intact.gpkg"), overwrite=TRUE)
aqua_sf_intact<-st_read(file.path(spatialOutDir,"aqua_sf_intact.gpkg"))

table(aqua_sf_intact$DisturbCat, aqua_sf_intact$OGCmt_Intact)
hist(aqua_sf_intact$TotDisturb)

