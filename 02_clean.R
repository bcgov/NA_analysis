# Copyright 2021 Province of British Columbia
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

#Multiply attribute rasters by nonPA - ie clip out attribute area 'at risk'

At_riskFN <- function(j, PPA, suffix){
    df <-raster(file.path(spatialOutDir,elementNin[j]))
    RasOut<-df*PPA
    OutName<-paste0(sub(".tif.*", "", elementNin[j]),'_atriskr',suffix)
    writeRaster(RasOut,filename=file.path(spatialOutDir,OutName),format="GTiff", overwrite=TRUE)
    RasOut
}

#Combine PPA and OECM  - for this round
PPAIin<-PPAIr*OECMIr
writeRaster(PPAIin,filename=file.path(spatialOutDir,'PPAIin'),format="GTiff", overwrite=TRUE)
PPAIin<-raster(file.path(spatialOutDir,'PPAIin.tif'))

#suffix for file name
PPAsuffix<-'PO'

#List of Conservation Elements
elementNin<-c('OG_26r.tif','OG_5r.tif','OG_recruitr.tif','CHR.tif','KBAr.tif')
ConservationElementsL<-lapply(1:length(elementNin), function(i) At_riskFN(i, PPAIin, PPAsuffix))

#List of Landscape Elements
elementNin<-c('Intact.tif','corridorr.tif','BECrepr.tif','CHR.tif','KBAr.tif')
LandscapeElementsL<-lapply(1:length(elementNin), function(i)At_riskFN(i, PPAIin, PPAsuffix))

#stragglers
elementNin<-c('wetlandsR.tif')
StraglerElementsL<-lapply(1:length(elementNin), function(i)At_riskFN(i, PPAIin, PPAsuffix))

elementNin<-c('BCr.tif')
Stragler2ElementsL<-lapply(1:length(elementNin), function(i)At_riskFN(i, PPAIin, PPAsuffix))

elementNin<-c('BEC_macroR.tif')
Stragler3ElementsL<-lapply(1:length(elementNin), function(i)At_riskFN(i, PPAIin, PPAsuffix))


#Check the OG layer for the 2.7M OG.value has the original 3 codes
#multiply by PPA+OECM
OG.check<-OG.value*PPAIin

##Intersect Connectivity analysis raster results with aqua_sf
norm_cum_curr<-raster(file.path(ConnectSpatialDir,'/out/spatial/norm_cum_curr.tif'))
norm_cum_curr[norm_cum_curr<4] <- NA
norm_cum_curr[norm_cum_curr==4] <- 1
writeRaster(norm_cum_curr, filename=file.path(spatialOutDir,'norm_cum_curr'), format="GTiff", overwrite=TRUE)

Connextract<- exact_extract(norm_cum_curr, aqua_sf_in, 'count', progress=TRUE, force_df=TRUE)
saveRDS(Connextract,file='tmp/Connextract')
Connextract<-readRDS(file='tmp/Connextract')
Connect_LUT<- Connextract %>%
  mutate(aqua_id=as.numeric(rownames(Connextract))) %>%
  dplyr::mutate(ConnectHa=count*100) %>%
  dplyr::select(aqua_id,ConnectHa)

##Intersect smoothd Connectivity analysis raster results with aqua_sf
norm_cum_currSM3rcls<-raster(file.path(ConnectSpatialDir,'out/spatial/norm_cum_currSM3rcls.tif'))
norm_cum_currSM3rcls[norm_cum_currSM3rcls<4] <- NA
norm_cum_currSM3rcls[norm_cum_currSM3rcls==4] <- 1
writeRaster(norm_cum_currSM3rcls, filename=file.path(spatialOutDir,'connect'), format="GTiff", overwrite=TRUE)

Connextract2<- exact_extract(norm_cum_currSM3rcls, aqua_sf_in, 'count', progress=TRUE, force_df=TRUE)
saveRDS(Connextract2,file='tmp/Connextract2')
Connextract<-readRDS(file='tmp/Connextract2')
Connect2_LUT<- Connextract2 %>%
  mutate(aqua_id=as.numeric(rownames(Connextract2))) %>%
  dplyr::mutate(ConnectSM3Ha=count*100) %>%
  dplyr::select(aqua_id,ConnectSM3Ha)


####Add protected areas to watersheds - st_intersection fails - due to topology error
parks2017<- parks2017 %>%
  #st_make_valid() %>%
  st_set_precision(1000000)
aqua_sf<- aqua_sf %>%
  #st_make_valid() %>%
  st_set_precision(1000000)

intersect_pct1 <- st_intersection(aqua_sf, parks2017) %>%
  mutate(intersect_area = st_area(.))

geoms <- sf::st_read("/home/lb/Downloads/trouble_geom.shp", quiet = TRUE)
geoms <- st_set_precision(geoms, 1000000)
geom_int <- st_snap(geoms, geoms, tolerance = 5) %>%
  st_intersection() %>%
  st_collection_extract("POLYGON")

parks2017 = readOGR(file.path(spatialOutDir,"aqua_sf.gpkg"), layer="aqua_sf")
parks2017 = gBuffer(parks2017, width=0, byid=TRUE)

#Instead use parks raster -
parks2017R[parks2017R<1] <- NA

Parkextract<- exact_extract(norm_cum_currSM3rcls, aqua_sf_in, 'count', progress=TRUE, force_df=TRUE)
saveRDS(Parkextract,file='tmp/Parkextract')
Connextract<-readRDS(file='tmp/Parkextract')
Parkextract_LUT<- Parkextract %>%
  mutate(aqua_id=as.numeric(rownames(Parkextract))) %>%
  dplyr::mutate(ConnectSM3Ha=count*100) %>%
  dplyr::select(aqua_id,ConnectSM3Ha)


###############
Coho_CU<-readRDS('tmp/Coho_CU') %>%
  st_buffer(dist=0)

#Wshd assessment unit centre points - for pulling District and Coho CUs into aqua_sf
aquaXY <- st_centroid(aqua_sf)
aquapt <- st_coordinates(aquaXY)
aquapt <- aquaXY %>%
  cbind(aquapt) %>%
  st_drop_geometry()

aquapt <- st_as_sf(aquapt, coords= c("X","Y"), crs = 3005)

aquapt <- aquapt %>%
  mutate(aqua_id=as.numeric(rownames(aquapt)))
write_sf(aquapt, file.path(spatialOutDir,"aquapt.gpkg"))

#get District names so can be merged into aqua_sf
tsa_pts <-st_intersection(aquapt, nr_districts)
saveRDS(tsa_pts,'tmp/tsa_pts')

tsa_pts<-readRDS('tmp/tsa_pts') %>%
  st_drop_geometry() %>%
  dplyr::select(ASSESSMENT_UNIT_SOURCE_ID,DISTRICT_NAME)
