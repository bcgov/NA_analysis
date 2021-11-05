# Copyright 2020 Province of British Columbia
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

#Make sf of landscape elements for summarizing ecosystem elements
BCS<-read_stars(file.path(spatialOutDir,'BCr_atriskrPO.tif'))
BC <- st_as_sf(BCS, as_points = FALSE, merge = TRUE)
st_crs(BC)<-3005
BC<-BC %>%
  st_buffer(dist=0)
write_sf(BC, file.path(spatialOutDir,"BC.gpkg"))

IntactS<-read_stars(file.path(spatialOutDir,'Intact_atriskrPO.tif'))
Intact <- st_as_sf(IntactS, as_points = FALSE, merge = TRUE)
st_crs(Intact)<-3005
Intact<-Intact %>%
  st_buffer(dist=0)
write_sf(Intact, file.path(spatialOutDir,"Intact.gpkg"))
#Intact<-st_read(file.path(spatialOutDir,"Intact.gpkg"))
CorridorS<-read_stars(file.path(spatialOutDir,'corridorr_atriskrPO.tif'))
Corridor <- st_as_sf(CorridorS, as_points = FALSE, merge = TRUE)
st_crs(Corridor)<-3005
Corridor<-Corridor %>%
  st_buffer(dist=0)
write_sf(Corridor, file.path(spatialOutDir,"Corridor.gpkg"))

#BECrep
BECrepS<-read_stars(file.path(spatialOutDir,'BECrepr_atriskrPO.tif'))
BECrep <- st_as_sf(BECrepS, as_points = FALSE, merge = TRUE)
write_sf(BECrep, file.path(spatialOutDir,"BECrep.gpkg"))

#Macro Refugia
BEC_MacroRS<-read_stars(file.path(spatialOutDir,'BEC_macroR_atriskrPO.tif'))
BEC_MacroR <- st_as_sf(BEC_MacroRS, as_points = FALSE, merge = TRUE)
st_crs(BEC_MacroR)<-3005
BEC_MacroR<-BEC_MacroR %>%
  st_buffer(dist=0) %>%
  dplyr::filter(BEC_macroR_atriskrPO.tif==1)
write_sf(BEC_MacroR, file.path(spatialOutDir,"BEC_MacroR.gpkg"))


#IntactCorridor<-Intact %>%
#  st_union(Corridor) #fails

#rgeos::gUnion method - works but takes for ever...
#Corridor.sp<-as_Spatial(Corridor)
#Intact.sp<-as_Spatial(Intact)
#system.time(IntactCorridor2 <- st_as_sf(rgeos::gUnion(Intact.sp,Corridor.sp,id='id')))
#write_sf(IntactCorridor, file.path(spatialOutDir,"IntactCorridor.gpkg"))

#Raster method - fast
IntactCorridor<-raster(file.path(spatialOutDir,'Intact_atriskrPO.tif'))
CorridorR<-raster(file.path(spatialOutDir,'corridorr_atriskrPO.tif'))

IntactCorridor[CorridorR==1]<-1
writeRaster(IntactCorridor, filename=file.path(spatialOutDir,"IntactCorridor.tif"), format="GTiff", overwrite=TRUE)

IntactCorridorS<-read_stars(file.path(spatialOutDir,'IntactCorridor.tif'))
IntactCorridor.sf <- st_as_sf(IntactCorridorS, as_points = FALSE, merge = TRUE)
st_crs(IntactCorridor.sf)<-3005

IntactCorridor.sf<-IntactCorridor.sf %>%
  st_buffer(dist=0)
write_sf(IntactCorridor.sf, file.path(spatialOutDir,"IntactCorridor.gpkg"))

