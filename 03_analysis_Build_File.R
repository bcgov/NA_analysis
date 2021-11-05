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

BCR<-raster(file.path(spatialOutDir,'BCr_atriskrPO.tif'))
IntactR<-raster(file.path(spatialOutDir,'Intact_atriskrPO.tif'))
CorridorR<-raster(file.path(spatialOutDir,'corridorr_atriskrPO.tif'))
#IntactCorridor combination of Intact_atriskrPO and corridorr_atriskrPO with 02_clean_Binary.R
IntactCorridorR<-raster(file.path(spatialOutDir,'IntactCorridor.tif'))
BECrepR<-raster(file.path(spatialOutDir,'BECrepr_atriskrPO.tif'))
BEC_macroRR<-raster(file.path(spatialOutDir,'BEC_macroR_atriskrPO.tif'))
#Make a list of the Landscape Elements
LandElementsN<-c('UnProtectedArea','BEC_MacroR','BECrep','IntactCorridor','Intact','Corridor')
LandElements<-list(BCR,BEC_macroRR, BECrepR, IntactCorridorR, IntactR, CorridorR)

#Make a raster stack of the ecosystem elements
OG<-raster(file.path(spatialOutDir,'OG_26r_atriskrPO.tif'))
CH<-raster(file.path(spatialOutDir,'CHR_atriskrPO.tif'))
KBA<-raster(file.path(spatialOutDir,'KBAr_atriskrPO.tif'))
OG_recruit<-raster(file.path(spatialOutDir,'OG_recruitr_atriskrPO.tif'))
wetlands<-raster(file.path(spatialOutDir,'wetlandsR_atriskrPO.tif'))
#Make a list of the Ecosystem Elements names, then make the raster stack
EcoElementsN<-c('OG','CH','KBA','OG_recruit','wetlands')
EcoElements<-list(OG, CH, KBA,OG_recruit,wetlands)

EcoLandElementsN<-c(LandElementsN,EcoElementsN)
EcoLandStack1<-stack(BCR,BEC_macroRR, BECrepR, IntactCorridorR, IntactR, CorridorR,OG, CH, KBA,OG_recruit,wetlands)
saveRDS(EcoLandStack1,file='tmp/EcoLandStack1')


#num_bins<-length(Conservation_Class)
# Requires layers - bec_sf, waterpt
aqua_sf<-readRDS(file = 'tmp/AOI/aqua_sf_in')
aqua_sfE <- aqua_sf %>%
  mutate(aqua_id=as.numeric(rownames(aqua_sf))) %>%
  mutate(CEIntact_Ha=ASSESSMENT_UNIT_AREA_HA-DISTRB_HUMAN_HIST_HA)

ExtractFn <- function(strata, EcoStack){
  extract_df <- exact_extract(EcoStack, strata, 'count', progress=TRUE, force_df=TRUE)
  colnames(extract_df)<-paste0(EcoLandElementsN,'_ha')
  LUT<- extract_df %>%
    mutate(aqua_id=as.numeric(rownames(extract_df)))
  #EcoElsum<-lapply(extract_df, sum)
  #EcoEl.df<-data.frame(LandElement=LandElementsN[i],EcoElsum)
  #return(EcoEl.df)
  return(LUT)
}

LandEco_LUT<-ExtractFn(aqua_sfE, EcoLandStack1)

#Join strata and select criteria attributes data back to watersheds
wshd_1<-aqua_sfE %>%
  #st_drop_geometry() %>%
  left_join(LandEco_LUT, by='aqua_id')

  #dplyr::select(aqua_id, paste0(EcoLandElementsN,'_ha'))

#Set NA to 0
wshd_1[is.na(wshd_1)] <- 0
saveRDS(wshd_1,file='tmp/wshd_1')

write_sf(wshd_1, file.path(spatialOutDir,"wshd_1.gpkg"))
write_sf(aqua_sfE, file.path(spatialOutDir,"aqua_sfE.gpkg"))

