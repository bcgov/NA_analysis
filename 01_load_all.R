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

bc <- bcmaps::bc_bound()
Prov_crs<-crs(bc)
Prov_crs_Stars<-st_crs(bc)
#Prov_crs<-"+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#Provincial Raster to place rasters in the same reference
BCr_file <- file.path(spatialOutDir,"BCr.tif")
if (!file.exists(BCr_file)) {
  BC<-bcmaps::bc_bound_hres(class='sf')
  st_crs(BC)<-Prov_crs_Stars
  saveRDS(BC,file='tmp/BC')
  ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                   ymn=173787.5, ymx=1748187.5,
                   crs=Prov_crs,
                   res = c(100,100), vals = 1)
  ProvRast_S<-st_as_stars(ProvRast)
  write_stars(ProvRast_S,dsn=file.path(spatialOutDir,'ProvRast_S.tif'))
  BCr <- fasterize(BC,ProvRast)
  BCr_S <-st_as_stars(BCr)
  write_stars(BCr_S,dsn=file.path(spatialOutDir,'BCr_S.tif'))
  writeRaster(BCr, filename=BCr_file, format="GTiff", overwrite=TRUE)
  writeRaster(ProvRast, filename=file.path(spatialOutDir,'ProvRast'), format="GTiff", overwrite=TRUE)
} else {
  BCr <- raster(BCr_file)
  ProvRast<-raster(file.path(spatialOutDir,'ProvRast.tif'))
  BCr_S <- read_stars(file.path(spatialOutDir,'BCr_S.tif'))
  BC <-readRDS('tmp/BC')
}

#Read in provincial aquatic CE data and rasterize ECA, Total and Human disturbance
aqua_file <- file.path("tmp/aqua_sf_in")
if (!file.exists(aqua_file)) {
  aqua_gdb <- list.files(file.path(NALibrary, "AquaticCE"), pattern = ".gdb", full.names = TRUE)[1]
  aqua_list <- st_layers(aqua_gdb)
  aqua_sf_in <- read_sf(aqua_gdb, layer = "CEF_AQUATICS_ASSESSMT_POLY_2018_20201015") %>%
    st_buffer(dist=0) %>%
    mutate(aqua_id=as.numeric(rownames(aqua_sf_in))) %>%
    dplyr::select(aqua_id, Shape_Area, LOCAL_WATERSHED_CODE, ASSESSMENT_UNIT_GROUP,
                  WATERSHED_ORDER,WATERSHED_MAGNITUDE, FRESHWATER_ECOREGION,
                  ASSESSMENT_UNIT_AREA_HA, ASSESSMENT_UNIT_AREA_KM2, ASSESSMENT_UNIT_SOURCE_ID, REPORTING_UNIT_MAX,
                  FOWN_PROTECTED_HA,
                  GLACIERS_AND_SNOW_HA,GLACIERS_AND_SNOW_PCNT,ALPINE_NONFOREST_HA, ALPINE_NONFOREST_PCNT, ALPINE_SUBALPINE_BARREN_HA,
                  FOREST_LAND_HA, FOREST_LAND_PCNT,
                  DISTRB_HUMAN_CRNT_20YR_HA, DISTRB_HUMAN_HIST_HA, NATURAL_LANDBASE_HA)

  saveRDS(aqua_sf_in,file=aqua_file)
  saveRDS(aqua_sf_in,file='tmp/AOI/aqua_sf_in')

  Tot_Disturb <-fasterize(aqua_sf, ProvRast, field="DISTRB_ALL_TOT_PCNT")
  saveRDS(Tot_Disturb,file='tmp/Tot_Disturb')
  saveRDS(Tot_Disturb,file='tmp/AOI/Tot_Disturb')
  Human_Disturb <-fasterize(aqua_sf, ProvRast, field="NATURAL_LANDBASE_PCNT")
  saveRDS(Human_Disturb,file='tmp/AOI/Human_Disturb')
} else {
  Tot_Disturb<-readRDS(file='tmp/Tot_Disturb')
  Human_Disturb<-readRDS(file='tmp/Human_Disturb')
  aqua_sf_in<-readRDS(aqua_file)
  #write_sf(aqua_sf_in, file.path(spatialOutDir,"aqua_sf_in.gpkg"))
}

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
saveRDS(aquapt,file='tmp/aquapt')
saveRDS(aquapt,file='tmp/AOI/aquapt')
aquapt<-read_sf(file.path(spatialOutDir,"aquapt.gpkg"))

#Updated BEC
#BECin<-bcdc_get_data("WHSE_FOREST_VEGETATION.BEC_BIOGEOCLIMATIC_POLY")
BECin<-readRDS(file='tmp/BEC')

BEC_LUT<-read_csv(file.path(SpatialDir,'v_datadeliver_tiff_bec.csv')) %>%
  dplyr::rename(BGC_LABEL = becsubzone)

BEC<- BECin %>%
  left_join(BEC_LUT) %>%
  mutate(NDTn=as.integer(substr(NATURAL_DISTURBANCE,4,4)))
saveRDS(BEC,file='tmp/BEC')
saveRDS(BEC,file='tmp/AOI/BEC')
BEC<-readRDS(file='tmp/BEC')
write_sf(BEC, file.path(spatialOutDir,"BEC.gpkg"))
BEC<-read_sf(file.path(spatialOutDir,"BEC.gpkg"))

ws <- get_layer("wsc_drainages", class = "sf") %>%
  dplyr::select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
  dplyr::filter(SUB_DRAINAGE_AREA_NAME %in% c("Nechako", "Skeena - Coast"))
st_crs(ws)<-3005
saveRDS(ws, file = "tmp/ws")
write_sf(ws, file.path(spatialOutDir,"ws.gpkg"))
ws<-readRDS(file = "tmp/ws")

#Parks, OECM, Unprotected
parks_file <- file.path(spatialOutDir,"PPAr.tif")
if (!file.exists(parks_file)) {
  PPA<-st_read(file.path(NALibrary,'Protected/ppa.gpkg')) %>%
    mutate(RastVal=1)
  saveRDS(PPA,file='tmp/PPA')
  saveRDS(PPA,file='tmp/AOI/PPA')
  PPA<-readRDS(file='tmp/PPA')
  PPAr<-PPA %>%
    fasterize(ProvRast,field="RastVal")
  writeRaster(PPAr, filename=file.path(spatialOutDir,"PPAr.tif"), format="GTiff", overwrite=TRUE)
  saveRDS(PPAr,file='tmp/PPAr')
  saveRDS(PPAr,file='tmp/AOI/PPAr')
  PPAr<-readRDS(file='tmp/AOI/PPAr')

  #Make Inverse PPA
  PPAIr<-PPA %>%
    mutate(rastVal=ifelse(RastVal==1,2,NA)) %>% #Change...
    fasterize(ProvRast, field="rastVal",background=1)#Change..
  PPAIr[PPAIr==2]<-NA
  writeRaster(PPAIr, filename=file.path(spatialOutDir,"PPAIr.tif"), format="GTiff", overwrite=TRUE)
  PPAIr<-raster(file.path(spatialOutDir,"PPAIr.tif"))

  PPAIrr <- PPAIr %>%
    mask(BC) %>%
    crop(BC)

  writeRaster(PPAIr1, filename=file.path(spatialOutDir,'PPAIr'), format="GTiff", overwrite=TRUE)

  ## OECM
  OECM<-st_read(file.path(NALibrary,'Protected/oecm.gpkg'))  %>%
    mutate(RastVal=1)
  saveRDS(OECM,file='tmp/OECM')
  saveRDS(OECM,file='tmp/AOI/OECM')
  OECM<-readRDS(file='tmp/OECM')
  OECMr<-OECM %>%
    fasterize(ProvRast,field="RastVal")
  writeRaster(OECMr, filename=file.path(spatialOutDir,"OECMr.tif"), format="GTiff", overwrite=TRUE)
  saveRDS(OECMr,file='tmp/OECMr')
  saveRDS(OECMr,file='tmp/AOI/OECMr')
  OECMr<-readRDS(file='tmp/AOI/OECMr')

  #Make Inverse PPA
  OECMIr<-OECM %>%
    mutate(rastVal=ifelse(RastVal==1,2,NA)) %>%
    fasterize(ProvRast, field="rastVal",background=1)
  OECMIr[OECMIr==2]<-NA
  writeRaster(OECMIr, filename=file.path(spatialOutDir,"OECMIr.tif"), format="GTiff", overwrite=TRUE)
  OECMIr<-raster(file.path(spatialOutDir,"OECMIr.tif"))

 OECMIr1 <- OECMIr %>%
    mask(BC) %>%
    crop(BC)

 writeRaster(OECMIr1, filename=file.path(spatialOutDir,'OECMIr'), format="GTiff", overwrite=TRUE)

Unprotected<-st_read(file.path(NALibrary,'Protected/unprotected.gpkg'))

#Proposed Protection
ProposedPPA_gdb <- file.path(NALibrary, "ProposedPA/CONFIDENTIAL_Draft_Master_RALCP_Do_Not_Share.gdb")
ProposedPPA_list <- st_layers(ProposedPPA_gdb)
ProposedPPA_sf <- read_sf(ProposedPPA_gdb, layer = "RALCP_Projects") %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(RastVal=1)

saveRDS(ProposedPPA_sf,file='tmp/ProposedPPA_sf')
ProposedPPA_sf<-readRDS(file='tmp/ProposedPPA_sf')
ProposedPPA<-ProposedPPA_sf %>%
  fasterize(ProvRast,field="RastVal")

writeRaster(ProposedPPA, filename=file.path(spatialOutDir,"ProposedPPA.tif"), format="GTiff", overwrite=TRUE)
saveRDS(ProposedPPA,file='tmp/ProposedPPA')
ProposedPPA<-readRDS(file='tmp/ProposedPPAr')
write_sf(ProposedPPA_sf, file.path(spatialOutDir,"ProposedPPA_sf.gpkg"), overwrite=TRUE)

#Make Inverse PPA
ProposedPPAI<-ProposedPPA_sf %>%
  mutate(rastVal=ifelse(RastVal==1,2,NA)) %>%
  fasterize(ProvRast, field="rastVal",background=1)
ProposedPPAI[ProposedPPAI==2]<-NA
writeRaster(ProposedPPAI, filename=file.path(spatialOutDir,"ProposedPPAI.tif"), format="GTiff", overwrite=TRUE)
ProposedPPAI<-raster(file.path(spatialOutDir,"ProposedPPAI.tif"))

ProposedPPAI1 <- ProposedPPAI %>%
  mask(BC) %>%
  crop(BC)

writeRaster(ProposedPPAI1, filename=file.path(spatialOutDir,'ProposedPPAI'), format="GTiff", overwrite=TRUE)

} else {
  PPAr<-raster(file.path(spatialOutDir,"PPAr.tif"), format="GTiff")
  PPA<-readRDS(file='tmp/PPA')
 PPAIr<-raster(file.path(spatialOutDir,'PPAIr.tif'))
 OECMIr<-raster(file.path(spatialOutDir,"OECMIr.tif"))
 OECMr<-readRDS(file='tmp/AOI/OECMr')
 ProposedPPA<-raster(file.path(spatialOutDir,'ProposedPPA.tif'))
 ProposedPPAI<-raster(file.path(spatialOutDir,'ProposedPPAI.tif'))

}



#Critical Habitat
CH_file <- file.path("tmp/CH_sf")
if (!file.exists(CH_file)) {
  CH_gdb <- file.path(SpatialDir, "Protected/CriticalHabitat.gdb")
  CH_list <- st_layers(CH_gdb)
  CH_sf <- read_sf(CH_gdb, layer = "critical_flat") %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(OVerlapCat=cut(Join_Count, breaks=c(-Inf, 1, 2, 3, 4, 5, 10, 20, 30, Inf))) %>%
    mutate(CH=case_when(Join_Count > 0 ~ 1, TRUE ~ NA_real_))
  saveRDS(CH_sf,file=CH_file)
  write_sf(CH_sf, file.path(spatialOutDir,"CH_sf.gpkg"), overwrite=TRUE)

  CHR<-CH_sf %>%
    st_cast("MULTIPOLYGON") %>%
    # st_buffer(dist=0) %>%
    # st_transform(3005)
    fasterize(ProvRast,field="CH")
  writeRaster(CHR, filename=file.path(spatialOutDir,"CHR"), format="GTiff", overwrite=TRUE)

} else {
  CH_sf<-readRDS(file=CH_file)
  CHR<-raster(file.path(spatialOutDir,"CHR.tif"), format="GTiff")
}

#Forest Districts
nr_districts <- bcmaps::nr_districts()
write_sf(nr_districts, file.path(spatialOutDir,"nr_districts.gpkg"))

#Intact Raster Layer
Intact<-raster(file.path(HFLibrary,'IntactBinary.tif'))
saveRDS(Intact,file='tmp/Intact')
saveRDS(Intact,file='tmp/AOI/Intact')
writeRaster(Intact, filename=file.path(spatialOutDir,'Intact'), format="GTiff", overwrite=TRUE)

#Connectivity - read in clean corridor and re-rasterize and set 0 to NA
corridor<-read_sf(file.path(NALibrary,"Corridor/corridor.gpkg"))
corridorr<-fasterize(corridor, ProvRast, field="intact")
corridorr[corridorr == 0] <- NA
writeRaster(corridorr, filename=file.path(spatialOutDir,'corridorr'), format="GTiff", overwrite=TRUE)

#BEC representation
BECrep<-read_sf(file.path(NALibrary,"BEC/bec_scenario_17.gpkg")) %>%
  mutate(RastVal=1)

BECrepr<-fasterize(BECrep, ProvRast, field="RastVal")
BECrepr[BECrepr == 0] <- NA
writeRaster(BECrepr, filename=file.path(spatialOutDir,'BECrepr'), format="GTiff", overwrite=TRUE)

#KBA EPSG10208
KBA<-read_sf(file.path(NALibrary,"KBA/draft_kbas_reproj.gpkg")) %>%
  mutate(RastVal=1)

KBAr<-fasterize(KBA, ProvRast, field="RastVal")
writeRaster(KBAr, filename=file.path(spatialOutDir,'KBAr'), format="GTiff", overwrite=TRUE)

#Updated OG from Panel
#OG
OG_5<-read_sf(file.path(NALibrary,"OG/og_5-mill-ha.gpkg")) %>%
  mutate(rastVal=ifelse(value>0,1,0))
OG_5r<-fasterize(OG_5, ProvRast, field="rastVal")
writeRaster(OG_5r, filename=file.path(spatialOutDir,'OG_5r'), format="GTiff", overwrite=TRUE)

OG_26<-read_sf(file.path(NALibrary,"OG/og_2.6-mill-ha_oct25.gpkg")) %>%
  mutate(rastVal=ifelse(value>0,1,0))
OG_26r<-fasterize(OG_26, ProvRast, field="rastVal")
writeRaster(OG_26r, filename=file.path(spatialOutDir,'OG_26r'), format="GTiff", overwrite=TRUE)

OG.sf<-read_sf(file.path(NALibrary,"OG/TechPanel_OGMapP1_2021_10_05.gpkg"))
table(OG.sf$value,OG.sf$label)

#Rasterize OG background is NA
OG.sf<-OG.sf %>%
  mutate(rastVal=ifelse(value>0,1,0))
OGNA<-fasterize(OG.sf, ProvRast, field="rastVal")
writeRaster(OGNA, filename=file.path(spatialOutDir,'OGNA'), format="GTiff", overwrite=TRUE)
#OG.value<-fasterize(OG.sf, ProvRast, field="value")

#Recruitment layer from OG Panel
OG_recruit<-read_sf(file.path(NALibrary,"OG/Recruitment_2021_08_07.shp"))
table(OG_recruit$value,OG_recruit$label)
OG_recruitr<-fasterize(OG_recruit, ProvRast, field="value")
writeRaster(OG_recruitr, filename=file.path(spatialOutDir,'OG_recruitr'), format="GTiff", overwrite=TRUE)

#Interior Wet Belt
int_wet_belt<-read_sf(file.path(NALibrary,"Boundaries/int_wet_belt/int_wet_belt_all.gpkg"))
saveRDS(int_wet_belt,file='tmp/int_wet_belt')

#Harmony
caribou_rainforest_conservation_area<-read_sf(file.path(NALibrary,"Boundaries/caribou_rainforest_conservation_area.gpkg"))
saveRDS(caribou_rainforest_conservation_area,file='tmp/caribou_rainforest_conservation_area')

#LUP areas
lup_combined<-read_sf(file.path(NALibrary,"ProposedPA/lup_combined.gpkg")) %>%
  st_zm(drop=TRUE, what="ZM")
saveRDS(lup_combined,file='tmp/lup_combined')

#Regions
nr_regions <- bcmaps::nr_regions()
write_sf(nr_regions, file.path(spatialOutDir,"nr_regions.gpkg"))
nr_regions<-st_read(file.path(spatialOutDir,"nr_regions.gpkg"))

#Wetlands - select >=5ha
wetlands.sf<-read_sf(file.path(NALibrary,"Wetlands/fa_wetlands_full.gpkg")) %>%
  dplyr::filter(AREA_HA>=5) %>%
  #dplyr::select(WTRBDPLD, AREA_HA) %>%
  mutate(rastVal=1)
wetlandsR<-fasterize(wetlands, ProvRast, field="rastVal")
writeRaster(wetlandsR, filename=file.path(spatialOutDir,'wetlandsR'), format="GTiff", overwrite=TRUE)
write_sf(wetlands.sf, file.path(spatialOutDir,"wetlands.sf.gpkg"))

#Macro Refugia
BEC_macroR<-raster(file.path(RefugiaLibrary,"BECOverz.tif"))
BEC_macroR[BEC_macroR!=1]<-NA
writeRaster(BEC_macroR, filename=file.path(spatialOutDir,'BEC_macroR'), format="GTiff", overwrite=TRUE)


