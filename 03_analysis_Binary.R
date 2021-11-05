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

#Summarize landscape and ecosystem elements across the Province and various Plan Areas
ProposedPPA_sf<-readRDS('tmp/ProposedPPA_sf')
int_wet_belt<-readRDS('tmp/int_wet_belt')
caribou_rainforest_conservation_area<-readRDS('tmp/caribou_rainforest_conservation_area')
lup_combined<-readRDS('tmp/lup_combined')
#Make a list of the plan areas
strataL<-list(ProposedPPA_sf, lup_combined, int_wet_belt, caribou_rainforest_conservation_area)
PlanAreas<-c('ProposedPPA_sf','lup_combined','int_wet_belt_all','caribou_rainforest_conservation_area')

#Summarize ecosystem elements by landscape elements - from 02_clean_Bineary.R
Intact<-st_read(file.path(spatialOutDir,"Intact.gpkg"))
Corridor<-st_read(file.path(spatialOutDir,"Corridor.gpkg"))
IntactCorridor<-st_read(file.path(spatialOutDir,"IntactCorridor.gpkg"))
BECrep<-st_read(file.path(spatialOutDir,"BECrep.gpkg"))
BC<-st_read(file.path(spatialOutDir,"BC.gpkg"))
BEC_MacroR<-st_read(file.path(spatialOutDir,"BEC_MacroR.gpkg"))
#Make a list of the Landscape Elements
LandElementsN<-c('PlanArea','BEC_MacroR','BECrep','IntactCorridor','Intact','Corridor')
LandElements<-list(BC,BEC_MacroR, BECrep, IntactCorridor, Intact, Corridor)

#Make a raster stack of the ecosystem elements
OG<-raster(file.path(spatialOutDir,'OG_26r_atriskrPO.tif'))
CH<-raster(file.path(spatialOutDir,'CHR_atriskrPO.tif'))
KBA<-raster(file.path(spatialOutDir,'KBAr_atriskrPO.tif'))
OG_recruit<-raster(file.path(spatialOutDir,'OG_recruitr_atriskrPO.tif'))
wetlands<-raster(file.path(spatialOutDir,'wetlandsR_atriskrPO.tif'))
#Make a list of the Ecosystem Elements names, then make the raster stack
EcoElementsN<-c('OG','CH','KBA','OG_recruit','wetlands')
EcoStack1<-stack(OG, CH, KBA,OG_recruit,wetlands)
saveRDS(EcoStack1,file='tmp/EcoStack1')

#Clip the elements by each of the plan areas
#Note: Modify to use lapply and make a list of raster stacks
#!!!Warning this code block takes several hours to run!!!!
EcoStack2<-terra::mask(EcoStack1,strataL[[1]])
saveRDS(EcoStack2,file='tmp/EcoStack2')

EcoStack3<-terra::mask(EcoStack1,strataL[[2]])
saveRDS(EcoStack3,file='tmp/EcoStack3')

EcoStack4<-terra::mask(EcoStack1,strataL[[3]])
saveRDS(EcoStack4,file='tmp/EcoStack4')

EcoStack5<-terra::mask(EcoStack1,strataL[[4]])
saveRDS(EcoStack5,file='tmp/EcoStack5')

EcoStackL<-list(EcoStack1,EcoStack2,EcoStack3,EcoStack4,EcoStack5)
saveRDS(EcoStackL,'tmp/EcoStackL')

#use extract to summarize areas by zones ie Ecosystem Elements by Landscape Elements
#pass in a list index to perform the extract by plan area
ExtractFn <- function(i, EcoStack){
  extract_df <- exact_extract(EcoStack, LandElements[[i]], 'count', progress=TRUE, force_df=TRUE)
  colnames(extract_df)<-EcoElementsN
  EcoElsum<-lapply(extract_df, sum)
  EcoEl.df<-data.frame(LandElement=LandElementsN[i],EcoElsum)
  return(EcoEl.df)
}

#Go through each plan area and generate a data frame of values and percentages
#Need to run Province first to calculate the percentages
#Function to create percentages of the area summaries
PerCentFn <- function(i, Plandf) {
  EcoNameO<-paste0(EcoElementsN[i],'_pc')
  TotalColumn<-BCtotal %>%
    select(!!EcoElementsN[i])
  df<-Plandf %>%
    mutate(!!EcoNameO:= !!rlang::sym(EcoElementsN[i])/as.numeric(TotalColumn)*100) %>%
    dplyr::select(!!rlang::sym(EcoElementsN[i]),!!EcoNameO)
  return(df)
}

#First for the entire Province
LandscapeElementsL<-lapply(1:length(LandElementsN), function(j) ExtractFn(j, EcoStackL[[1]]))
#Make a total data frame to hold the provincial totals by using the BC row which holds the Provincial totals
Totaldf<-bind_rows(LandscapeElementsL, .id = 'Total')
BCtotal<-Totaldf %>%
  dplyr::filter(LandElement=='PlanArea')

#Loop through each row in table and calculate percentage - then transpose list to a single data frame
CalcPC<-lapply(1:length(EcoElementsN), function(j) PerCentFn(j, Totaldf))
Totaldfpc<-cbind(Totaldf$LandElement, data.frame(CalcPC))

#Second loop through plan areas by masking them according to the plan area
#Plan Areas does not include BC, where as the EcoStackL does - this the difference in the counters in the loop
for (i in 1:length(PlanAreas)) {
#for (i in 1:1) {
  LandscapeElementsL<-lapply(1:length(LandElementsN), function(j) ExtractFn(j, EcoStackL[[i+1]]))
  pldf<-bind_rows(LandscapeElementsL, .id = PlanAreas[[i]])
  Plandf<-paste0(PlanAreas[i],'df')
  assign(Plandf, pldf)

  CalcPC<-lapply(1:length(EcoElementsN), function(j) PerCentFn(j, pldf))
  pldfpc<-cbind(pldf$LandElement, data.frame(CalcPC))
  colnames(pldfpc)<-c("LandElement",colnames(pldfpc)[2:11])
  dfNamepc<-paste0(Plandf,'pc')
  assign(dfNamepc, pldfpc)
}


NA_BinarySummaryL<-list(Totaldfpc,ProposedPPA_sfdfpc,lup_combineddfpc,
                        int_wet_belt_alldfpc,caribou_rainforest_conservation_areadfpc)
SummaryNames<-c('Totaldfpc', 'ProposedPPA_sfdfpc','lup_combineddfpc',
                'int_wet_belt_alldfpc','caribou_rainforest_areadfpc')
WriteXLS(NA_BinarySummaryL,file.path(dataOutDir,paste('NA_BinarySummary.xlsx',sep='')),SheetNames=SummaryNames)


#caribou_rainforest_conservation_areadfpc$LandElement[caribou_rainforest_conservation_areadfpc$LandElement == "BC"] <- "PlanArea"

#Summarize by NR Region
#Rasters of Landscape Elements for Regional Summary
IntactR<-raster(file.path(spatialOutDir,'Intact_atriskrPO.tif'))
CorridorR<-raster(file.path(spatialOutDir,'corridorr_atriskrPO.tif'))
#IntactCorridor combination of Intact_atriskrPO and corridorr_atriskrPO with 02_clean_Binary.R
IntactCorridorR<-raster(file.path(spatialOutDir,'IntactCorridor.tif'))
BECrepR<-raster(file.path(spatialOutDir,'BECrepr_atriskrPO.tif'))
BEC_macroRR<-raster(file.path(spatialOutDir,'BEC_macroR_atriskrPO.tif'))

EcoStackNR<-stack(OG, CH, KBA,OG_recruit,wetlands,IntactR,CorridorR,IntactCorridorR,BECrepR,BEC_macroRR)
RegionEcoLandN<-c(EcoElementsN,c('IntactR','CorridorR','IntactCorridorR','BECrepR','BEC_MacroR'))

extract_df <- exact_extract(EcoStackNR, nr_regions, 'count', progress=TRUE, force_df=TRUE,
                            full_colnames=TRUE)
#extract_df1<-extract_df %>%
#  dplyr::select(all_of(RegionEcoLandN))
extract_df<-extract_df1
colnames(extract_df)<-RegionEcoLandN
extract_df$Region<-nr_regions$REGION_NAME
NR_by_EcoLand1<-extract_df %>%
  mutate(Region=nr_regions$REGION_NAME) %>%
  dplyr::select(Region,all_of(RegionEcoLandN))

#add a total row
NR_by_EcoLand2<-NR_by_EcoLand1 %>%
  summarise_at(RegionEcoLandN, 'sum') %>%
  mutate(Region='Total')
NR_by_EcoLand<-rbind(NR_by_EcoLand,NR_by_EcoLand1)

BCRtotal<-NR_by_EcoLand %>%
  dplyr::filter(Region=='Total')

#Calculate Percentages
PerCentFn <- function(i, Plandf) {
  EcoNameO<-paste0(RegionEcoLandN[i],'_pc')
  TotalColumn<-BCRtotal %>%
    select(!!RegionEcoLandN[i])
  df<-Plandf %>%
    mutate(!!EcoNameO:= !!rlang::sym(RegionEcoLandN[i])/as.numeric(TotalColumn)*100) %>%
    dplyr::select(!!rlang::sym(RegionEcoLandN[i]),!!EcoNameO)
  return(df)
}
CalcPC<-lapply(1:length(RegionEcoLandN), function(j) PerCentFn(j, NR_by_EcoLand))
NR_by_EcoLandpc<-cbind(NR_by_EcoLand$Region, data.frame(CalcPC))

WriteXLS(NR_by_EcoLandpc,file.path(dataOutDir,paste('NR_by_EcoLandpc.xlsx',sep='')),SheetNames='NR Regions')




###############
OG_atriskr<-raster(file.path(spatialOutDir,'OG_atriskr.tif'))
CH_atriskr<-raster(file.path(spatialOutDir,'CH_atriskr.tif'))
KBA_atriskr<-raster(file.path(spatialOutDir,'KBA_atriskr.tif'))
OG_recruit_atriskr<-raster(file.path(spatialOutDir,'OG_recruit_atriskr.tif'))

Intact_atriskr<-raster(file.path(spatialOutDir,'Intact_atriskr.tif'))
Corridor_atriskr<-raster(file.path(spatialOutDir,'Corridor_atriskr.tif'))
BECrep_atriskr<-raster(file.path(spatialOutDir,'BECrep_atriskr.tif'))

BinaryStack<-stack(OG_atriskr,CH_atriskr,Intact_atriskr,Corridor_atriskr,BECrep_atriskr,KBA_atriskr)

Heat2<-sum(BinaryStack, na.rm=TRUE)
writeRaster(Heat2, filename=file.path(spatialOutDir,'Heat2'), format="GTiff", overwrite=TRUE)


