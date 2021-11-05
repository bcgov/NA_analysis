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

num_bins<-length(Conservation_Class)
EcoElementsN<-c('OG','CH','KBA','OG_recruit','wetlands')
LandElements<-list(BCR,BEC_macroRR, BECrepR, IntactCorridorR, IntactR, CorridorR)
EcoLandElementsN<-c(LandElementsN,EcoElementsN)

# Requires layers - wshd_1
#first bin the CE intact variable
wshd_2<-readRDS(file = 'tmp/wshd_1') %>%
mutate(Intactclass=findInterval(CEIntact_Ha, unique(quantile(CEIntact_Ha,
                                                             prob = seq(0, 1, 1/num_bins)))))
#clamp the num_bins to num_bins
wshd_2$Intactclass[(wshd_2$Intactclass>num_bins)] <- num_bins

#Class each of the Landscape and Ecosystem elements
#calculate conservation class based on normalizing data and 5 bins
classFn <- function(df, i) {
  EcoNameO<-paste0(EcoLandElementsNha[i],'class')
  classN<-paste0(EcoNameO,'N')
  df1<-df %>%
   mutate(!!EcoNameO:=findInterval(!!rlang::sym(EcoLandElementsNha[i]),
                      unique(quantile(!!rlang::sym(EcoLandElementsNha[i]),prob = seq(0, 1, 1/num_bins))))) %>%
 left_join(CClass_LUT, by=setNames(nm=EcoNameO,'C_Value')) %>%
    dplyr::rename(!!classN:=C_Class) %>%
    dplyr::select(aqua_id, !!EcoNameO,  !!classN)
  return(df1)
}

#set Names for function call
EcoLandElementsN<-c(LandElementsN,EcoElementsN)
EcoLandElementsNha<-paste0(EcoLandElementsN,'_ha')
EcoLandElementsNhaclass<-paste0(EcoLandElementsN,'_haclass')

#Call function and bind lists as a data frame
Class_LUTL<-lapply(1:length(EcoLandElementsN), function(j) classFn(wshd_2,j))
Class_LUT<-cbind(data.frame(Class_LUTL)) %>%
  dplyr::select(!contains('.'))

#Join strata and select criteria attributes data back to watersheds
wshd_3<-wshd_2 %>%
  #st_drop_geometry() %>%
  left_join(Class_LUT, by='aqua_id')

#Set NA to 0
#wshd_3[is.na(wshd_3)] <- 0
saveRDS(wshd_3,file='tmp/wshd_3')
write_sf(wshd_3, file.path(spatialOutDir,"wshd_3.gpkg"))
