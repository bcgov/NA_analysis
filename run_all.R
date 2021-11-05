# Copyright 2018 Province of British Columbia
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

source('header.R')
source('packages.R')

#Most analysis routines load what they need
#load clean were used to preprocess
#source('01_load_all.R')
#source('02_clean.R')

#Make high and low elevation BEC rasters if needed
source('02_clean_BEC_group.R')

#Clean files for use with binary analysis
source('02_clean_Binary.R')

#Conservation categories
Conservation_Class <- c('Negligible','Low','Medium','High','VHigh')
CClass_LUT<-data.frame(C_Class = Conservation_Class <- c('Negligible','Low','Medium','High','VHigh'),
                       C_Value = c(1,2,3,4,5))

#Identify PPA to use
#Combine PPA and OECM
PPAIin<-PPAIr*OECMIr
writeRaster(PPAIin,filename=file.path(spatialOutDir,'PPAIin'),format="GTiff", overwrite=TRUE)
#suffix for file run
PPAsuffix<-'PO'

#Create a stack of at risk layers and sum
#This prepares the data for the planning area summary and for the Build watershed file code
source('03_analysis_Binary.R')

#Build watershed based file - get areas of each element from the 03_analysis_Binary.R run
#within watershed assessment units
source('03_analysis_Build_File.R')

#Calculate Bins for each element folded into Watershed file - generic at this point
source('03_analysis_Set_ElementClass.R')

#Ranking strategies for elements by watersheds
#Lots of exploratory code in this script
source('03_analysis_Set_Rank.R')
