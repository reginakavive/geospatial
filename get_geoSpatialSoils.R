
#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("terra", "sf", "rgl", "rgdal", "sp", "geodata", "tidyverse", "geosphere", "countrycode")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))



#################################################################################################################
## functions to read from "Global_GeoData/Landing/", crop and write the result in "useCaseName/Crop/raw"
#################################################################################################################
#' iSDA layers are aggregated at 1km res and are obtained using geodata; for info on variables and units refer to
#' https://rdrr.io/github/rspatial/geodata/man/soil_af_isda.html
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/soil/iSDA
#'
#' @examples get_geoSpatial_soiliSDA(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)
get_geoSpatial_soiliSDA <- function(country, useCaseName, Crop, overwrite){
  
  #TODO create a look up table to check use case - country names
  ## get country abbreviation to used in gdam function
  countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## create a directory to store the cropped data: 
  pathOut <- paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/raw/Soil/iSDA", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(countryCC, level = 3, path='.')
  
  ## read soil layers and crop
  listRaster_iSDA <-list.files(path="./EiA_DA_AW_V1/AgWise_Data/data_sourcing/Global_GeoData/Landing/Soil/iSDA", pattern=".tif$")
  
  readLayers_iSDA <- terra::rast(paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/Global_GeoData/Landing/Soil/iSDA", listRaster_iSDA, sep="/"))
  
  croppedLayer_iSDA <- terra::crop(readLayers_iSDA, countryShp)
  
  ## save result
  terra::writeRaster(croppedLayer_iSDA, paste0(pathOut, "/iSDA_geospatial_soils.tif", sep=""), filetype="GTiff", overwrite = overwrite)
  
  return(croppedLayer_iSDA)
}





#################################################################################################################
## functions to read from "Global_GeoData/Landing/", crop and write the result in "useCaseName/Crop/raw"
#################################################################################################################
#' soilGrids layers are aggregated at 1km res and are obtained from geodata; for info on variables and units refer to
#' https://rdrr.io/github/rspatial/geodata/man/soil_af_elements.html
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param overwrite default is FALSE 
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/raw/Soil/soilGrids
#'
#' @examples get_geoSpatial_soilGrids(country = "Rwanda", useCaseName = "Rwanda_RAB", Crop = "Potato", overwrite = TRUE)
get_geoSpatial_soilGrids <- function(country, useCaseName, Crop, overwrite){
  
  ## get country abbreviation to used in gdam function
  countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## create a directory to store the cropped data: 
  pathOut <- paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/raw/Soil/soilGrids", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(countryCC, level = 3, path='.')
  
  ## read soil layers and crop
  listRaster_soilGrids <-list.files(path="./EiA_DA_AW_V1/AgWise_Data/data_sourcing/Global_GeoData/Landing/Soil/soilGrids", pattern=".tif$")
  
  readLayers_soilGrids <- terra::rast(paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/Global_GeoData/Landing/Soil/soilGrids", listRaster_soilGrids, sep="/"))
  
  croppedLayer_soilGrids <- terra::crop(readLayers_soilGrids, countryShp)
  
  ## save result
  terra::writeRaster(croppedLayer_soilGrids, paste0(pathOut, "/soilGrids_geospatial_soils.tif", sep=""), filetype="GTiff", overwrite = overwrite)
  
  return(croppedLayer_soilGrids)
}






#################################################################################################################
## functions to read from "useCaseName/Crop/raw" and do data processing/derived variables etc and write the result in "UseCase/Crop/transform"
#################################################################################################################

#' @description function to transform soil data and generate derived variables
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param resFactor is an aggregation factor to change the resolution of the layers, soil data in global are at 1km res
#' @param overwrite default is FALSE 
#' @param pathOut path to save the result: TODO When the data architect (DA) is implemented pathOut = "usecaseName/crop/transform/soil"
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/transform/Soil/soilGrids
#'
#' @examples soil_iSDA_transform(useCaseName = "Rwanda_RAB", Crop = "Potato", resFactor=1, overwrite = TRUE)
soil_iSDA_transform <- function(useCaseName, Crop, resFactor=1, overwrite = FALSE){
  
  ## create a directory to store the transformed data: with DA this will be in "usecaseName/crop/transform"
  
  pathOut <- paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/transform/Soil/iSDA", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  pathIn <- paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/",Crop, "/raw/Soil/iSDA", sep="")
  
  ## read, crop and save 
  listRaster_iSDA <-list.files(path=pathIn, pattern=".tif$")
  cropped4Country <- terra::rast(paste(pathIn, "/",listRaster_iSDA, sep=""))
  
  ## get soil organic matter as a function of organic carbon
  cropped4Country$`SOM_0-20cm` <- (cropped4Country$`oc_0-20cm` * 2)/10
  cropped4Country$`SOM_20-50cm` <- (cropped4Country$`oc_20-50cm` * 2)/10
  
  ## Aggregation iSDA at ~1km resolution
  if(resFactor > 1){
    transformedLayer <- aggregate(cropped4Country, fun=mean, fact=resFactor)
    
    transformedLayer$`texture.class_0-20cm` <- round(transformedLayer$`texture.class_0-20cm`, digits=0)
    textop <- levels(cropped4Country$`texture.class_0-20cm`)[[1]]
    textop <- textop[textop$value %in% unique(transformedLayer$`texture.class_0-20cm`)[,1], ]
    levels(transformedLayer$`texture.class_0-20cm`)[[1]] <- textop
    
    transformedLayer$`texture.class_20-50cm` <- round(transformedLayer$`texture.class_20-50cm`, digits=0)
    texbottom <- levels(cropped4Country$`texture.class_20-50cm`)[[1]]
    texbottom <- texbottom[texbottom$value %in% unique(transformedLayer$`texture.class_20-50cm`)[,1], ]
    levels(transformedLayer$`texture.class_20-50cm`)[[1]] <- texbottom
    
  }else{
    transformedLayer <- cropped4Country
  }
  
  
  ##### permanent wilting point ####
  transformedLayer$'PWP_0-20cm' <- (-0.024 * transformedLayer$`sand.tot.psa_0-20cm`/100) + 0.487 *
    transformedLayer$`clay.tot.psa_0-20cm`/100 + 0.006 * transformedLayer$`SOM_0-20cm` + 
    0.005*(transformedLayer$`sand.tot.psa_0-20cm`/100 * transformedLayer$`SOM_0-20cm`) - 
    0.013*(transformedLayer$`clay.tot.psa_0-20cm`/100 * transformedLayer$`SOM_0-20cm`) + 
    0.068*(transformedLayer$`sand.tot.psa_0-20cm`/100 * transformedLayer$`clay.tot.psa_0-20cm`/100 ) + 0.031
  transformedLayer$'PWP_0-20cm' <- (transformedLayer$'PWP_0-20cm' + (0.14 * transformedLayer$'PWP_0-20cm' - 0.02))*100
  
  
  
  transformedLayer$'PWP_20-50cm' <- (-0.024 * transformedLayer$`sand.tot.psa_20-50cm`/100) + 0.487 *
    transformedLayer$`clay.tot.psa_20-50cm`/100 + 0.006 * transformedLayer$`SOM_20-50cm` + 
    0.005*(transformedLayer$`sand.tot.psa_20-50cm`/100 * transformedLayer$`SOM_20-50cm`) - 
    0.013*(transformedLayer$`clay.tot.psa_20-50cm`/100 * transformedLayer$`SOM_20-50cm`) + 
    0.068*(transformedLayer$`sand.tot.psa_20-50cm`/100 * transformedLayer$`clay.tot.psa_20-50cm`/100 ) + 0.031
  transformedLayer$'PWP_20-50cm' <- (transformedLayer$'PWP_20-50cm' + (0.14 * transformedLayer$'PWP_20-50cm' - 0.02))*100
  
  
  
  ##### FC ######
  transformedLayer$'FC_0-20cm' <- -0.251 * transformedLayer$`sand.tot.psa_0-20cm`/100 + 0.195 * 
    transformedLayer$`clay.tot.psa_0-20cm`/100 + 0.011 * transformedLayer$`SOM_0-20cm` + 
    0.006*(transformedLayer$`sand.tot.psa_0-20cm`/100 * transformedLayer$`SOM_0-20cm`) - 
    0.027*(transformedLayer$`clay.tot.psa_0-20cm`/100 * transformedLayer$`SOM_0-20cm`) + 
    0.452*(transformedLayer$`sand.tot.psa_0-20cm`/100 * transformedLayer$`clay.tot.psa_0-20cm`/100) + 0.299
  transformedLayer$'FC_0-20cm' <- (transformedLayer$`FC_0-20cm` + (1.283 * transformedLayer$`FC_0-20cm`^2 - 0.374 * transformedLayer$`FC_0-20cm` - 0.015))*100
  
  
  transformedLayer$'FC_20-50cm' <- -0.251 * transformedLayer$`sand.tot.psa_20-50cm`/100 + 0.195 * 
    transformedLayer$`clay.tot.psa_20-50cm`/100 + 0.011 * transformedLayer$`SOM_20-50cm` + 
    0.006*(transformedLayer$`sand.tot.psa_20-50cm`/100 * transformedLayer$`SOM_20-50cm`) - 
    0.027*(transformedLayer$`clay.tot.psa_20-50cm`/100 * transformedLayer$`SOM_20-50cm`) + 
    0.452*(transformedLayer$`sand.tot.psa_20-50cm`/100 * transformedLayer$`clay.tot.psa_20-50cm`/100) + 0.299
  transformedLayer$'FC_20-50cm' <- (transformedLayer$`FC_20-50cm` + (1.283 * transformedLayer$`FC_20-50cm`^2 - 0.374 * transformedLayer$`FC_20-50cm` - 0.015))*100
  
  ##### soil water at saturation ######
  
  transformedLayer$'SWS_0-20cm' <- 0.278*(transformedLayer$`sand.tot.psa_0-20cm`/100)+0.034*
    (transformedLayer$`clay.tot.psa_0-20cm`/100)+0.022*transformedLayer$`SOM_0-20cm` -
    0.018*(transformedLayer$`sand.tot.psa_0-20cm`/100*transformedLayer$`SOM_0-20cm`)- 0.027*
    (transformedLayer$`clay.tot.psa_0-20cm`/100*transformedLayer$`SOM_0-20cm`)-
    0.584 * (transformedLayer$`sand.tot.psa_0-20cm`/100*transformedLayer$`clay.tot.psa_0-20cm`/100)+0.078
  transformedLayer$'SWS_0-20cm' <- (transformedLayer$'SWS_0-20cm' +(0.636*transformedLayer$'SWS_0-20cm'-0.107))*100
  transformedLayer$'SWS_0-20cm' <- (transformedLayer$`FC_0-20cm`/100+transformedLayer$`SWS_0-20cm`/100-(0.097*transformedLayer$`sand.tot.psa_0-20cm`/100)+0.043)*100
  
  
  
  transformedLayer$'SWS_20-50cm' <- 0.278*(transformedLayer$`sand.tot.psa_20-50cm`/100)+0.034*
    (transformedLayer$`clay.tot.psa_20-50cm`/100)+0.022*transformedLayer$`SOM_20-50cm` -
    0.018*(transformedLayer$`sand.tot.psa_20-50cm`/100*transformedLayer$`SOM_20-50cm`)- 0.027*
    (transformedLayer$`clay.tot.psa_20-50cm`/100*transformedLayer$`SOM_20-50cm`)-
    0.584 * (transformedLayer$`sand.tot.psa_20-50cm`/100*transformedLayer$`clay.tot.psa_20-50cm`/100)+0.078
  transformedLayer$'SWS_20-50cm' <- (transformedLayer$'SWS_20-50cm' +(0.636*transformedLayer$'SWS_20-50cm'-0.107))*100
  transformedLayer$'SWS_20-50cm' <- (transformedLayer$`FC_20-50cm`/100+transformedLayer$`SWS_20-50cm`/100-(0.097*transformedLayer$`sand.tot.psa_20-50cm`/100)+0.043)*100
  
  
  names(transformedLayer) <- gsub("0-20cm", "top", names(transformedLayer))
  names(transformedLayer) <- gsub("20-50cm", "bottom", names(transformedLayer))
  names(transformedLayer) <- gsub("_0-200cm", "", names(transformedLayer))
  names(transformedLayer) <- gsub("\\.", "_",  names(transformedLayer)) 
  
  
  ### write out the result
  terra::writeRaster(transformedLayer, paste0(pathOut ,"/iSDA_soils_transformed.tif", sep=""), filetype="GTiff", overwrite = overwrite)
  
  return(transformedLayer)
  
}








#################################################################################################################
## functions to read from "useCaseName/Crop/raw" and do data processing/derived variables etc and write the result in "UseCase/Crop/transform"
#################################################################################################################

#' @description function to transform soil data and generate derived variables
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param resFactor is an aggregation factor to change the resolution of the layers, soil data in global are at 1km res
#' @param overwrite default is FALSE 
#' @param pathOut path to save the result: TODO When the data architect (DA) is implemented pathOut = "usecaseName/crop/transform/soil"
#'
#' @return raster files cropped from global data and the result will be written out in useCaseName/Crop/transform/Soil/soilGrids 
#'
#' @examples soil_soilGrids_transform(useCaseName = "Rwanda_RAB", Crop = "Potato", resFactor=1, overwrite = TRUE)

soil_soilGrids_transform <- function(useCaseName, Crop, resFactor=1, overwrite = FALSE, pathOut){
  
  ## create a directory to store the transformed data: with DA this will be in "usecaseName/crop/transform"
  pathOut <- paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/transform/Soil/soilGrids", sep="")
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  pathIn <- paste("./EiA_DA_AW_V1/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/",Crop, "/raw/Soil/soilGrids", sep="")
  
  ## read, crop and save 
  listRaster_soilGrids <-list.files(path=pathIn, pattern=".tif$")
  cropped4Country <- terra::rast(paste(pathIn, "/",listRaster_soilGrids, sep=""))
  
  ## Aggregation iSDA at ~1km resolution
  if(resFactor > 1){
    transformedLayer <- aggregate(cropped4Country, fun=mean, fact=resFactor)
  }else{
    transformedLayer <- cropped4Country
  }
  
  names(transformedLayer) <- gsub("0-30cm", "0_30", names(transformedLayer))
  
  terra::writeRaster(transformedLayer, paste0(pathOut ,"/soilGrids_soils_transformed.tif", sep=""), filetype="GTiff", overwrite = overwrite)
  
  return(transformedLayer)
  
}



