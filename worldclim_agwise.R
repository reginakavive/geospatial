## Functions to source worldclim future data and bioclimatic historical data

# source required packages
list.of.packages <- c("terra","raster","geodata","sf")
if (!require("pacman")) install.packages("pacman")
#install+load
pacman::p_load(list.of.packages,character.only = T)


#' Extract worldclim bioclimatic data
#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @res    resolution
#' @useCaseName name of the usecase
#' @Crop name of the drop
#' @country  country name
#' @return data.frame or SpatRast
#' @examples
#' worldclim_bio(res, country,useCaseName,Crop, raster = TRUE, coords = NULL)
#' worldclim_bio( 10, "Rwanda","Rwanda_RAB", "Maize", TRUE, NULL)


#Extracts worldclim historical bioclimatic data (1970-2000)
worldclim_bio<- function(res, country,useCaseName,Crop, raster = TRUE, coords = NULL){
  
  #lookup usecasename, country and crop names from given existing lists
  #path to existing list to be specified
  # stopifnot(country %in% countrylist)
  # stopifnot(useCaseName %in% usecaselist)
  # stopifnot(Crop %in% croplist)
  
  #specify pathOut
  pathOut<-paste0("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/raw/worldclim/historical/", sep="")
  ifelse(!dir.exists(file.path(pathOut)), dir.create(file.path(pathOut),recursive = TRUE), FALSE)
  
  #downloaded file path 
  ifelse(!dir.exists(file.path("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Worldclim/historical/")), 
         dir.create(file.path("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Worldclim/historical/"),recursive = TRUE), FALSE)
  url<- "/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Worldclim/historical/"
  
  #url to download from  ##historical climate data (1970-2000)
  url_download<-"https://geodata.ucdavis.edu/climate/worldclim/2_1/base/"
  
  stopifnot(res %in% c("0.5","2.5", "5", "10", "0.5m","2.5m", "5m", "10m","30", "30s"))
  var<-"bio"
  res<-ifelse ((grepl("m", res,)), res, paste0(res, "m"))
  if (res=="0.5m" || res=="0.5" || res=="30" || res=="30s") {res<-"30s"}
  
  
  ras.all<-raster::stack()
  #loop through list of required vars
  #for (i in var) {
    #var<-i
    
    #file name
    file<-paste0("wc2.1_",res,"_",var)
    
    #check if file available
    if(!file.exists(paste0(url, file, ".zip"))){    #if not available download, unzip and stack, retun rasterstack
      options(timeout=0)
      download.file(paste0(url_download,file,".zip"), paste0(url,file,".zip"), mode="wb")
    }
    suppressWarnings(unzip(paste0(url, file,".zip"), exdir=url, overwrite=FALSE))
    #list files for matching var, res
    if(var == "bio"){
      # 19 bioclimatic variables
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s_%d.tif", res, var, 1:19))
    }else if (var == "elev"){
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s.tif", res, var))
      
    }else {
      rasfiles<-file.path(url, sprintf("wc2.1_%s_%s_%02d.tif", res, var, 1:12))
    }
    #stack all the rasters 
    ras <- raster::stack(rasfiles)
    
    # and return raster stack for all provided vars
    ras.all <- raster::stack(ras.all,ras)
    
    
  #}
  
  if (raster) {       #for raster output raster=TRUE
    
    #crop raster for given aoi/ coords bounds
    aoi_country <-geodata::gadm(country, path= pathOut)
    ras.all <- suppressWarnings(terra::rast (ras.all))
    ras.all <- suppressWarnings(terra::crop(ras.all,aoi_country))
    #crop raster for given aoi/ coords bounds within country
    
    #ras.all <- suppressWarnings(terra::crop(terra::mask(ras.all, sf::as_Spatial(aoi) )),sf::as_Spatial(aoi))
    if (!is.null(coords)){
      ras.all <- suppressWarnings(terra::rast (ras.all))
      aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
      # Subset AOI
      ras.all <- suppressWarnings(terra::crop(ras.all, aoi))
    }
    terra::writeRaster(ras.all, paste0(pathOut, "/" , file,  sep=""), filetype="GTiff",overwrite=TRUE)
    return(ras.all)
    
  } else{             #for TABLE output raster=False
    df <- data.frame()
    for (pnt in seq(1:nrow(coords))){
      lon <- coords[pnt, 1]
      lat <- coords[pnt, 2]
      df1 <- data.frame(terra::extract(ras.all,data.frame(lon,lat)))
      X <- lon
      Y <- lat
      df1<- data.frame(X, Y, df1)
      df <- rbind(df, df1)
    }
    
    names(df) <- sub(sprintf("wc2.1_%s_", res), "", names(df))
    names(df) <- sub("layer", "elev", names(df))
    return(df)
  }
  
}








###################################################################################################
###################################################################################################

#' Extract worldclim future data
#'

#' @coords data.frame with 2 columns (Latitude and Longitude)
#' @raster optional boolean to export results in SpatRast (terra) format
#' @res    resolution  can be either 30s, 2.5m, 5m or 10m
#' @var    list of required variables c("tmin", "tmax", "prec", "bio")
#' @ssp    Shared Socio-economic Pathways (SSPs): 126, 245, 370 and 585.
#' @model  global climate models (GCMs)
#' @period time periods: 2021-2040, 2041-2060, 2061-2080, and 2081-2100. 
#' @useCaseName name of the usecase
#' @Crop name of the drop
#' @country  country name
#' @return SpatRast
#' @examples
#'worldclim_future(var="prec", res=10,  ssp=126, model='ACCESS-CM2', period='2021-2040',
#'               country= 'Kenya',useCaseName = "Rwasis", Crop="Maize",  raster = TRUE, coords = NULL)



#Extracts worldclim future projected data 
worldclim_future <- function(var, res,  ssp, model, period, country,useCaseName,Crop,  raster = TRUE, coords = NULL){
  #define function elements and paths
  
  #lookup usecasename, country and crop names from given lists
  #path to existing list req
  # stopifnot(country %in% countrylist )
  # stopifnot(useCaseName %in% usecaselist  )
  # stopifnot(Crop %in% croplist)
  
  #specify pathOut
  pathOut<-paste0("/home/jovyan/agwise/AgWise_Data/data_sourcing/UseCase_", useCaseName, "/", Crop, "/raw/worldclim/future/", sep="")
  ifelse(!dir.exists(file.path(pathOut)), dir.create(file.path(pathOut),recursive = TRUE), FALSE)
  
  #downloaded file path (downloaded once)
  ifelse(!dir.exists(file.path("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Worldclim/future")), 
         dir.create(file.path("/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Worldclim/future/"),recursive = TRUE), FALSE)
  url<- "/home/jovyan/agwise/AgWise_Data/data_sourcing/Global_GeoData/Landing/Worldclim/future/"
  url_uc<- pathOut
  
  # url to download from  ##future climate data 
  url_download<-"https://geodata.ucdavis.edu/cmip6/"
  
  stopifnot(res %in% c("2.5", "5", "10", "2.5m", "5m", "10m"))
  stopifnot(var %in% c( "tmin", "tmax", "prec", "bio","bioc"))
  stopifnot(ssp %in% c("126","245","370","585"))
  stopifnot(period %in% c("2021-2040", "2041-2060", "2061-2080", "2081-2100"))
  stopifnot(model %in% c("ACCESS-CM2","ACCESS-ESM1-5","AWI-CM-1-1-MR","BCC-CSM2-MR","CMCC-ESM2","CNRM-CM6-1-HR","CNRM-CM6-1","MPI-ESM1-2-LR",
                         "CNRM-ESM2-1","CanESM5-CanOE","CanESM5","EC-Earth3-Veg-LR","EC-Earth3-Veg","FIO-ESM-2-0","GFDL-ESM4","GISS-E2-1-G","UKESM1-0-LL",
                         "GISS-E2-1-H","HadGEM3-GC31-LL","INM-CM4-8","INM-CM5-0","IPSL-CM6A-LR","MIROC-ES2L","MIROC6","MPI-ESM1-2-HR","MRI-ESM2-0"))
  res<-ifelse ((grepl("m", res,)), res, paste0(res, "m"))
  ssp<-paste0("ssp",ssp)
  if (var=="bioc") {var<-"bio"}
  
  
  if (res=="2.5m") {url_download<-paste0(url_download,"2.5m/")}
  else if (res=="5m") {url_download<-paste0(url_download,"5m/")}
  else if (res=="10m") {url_download<-paste0(url_download,"10m/")}
  
  ras.all<-raster::stack()
  #loop through list of required vars
  
      for (i in var) {
        var<-i
        
        
        if (var=="bio") {var<-"bioc"}
        
        
        
        file<-paste0("wc2.1_",res,"_",var,"_",model,"_",ssp,"_",period)
        
        if(!file.exists(paste0(url, file,".tif"))){    #if not available download,  and stack, retuRn rasterstack 
          options(timeout=0)
          download.file(paste0(url_download,model,"/",ssp,"/",file,".tif"), paste0(url,file,".tif"), mode="wb")
          
        }
        #suppressWarnings(unzip(paste0(url, file,".zip"), exdir=url, overwrite=FALSE)) 
        
        #file_url<-paste0(url,"share/spatial03/worldclim/cmip6/7_fut/",res,"/",model,"/",ssp,"/")
        
        rasfiles<-paste0(url,file,".tif")
        
        #stack all the rasters 
        
        ras <- raster::stack(rasfiles)
        
        
        # and return raster stack for all provided vars
        ras.all <- raster::stack(ras.all,ras)
        
      }
  
  if (raster) { #for raster output raster=TRUE
    #crop raster for given aoi/ coords bounds
    aoi_country <-geodata::gadm(country, path= pathOut)
    ras.all <- suppressWarnings(terra::rast (ras.all))
    ras.all <- suppressWarnings(terra::crop(ras.all,aoi_country))
    
    #crop raster for any given aoi/ coords bounds
    if (!is.null(coords)){
      ras.all <- suppressWarnings(terra::rast (ras.all))
      aoi <- suppressWarnings(terra::vect(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = min(coords[,1]), xmax = max(coords[,1]), ymax = max(coords[,2]), ymin = min(coords[,2])), crs = sf::st_crs(4326))))))
      # Subset AOI
      ras.all <- suppressWarnings(terra::crop(ras.all, aoi))
    }
    #write cropped raster stack
    terra::writeRaster(ras.all, paste0(pathOut, "/" , file,  sep=""), filetype="GTiff")
    return(ras.all)
    
  } else{             #for TABLE output; raster=False
    df <- data.frame()
    for (pnt in seq(1:nrow(coords))){
      lon <- coords[pnt, 1]
      lat <- coords[pnt, 2]
      df1 <- data.frame(terra::extract(ras.all,data.frame(lon,lat)))
      X <- lon
      Y <- lat
      df1<- data.frame(X, Y, df1)
      df <- rbind(df, df1)
    }
    
    names(df) <- sub(sprintf("wc2.1_%s_", res), "", names(df))
    names(df) <- sub(("wc2.1_2.5m_"), "", names(df))
    
    return(df)
  }
  
}


