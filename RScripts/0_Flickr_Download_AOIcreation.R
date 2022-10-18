
library(rgdal)
library(httr)
library(RCurl)
library(rjson)
# library(jsonlite)
library(raster)
library(rgdal)
library(rgeos)
library(openxlsx)
library(stringr)
library(doSNOW)
library(parallel)
library(sf)

n.thread <- detectCores() # 1 
# proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468


#` proj4strings
proj4.LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # WGS84 EPSG:4326 
proj4.UTM52N <- "+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # UTM52N (WGS84) EPSG:32652
proj4.MODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" 
proj4.TM2010 <- "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


# Login credentials
api.key <- "e8008cb908d630a5f6e9b9d97f351c79" # API key for Flickr API goes here
api.secret <- "f86de9bc07e449fe" # Not used


# locations 
path_data = "~/Dropbox/KIT/CES_SEOUL/DATA/"

# Search data: either hashtag or location (bbox = Bounding Box)
# hashtag <- "landscape" # Set "" for all hashtags
hashtag <- "" # Set "" for all hashtags

 
# library(maptools)
# library(ggplot2)

kr_adm_shp = readOGR(paste0(path_data, "GIS/CTPRVN_20220324"), layer = "ctp_rvn", verbose = T)
kr_adm_shp$CTP_KOR_NM = NULL
sdg_adm_shp = kr_adm_shp[kr_adm_shp$CTP_ENG_NM %in% c("Seoul", "Incheon", "Gyeonggi-do"),]

sdg_adm_sf = st_as_sf(sdg_adm_shp)

# plot(sdg_adm_sf, add=F) # slow 
 
sf::write_sf(sdg_adm_sf, dsn = paste0(path_data, "GIS/sdg_adm_sf.shp"))

sdg_adm_ll_sf = st_transform(sdg_adm_sf, proj4.LL)
  
### Create 0.1 deg long-lat grid 
sdg_ll_ext = extent(sdg_adm_ll_sf)

# plot(sdg_ll_ext, add=F)
 
 
  
RES = 0.1

sdg_r_dummy = raster(resolution=RES, xmn=floor(sdg_ll_ext@xmin-0.01), xmx=ceiling(sdg_ll_ext@xmax +0.01), ymn=floor(sdg_ll_ext@ymin -0.01), ymx=ceiling(sdg_ll_ext@ymax + 0.01 ))
sdg_r_dummy[] = 1:ncell(sdg_r_dummy)
 
plot(sdg_r_dummy, add=F)
# plot(sdg_adm_ll_sf, add=T)

sdg_r_dummy_1km = projectRaster(sdg_r_dummy, res = 1000, crs =  proj4.UTM52N, method = "ngb")

writeRaster(sdg_r_dummy, filename = paste0(path_data, "GIS/sdg_r_dummy.tif"))
writeRaster(sdg_r_dummy_1km, filename = paste0(path_data, "GIS/sdg_r_dummy_1km.tif"))




plot(sdg_r_dummy_1km)
# plot(sdg_adm_ll_sf, add=T)

sdg_aoi_all = rasterToPolygons(sdg_r_dummy)
proj4string(sdg_aoi_all)= proj4.LL


sdg_adm_ll_shp = as_Spatial(sdg_adm_ll_sf)
proj4string(sdg_adm_ll_shp) = proj4.LL
sdg_over = over(sdg_aoi_all, sdg_adm_ll_shp, returnList = F)
 
sdg_overlap_idx = !is.na(sdg_over$CTP_ENG_NM) 

sdg_aoi  = sdg_aoi_all[sdg_overlap_idx,]
names(sdg_aoi) <- "CELL_ID"

plot(sdg_r_dummy, xlab= "Lon", ylab="Lat")
plot(sdg_aoi, add=T, col = "red")
plot(sdg_adm_ll_shp, add=T, col = "green")

sdg_aoi@data = cbind(CELL_ID= sdg_aoi$CELL_ID,sdg_over[sdg_overlap_idx,])

# 1km 

sdg_aoi_all_1km = rasterToPolygons(sdg_r_dummy_1km)
proj4string(sdg_aoi_all_1km)= proj4.UTM52N

sdg_adm_UTM52N_shp = spTransform(sdg_adm_ll_shp, CRSobj = crs(proj4.UTM52N))
sdg_over_1km = over(sdg_aoi_all_1km, sdg_adm_UTM52N_shp, returnList = F)

sdg_overlap_1km_idx = !is.na(sdg_over_1km$CTP_ENG_NM) 

sdg_aoi_1km  = sdg_aoi_all_1km[sdg_overlap_1km_idx,]


names(sdg_aoi_1km) <- "CELL_ID"

plot(sdg_r_dummy_1km, xlab= "X", ylab="Y")
plot(sdg_aoi_1km, add=T, col = "red")
plot(sdg_adm_UTM52N_shp, add=T, col = "green")

sdg_aoi_1km@data = cbind(CELL_ID= sdg_aoi_1km$CELL_ID,sdg_over_1km[sdg_overlap_1km_idx,])

sdg_aoi_1km$CELL_ID_1km = 1:nrow(sdg_aoi_1km)
 
writeOGR(sdg_aoi_1km, dsn = paste0(path_data, "GIS"), layer = "FlickrSDG_AOI_1km_19July2022", driver = "ESRI Shapefile", overwrite_layer = T)
 
 


# costarica_aoi = readOGR(dsn = "../Costa Rica_Data/FlickrCR_download/Aug2019_V1/", layer = "FlickrC_AOI_Nphotos.shp")
# 
# sum(costarica_aoi$NPHOTOS_)
# 
# sum(costarica_aoi$NPHOTOS)

# 
# #### NEW. TO sort out
# dt = readOGR("../Costa Rica_Data/FlickrCR_download/Aug2019_V1/FlickrCR_AOI.shp")
# dt$NPHOTOS = nphotos.done.v
# dt$NPHOTOS_UNIQUE = nphotos.done.reduced.v
# 
# 
# nphotos_perct = quantile(dt$NPHOTOS_UNIQUE, probs= seq(0, 1, 0.1))
# nphotos_perct = unique(nphotos_perct)
# 
# nphotos_cut = cut(dt$NPHOTOS_UNIQUE, breaks= as.numeric(nphotos_perct))
# 
# 
# # levels(nphotos_cut)
# aoi_col = rev(topo.colors(length(nphotos_perct)))[as.numeric(nphotos_cut)]
# plot(costarica_ext, xlab= "Lon", ylab="Lat", border=NA, col=NA)
# 
# plot(dt, col = aoi_col, add=T, border="grey")
# plot(costarica_adm2_ll, add=T, col=NA, border="grey")
# plot(costarica_natpark, add=T, col=NA, border='red')
# 
# legend("bottomright", title = "# of Flickr photos", legend = levels(nphotos_cut), col = rev(topo.colors(length(nphotos_perct))), pch=15, bty="n")

# plot(costarica_aoi, add=T)
