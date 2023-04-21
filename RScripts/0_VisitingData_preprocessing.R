library(stringr)

library(rgdal)
# library(httr)
# library(RCurl)
# library(rjson)
# library(jsonlite)
# library(raster)
library(rgdal)
library(rgeos)
library(openxlsx)
library(stringr)
library(doMC)
library(parallel)

library(openxlsx)


library(terra)
library(rinat)
library(sf)
library(dplyr)
library(tmap)
library(leaflet)


# proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468


# proj4strings
proj4_ll <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # WGS84 EPSG:4326 
proj4.UTM52N <- "+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # UTM52N (WGS84) EPSG:32652
proj4.MODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" 
proj4.TM2010 <- "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468



# locations 
path_data = "~/Dropbox/KIT/CES_SEOUL/DATA/"
path_wd = "~/Dropbox/KIT/CES_SEOUL/CESKR/"

 

 
 
# Time span
savedir <- "July2022_V1/"
workdir <- "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_download/"
 
if (!dir.exists(paste0(workdir, savedir, "/Xlsx"))) { 
    dir.create(paste0(workdir, savedir, "/Xlsx"), recursive = T)
    dir.create(paste0(workdir, savedir, "/Rds"), recursive = T)
    dir.create(paste0(workdir, savedir, "/iNaturalists"), recursive = T)
    
}


aoi_poly_in = readOGR( dsn = paste0(path_data, "GIS/"), layer = "FlickrSDG_AOI_19July2022")




## 문광부

clt_dt1 = read.csv("../DATA/Visiting/kml to excel_total_UTF8.csv")
clt_dt1 = cbind(Category = "Total", clt_dt1)
clt_dt2 = read.csv("../DATA/Visiting/kml to excel_foreigner_UTF8.csv")
clt_dt2 = cbind(Category = "Foreigner", clt_dt2)
clt_dt3 = read.csv("../DATA/Visiting/kml to excel_local_UTF8.csv")
clt_dt3 = cbind(Category = "Local", clt_dt3)



clt_dt2$Type.Tier1. = NA
clt_dt3$Type.Tier1. = NA

clt_dt = rbind(clt_dt1, clt_dt2, clt_dt3)

clt_dt = clt_dt[!(is.na(clt_dt$longitude)),]
clt_dt = clt_dt[!(is.na(clt_dt$latitude)),]

colnames(clt_dt)[9:26] = paste0("Cnt", 2005:2022) 
colnames(clt_dt)[27] = "CntTotal"
# clt_dt1$latitude

clt_dt$Type.Tier1. = stringi::stri_trans_general(clt_dt$Type.Tier1. , "Hangul-Latin" )
clt_dt$Site = stringi::stri_trans_general(clt_dt$Site , "Hangul-Latin" )
clt_dt$Si.Gun.Gu = stringi::stri_trans_general(clt_dt$Si.Gun.Gu , "Hangul-Latin" )
clt_dt$Si.Do = stringi::stri_trans_general(clt_dt$Si.Do , "Hangul-Latin" )


# clt_coords = cbind(clt_dt$longitude, clt_dt$latitude)


# pts_1 = sf::st_multipoint(cbind(clt_dt$longitude, clt_dt$latitude))
#  
# clt_sf = sf::st_as_sf(clt_dt, geom = st_geometry(pts_1))
# 
# nrow(clt_sf)
# 
# st_crs(clt_sf) = st_crs(aoi_poly_in_sf)
# 
# # clt_sf = st_transform(clt_sf, crs = st_crs(aoi_poly_in_sf))
# 
# clt_sf_sdg = st_crop(clt_sf, st_bbox(aoi_poly_in_sf))
#  
# 
# clt_sf_foreign =clt_sf_sdg[clt_sf_sdg$Category == "Foreigner",]
# clt_sf_local =clt_sf_sdg[clt_sf_sdg$Category == "Local",]
# clt_sf_total =clt_sf_sdg[clt_sf_sdg$Category == "Total",]
# 
# # factor(clt_sf_total$Type.Tier1., levels = )
# 
# write_sf(clt_sf_foreign, paste0(path_data, "GIS"), layer = "MCST_visiting_foreign", driver = "ESRI Shapefile", overwrite=T)
# write_sf(clt_sf_local, paste0(path_data, "GIS"), layer = "MCST_visiting_local", driver = "ESRI Shapefile")
# write_sf(clt_sf_total, paste0(path_data, "GIS"), layer = "MCST_visiting_total", driver = "ESRI Shapefile")

clt_dt$longitude

pts_coords =  cbind(x=clt_dt$longitude, y=clt_dt$latitude)
clt_spdf = SpatialPointsDataFrame(coords = pts_coords, data = clt_dt, proj4string = CRS(proj4_ll))

clt_spdf = spTransform(clt_spdf, CRSobj = CRS(proj4.UTM52N))


# t1 = sf::as_Spatial(clt_sf_foreign, cast = T)
writeOGR(clt_spdf, dsn = paste0(path_data, "GIS"), layer = "MCST_visiting_sp", driver = "ESRI Shapefile", overwrite_layer = T)

clt_foreign =clt_spdf[clt_spdf$Category == "Foreigner",]
clt_local =clt_spdf[clt_spdf$Category == "Local",]
clt_total =clt_spdf[clt_spdf$Category == "Total",]
# 

clt_list = list(clt_total, clt_local, clt_foreign)
names(clt_list) = c("Total", "Local", "Foreign")

for(i in 1:length(clt_list)) { 
    
    clt_sf_in = st_as_sf(clt_list[[i]])
    
    p_tmp =
        ggmap(myMap)   +
        geom_point(data = clt_sf_in, aes(x = longitude, y = latitude, color = CntTotal), size = 0.8, alpha = 1)   +
        coord_equal() + 
        xlab('Longitude') +
        ylab('Latitude') +
        scale_colour_gradientn(colours = rev(c("red", "orange", "yellow",  "white"))) # + 
    
    # scale_color_continuous(limits = c(0.01, max(clt_sf_in$CntTotal)), type = "viridis") +
    # stat_density2d(aes(x = longitude, y = latitude, fill = ..level..),  alpha = .5,
    # h = 1/24, n = 300,
    # geom = "polygon", data = clt_sf_in)# +
    # scale_fill_viridis_c(option = "plasma") #+
    # theme(legend.position = 'none')
    
    
    ggplot2::ggsave(filename = paste0("Plots/CLT_map_TotalCnt", names(clt_list)[i], ".png"), plot = p_tmp, device = "png", width = 12, height = 6)
}










mcst_cnt = readOGR( paste0(path_out_final, "Visiting/", "MCST_visiting_sp.shp"))

mcst_foreign =  mcst_cnt[mcst_cnt$Categry == "Foreigner",]
mcst_local =mcst_cnt[mcst_cnt$Categry == "Local",]
mcst_total =mcst_cnt[mcst_cnt$Categry == "Total",]
# 


mcst_annual = mcst_total[,9:26]@data

png(paste0("Plots/MCST_annual.png"),width = 1600, height = 2000, pointsize = 20)
par(mfrow=c(3,1))
barplot(colSums(mcst_annual/1E6, na.rm=T), ylab = "Total visitors (million)", names = 2005:2022, las=2)
barplot(colSums(mcst_local[,9:26]@data/1E6, na.rm=T), ylab = "Local visitors (million)", names = 2005:2022, las=2)
barplot(colSums(mcst_foreign[,9:26]@data/1E6, na.rm=T), ylab = "Foreign visitors (million)", names = 2005:2022, las=2)

dev.off()




# https://github.com/r-spatial/sf/issues/114

# mcst_total_p = st_cast(st_as_sfc(mcst_total), "POINT", group_or_split = FALSE)
# mcst_local_p = st_cast(st_as_sfc(mcst_local), "POINT", group_or_split = FALSE)
# mcst_foreign_p = st_cast(st_as_sfc(mcst_foreign), "POINT", group_or_split = FALSE)
# 
# mcst_total_p = cbind(mcst_total_p, mcst_total$CntTotl)

# mcst_total2 = st_cast((mcst_total[1:10,"CntTotl"]), "POINT", group_or_split = FALSE)
# st_cast(st_sfc(x), "POINT")
# as_Spatial(mcst_total)
# mcst_total2 = (readOGR(paste0(path_data, "GIS/", "MCST_visiting_total.shp")))




# sf::sf_use_s2(T)
# mcst_foreign_1km2 = sf::st_join(sdg_aoi_1km[,], mcst_foreign, prepared=T, join = st_within, left=T)
# table(mcst_foreign_1km2)

# mcst_foreign_1km = sf::st_join(mcst_foreign, sdg_aoi_1km[,], prepared=T, join = st_contains, left=F)
# 
# 
# mcst_total_1km = sf::st_join(mcst_total, sdg_aoi_1km[,], prepared=T, join = st_within, left=T)
# mcst_local_1km = sf::st_join(mcst_local, sdg_aoi_1km[,], prepared=T, join = st_within, left=T)
# 
# mcst_foreign_1km = sf::st_join(sdg_aoi_1km[,], mcst_foreign, prepared=T, left=F)
# mcst_local_1km = sf::st_join(sdg_aoi_1km[,], mcst_local, prepared=T, left=F)
# mcst_total_1km = sf::st_join(sdg_aoi_1km[,], mcst_total, prepared=T, left=F)
# # a = st_within( sdg_aoi_1km,   mcst_total[1:50,"CntTotl"] )
# #  
# a = as_Spatial(mcst_total[,])
# b = as_Spatial(sdg_aoi_1km)
# 
# proj4string(a) = proj4string(b)
# 
# c = sp::over(b,a, returnList = F)
# 
# str(mcst_foreign_1km)



# rasterize based on geometry and a column named "value". Change the name of this column if necessary
# r.enn2mean<-st_rasterize(enn2mean %>% dplyr::select(value, geometry))


# mcst_total_r = stars::st_rasterize(mcst_total[,"CntTotl"])
# mcst_total_r = rasterizeGeom(terra::vect(as_Spatial(mcst_total[,"CntTotl"])), raster = sdg_r_dummy_1km)

# # coerce from sf *via* sp
# library(sf)
# vsf <- st_read(f)
# vsp <- as(st_as_sf(mcst_total), "Spatial")
# v <- vect(vsp)


# mcst_total_r = rasterize(vect(mcst_total), sdg_r_dummy_1km, field = "CntTotl", fun = "sum",  na.rm=T)

mcst_total_ras = rast(lapply(9:27, FUN = function(x) rasterize(vect(mcst_total), sdg_r_dummy_1km, field =names(mcst_total)[x], fun = "sum",  na.rm=T)))

mcst_types =  c("gwangwangjangso mich siseol", "jayeon mich saengtaehwangyeong","munhwa")

mcst_total_byType_ras = lapply(1:3, FUN = function(x1) rast(lapply(9:27, FUN = function(x2) rasterize(vect(mcst_total[mcst_total$Typ_T1_ == mcst_types[x1],]), sdg_r_dummy_1km, field = names(mcst_total)[x2], fun = "sum",  na.rm=T))))


# mcst_total_r = rasterize(vect(mcst_total), sdg_r_dummy_1km, field = "CntTotl", fun = "sum",  na.rm=T)

plot(mcst_total_byType_ras[[1]][["CntTotl_sum"]])
plot(mcst_total_byType_ras[[2]][["CntTotl_sum"]])
plot(mcst_total_byType_ras[[3]][["CntTotl_sum"]])

names(mcst_total_byType_ras) = c("NormalTourism", "EcoTourism", "CulturalPlace")



mcst_local_ras = rast(lapply(9:27, FUN = function(x) rasterize(vect(mcst_local), sdg_r_dummy_1km, field =names(mcst_total)[x], fun = "sum",  na.rm=T)))

mcst_foreign_ras = rast(lapply(9:27, FUN = function(x) rasterize(vect(mcst_foreign), sdg_r_dummy_1km, field =names(mcst_foreign)[x], fun = "sum",  na.rm=T)))



writeRaster(mcst_total_ras, filename = "../DATA/Visiting/mcst_total_ras.tif", overwrite=T)
writeRaster(mcst_local_ras, filename = "../DATA/Visiting/mcst_local_ras.tif", overwrite=T)
writeRaster(mcst_foreign_ras, filename = "../DATA/Visiting/mcst_foreign_ras.tif", overwrite=T)

writeRaster(mcst_total_byType_ras[[1]], filename = "../DATA/Visiting/mcst_total_ras_normaltourism.tif", overwrite=T)
writeRaster(mcst_foreign_ras, filename = "../DATA/Visiting/mcst_total_ras_ecotourism.tif", overwrite=T)
writeRaster(mcst_foreign_ras, filename = "../DATA/Visiting/mcst_total_ras_culturalplace.tif", overwrite=T)







### Biplot

mcst_total_v = as.numeric(values(mcst_total_ras$CntTotl_sum))
mcst_foreign_v = as.numeric(values(mcst_foreign_ras$CntTotl_sum))
mcst_local_v = as.numeric(values(mcst_local_ras$CntTotl_sum))

mcst_idx = !is.na(mcst_total_v)



