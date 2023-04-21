library(corrplot)
# library(fasterize)
library(ggmap)
library(stringr)
library(ggplot2)
library(rgdal)
library(httr)
library(RCurl)
# library(rjson)
# library(jsonlite)
# library(raster)
library(rgdal)
library(rgeos)
library(openxlsx)
library(stringr)
library(doMC)
# library(parallel)

library(data.table)

library(terra)
library(sf)
library(dplyr)
library(tmap)
library(leaflet)
 


# locations 
path_data = "~/Dropbox/KIT/CES_SEOUL/CESKR/DATA/"
 
path_base = "~/Dropbox/KIT/CES_SEOUL/CESKR/"
path_out_csv = "~/Dropbox/KIT/CES_SEOUL/CESKR/FlickrSDG_result/MergedCSV/"

path_SDG_xls = "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_download/July2022_V1/Xlsx/"
path_out_network =  "~/Dropbox/KIT/CES_SEOUL/CESKR/FlickrSDG_result/Network/"

path_out_final =  "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_result/Final/"

years = 2005:2022

# proj4strings
proj4_ll <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # WGS84 EPSG:4326 
proj4.UTM52N <- "+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # UTM52N (WGS84) EPSG:32652
proj4.MODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" 
proj4.TM2010 <- "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
 
# 1km grid
sdg_aoi_1km = read_sf(dsn = paste0(path_data, "GIS"), layer = "FlickrSDG_AOI_1km_19July2022")
sdg_aoi_1km$CELL_ID_ = NULL

# 1km raster
sdg_r_dummy_1km = rast( paste0(path_data, "GIS/sdg_r_dummy_1km.tif"))




### MCST

mcst_total_ras = rast(paste0(path_data,  "/Visiting/mcst_total_ras.tif"))
mcst_foreign_ras = rast(paste0(path_data,  "/Visiting/mcst_foreign_ras.tif"))
mcst_local_ras = rast(paste0(path_data,  "/Visiting/mcst_local_ras.tif"))

mcst_total_df = values(mcst_total_ras)
mcst_foreign_df = values(mcst_foreign_ras)
mcst_local_df = values(mcst_local_ras)

mcst_total_df[!is.finite(mcst_total_df)] = NA
mcst_local_df[!is.finite(mcst_local_df)] = NA
mcst_foreign_df[!is.finite(mcst_foreign_df)] = NA

mcst_idx = !is.na(rowSums(mcst_total_df))

mcst_total_dt = data.table(mcst_total_df[mcst_idx,])
mcst_local_dt = data.table(mcst_local_df[mcst_idx,])
mcst_foreign_dt = data.table(mcst_foreign_df[mcst_idx,])

# table(mcst_idx)





## Flickr data

### Flickr PUD
years_total = c(years, "Total")


pud_df = foreach(i = 1:(length(years_total)), .combine = "cbind") %do% {
    
    year_tmp = years_total[i]
    pud_tmp_r = rast(paste0(path_out_final, "/Flickr/PUD/Tiff/FlickrSDG_PUD_1km_", year_tmp, ".tif"))
    
    pud_tmp_v = values(pud_tmp_r)[mcst_idx]
    
    pud_tmp_v[is.na(pud_tmp_v)] = NA
    
     
    return(pud_tmp_v)
}


### Flickr APUD


apud_df = foreach(i = 1:(length(years_total)), .combine = "cbind") %do% {
    
    year_tmp = years_total[i]
    apud_tmp_r = rast(paste0(path_out_final, "/Flickr/APUD/Tiff/FlickrSDG_APUD_1km_", year_tmp, ".tif"))
    
    apud_tmp_v = values(apud_tmp_r)[mcst_idx]
    
    apud_tmp_v[is.na(apud_tmp_v)] = NA
    
     
    return(apud_tmp_v)
}


pud_df = data.table(pud_df)
apud_df = data.table(apud_df)
pud_df = data.table(pud_df)

dim(apud_df)

nonESpud_df = data.table(pud_df - apud_df)

colnames(pud_df) = colnames(apud_df) = colnames(nonESpud_df) = years_total

plot(pud_df$Total, apud_df$Total)
plot(pud_df$Total, nonESpud_df$Total)
plot(apud_df$Total, nonESpud_df$Total)

anova(lm(pud_df$Total ~ apud_df$Total))
anova(lm(pud_df$Total ~ nonESpud_df$Total))

# corrplot(cor(cbind(pud_df$Total, apud_df$Total, nonESpud_df$Total)), method = "number")

corrplot(cor(cbind(PUD=unlist(pud_df[,-19]), PUD_ES = unlist(apud_df[, -19]), PUD_nonES = unlist(nonESpud_df[,-19]))), method = "number")



int_1.dt = cbind(MCST_Total = unlist(mcst_total_dt[,-19]),MCST_Local = unlist(mcst_local_dt[,-19]),MCST_Foreign = unlist(mcst_foreign_dt[,-19]), PUD=unlist(pud_df[,-19]), PUD_ES = unlist(apud_df[, -19]), PUD_nonES = unlist(nonESpud_df[,-19]))


corrplot(cor(int_1.dt, method = "spearman", use = "complete.obs"), method = "number", diag = F)





### iNaturalists
search_results_prefix =  paste0(workdir, savedir, "/iNaturalists/iNaturalists_AOIs_n73322")

inat_sf = read_sf(paste0(search_results_prefix, ".shp"))
# 
inat_utm_sf = sf::st_transform(inat_sf, crs = st_crs(sdg_aoi_1km))
inat_utm_sp = as_Spatial(inat_utm_sf)



inat_utm_sp$cmmn_nm_fac = factor(inat_utm_sp$cmmn_)

table( levels(inat_utm_sp$cmmn_nm_fac ))

cmmn_nm_factors = levels(inat_utm_sp$cmmn_nm_fac )

inat_utm_sp$cmmn_nm_num = as.numeric(inat_utm_sp$cmmn_nm_fac)

inat_total_r = rasterize(vect(inat_utm_sp), sdg_r_dummy_1km, fun = "sum",  na.rm=T)

writeRaster(inat_total_r, filename = paste0("../FlickrSDG_result/iNaturalists/inat_total_count.tif"), overwrite=T)





taxa_names_tb = table(inat_utm_sp$icn__)
taxa_names_tb = rev(sort(taxa_names_tb, F))
taxa_names = names(taxa_names_tb)




foreach(taxa_tmp = taxa_names) %do% {
    
    inat_tmp = st_as_sf(inat_utm_sp) %>% filter(icn__ ==taxa_tmp)
    
    
    inat_tmp_r = rasterize(vect(inat_tmp), sdg_r_dummy_1km, fun = "sum",  na.rm=T)
    
    
    writeRaster(inat_tmp_r, filename = paste0("../FlickrSDG_result/iNaturalists/inat_", taxa_tmp, "_count.tif"), overwrite=T)
    
    
}



myMap2 <- get_stamenmap(bbox=bbox(as_Spatial(inat_sf)), zoom = 9, maptype="toner-lines", crop=TRUE)
ggmap(myMap2)


foreach(taxa_tmp = taxa_names) %do% {
    
    inat_tmp = st_as_sf(inat_utm_sp) %>% filter(icn__ ==taxa_tmp)
    
    
    inat_tmp_r = rasterize(vect(inat_tmp), sdg_r_dummy_1km, fun = "sum",  na.rm=T)
    
     
    p2_tmp = ggmap(myMap2)   +
        geom_point(data = inat_tmp, aes(x = Lon, y = Lat), size = 0.1, alpha = 0.05)   +
        coord_equal() +
        xlab('Longitude') +
        ylab('Latitude') +
        stat_density2d(aes(x = Lon, y = Lat, fill = ..level..),  alpha = .5,
                       h = 1/24, n = 300,
                       geom = "polygon", data = inat_tmp) +
        scale_fill_viridis_c() +
        theme(legend.position = 'none') +
        ggtitle(paste0(taxa_tmp, " (n=", nrow(inat_tmp), ")"))
    
    
    ggplot2::ggsave(filename = paste0("Plots/iNat_", taxa_tmp, "_map_bw.png"), plot = p2_tmp, device = "png", width = 12, height = 6)
    
}



p3_tmp = ggplot(data = inat_sf, aes(x = Lon, y = Lat, colour =icn__ ), size = 0.01, alpha = 0.05)   +
    geom_point(size=0.1, alpha=0.5) +
    coord_equal() +
    # theme(legend.position = 'left', legend.title = "Taxa group") +
    xlab('Longitude') +
    ylab('Latitude') + 
    scale_fill_viridis_c() +
    ggtitle(paste0("iNaturalists Taxa name"))

ggplot2::ggsave(filename = paste0("Plots/iNat_taxa_map.png"), plot = p3_tmp, device = "png", width = 12, height = 6)




length(table(inat_utm_sp$scnt_))
length(table(inat_utm_sp$cmmn_))
length(table(inat_utm_sp$spcs_))
length(table(inat_utm_sp$icn__))
names(table(inat_utm_sp$icn__))



tb1 =(table(inat_utm_sp$scnt_))

tb1_df = data.frame(tb1)

tb1_df= tb1_df[order(tb1_df$Freq, decreasing = T),]
colnames(tb1_df) = c("Scienfitic name", "Frequency")
# write.xlsx(tb1_df, file = "../FlickrSDG_result/iNaturalists/ScientificName_frequency_Dec.xlsx", rowNames=F)

# sort(tb1[tb1>100], F)
sum(tb1[tb1>200]) / sum(tb1)



tb_group = (table(inat_utm_sp$icn__))
tb_group2 = sort(round(tb_group / sum(tb_group) * 100, 2), F)

# write.xlsx(tb_group2, file = "../FlickrSDG_result/iNaturalists/Group_frequency_Dec.xlsx", rowNames=F)

# clt data 
# mcst_foreign = st_transform(read_sf( paste0(path_data, "GIS/", "MCST_visiting_foreign.shp")), crs = st_crs(proj4.UTM52N))
# mcst_local = st_transform(read_sf( paste0(path_data, "GIS/", "MCST_visiting_local.shp")), crs = st_crs(proj4.UTM52N))
# mcst_total = st_transform(read_sf( paste0(path_data, "GIS/", "MCST_visiting_total.shp")), promote_to_multi= F, crs = st_crs(proj4.UTM52N))





inat_df = foreach(taxa_tmp = taxa_names, .combine = "cbind") %do% {
    
    inat_tmp_r = raster(paste0("../FlickrSDG_result/iNaturalists/inat_", taxa_tmp, "_count.tif"))
    
    inat_tmp_v = values(inat_tmp_r)[mcst_idx]
    # plot(mcst_total_v[mcst_idx], inat_tmp_v[mcst_idx])
    
    return(inat_tmp_v)
}

colnames(inat_df) = taxa_names

inat_total_v = rowSums(inat_df, na.rm=T)


int_df =  cbind(MCSTTotal = mcst_total_v[mcst_idx],MCSTLocal = mcst_local_v[mcst_idx],  MCSTforeign = mcst_foreign_v[mcst_idx], iNatTotal = inat_total_v, inat_df)


int_df[is.na(int_df)] = 0

inat_annual_df = foreach(taxa_tmp = taxa_names, .combine = "cbind") %do% {
    foreach(taxa_tmp = taxa_names, .combine = "cbind") %do% {
        inat_tmp_r = raster(paste0("../FlickrSDG_result/iNaturalists/inat_", taxa_tmp, "_count.tif"))
        
        inat_tmp_v = values(inat_tmp_r)[mcst_idx]
        # plot(mcst_total_v[mcst_idx], inat_tmp_v[mcst_idx])
        
        return(inat_tmp_v)
    }
}







png(paste0("Plots/MCST_iNat_group.png"), width = 1000, height = 1000, pointsize = 20)

corrplot(cor(int_df[,], method = "spearman"), method = "ellipse", diag = T)

dev.off()



png(paste0("Plots/MCST_iNat.png"), width = 1000, height = 1000, pointsize = 20)

corrplot(cor(int_df[,1:4], method = "spearman"), method = "number", diag = T)

dev.off()


c1 = cor(int_df[,])
c1[c1>0.5]

tb3 = data.frame(round(c1[-c(1:3),1:3], 2))
rownames(tb3)

write.xlsx(tb3, file = "../FlickrSDG_result/iNaturalists/MCST_iNat_Spearman.xlsx")



png(paste0("Plots/MCST_iNat_group.png"), width = 1000, height = 1000, pointsize = 20)

corrplot(cor(int_df[,], method = "spearman"), method = "ellipse", diag = T)

dev.off()


inat_df = foreach(taxa_tmp = taxa_names, .combine = "cbind") %do% {
    
    inat_tmp_r = raster(paste0("../FlickrSDG_result/iNaturalists/inat_", taxa_tmp, "_count.tif"))
    
    inat_tmp_v = values(inat_tmp_r)[mcst_idx]
    
    inat_tmp_v[is.na(inat_tmp_v)] = 0
    
    plot(mcst_total_v[mcst_idx], inat_tmp_v)
    
    return(inat_tmp_v)
}







