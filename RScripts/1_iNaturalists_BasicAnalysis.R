# library(stringi)
library(ggmap)
library(stringr)
library(ggplot2)
library(rgdal)
library(httr)
library(RCurl)
library(rjson)
# library(jsonlite)
# library(raster)
library(rgdal)
library(rgeos)
library(openxlsx)
# library(doMC)
library(foreach)
 

library(terra)
library(rinat)
library(sf)
library(dplyr)
library(tmap)
 

# proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468


# proj4strings
proj4_ll <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # WGS84 EPSG:4326 
proj4.UTM52N <- "+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # UTM52N (WGS84) EPSG:32652
proj4.MODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" 
proj4.TM2010 <- "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468



# locations 
path_GIS = "~/Dropbox/GIS Data/"
path_data = "~/Dropbox/KIT/CES_SEOUL/DATA/"
path_wd = "~/Dropbox/KIT/CES_SEOUL/CESKR/"



savedir <- "Sep2023_Korea_V2/"
workdir <- "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_download/"


system.time({
    AOI_poly_in <- sfarrow::st_read_parquet(paste0(path_GIS, "Korea/Admin/", "Korea_Grid_10km_EPSG5186.parquet")) # , col_select = c("gid_e", "geom"))
}) # 33 sec


# AOI_poly_in = read_sf( dsn = paste0(path_data, "GIS/"), layer = "FlickrSDG_AOI_19July2022")

search_results_prefix =  paste0(workdir, savedir, "/iNaturalists/iNaturalists_AOIs_n334485")

# Rds data
inat_dt = readRDS(paste0(search_results_prefix, ".Rds"))

# SHP data
inat_sf = read_sf(paste0(search_results_prefix, ".shp"))


# inat_obs_pcsp_popup_sf <- inat_sf %>%
#     mutate(popup_html = paste0("<p><b>", cmmn_nm, "</b><br/>",
#                                "<i>", scntfc_, "</i></p>",
#                                "<p>Observed: ", datetim, "<br/>",
#                                "User: ", usr_lgn, "</p>",
#                                "<p><img src='", imag_rl, "' style='width:100%;'/></p>")
#     )

# htmltools::p("iNaturalist Observations in Placerita Canyon SP",
#              htmltools::br(),
#              inat_obs_pcsp_popup_sf$datetim %>%
#                  as.Date() %>%
#                  range(na.rm = TRUE) %>%
#                  paste(collapse = " to "),
#              style = "font-weight:bold; font-size:110%;")
# 
# leaflet(inat_sf) %>%
#     addProviderTiles("Esri.WorldStreetMap") %>%
#     addPolygons() %>%
#     addCircleMarkers(data = inat_obs_pcsp_popup_sf,
#                      popup = ~popup_html,
#                      radius = 5)

# 
# 
AOI_poly_in_sf = sf::st_transform(AOI_poly_in, crs = st_crs(inat_sf))
# 
# df<-plot_data %>% 
#     filter(!is.na(ethnic)) %>% # subset data with ethnicity only
#     as.data.frame()    
# 
# ggplot(plot_data, 
#        aes(x= long, y = lat, group = id,fill="", color="")) + 
#     geom_polygon() + 
#     geom_polygon(data = df, 
#                  aes(color = ethnic), size=1) + 
#     geom_polygon(data=df, aes(x= long, y = lat, group = id))+
#     scale_fill_manual(values="black", guide=F)+
#     scale_color_manual(name="ethnic",
#                        labels=c("","Shaygiyya, Ja'aliyyin and Danagla (Arab)"),
#                        values=c("black","red"), 
#                        breaks = c("NA","Shaygiyya, Ja'aliyyin and Danagla (Arab)")) +
#     coord_equal()



myMap <- get_stamenmap(bbox=bbox(as_Spatial(inat_sf)),zoom = 7, maptype="terrain", crop=TRUE)
ggmap(myMap)
# 
# p1 = ggplot(inat_sf, aes(x = Lon, y = Lat)) + 
#     coord_equal() + 
#     xlab('Longitude') + 
#     ylab('Latitude') + 
#     geom_point(size = 0.1, alpha = 0.05) + 
#     stat_density2d(aes(fill = ..level..), alpha = .5,
#                    h = 0.05, n = 300,
#                    geom = "polygon", data = inat_sf) +
#     
#     scale_fill_viridis_c() + 
#     theme(legend.position = 'none')
# 
#  

p1 = ggmap(myMap)   +
    geom_point(data = inat_sf, aes(x = Lon, y = Lat), size = 0.1, alpha = 0.05)   +
    coord_equal() + 
    xlab('Longitude') +
    ylab('Latitude') +
    stat_density2d(aes(x = Lon, y = Lat, fill = ..level..),  alpha = .5,
                   h = 1/24, n = 300,
                   geom = "polygon", data = inat_sf) +
    scale_fill_viridis_c() +
    theme(legend.position = 'none')


ggplot2::ggsave(filename = "Plots/iNat_map1.png", plot = p1, device = "png", width = 24, height = 12)




taxa_names_tb = table(inat_sf$icnc_t_)
taxa_names_tb = rev(sort(taxa_names_tb, F))
taxa_names = names(taxa_names_tb)


png("Plots/iNat_taxa_barplot.png", width = 1200)

barplot(taxa_names_tb, ylab="Frequency", xlab="Taxa")

dev.off()


foreach(taxa_tmp = taxa_names) %do% {

        inat_tmp = inat_sf %>% filter(icnc_t_ ==taxa_tmp)
    
    p2_tmp = ggmap(myMap)   +
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
    
    
    ggplot2::ggsave(filename = paste0("Plots/iNat_", taxa_tmp, "_map.png"), plot = p2_tmp, device = "png", width = 24, height = 12)
    
}












