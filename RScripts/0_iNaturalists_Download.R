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


  
print("metadata download start..")

 
# Retreiving the data

target_ids_all <- aoi_poly_in$CELL_ID # SDG

aois_done <- list.files(paste0(workdir, "/", savedir, "/iNaturalists/"), pattern = "^AOI_*.")
aois_done_v <- (as.numeric(sapply(aois_done, FUN = function(x)  (str_split(x, pattern = "_")[[1]][3]))))

wantToDeleteDup <- FALSE



target_ids <-   (setdiff(target_ids_all, aois_done_v))
length(target_ids_all) - length(target_ids) 
cat(length(target_ids), "to go")

name_machine <- Sys.info()["nodename"]


# sink(paste0(workdir, "/logs/", name.machine, "_", Sys.time(), "_output.txt"))	   # Redirect output to the file


if (length(target_ids)==0) {
    ids_togo = NA
    stop("nothing to go")
} else {
    
    ids_togo = 1:length(target_ids)
}

stopifnot(!is.na(ids_togo))



# leaflet(aoi_poly_in) %>% 
#     addTiles() %>% 
#     addPolygons()

 

final_res_msg = foreach (i = ids_togo, .errorhandling = "pass", .inorder = F, .verbose = F) %do% {
    
    aoi_cellid = target_ids[i]
    aoi_cellid_idx = which(aoi_poly_in$CELL_ID == aoi_cellid)
    
    aoi_cellregion = aoi_poly_in$CTP_ENG_NM[aoi_cellid_idx]
    stopifnot(!is.na(aoi_cellid))
    
 
    
    print(paste0("cell_id=",aoi_cellid))
    
    aoi <- aoi_poly_in[aoi_cellid_idx, ]
    aoi_bbox <- bbox (aoi)
    aoi_bbox.txt <- paste(aoi_bbox[1,1], aoi_bbox[2,1], aoi_bbox[1,2], aoi_bbox[2,2], sep=",")
    
    search_results_prefix =  paste0(workdir, savedir, "/iNaturalists/iNaturalists_AOI_CellID_", formatC(aoi_cellid, width = 6, flag = "0"), "_", aoi_cellregion)
    
    existYN = str_detect(list.files( paste0(workdir, savedir, "/iNaturalists/")), pattern=as.character(aoi_cellid)) %>% any
    
      
    
    if (existYN) {
        load(search_results_prefix)
    } else {
    # Note we have to rearrange the coordinates of the bounding box a
        # little bit to give get_inat_obs() what it expects
        inat_obs_df <- get_inat_obs(bounds = aoi_bbox[c(2,1,4,3)], 
                                    taxon_name = NULL,
                                    year = NULL,
                                    month = NULL,
                                    maxresults = 1E4)
        
        if (nrow(inat_obs_df) >= 1E4) {
            cat("+1E4 records")
            
            stop()
            
        } else if (nrow(inat_obs_df) == 0) {
            cat("no records")

            return()
        }
        
        
         
        inat_obs_sf <- inat_obs_df %>% 
            # select(longitude, latitude, datetime, common_name, 
                   # scientific_name, image_url, user_login) %>% 
            st_as_sf(coords=c("longitude", "latitude"), crs=4326)
        dim(inat_obs_sf)
        ## [1] 100 6
        
        # Next, filter out observations that lie outside 
        inat_obs_pcsp_sf <- inat_obs_sf %>% st_intersection(st_as_sf(aoi))
         print(nrow(inat_obs_pcsp_sf))
        
        search_results_fname2 =  paste0(search_results_prefix, "_n", nrow(inat_obs_pcsp_sf), ".Rds")
        
        ## Save the data frame to disk
        saveRDS(inat_obs_pcsp_sf, file = search_results_fname2)
        
        print("done")
    }
}




final_df = foreach (i = ids_togo, .errorhandling = "remove", .inorder = F, .verbose = F) %do% {
    
    aoi_cellid = target_ids[i]
    aoi_cellid_idx = which(aoi_poly_in$CELL_ID == aoi_cellid)
    
    aoi_cellregion = aoi_poly_in$CTP_ENG_NM[aoi_cellid_idx]
    stopifnot(!is.na(aoi_cellid))
  
    
    # print(paste0("cell_id=",aoi_cellid))
    
    aoi <- aoi_poly_in[aoi_cellid_idx, ]
   

    search_results_prefix =  paste0(workdir, savedir, "/iNaturalists/iNaturalists_AOI_CellID_", formatC(aoi_cellid, width = 6, flag = "0"), "_", aoi_cellregion)
    
    fnames_v = list.files( paste0(workdir, savedir, "/iNaturalists/"), full.names = T)
    
    existYN_v = str_detect(fnames_v, pattern=as.character(aoi_cellid)) 
    existYN = any(existYN_v)
    
    if (existYN) {
        

        rm(inat_obs_sf)
        inat_obs_pcsp_sf = readRDS( fnames_v[which(existYN_v)])
        
        # cnt = nrow(inat_obs_pcsp_sf)
        
        inat_obs_pcsp_sf = cbind(CellID=aoi_cellid, inat_obs_pcsp_sf)
        
        return(inat_obs_pcsp_sf)
        
    } else {
        
        return(NULL)
    }
        
}


final_df2 = do.call("rbind", final_df)


search_results_prefix_all =  paste0(workdir, savedir, "/iNaturalists/iNaturalists_AOIs_n", nrow(final_df2), "")



nrow(final_df2)
inat_lonlat = t(sapply(final_df2$geometry, FUN = function(x) x[1:2]))

final_df2 = cbind( inat_lonlat, final_df2 )

final_df2$CELL_ID = NULL
final_df2$CTPRVN_CD = NULL
colnames(final_df2)[1:2] = c( "Lon", "Lat")
colnames(final_df2)[11:12] = c("ImgURL", "userlogin")


final_df2$year = substr(final_df2$datetime,1,4)
final_df2$month = as.numeric(substr(final_df2$datetime,6,7))


saveRDS(final_df2, file = paste0(search_results_prefix_all, ".Rds"))


# plot(as_Spatial(final_df2$geometry))

# dev.off()
# sf::plot_sf(final_df2$geometry)


writeOGR(as_Spatial(final_df2),  dsn = paste0(search_results_prefix_all, ".shp"), layer = "", driver= "ESRI Shapefile", overwrite=T)

# str(inat_lonlat)

final_df3 = as.data.frame(  final_df2)

final_df3$geometry = NULL




write.xlsx(x = final_df3, file = paste0(search_results_prefix_all, ".xlsx"))


barplot(table(final_df3$month), ylab="Frequency", xlab="Month")

# 63149
 

    
    # inat_obs_pcsp_popup_sf <- inat_obs_pcsp_sf %>% 
    #     mutate(popup_html = paste0("<p><b>", common_name, "</b><br/>",
    #                                "<i>", scientific_name, "</i></p>",
    #                                "<p>Observed: ", datetime, "<br/>",
    #                                "User: ", user_login, "</p>",
    #                                "<p><img src='", image_url, "' style='width:100%;'/></p>")
    #     )
    # 
    # 
    # 
    # 
    # htmltools::p("iNaturalist Observations in Placerita Canyon SP",
    #              htmltools::br(),
    #              inat_obs_pcsp_popup_sf$datetime %>% 
    #                  as.Date() %>% 
    #                  range(na.rm = TRUE) %>% 
    #                  paste(collapse = " to "),
    #              style = "font-weight:bold; font-size:110%;")
    # leaflet(placertia_sf) %>% 
    #     addProviderTiles("Esri.WorldStreetMap") %>% 
    #     addPolygons() %>% 
    #     addCircleMarkers(data = inat_obs_pcsp_popup_sf,
    #                      popup = ~popup_html, 
    #                      radius = 5)
    # 
    
    
       
 

# save.image(paste0(workdir, savedir, "/Flickr_CR_workspace_metadata_download_17Aug2019.RData"))



