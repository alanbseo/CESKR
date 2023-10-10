library(imager)
# library(httr)
# library(RCurl)
# library(rjson)
# library(raster)
library(rgdal)
library(rgeos)
library(doMC)
# library(openxlsx)
library(readxl)
library(stringr)


n_thread <- detectCores() * 0.5 # 1 
proj4_LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468

proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";
# proj4.EUR_ETRS89_LAEA1052 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" # EPSG:3035



path_apikey = "~/Dropbox/KIT/FlickrEU/deepGreen/"

# Login credentials
apikey_con = file(paste0(path_apikey, "/Flickr_API_KEY.txt"), open = "r")
api_key = readLines(apikey_con)
close(apikey_con)
# 


path_GIS = "~/Dropbox/GIS Data/"

path_data = "~/Dropbox/KIT/CES_SEOUL/CESKR/DATA/"

photodir <-  "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_download/Photos_Sep2023_V2/"


checkImage = FALSE


# Time span
mindate <- "2005-01-01"
maxdate <- "2023-08-31"
# savedir <- substr(mindate, 6, 10)
savedir <- "Sep2023_Korea_V2/"
workdir <- "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_download/"
# workdir <-  "~/Dropbox/KIT/FlickrEU/Costa Rica_Data/FlickrCR_download/"
gisdir = "../GIS"




if (!dir.exists(paste0(photodir))) { 
    dir.create(photodir, recursive = T)
}




system.time({
    AOI_poly_in <- sfarrow::st_read_parquet(paste0(path_GIS, "Korea/Admin/", "Korea_Grid_10km_EPSG5186.parquet")) # , col_select = c("gid_e", "geom"))
})  




#### Download photos 

AOI_poly_in$CELL_Region = (str_sub(AOI_poly_in$gid_e, 1, 2)) # 30

aoi_cellregion_path_v = paste0(workdir, savedir,  "/Xlsx/AOI_CellRegion_",  AOI_poly_in$CELL_Region)


lst = list.files(paste0(workdir, savedir,  "/Xlsx/"), pattern = "^AOI\\_CellID\\_*.*\\.xlsx$", recursive = T)

str(lst)

konflikt_aois = str_detect(lst[], pattern = "\ ")

# (?<=eat)[a-z]*
done_aois = str_extract(lst[], pattern = "(?<=AOI_CellID_)[a-zA-Z0-9]*")
tb1 = table(done_aois)
stopifnot(all(tb1==1))
# print(done_aois)
# 
# done_v = (target_ids %in% done_aois)
# done_tb = table(done_v)
# print(done_tb)






# aoi.idx <- 1


n_thread = 1 

if (n_thread > 1) { 
    registerDoMC(n_thread)
}

Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")






foreach (i = 1:length(done_aois), .inorder = F, .errorhandling = "stop", .verbose = F) %do% { 
    
    cat(i)
    aoi_cellid = done_aois[i]
    
    aoi_cellid_idx =  match(aoi_cellid, AOI_poly_in$gid_e )
    
    aoi_cellregion = AOI_poly_in$CELL_Region[aoi_cellid_idx]
    
    stopifnot(!is.na(aoi_cellregion))
    
    aoi_cellregion_path = paste0(workdir, savedir,  "/Xlsx/AOI_CellRegion_", aoi_cellregion)
    
    
    print(paste0("cell_id ",aoi_cellid))
    
    aoi <- AOI_poly_in[aoi_cellid_idx, ]
    aoi_bbox <- sf::st_transform(AOI_poly_in[aoi_cellid_idx,], proj4_LL) %>% sf::st_bbox()
    aoi_bbox.txt <- paste(aoi_bbox[1], aoi_bbox[2], aoi_bbox[3], aoi_bbox[4], sep=",")
    
    
    aoi_fname_tmp = paste0( workdir, savedir,  "/Xlsx/", lst[i])
    
    
    tryCatch(
        aoi.dt.raw <- data.frame(read_excel(aoi_fname_tmp, sheet = 1))
        , error= function(e) {
            print(e); print("XLSX is null"); return(NULL)
        }
    )
    
    
    
    
    
    
    # aoi.dt$Title <- iconv(aoi.dt$Title, from="UTF-8", to="ASCII", sub="")
    # aoi.dt$Username <- iconv(aoi.dt$Username, from="UTF-8", to="ASCII", sub="") # Special characters.. be careful as we flattend the UTF-8 usernames to ASCII
    
    
    photo.ids.unique <- as.numeric( unique( aoi.dt.raw$PhotoID)  )
    aoi.dt <- aoi.dt.raw[match(photo.ids.unique, aoi.dt.raw$PhotoID ),]
    # table(aoi.dt$PhotoID == 16469798504)
    # print(nrow(aoi.dt))
    
    
    if (is.null(nrow(aoi.dt)) || nrow(aoi.dt)==0) {   
        # imgdir <- paste0(photodir, "/", "AOI_", aoi_cellid, "_EMPTY")
        # if (!dir.exists(imgdir)) { 
        #     dir.create(imgdir, recursive = T)
        #     cat("AOI_", aoi_cellid, "_created >")
        # }
        print(paste(aoi_cellid, " has no photos >")   )
        return(NULL)
    } else {
        print(paste(aoi_cellid, " has ", nrow(aoi.dt), " photos >")) 
        
    }
    
    # numeric.cols <- c( "Year", "Longitude", "Latitude", "Geocontext", "LocationAccuracy", "N_FlickrTag")
    # 
    # aoi.dt[, numeric.cols] <- sapply(numeric.cols, FUN = function(x) as.numeric(aoi.dt[,x]))
    # # str(aoi.dt)
    # 
    # aoi.sp <- SpatialPoints(coords = cbind(x= aoi.dt$Longitude, y = aoi.dt$Latitude), proj4string = crs( proj4.LL))
    # aoi.sp.etrs <- spTransform(aoi.sp, CRSobj = proj4.etrs_laea)
    # 
    # aoi.lc <- extract(corine2012, aoi.sp.etrs, fn = c)
    # stopifnot(length(aoi.lc)== length(aoi.sp))
    # 
    # aoi.clc_code <- corine.tb$CLC_CODE[ match(aoi.lc,  corine.tb$GRID_CODE)]
    # aoi.clc_lebel3 <- corine.tb$LABEL3[ match(aoi.lc,  corine.tb$GRID_CODE)]
    # 
    # aoi.dt$Landcover <- aoi.clc_code
    # aoi.dt$LandcoverDesc <- aoi.clc_lebel3
    
    
    foreach (p.idx = 1:nrow(aoi.dt), .inorder = F, .errorhandling = "stop", .verbose = F) %do% { 
        photo.year <- aoi.dt$Year[p.idx]
        # photo.landcoverdesc <- aoi.dt$LandcoverDesc[p.idx]
        photo.owner <- aoi.dt$Owner[p.idx]
        photo.id <- aoi.dt$PhotoID[p.idx]
        photo.date <- aoi.dt$Date[p.idx]
        
        imgdir.annual <- paste0(photodir, "/", "AOI_CellID", formatC(NULL, width = 6, flag = "0"), "_", aoi_cellregion, "/", photo.year)
        
        if (!dir.exists(imgdir.annual)) { 
            cat("AOI_", aoi_cellid, "_create ", photo.year, "_s>")
            dir.create(imgdir.annual, recursive = T)
        }
        
        server = str_split(aoi.dt$URL, "/")[[p.idx]][4]
        secret = str_split(str_split(aoi.dt$URL, "/")[[p.idx]][5], "_")[[1]][2]
        # photourl = aoi.dt[p.idx]
        photo_url <- paste0("https://live.staticflickr.com/", server, "/",photo.id, "_", secret, "_", "z.jpg")
        
        temp <- paste(imgdir.annual, "/photoid_", photo.id, "_date_", photo.date, "_owner_", photo.owner, ".jpg", sep="")
        
        if (!file.exists(temp)) {
            
            cat("AOI_", aoi_cellid, "_photoid", photo.id, "_s>")
            tryCatch(download.file(photo_url, temp, mode="wb", cacheOK = T), error= function(e) {print(e); print("continue..")})
            
        } else { 
            
            
            if (checkImage) { 
                img <- imager::load.image(temp)
                
                if (!is.cimg(img)) {
                    print("re-download the file")
                    cat("AOI_", aoi_cellid, "_photoid", photo.id, "_s>")
                    
                    tryCatch(download.file(photo_url, temp, mode="wb", cacheOK = T), error= function(e) {print(e); print("continue..")})
                } else {
                    # cat(aoi.idx, "_", photo.id, "_s>")
                    cat(".")
                    
                }
                
            }
        }
        
    }
    
    # write.xlsx(aoi.dt, file = aois.done.newnames[aoi.idx], overwrite=T)
    # read.xlsx(aois.done.newnames[aoi.idx])
    
    return(NULL)
}






