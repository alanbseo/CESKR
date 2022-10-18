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


n.thread <- detectCores() * 0.5 # 1 
proj4.LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468

proj4.etrs_laea <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs";
# proj4.EUR_ETRS89_LAEA1052 <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" # EPSG:3035

 

path_apikey = "~/Dropbox/KIT/FlickrEU/deepGreen/"

# Login credentials
apikey_con = file(paste0(path_apikey, "/Flickr_API_KEY.txt"), open = "r")
api_key = readLines(apikey_con)
close(apikey_con)
# 
 

 
 
photodir <-  "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_download/Photos"
 

checkImage = FALSE

 
# Time span
mindate <- "2005-01-01"
maxdate <- "2022-07-19"
# savedir <- substr(mindate, 6, 10)
savedir <- "July2022_V1/"
workdir <- "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_download/"
# workdir <-  "~/Dropbox/KIT/FlickrEU/Costa Rica_Data/FlickrCR_download/"
gisdir = "../GIS"

if (!dir.exists(paste0(photodir))) { 
    dir.create(photodir, recursive = T)
}


aoi_poly_in = readOGR( dsn = paste0(path_data, "GIS/"), layer = "FlickrSDG_AOI_19July2022")




#### Download photos 
aois.done.newnames <- list.files(paste0(workdir, "/", savedir, "/Xlsx/"), pattern = "^AOI.*.\\.xlsx$", full.names = T)

# aoi.idx <- 1
 


registerDoMC(n.thread)
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")


# target.ids <- readRDS("Bayern_aoiid.Rds")
# target.ids <- 13225:16370 # Bayern 
 
target.ids.all <- aoi_poly_in$CELL_ID
 


foreach (i = 1:length(target.ids.all), .inorder = F, .errorhandling = "stop", .verbose = F) %do% { 
    
 
    
    # print("process")
    aoi.tmp <- aois.done.newnames[i]
    aoi_cellid = target.ids.all[i]
    aoi_cellid_idx = which(aoi_poly_in$CELL_ID == aoi_cellid)
    
    print(paste0("i=", i, " CellID=", aoi_cellid))
    
    aoi_region = aoi_poly_in$CTP_ENG_NM[aoi_cellid_idx]
     
    
    # aoi.dt <- read.xlsx(aoi.tmp, 1, detectDates = F)
    aoi.dt.raw <- data.frame(read_excel(aoi.tmp, sheet = 1))
    # aoi.dt$Title <- iconv(aoi.dt$Title, from="UTF-8", to="ASCII", sub="")
    # aoi.dt$Username <- iconv(aoi.dt$Username, from="UTF-8", to="ASCII", sub="") # Special characters.. be careful as we flattend the UTF-8 usernames to ASCII
    
    
    photo.ids.unique <- as.numeric( unique( aoi.dt.raw$PhotoID)  )
    aoi.dt <- aoi.dt.raw[match(photo.ids.unique, aoi.dt.raw$PhotoID ),]
    # table(aoi.dt$PhotoID == 16469798504)
    # print(nrow(aoi.dt))
    
    
    if (is.null(nrow(aoi.dt)) || nrow(aoi.dt)==0) {   
        # write.xlsx(data.frame(NA), aois.done.newnames[aoi.idx], overwrite=T)
        imgdir <- paste0(photodir, "/", "AOI_", i, "_EMPTY")
        if (!dir.exists(imgdir)) { 
            dir.create(imgdir, recursive = T)
            cat("AOI_", aoi_cellid, "_created >")
            
        }
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
    
    
    foreach (p.idx = 1:nrow(aoi.dt), .inorder = F, .errorhandling = "stop", .verbose = F) %dopar% { 
        photo.year <- aoi.dt$Year[p.idx]
        # photo.landcoverdesc <- aoi.dt$LandcoverDesc[p.idx]
        photo.owner <- aoi.dt$Owner[p.idx]
        photo.id <- aoi.dt$PhotoID[p.idx]
        photo.date <- aoi.dt$Date[p.idx]
        
        imgdir.annual <- paste0(photodir, "/", "AOI_CellID", formatC(aoi_cellid, width = 6, flag = "0"), "_", aoi_region, "/", photo.year)
        
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
                    
                    download.file(photourl, temp, mode="wb")
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





