library(httr)
# library(RCurl)
library(rjson)
# library(jsonlite)
# library(terra)
library(dplyr)
# library(rgdal)
library(rgeos)
library(openxlsx)
library(stringr)
library(doSNOW)
library(foreach) # %do% and %dopar%

library(parallel)
library(sf)
# library(arrow)
# install.packages('sfarrow')
library(sfarrow)

# proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468


#` proj4strings
proj4_LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0" # WGS84 EPSG:4326 
proj4.UTM52N <- "+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # UTM52N (WGS84) EPSG:32652
proj4.MODIS <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs" 
proj4.TM2010 <- "+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468



# locations 
path_GIS = "~/Dropbox/GIS Data/"
path_data = "~/Dropbox/KIT/CES_SEOUL/CESKR/DATA/"
path_wd = "~/Dropbox/KIT/CES_SEOUL/CESKR/"

# Search data: either hashtag or location (bbox = Bounding Box)
# hashtag <- "landscape" # Set "" for all hashtags
hashtag <- "" # Set "" for all hashtags





# Login credentials
# api.key <- "" # An API key for Flickr API
# api.secret <- "" # Not used
# apikey_con = file("Flickr_API_KEY.txt", open = "w")
# writeLines(api.key, con = apikey_con)
# close(apikey_con)

apikey_con = file("Flickr_API_KEY.txt", open = "r")
api_key = readLines(apikey_con)
close(apikey_con)

# 
name_machine <- Sys.info()["nodename"]


# Time span
mindate <- "2005-01-01"
maxdate <- "2023-08-31"
# savedir <- substr(mindate, 6, 10)
savedir <- "Sep2023_Korea_V2/"
workdir <- "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_download/"
# workdir <-  "~/Dropbox/KIT/FlickrEU/Costa Rica_Data/FlickrCR_download/"
gisdir = "../GIS"

if (!dir.exists(paste0(workdir, savedir, "/Xlsx"))) { 
    dir.create(paste0(workdir, savedir, "/Xlsx"), recursive = T)
    dir.create(paste0(workdir, "/logs"), recursive = T)
    
}

# Read Area of Interest polygons
# system.time({
# AOI_poly_in = read_sf(paste0(path_GIS, "Korea/Admin/", "Korea_PopGrid_100m_EPSG5186.gpkg"))
# }) # 71 sec
# AOI_poly_in$gid = NULL
# AOI_poly_in$OBJECTID = NULL
# AOI_poly_in$Shape_Length = NULL
# AOI_poly_in$Shape_Area = NULL

# sfarrow::st_write_parquet(obj=AOI_poly_in, dsn=file.path(paste0(path_GIS, "Korea/Admin/", "Korea_PopGrid_100m_smallsize_EPSG5186.parquet")))

# read usuing sfarrow
# if (!exists("AOI_poly_in")) { 
#     system.time({
#         # AOI_poly_in <- sfarrow::st_read_parquet(paste0(path_GIS, "Korea/Admin/", "Korea_PopGrid_100m_smallsize_EPSG5186.parquet")) # , col_select = c("gid_e", "geom"))
#     }) # 33 sec
# }



# read usuing sfarrow
if (!exists("AOI_poly_in")) {
    
    AOI_poly_in = read_sf(paste0(path_GIS, "Korea/Admin/", "SGIS/SGIS_Grid_10K.shp"))
    names(AOI_poly_in)[1] = c("gid")
    
    kalphabet = "가나다라마바사아자차카타파하"
    kalphabet_v = strsplit(kalphabet, split = "")[[1]]
    alphabet_v = LETTERS[seq_along(kalphabet_v)]
    
    
    nvec = alphabet_v
    names(nvec) = kalphabet_v
    
    str_replace_all(kalphabet, pattern = kalphabet_v,  replacement = alphabet_v)
    str_replace_all(kalphabet, pattern = c("가" = "A", "다" = "C"))
    str_replace_all(kalphabet, pattern = nvec) # use named vector
    AOI_poly_in$gid_e  = str_replace_all(AOI_poly_in$gid, pattern = nvec) # use named vector
    
    # plot(AOI_poly_in["gid_e"], col = topo.colors(nrow(AOI_poly_in)))
    
    sfarrow::st_write_parquet(obj=AOI_poly_in, dsn=file.path(paste0(path_GIS, "Korea/Admin/", "Korea_Grid_10km_EPSG5186.parquet")))
    write_sf(obj=AOI_poly_in, dsn=file.path(paste0(path_GIS, "Korea/Admin/", "Korea_Grid_10km_EPSG5186.shp")))
    
    
    
} else {
    
    
    
    
    system.time({
        AOI_poly_in <- sfarrow::st_read_parquet(paste0(path_GIS, "Korea/Admin/", "Korea_Grid_10km_EPSG5186.parquet")) # , col_select = c("gid_e", "geom"))
    }) # 33 sec
}


# Search parameters
sort <- "date-taken-asc" # "date-posted-desc" # "date-taken-asc" # "interestingness-desc" # Sort by Interestingness (or: relevance)
max.perpage <- 250 # number per page maximum 250
# n_points <- nrow(AOI_poly_in)
# 10122596



max_try = 20
geturl.opts <- list(timeout = 20, maxredirs = max_try, verbose = T)




# Assign CELL IDs
AOI_poly_in$CELL_ID = AOI_poly_in$gid_e # Grid ID in English 
AOI_poly_in$CELL_Region = (str_sub(AOI_poly_in$gid_e, 1, 2)) # 30

target_ids <- AOI_poly_in$CELL_ID


# aoi_cellid_idxs = 3E5:1 # length(target_ids) # seq_along(target_ids)
# aoi_cellid_idxs = 4E5:4.001E5

# aoi_cellid_idxs = 2.5E5:3.5E5 # :length(target_ids) # match("BB873799", target_ids)
# aoi_cellid_idxs =match("CG662465", target_ids)
aoi_cellid_idxs = 1:length(target_ids) # seq_along(target_ids)



aoi_cellregion_path_v = paste0(workdir, savedir,  "/Xlsx/AOI_CellRegion_",  AOI_poly_in$CELL_Region)


lst = list.files(paste0(workdir, savedir,  "/Xlsx/"), pattern = "^AOI\\_CellID\\_*.*\\.xlsx$", recursive = T)

str(lst)


# (?<=eat)[a-z]*
done_aois = str_extract(lst[], pattern = "(?<=AOI_CellID_)[a-zA-Z0-9]*")

print(done_aois)

undone = (target_ids %in% done_aois)
table(undone)
length(undone)

# done_flag = foreach (aoi_cellid_idx = 1:length(target_ids), .errorhandling = "stop", .inorder = T, .verbose = F) %do% {
#     
#     cat(">")
#  
#     lst = list.files(paste0(workdir, savedir,  "/Xlsx/AOI_CellRegion_", AOI_poly_in$CELL_Region[aoi_cellid_idx]), pattern =AOI_poly_in$CELL_ID[aoi_cellid_idx])
#     
#      
#     return (length(lst) > 0)
#     
# } 




if (length(target_ids)==0) {
    stop("nothing to go")
} else {
    print("Range of cells")
    print(range(aoi_cellid_idxs))
}



n_thread <- 1  # detectCores()  


DoSNOW = FALSE # then doMC

if (DoSNOW) { 
    cl = makeCluster(n_thread)
    registerDoSNOW(cl)
} else {
    doMC::registerDoMC(n_thread)
}

# Retreiving the data

print("metadata downloading..")
# sink(paste0(workdir, "/logs/", name_machine, "_", Sys.time(), "_output.txt"), append = T)	   # Redirect output to the file



SLEEP_SEC = 0 

final_res_msg = foreach (aoi_cellid_idx = aoi_cellid_idxs, .errorhandling = "stop", .inorder = F, .verbose = F, .packages = c("foreach", "dplyr", "sf")) %do% { # .packages = c("rgeos", "rgdal", "rjson", "stringr", "sf", "httr", "foreach")
    
    
    
    aoi_cellid =  AOI_poly_in$CELL_ID[aoi_cellid_idx] 
    
    aoi_cellregion = AOI_poly_in$CELL_Region[aoi_cellid_idx]
    
    stopifnot(!is.na(aoi_cellregion))
    
    aoi_cellregion_path = paste0(workdir, savedir,  "/Xlsx/AOI_CellRegion_", aoi_cellregion)
    
    
    if (!dir.exists(aoi_cellregion_path)) { 
        dir.create(aoi_cellregion_path, recursive = T)
    }
    
    aois_done <- list.files(paste0(aoi_cellregion_path), pattern = "^AOI\\_CellID\\_*.*\\.xlsx$")
    aois_done_v <- sapply(aois_done, FUN = function(x) (stringr::str_split(x, pattern = "_")[[1]][3]))
    
    if (aoi_cellid %in% aois_done_v) {
        print("skip")
        return(NULL)
    } else {
        print(paste0("sleep for ", SLEEP_SEC, " sec"))
        Sys.sleep(SLEEP_SEC)
    }
    
    
    print(paste0("cell_id ",aoi_cellid))
    
    aoi <- AOI_poly_in[aoi_cellid_idx, ]
    aoi_bbox <- sf::st_transform(AOI_poly_in[aoi_cellid_idx,], proj4_LL) %>% sf::st_bbox()
    aoi_bbox.txt <- paste(aoi_bbox[1], aoi_bbox[2], aoi_bbox[3], aoi_bbox[4], sep=",")
    
    
    # extras <- c("description, license, date_upload, date_taken, owner_name, icon_server, original_format, last_update, geo, tags, machine_tags, o_dims, views, media, path_alias, url_sq, url_t, url_s, url_q, url_m, url_n, url_z, url_c, url_l, url_o")
    extras <- c("date_taken,owner_name,geo,tags,machine_tags") #,url_z")
    
    # api <- paste("https://api.flickr_com/services/rest/?method=flickr_photos_search&format=json&api_key=", api.key, "&nojsoncallback=1&page=1&per_page=", max.perpage, "&bbox=", aoi_bbox.txt, "&min_taken_date=", mindate, "&max_taken_date=", maxdate, "&sort=", sort, "&privacy_filter=", "1",sep="")
    api <- paste0("https://api.flickr.com/services/rest/?method=flickr.photos.search&format=json&api_key=", api_key, "&nojsoncallback=1&page=1&per_page=", max.perpage, "&bbox=", aoi_bbox.txt, "&min_taken_date=", mindate, "&max_taken_date=", maxdate, "&sort=", sort, "&privacy_filter=", "1", "&extras=", extras)
    
    
    httr_tmp  <- httr::GET(api, httr::add_headers("--http1.1"))
    raw_data_tmp <- httr::content(httr_tmp, "text", encoding = "UTF-8") 
    
    
    data_1st <- rjson::fromJSON(raw_data_tmp, unexpected.escape="keep", method="C")
    
    if (data_1st$stat != "ok") { 
        print(paste0("error..", data_1st$stat)   )
        
        stop(paste0("error..", data_1st$stat)   )
        
        return(NULL)
    }
    
    
    n_pages <-  data_1st$photos$pages
    cat( "Region=", aoi_cellregion,  "cell_idx=", aoi_cellid_idx, " poly_id=", aoi_cellid, " n_pages=", n_pages, "\n")
    
    ### Download metadata
    if ( data_1st$photos$pages <= 0 || length(data_1st$photos$photo) ==0) { # e.g., no photos with one (invalid) page
        if (data_1st$stat == "ok") { 
            print("no photos")
            # write.xlsx(data.frame(NA), file = paste0( workdir, savedir,  "/Xlsx/AOI_CellRegion_", aoi_cellregion, "/CellID_", formatC(aoi_cellid, width = 4, flag = "0"), "_", aoi_cellregion ,"_n0.xlsx"), overwrite=T)
            writeLines("no data", con = paste0( workdir, savedir,  "/Xlsx/AOI_CellRegion_", aoi_cellregion, "/AOI_CellID_", formatC(aoi_cellid, width = 4, flag = "0"), "_n0.xlsx"))
            
            
            return(NULL)
        } else {
            print(paste0("error..", data_1st$stat)   )
            stop(paste0("error..", data_1st$stat)   )
            return(NULL)
            
        }
        
    } else {  
        
        
        print("photos exist")
        
        
        # 
        # return(T)
        
        
        data_l_tmp <- vector("list", length = n_pages)
        data_l_tmp[[1]] <- data_1st
        
        if (n_pages > 1) {
            
            data_l_tmp[2:length(data_l_tmp)]  <- foreach (p_idx = 2:n_pages) %do% {
                
                cat(p_idx, ">")
                api_tmp <- paste0("https://api.flickr.com/services/rest/?method=flickr.photos.search&format=json&api_key=", api_key, "&nojsoncallback=1&page=", p_idx, "&per_page=", max.perpage, "&bbox=", aoi_bbox.txt, "&min_taken_date=", mindate, "&max_taken_date=", maxdate, "&sort=", sort, "&privacy_filter=", "1", "&extras=", extras)
                
                # Unlike standard photo queries, geo (or bounding box) queries will only return 250 results per page.
                # https://www.flickr_com/services/api/flickr_photos_search.html
                
                # raw_data_tmp <- getURL(api_tmp, ssl_verifypeer = FALSE, .opts = geturl.opts) # , .encoding = "UTF-8", .mapUnicode = T)
                
                raw_data_tmp <- httr::content(httr::GET(api_tmp, httr::add_headers("--http1.1")), "text", encoding = "UTF-8") 
                #    
                # rjson::fromJSON(raw_data_tmp, unexpected.escape="skip", method="R", encoding="UTF-8")
                #   
                #   library(jsonlite)
                #   str(jsonlite::fromJSON(raw_data_tmp, encoding="UTF-8"))
                #       RJSONIO::fromJSON(raw_data_tmp, encoding = "UTF-8")
                
                
                
                # data_l_tmp[[p_idx]] <- fromJSON(raw_data_tmp, unexpected.escape="skip", method="C")
                res_tmp <- rjson::fromJSON(raw_data_tmp, unexpected.escape="skip", method="C")
                
                if (res_tmp$stat != "ok") { 
                    print(paste0("error..", res_tmp$stat))
                    stop("error")
                }
                
                return(res_tmp)
                
            }
            
            
            
        } else {
            # do nothing
        }
        
        
        ## Save meta information
        # flickrphotos_metadata_list_l[[cell_idx]] <- data_l_tmp
        flickrphotos_metadata_specific_df_l <- vector("list", length = n_pages)
        
        # flickrphotos_metadata_df[aoi_cellid_idx, "n_pages"] <- n_pages
        
        
        
        for (p_idx in 1:n_pages) { 
            
            print(paste("page ", p_idx))
            
            
            data_tmp <- data_l_tmp[[p_idx]] #  flickrphotos_metadata_list_l[[cell_idx]][[p_idx]]
            # flickrphotos_metadata_df[i, "nallphotos"] <- sum(flickrphotos_metadata_df[i, "nallphotos"] + length(data_tmp$photos$photo), na.rm = T) # todo fix the code
            # print(" flickrphotos_metadata_df[i, nallphotos")
            # print(flickrphotos_metadata_df[i, "nallphotos"])
            
            
            nphotos_tmp <- length(data_tmp$photos$photo)
            print(paste(nphotos_tmp, "photos exist"))
            
            if (nphotos_tmp < 1) { 
                next() # such conditions existed 
                # stop() # stop does not work!! just pass to the next iter
            }
            
            flickrphotos_metadata_specific_df_l[[p_idx]] <- vector("list", length = nphotos_tmp)
            
            
            res_l1 <- foreach (u = 1:nphotos_tmp, .errorhandling = "stop") %do% {
                
                
                info_l <- data_tmp$photos$photo[[u]]
                
                # if (info_l$stat == "ok") { 
                aoi_ll = aoi %>% sf::st_transform(proj4_LL) %>% as_Spatial 
                
                photo_sp <- sp::SpatialPoints(t(as.matrix(as.numeric(c(info_l$longitude, info_l$latitude)))), proj4string = CRS(sp::proj4string(aoi_ll)))
                
                sp::proj4string(photo_sp) = proj4_LL
                sp::proj4string(aoi_ll) = proj4_LL
                intersectYN <- rgeos::gIntersects(photo_sp, aoi_ll)
                cat(paste0(">", u, ifelse(intersectYN, "Y",  "N")))
                # 
                if (intersectYN) {
                    
                    # names(info_l)
                    # [1] "id"                   "owner"                "secret"               "server"              
                    # [5] "farm"                 "title"                "ispublic"             "isfriend"            
                    # [9] "isfamily"             "datetaken"            "datetakengranularity" "datetakenunknown"    
                    # [13] "ownername"            "tags"                 "machine_tags"         "latitude"            
                    # [17] "longitude"            "accuracy"             "context"              "place_id"            
                    # [21] "woeid"                "geo_is_family"        "geo_is_friend"        "geo_is_contact"      
                    # [25] "geo_is_public"        "url_z"                "height_z"             "width_z"      
                    # 
                    # names(info_l)
                    # [1] "id"                   "owner"                "secret"               "server"               "farm"                
                    # [6] "title"                "ispublic"             "isfriend"             "isfamily"             "datetaken"           
                    # [11] "datetakengranularity" "datetakenunknown"     "ownername"            "tags"                 "latitude"            
                    # [16] "longitude"            "accuracy"             "context"              "place_id"             "woeid"               
                    # [21] "geo_is_family"        "geo_is_friend"        "geo_is_contact"       "geo_is_public"       
                    # > 
                    #   
                    photo.id <- info_l$id
                    
                    photo.owner <- info_l$owner #   info_l$photo$owner$nsid
                    photo.date <- as.Date.character(info_l$datetaken, tz = "GMT") # info_l$photo$dates$taken
                    photo.year <- substr(photo.date, start = 1, stop = 4)
                    photo_landcover <- NA #  as.character(aoi$LN)
                    photo_longitude <- info_l$longitude
                    photo_latitude <- info_l$latitude
                    photo.datetakengranularity<- info_l$datetakengranularity
                    photo.datetakenunknown<- info_l$datetakenunknown
                    
                    photo.title <- info_l$title
                    photo.accuracy <- info_l$accuracy
                    # Recorded accuracy level of the location information. Current range is 1-16 :
                    # World level is 1
                    # Country is ~3
                    # Region is ~6
                    # City is ~11
                    # Street is ~16
                    
                    
                    names(info_l)
                    
                    photo.tags <- strsplit(info_l$tags, " ")[[1]]
                    photo.ntag <- length(photo.tags)
                    photo.tags_delimited <- paste0(photo.tags, collapse=", ")
                    
                    photo_machinetags <- strsplit(info_l$machine_tags, " ")[[1]]
                    photo_nmachinetag <- length(photo_machinetags)
                    photo_machinetags_delimited <- paste0(photo_machinetags, collapse=", ")
                    
                    
                    photo.username <- info_l$ownername  # $owner$username
                    photo.realname <- NA # (info_l$photo$owner$realname)
                    photo.place_id <- info_l$place_id # (info_l$photo$location$place_id)
                    photo.woeid <- info_l$woeid  # (info_l$photo$location$woeid)
                    photo.geocontext <- info_l$context # 0, not defined. 1, indoors. 2, outdoors.
                    
                    
                    
                    
                    # photo.url <- info_l$url_z
                    # 
                    # if (is.null(photo.url) || is.na(photo.url) || photo.url=="") { 
                    #     
                    # farm <- info_l$farm
                    # server <- info_l$server
                    # secret <-info_l$secret
                    
                    
                    # <photo id="3909225696" secret="6bca6c9c41" server="2673" farm="3" dateuploaded="1252654170" isfavorite="0" license="0" safety_level="0" rotation="0" originalsecret="5282557aeb" originalformat="jpg" views="178" media="photo">
                    #     <owner nsid="30206765@N08" username="nemiso" realname="Guntae Park" location="" iconserver="4672" iconfarm="5" path_alias="nemiso">
                    #     
                    #     https://live.staticflickr.com/2673/3909225696_425eca2283_h.jpg
                    #     https://live.staticflickr.com/{server-id}/{id}_{secret}_{size-suffix}.jpg
                    # 
                    # [1] "https://live.staticflickr.com/2673/8435678911_6bca6c9c41_h.jpg"
                    # 
                    # <photo id="3909225696" secret="6bca6c9c41" server="2673" farm="3" dateuploaded="1252654170" isfavorite="0" license="0" safety_level="0" rotation="0" originalsecret="5282557aeb" originalformat="jpg" views="178" media="photo">
                    #     
                    # https://live.staticflickr.com/2673/3909225696_6bca6c9c41_h.jpg
                    # https://live.staticflickr.com/2673/3909225696_6bca6c9c41_h.jpg
                    
                    photo_url <- paste0("https://live.staticflickr.com/", info_l$server, "/",photo.id, "_", info_l$secret, "_", "z.jpg")
                    
                    
                    # [1] "PhotoID"          "Owner"            "Date"             "Year"             "Landcover"       
                    # [6] "Longitude"        "Latitude"         "Place_id"         "Woeid"            "Geocontext"      
                    # [11] "LocationAccuracy" "Title"            "N_FlickrTag"      "FlickrTags"       "Username"        
                    # [16] "Realname"         "URL"   
                    
                    res_l_tmp <- list(PhotoID = photo.id, Owner = photo.owner, Date = as.character(photo.date), Year = photo.year, Landcover=photo_landcover,  Longitude =  photo_longitude,Latitude =  photo_latitude, Place_id = photo.place_id, Woeid= photo.woeid, Geocontext=photo.geocontext, LocationAccuracy = photo.accuracy, DateTakenGranularity = photo.datetakengranularity,  Datetakenunknown = photo.datetakenunknown, Title= photo.title, N_FlickrTag= photo.ntag, FlickrTags = photo.tags_delimited, MachineTags = photo_machinetags_delimited,  Username = photo.username, Realname=photo.realname, URL = photo_url)
                    
                    res_tmp <- data.frame(t(sapply(res_l_tmp, FUN = function(x) ifelse(is.null(x), yes = NA, no = x))))
                    
                    
                    return(  res_tmp )
                    
                    
                }
                
            }
            
            flickrphotos_metadata_specific_df_l[[p_idx]] <- do.call(rbind, res_l1)
        }
        
        metadata_tmp <- data.frame(do.call(rbind, flickrphotos_metadata_specific_df_l))
        
        ### 
        if (nrow(metadata_tmp) == 0) { 
            print("No photo within the cell")
            writeLines("no data", con = paste0( workdir, savedir,  "/Xlsx/AOI_CellRegion_", aoi_cellregion, "/AOI_CellID_", formatC(aoi_cellid, width = 4, flag = "0"), "_n0.xlsx"))
            
            
            return(T)
            
        }
        
        
        metadata_tmp <- metadata_tmp[!is.na(metadata_tmp$PhotoID),]
        
        metadata_tmp = metadata_tmp[match(unique(metadata_tmp$PhotoID), metadata_tmp$PhotoID),]
        
        
        
        # Get information about a user.
        # api_key (Required)
        # user_id (Required) The NSID of the user to fetch information about. 
        
        owner_v = unique(as.character(metadata_tmp$Owner))
        cat("# of users :", length(owner_v), " ")
        
        userinfo_df = foreach (o_idx = 1:length(owner_v), .combine="rbind", .errorhandling = "stop") %do% {
            
            
            owner_tmp =  (owner_v[o_idx])
            cat(">", o_idx, ">")
            cat(owner_tmp)
            api_tmp <- paste0("https://api.flickr.com/services/rest/?method=flickr.profile.getProfile&format=json&api_key=", api_key, "&nojsoncallback=1&user_id=", owner_tmp )
            
            raw_data_tmp <- httr::content(httr::GET(api_tmp, httr::add_headers("--http1.1")), "text", encoding = "UTF-8") 
            
            
            # data_l_tmp[[p_idx]] <- fromJSON(raw_data_tmp, unexpected.escape="skip", method="C")
            res_tmp <- rjson::fromJSON(raw_data_tmp, unexpected.escape="skip", method="C")
            
            if (res_tmp$stat != "ok") { 
                print(paste0("error..", res_tmp$stat))
                stop("error")
            }
            Hometown = iconv(as.character(res_tmp$profile$hometown), from="UTF-8", to="ASCII", sub="")
            Hometown = ifelse(length(Hometown)==0, NA, Hometown)
            
            Country = iconv(as.character(res_tmp$profile$country), from="UTF-8", to="ASCII", sub="")
            Country = ifelse(length(Country)==0, NA, Country)
            
            City = iconv(as.character(res_tmp$profile$city), from="UTF-8", to="ASCII", sub="")
            City = ifelse(length(City)==0, NA, City)
            
            Occupation = iconv(as.character(res_tmp$profile$occupation), from="UTF-8", to="ASCII", sub="")
            Occupation = ifelse(length(Occupation)==0, NA, Occupation)
            
            api_tmp <- paste0("https://api.flickr.com/services/rest/?method=flickr.people.getInfo&format=json&api_key=", api_key, "&nojsoncallback=1&user_id=", owner_tmp )
            
            raw_data_tmp <- httr::content(httr::GET(api_tmp, httr::add_headers("--http1.1")), "text", encoding = "UTF-8") 
            
            
            # data_l_tmp[[p_idx]] <- fromJSON(raw_data_tmp, unexpected.escape="skip", method="C")
            res_tmp <- rjson::fromJSON(raw_data_tmp, unexpected.escape="skip", method="C")
            
            if (res_tmp$stat != "ok") { 
                print(paste0("error..", res_tmp$stat))
                stop("error")
            }
            UserLocation = iconv(as.character(res_tmp$person$location), from="UTF-8", to="ASCII", sub="")
            UserLocation = ifelse(length(UserLocation)==0, NA, UserLocation)
            
            
            UserURL = as.character(res_tmp$person$profileurl)
            
            return(c(UserLocation=UserLocation, UserHometown = Hometown,  UserOccupation = Occupation,UserCountry = Country,UserCity = City, UserURL=UserURL ))
        }
        # colnames(userinfo_df)
        # cbind(owner_v, userinfo_df) 
        
        if (!is.null(dim(userinfo_df))) { 
            userinfo_df_expanded = userinfo_df[match(metadata_tmp$Owner, owner_v),]
        } else {
            userinfo_df_expanded = data.frame(matrix(nrow = nrow(metadata_tmp), data=NA, ncol=length(userinfo_df)))
            userinfo_df_expanded[match(metadata_tmp$Owner, owner_v),] = userinfo_df
            # print(    userinfo_df)
            # print(    userinfo_df_expanded)
        }
        
        metadata_tmp =   cbind(metadata_tmp, userinfo_df_expanded)
        
        # print(nrow(metadata_tmp))
        # print(flickrphotos_metadata_df[i, "nallphotos"] )
        
        # stopifnot(nrow(metadata_tmp)== flickrphotos_metadata_df[i, "nallphotos"] )
        
        # saveRDS(metadata_tmp, file = paste0( workdir, savedir,  "Rds/AOI_CellID_", formatC(aoi_cellid, width = 6, flag = "0"), "_", aoi_cellregion, "_n", nrow(metadata_tmp), ".Rds"))
        
        # print("rds written")
        
        # metadata_tmp$FlickrTags <- iconv(metadata_tmp$FlickrTags, from="UTF-8", to="ASCII", sub="")
        metadata_tmp$Title <- iconv(metadata_tmp$Title, from="UTF-8", to="ASCII", sub="")
        
        
        openxlsx::write.xlsx(metadata_tmp, file = paste0(aoi_cellregion_path, "/AOI_CellID_", formatC(aoi_cellid, width = 4, flag = "0"), "_n", nrow(metadata_tmp), ".xlsx"), overwrite=T)
        
        print("xlsx written")
    }
    
    
    if ((aoi_cellid_idx %%1E5) ==0) {
        # print (paste0 ("Saving temporary results ", i, ">"))
        # save.image( file =paste0( "tmp/FlickrEU_temp_workspace_by_", i, "_",Sys.time(), ".RData"))
        
        print("gc")
        gc()
    }
    
    return(NULL)
}

# sink()				   # close the file output.txt


# save.image(paste0(workdir, savedir, "/Flickr_CR_workspace_metadata_download_17Aug2019.RData"))

if (DoSNOW) { 
    stopCluster(cl)
}