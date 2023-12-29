# install.packages("devtools")
# library(devtools)
# devtools::install_github("DougLuke/UserNetR")

library(bigmemory) # uuid
library(tidyverse)
library(data.table)

library(UserNetR)
library(stringr)
library(rgexf)
library(openxlsx)
library(igraph)
# library(rgdal)
library(scales)
library(ggplot2)

# library(reshape2)
library(plyr)
library(doSNOW)
library(abind)

library(sf)
library(raster)


# 1. Load data

# Mac Alan 
proj4_LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj4_UTM52N <- "+proj=utm +zone=52 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" # UTM52N (WGS84) EPSG:32652


path_base = "~/Dropbox/KIT/CES_SEOUL/CESKR/"
path_out_csv = "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/MergedCSV/"

path_xls = "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_Download/Sep2023_Korea_V2/Xlsx/"
path_out_network =  "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/Network/"

path_GIS = "~/Dropbox/GIS Data/"

### AOI 1 km for PUD/APUD

AOI_1km = read_sf(paste0(path_GIS, "Korea/Admin/", "SGIS/SGIS_Grid_1K.shp"))
AOI_1km$path = NULL
AOI_1km$layer = NULL

AOI_1km_utm = AOI_1km %>% st_transform(crs = proj4_UTM52N)


# nrow(AOI_1km)

# SGIS 1km 격자
AOI_boundary <- st_union(AOI_1km)

kalphabet = "가나다라마바사아자차카타파하"
kalphabet_v = strsplit(kalphabet, split = "")[[1]]
alphabet_v = LETTERS[seq_along(kalphabet_v)]

nvec = alphabet_v
names(nvec) = kalphabet_v

# str_replace_all(kalphabet, pattern = kalphabet_v,  replacement = alphabet_v)
# str_replace_all(kalphabet, pattern = c("가" = "A", "다" = "C"))
# str_replace_all(kalphabet, pattern = nvec) # use named vector
AOI_1km$gid_10km  = str_replace_all(substr(AOI_1km$GRID_1K_CD,1,4), pattern = nvec) # use named vector




path_out_final =  "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/Final/"

years = 2005:2023

setwd(path_base)


proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468




places_all_m = readRDS(paste0(path_out_network, "places_all_m.Rds"))

imgnet_all_m = readRDS(paste0(path_out_network, "/imagenet_all_m.Rds"))
imgnet_all_m2 = readRDS(paste0(path_out_network,"/imagenet_all_reduced_m2.rds"))
imgnet_all_m3 = readRDS(paste0(path_out_network, "imagenet_all_reduced_m3.rds")) 


imgnet_all_id_v = colnames(imgnet_all_m) # all 
imgnet_occurred = colnames(imgnet_all_m3) # threshold (> 1E-5)

cw_imgnet21k = readRDS(paste0(path_out_network, "/cw_IMAGENET_reduced.Rds"))

k_v = 1:200
cluster_df = sapply(k_v, FUN = function(x) cut_at(cw_imgnet21k, no = x))

# stopifnot
stopifnot(all(cw_imgnet21k$names == imgnet_occurred))


imgnet_occurred_freq = diag(imgnet_all_m3)
rm(imgnet_all_m, imgnet_all_m2, imgnet_all_m3)

# k_v[98]
# summary(as.numeric(table(cluster_df[,98])))
# tb1 = table(cluster_df[,98])
# length(tb1[tb1>9])


# cls_tmp = read.xlsx( paste0("Data/ClusterInfo_", dt_name, "_gte5.xlsx"))



imgnet21k_tags_ids = readLines("Data/imagenet21k_wordnet_ids.txt") %>% as.vector
imgnet21k_tags_verbose = readLines("Data/imagenet21k_wordnet_lemmas.txt")
imgnet21k_tags = as.vector(imgnet21k_tags_verbose)

imgnet21k_tags_df = data.frame(ID = imgnet21k_tags_ids, Tag = imgnet21k_tags) 


imgnet21k_tags_unique = unique(imgnet21k_tags)


K_optimal_imgnet21k = 61
K_optimal_places365 = 4


tag_cluster_v = (cluster_df[,which(k_v == K_optimal_imgnet21k)])

colnames(tag_cluster_v)

names(tag_cluster_v) =imgnet_occurred

str(tag_cluster_v)
tag_cluster_id_v = imgnet21k_tags_df$ID[match( names(tag_cluster_v), imgnet21k_tags_df$Tag)]


imgnet_all_tag_v = imgnet21k_tags_df$Tag[match( imgnet_all_id_v, imgnet21k_tags_df$ID)]


tag_occured_df = cbind(ID =tag_cluster_id_v, Cluster = as.numeric(tag_cluster_v), Tag = names(tag_cluster_v), Freq = imgnet_occurred_freq )


# cluster look-up table

imgnet_all_df = data.frame(ID = imgnet_all_id_v, Tag = imgnet_all_tag_v)

# imgnet_all_df$Cluster[match( imgnet_all_tag_v, names(tag_cluster_v))] = as.numeric(tag_cluster_v)[match(  imgnet_all_tag_v, names(tag_cluster_v))]

imgnet_all_df2 = left_join(imgnet_all_df, tag_occured_df, by = "ID", copy=TRUE, keep=F)
imgnet_all_df2$Tag.y = NULL
imgnet_all_df2$Tag = imgnet_all_df2$Tag.x
imgnet_all_df2$Tag.x = NULL

head(imgnet_all_df2)

if (FALSE) { 
    
    # Profiling major clusters (occurrence >= 5 )
    
    tb1 = (sort(table(tag_cluster_v), decreasing = F))
    # tb1_gt5 = rev(tb1[tb1>=5]) # 
    
    # summary(as.numeric(tb1))
    # head(tag_cluster_v[order(tag_cluster_v, decreasing = T)])
    
    
    tags_by = by(tag_occured_df, INDICES = tag_cluster_v, FUN = function(x) x)
    
    # tags_by2 = tags_by[names(tb1_gt5)]
    
    library(data.table)
    
    # wrong. ignored the frequency of the tags
    tags_sorted = lapply(tags_by, FUN = function(x) x$Tag[order(x$Freq, decreasing = T)  ]  %>% t %>% data.table)
    
    tags_sorted_dt <- data.table::rbindlist(tags_sorted, fill=T) 
    tags_sorted_df = as.data.frame(tags_sorted_dt)
    
    rownames(tags_sorted_df) = paste0("Cluster_", names(tags_by))
    colnames(tags_sorted_df) = paste0("Tag_", 1:ncol(tags_sorted_df))
    
    tags_sorted_df = cbind(ES = NA, Description =NA, tags_sorted_df)
    
    write.xlsx(tags_sorted_df, rowNames=T, file = paste0(path_out_network, "/imagenet21k_tagssorted_optimalK", K_optimal_imgnet21k,".xlsx"))
    
    
    
}







### Activity PUD


### imgnet 21k tags






# meta data files
xls_V = list.files(path_xls, pattern="\\.xlsx$", full.names = T, recursive = T)


n_points = length(xls_V)

# idxs = sample(10000:20000)
idxs = 1:n_points



# this is just to figure out cluster per polygon, which is 10 km in this case.. don't need. 

if (FALSE) { 
    # idxs = 19053
    
    # write cluster information for each photo
    foreach (i =rev(idxs), .errorhandling = "stop") %do% {
        # for (i in (idxs)) {
        
        if ((i %% 1 ) ==0) {
            cat("AOI_", i, ">")
        }
        in_filename_tmp = paste0(path_out_csv, "/", basename(xls_V[i])) 
        out_filename_tmp = paste0(path_out_final, "/", basename(xls_V[i])) 
        
        
        if (file.exists(out_filename_tmp)) {
            # next
            return()
        }
        
        
        # aoi_tmp = str_extract(xls_SDG_V[i], pattern = "Poly\\_[0-9]*")
        
        # aoi_tmp = paste0("AOI_", as.numeric(substr(aoi_tmp, start = 6, 11)))
        
        cell_id_tmp = str_extract(xls_V[i], pattern = "(?<=CellID\\_)[A-Z0-9]*")
        aoi_tmp = paste0("AOI_", cell_id_tmp)
        
        cluster_pts = NULL
        
        if (file.exists(in_filename_tmp)) { 
            dt = read.xlsx(in_filename_tmp)
            
            if (nrow(dt)==0) {
                # next
                return()
            }
        } else { 
            return()
        }
        
        # Imagenet 
        imgnet_df = dt[, paste0("IMGNET_21k_Top", 1:10)] 
        imgnet_df[imgnet_df==""] = NA
        
        
        imgnet_Cluster_df = lapply(imgnet_df,FUN = function(x) imgnet_all_df2$Cluster[match(x, imgnet_all_df2$Tag)]) %>% data.frame
        
        imgnet_Cluster_df[is.na(imgnet_Cluster_df)] = 0 # cluster 0
        
        imgnet_prob_df = dt[, paste0("IMGNET_21k_Prob", 1:10)] 
        imgnet_prob_df[imgnet_prob_df==""] = NA
        
        
        
        # weighted counting
        cluster_tmp_v =  foreach(x = 1:nrow(imgnet_df), .combine = c) %do% { 
            
            imgnet_prob_df[x,]
            imgnet_Cluster_df[x,]
            
            weighedsum = tapply(as.numeric(imgnet_prob_df[x,]), INDEX =as.character(imgnet_Cluster_df[x,]), FUN = sum )
            
            
            wt_prop = (weighedsum / sum(weighedsum))
            
            
            cluster_tmp = names(wt_prop)[which.max(wt_prop)]
            return(cluster_tmp )
        }
        
        
        dt$FlickrClusterOpt = cluster_tmp_v
        
        # for each photo
        write.xlsx(dt, file = out_filename_tmp)
        
    }
}


### ES/non-ES

ES_tb = read.xlsx(paste0(path_out_network, "/imagenet21k_tagssorted_optimalK61_Reviewed.xlsx"), 1)


# ES_tb = ES_tb[ES_tb$ES == "Y",]

ES_cluster_v = which(ES_tb$ES == "Y")

# i = 100 

# idxs = 19053



##### APUD and Cluster


# idxs = 19053

apud_columns = c(paste0("APUD_", years), "APUD_Total")

i = 300 






points_sf_tmp = foreach (i = (idxs)[], .errorhandling = "stop") %do% {
    # for (i in (idxs)) {
    
    if ((i %% 1 ) ==0) {
        cat("AOI_", i, ">")
    }
    in_filename_tmp = paste0(path_out_csv, "/", basename(xls_V[i]))
    
    cell_id_tmp = str_extract(xls_V[i], pattern = "(?<=CellID\\_)[A-Z0-9]*")
    aoi_tmp = paste0("AOI_", cell_id_tmp)
    
    cluster_pts = NULL
    
    if (file.exists(in_filename_tmp)) { 
        dt = read.xlsx(in_filename_tmp)
        
        if (nrow(dt)==0) {
            # next
            return()
        }
    } else { 
        return()
    }
    
    # 1km cell
    # spdf_tmp = SpatialPointsDataFrame(cbind(as.numeric(dt$Longitude), as.numeric(dt$Latitude)), proj4string = crs(proj4.LL), data = dt)
    # spdf_tmp = spTransform(spdf_tmp, CRSobj = crs(proj4.UTM52N))
    
    sf_tmp <- sf::st_as_sf(dt, coords = c("Longitude","Latitude"), crs =proj4_LL ) %>% st_transform(crs = proj4_UTM52N)
    
    
    return(sf_tmp)
    
}

df_names = names(points_sf_tmp[[3]])

# Column names not identical? 
points_sf_tmp = foreach(x = points_sf_tmp) %do% { 
    if (is.null(x)) { return(NULL)}
    names(x) = df_names
    return(x)
}

points_sf = do.call(rbind, points_sf_tmp)
nrow(points_sf)

# write_sf(points_sf, "FlickrKor_AllPoints_BySep2023.gpkg")

# names(points_sf)


imgnet_dt = points_sf[, c(25:34)] %>% data.table
imgnet_dt$geometry = NULL

# look-up cluster info
imgnet_Cluster_dt = apply(imgnet_dt, MARGIN = 2, FUN = function(x) {
    x1 = imgnet_all_df2$Cluster[match(x, imgnet_all_df2$Tag)]
    as.numeric(x1)
}) %>% data.table


# head(imgnet_Cluster_dt)


### Overlaying

FlickrInfo_1km = AOI_1km_utm
FlickrInfo_1km_joined = sf::st_join(points_sf, FlickrInfo_1km, join = st_covered_by, left=T)
str(FlickrInfo_1km_joined)

FlickrInfo_1km_joined$geometry = NULL

stopifnot(nrow(points_sf) == nrow(FlickrInfo_1km_joined))
# nrow(FlickrInfo_1km)

# table(FlickrInfo_1km_joined$GRID_1K_CD) # 13786 1km cells


# PUD
selected_columns = c("PhotoID", "Date", "Owner", "Year",paste0("IMGNET_21k_Top", 1:10),  paste0("IMGNET_21k_Prob", 1:10), paste0("PLACES365_Top", 1:10), paste0("PLACES365_Prob", 1:10))
Photos_by1km_l = tapply(FlickrInfo_1km_joined[,selected_columns], INDEX= FlickrInfo_1km_joined$GRID_1K_CD, FUN = rbind)
length(Photos_by1km_l)


x = Photos_by1km_l[[1]]

PUD_res_l = lapply(Photos_by1km_l, FUN = function(x) { 
    
    # Imagenet 
    imgnet_df = x[, paste0("IMGNET_21k_Top", 1:10)] 
    
    imgnet_df[imgnet_df==""] = NA
    
    imgnet_prob_df = x[, paste0("IMGNET_21k_Prob", 1:10)] 
    imgnet_prob_df[imgnet_prob_df==""] = NA
    
    # Places 
    places_df = x[, paste0("PLACES365_Top", 1:10)] 
    
    places_df[places_df==""] = NA
    
    places_prob_df = x[, paste0("PLACES365_Prob", 1:10)] 
    places_prob_df[places_prob_df==""] = NA
    
    
    # Total PUD
    # calculate number of duplicates 
    duplicated_by =  by(x$PhotoID, INDICES = list(x$Owner, x$Date), FUN = length, simplify =F)
    
    # N of duplicates per each photo
    noofduplicates_v = sapply(1:nrow(x), FUN = function(x1) {
        duplicated_by[[ x[x1,"Owner"], x[x1,"Date"] ]]
        
    })
    
    # Decreases
    imgnet_prob_df_adjusted =  sapply(1:nrow(imgnet_prob_df), FUN = function(x) imgnet_prob_df[x,] / noofduplicates_v[x]) %>% t 
     
    imgnet_prob_df_adjusted = matrix(unlist(imgnet_prob_df_adjusted), ncol = ncol(imgnet_prob_df_adjusted))
    
    
    # Assign Cluster  
    imgnet_Cluster_df = lapply(imgnet_df,FUN = function(x) imgnet_all_df2$Cluster[match(x, imgnet_all_df2$Tag)]) %>% data.frame
    
    imgnet_Cluster_df[is.na(imgnet_Cluster_df)] = 0 # cluster 0
    
    
    # weighted CES Y/N 
    CESYN_m = apply(imgnet_Cluster_df, 2, function(x) ifelse(x %in% ES_cluster_v, 1, 0)) 
     
    imgnet_prob_df_adjusted_CES = imgnet_prob_df_adjusted * CESYN_m
    
    # counting weighted PUD
    PUD_annual = by(imgnet_prob_df_adjusted, INDICES= x$Year, FUN = function(x2) sum(unlist(x2)))
    PUD_CES_annual = by(imgnet_prob_df_adjusted_CES, INDICES= x$Year, FUN = function(x2) sum(unlist(x2)))
    
    dummy_m = matrix(NA, nrow = length(years), ncol = 2)
    dimnames(dummy_m)[[1]] = years
    dummy_m[names(PUD_annual),1] = PUD_annual
    dummy_m[names(PUD_CES_annual),2] = PUD_CES_annual
    
    
    return(dummy_m)
    
})



flickr_idx = match(names(PUD_res_l), AOI_1km_utm$GRID_1K_CD)

PUD_annual_dt = abind(PUD_res_l, along = 3)
dim(PUD_annual_dt)

sum(PUD_annual_dt[,2,], na.rm=T)

PUD_total_dt = apply(PUD_annual_dt, MARGIN = c(3,2), FUN = sum, na.rm=T)
dim(PUD_total_dt)

PUD_total_dt_nonzero = PUD_total_dt
PUD_total_dt_nonzero[PUD_total_dt_nonzero==0] = NA

boxplot(PUD_total_dt, n=200, col = "blue", add=F, names =c("PUD", "PUD_ES")) 
 
 
# Total
PUD_total_sf = AOI_1km_utm
PUD_total_sf$PUD = PUD_total_sf$PUD_ES =PUD_total_sf$PUD_Diff = NA
PUD_total_sf$PUD[flickr_idx] =PUD_total_dt[,1]
PUD_total_sf$PUD_ES[flickr_idx] =PUD_total_dt[,2] 
PUD_total_sf$PUD_Diff[flickr_idx] =PUD_total_dt[,1] - PUD_total_dt[,2] 

PUD_total_sf$GRID_1K_CD = NULL

# Annual
PUD_annual_sf = AOI_1km_utm
PUD_annual_sf[, paste0("PUD", years)] = NA 
PUD_annual_sf[, paste0("PUD_ES", years)] = NA 

# PUD_annual_sf$geometry = NULL

PUD_annual_sf[flickr_idx, paste0("PUD", years)] = t(PUD_annual_dt[,1,])
PUD_annual_sf[flickr_idx, paste0("PUD_ES", years)] = t(PUD_annual_dt[,2,])
PUD_annual_sf$GRID_1K_CD = NULL

 
# PUD_annual_sf$geometry = AOI_1km_utm$geometry


write_sf(PUD_total_sf, "Flickr_KOR_PUD_Total.shp")
write_sf(PUD_annual_sf, "Flickr_KOR_PUD_Annual.shp")




dummy_1km_r = terra::rast(PUD_sf, res = 1000)
 
PUD_Total_rast = terra::rasterize(PUD_total_sf, dummy_1km_r, field = "PUD", fun = max )
                                  
PUD_ES_Total_rast = terra::rasterize(PUD_total_sf, dummy_1km_r, field = "PUD_ES", fun = max )

PUD_Diff_Total_rast = terra::rasterize(PUD_total_sf, dummy_1km_r, field = "PUD_Diff", fun = max )

PUD_rs = terra::rast(list(PUD_Total_rast,PUD_ES_Total_rast, PUD_Diff_Total_rast))


PUD_rs_LL = terra::project(PUD_rs, proj4_LL, method = "bilinear")


pdf("Flickr_KOR_PUD_2005-2023_Total.pdf", height = 12, width = 8)
terra::plot(PUD_rs_LL)
dev.off()



terra::writeRaster(PUD_rs, "FlickrKOR_PUD_2005-2023_Total.tif")
 


PUD_annual_rast_l = foreach(y = years) %do% {
    terra::rasterize(PUD_annual_sf, dummy_1km_r, field = paste0("PUD", y), fun = max )
}
PUD_ES_annual_rast_l = foreach(y = years) %do% {
    terra::rasterize(PUD_annual_sf, dummy_1km_r, field = paste0("PUD_ES", y), fun = max )
}


PUD_annual_rs = terra::rast(PUD_annual_rast_l)
PUD_ES_annual_rs = terra::rast(PUD_ES_annual_rast_l)


terra::writeRaster(PUD_annual_rs, filename = paste0("FlickrKOR_PUD_annual.tif"), overwrite=T) # ,bylayer= T, suffix = years,  format="GTiff")
writeRaster(PUD_ES_annual_rs, filename = paste0("FlickrKOR_PUD_ES_annual.tif")) # , bylayer= T, suffix = apud_columns, overwrite=T, format="GTiff")




stop("ends here")




if (FALSE) { 
    
    apud_df = matrix(data = 0, nrow = nrow(AOI_1km), ncol = length(apud_columns) ) %>% data.frame
    colnames(apud_df) = apud_columns
    # library(doMC)
    # registerDoMC(1)
    
    i= 500 
    # write cluster information for each photo
    res_tmp = foreach (i = (idxs)[], .errorhandling = "stop") %do% {
        # for (i in (idxs)) {
        
        if ((i %% 1 ) ==0) {
            cat("AOI_", i, ">")
        }
        # in_filename_tmp = paste0(path_out_final, "/", basename(xls_V[i])) 
        in_filename_tmp = paste0(path_out_csv, "/", basename(xls_V[i]))
        # out_filename_tmp = paste0(path_out_final, "/", basename(xls_SDG_V[i])) 
        # 
        # 
        # if (file.exists(out_filename_tmp)) {
        #     # next
        #     return()
        # }
        
        
        cell_id_tmp = str_extract(xls_V[i], pattern = "(?<=CellID\\_)[A-Z0-9]*")
        aoi_tmp = paste0("AOI_", cell_id_tmp)
        
        cluster_pts = NULL
        
        if (file.exists(in_filename_tmp)) { 
            dt = read.xlsx(in_filename_tmp)
            
            if (nrow(dt)==0) {
                # next
                return()
            }
        } else { 
            return()
        }
        
        # 1km cell
        # spdf_tmp = SpatialPointsDataFrame(cbind(as.numeric(dt$Longitude), as.numeric(dt$Latitude)), proj4string = crs(proj4.LL), data = dt)
        # spdf_tmp = spTransform(spdf_tmp, CRSobj = crs(proj4.UTM52N))
        
        sf_tmp <- sf::st_as_sf(dt, coords = c("Longitude","Latitude"), crs =proj4_LL ) %>% st_transform(crs = proj4_UTM52N)
        
        a = st_within(sf_tmp, AOI_1km_utm, sparse = T)
        AOI_1km_cells_tmp  = AOI_1km_utm[unlist(a),]
        
        plot_sf(AOI_1km_cells_tmp[])
        plot(sf_tmp[,1], add=F)
        
        AOI_1km_cells_tmp = sf::st_intersects(AOI_1km_utm, y = sf_tmp, sparse = T)
        
        write_sf(AOI_1km_cells_tmp, "AOI_1km_cells_tmp.gpkg")
        write_sf(sf_tmp, "sf_tmp.gpkg")
        
        
        # spdf_tmp$Owner = factor(spdf_tmp$Owner)
        # spplot(spdf_tmp, "Owner")
        
        
        # plot(AOI_1km_cells_tmp[,1], add=F, col="red")
        # plot(AOI_boundary %>% st_transform(crs = proj4_UTM52N), add=F)
        # plot(sf_tmp[,1], add=T, col="blue")
        # plot(AOI_1km_cells_tmp[,1], add=T, col="red")
        # 
        # 
        # plot(AOI_1km_cells_tmp[,1], add=F, col="red")
        # plot(sf_tmp[,1], add=T, col="blue")
        # 
        #  p1 = ggplotGrob(
        #      ggplot(sf_tmp) +
        #     geom_sf(aes(fill = Year)) + 
        #      geom_sf(data = AOI_1km_cells_tmp, colour = "red", fill = NA) + 
        #         geom_rect(
        #             aes(xmin = -80, xmax = -79, ymin = 30, ymax = 40), fill = NA, 
        #             col = "red"
        #         )
        #     
        #  )
        #  plot(p1)
        # + geom_sf(data = AOI_boundary, colour = "blue", fill = NA)
        
        
        
        # 
        # plot(sf_tmp[,3])
        # plot(AOI_1km_cells_tmp[,1], add=T, col="red")
        
        # AOI_join = sf::st_join(AOI_1km_cells_tmp, st_as_sf(sf_tmp))
        # AOI_join = AOI_join[!is.na(AOI_join$PhotoID),]
        
        # subcell_v = unique(sdg_join$CELL_ID_1km)
        
        
        # x1 = sf::st_within(sf_tmp, AOI_1km_utm)
        
        AOI_covered = st_covered_by(sf_tmp, AOI_1km_utm, sparse = T)
        
        subcell_v = as.numeric(AOI_covered)
        
        if (is.na(subcell_v) %>% all) { 
            print(i)
            print("no overlap")
            return()
            
        }
        
        # ggplot(sf_tmp[,]) +
        #          geom_sf(aes(fill = Year)) + 
        #           geom_sf(data = AOI_bo[,], colour = "red", fill = NA) 
        
        # j = 4
        # year_tmp = 2020
        
        if (length(subcell_v)==0) { 
            return()
        }
        
        subcell_cluster_l = foreach (j = 1:length(subcell_v)) %do% { 
            
            # cat(j)
            
            subcell_apud_annual = foreach (year_tmp = years, .combine = c) %do% { 
                
                
                # cat(year_tmp, ">")
                
                sdg_subcell_tmp = sdg_join[(sdg_join$CELL_ID_1km == subcell_v[j]) & (sdg_join$Year == year_tmp), ]
                
                if (nrow(sdg_subcell_tmp)==0) { 
                    # cat("no data>")
                    return(c(NA))    # NA
                }
                
                
                sdg_subcell_tmp$geometry = NULL
                # sdg_subcell_tmp = sdg_subcell_tmp[, c("Owner", "Date"
                
                # Imagenet 
                imgnet_df = sdg_subcell_tmp[, paste0("IMGNET_21k_Top", 1:10)] 
                
                imgnet_df[imgnet_df==""] = NA
                
                imgnet_prob_df = sdg_subcell_tmp[, paste0("IMGNET_21k_Prob", 1:10)] 
                imgnet_prob_df[imgnet_prob_df==""] = NA
                
                # calculate number of duplicates 
                duplicated_by =  by(sdg_subcell_tmp$PhotoID, INDICES = list(sdg_subcell_tmp$Owner, sdg_subcell_tmp$Date), FUN = length, simplify =F)
                
                noofduplicates_v = sapply(1:nrow(sdg_subcell_tmp), FUN = function(x) {
                    duplicated_by[[sdg_subcell_tmp[x,]$Owner, sdg_subcell_tmp[x,]$Date]]
                    
                })
                
                # Decrease 
                imgnet_prob_df_adjusted =  t(sapply(1:nrow(imgnet_prob_df), FUN = function(x) imgnet_prob_df[x,] / noofduplicates_v[x]))
                
                
                
                imgnet_Cluster_df = lapply(imgnet_df,FUN = function(x) imgnet_all_df2$Cluster[match(x, imgnet_all_df2$Tag)]) %>% data.frame
                
                imgnet_Cluster_df[is.na(imgnet_Cluster_df)] = 0 # cluster 0
                
                
                # weighted counting
                weightedsum = tapply(unlist(imgnet_prob_df_adjusted), INDEX =unlist(imgnet_Cluster_df), FUN = sum )
                
                
                wt_prop = (weightedsum / sum(weightedsum))
                
                # representative cluster
                cluster_tmp = names(wt_prop)[which.max(wt_prop)]
                
                # APUD
                sdg_subcell_tmp_ES = sdg_subcell_tmp[sdg_subcell_tmp$FlickrClusterK107 %in% ES_tb$Cluster,]
                if (nrow(sdg_subcell_tmp_ES)==0) { 
                    return(c(NA))    # NA
                }
                
                # calculate number of duplicates 
                duplicated_by =  by(sdg_subcell_tmp_ES$PhotoID, INDICES = list(sdg_subcell_tmp_ES$Owner, sdg_subcell_tmp_ES$Date), FUN = length, simplify =F)
                
                noofduplicates_v = sapply(1:nrow(sdg_subcell_tmp_ES), FUN = function(x) {
                    duplicated_by[[sdg_subcell_tmp_ES[x,]$Owner, sdg_subcell_tmp_ES[x,]$Date]]
                    
                })
                
                imgnet_df = sdg_subcell_tmp_ES[, paste0("IMGNET_21k_Top", 1:10)] 
                
                imgnet_df[imgnet_df==""] = NA
                
                imgnet_prob_df = sdg_subcell_tmp_ES[, paste0("IMGNET_21k_Prob", 1:10)] 
                imgnet_prob_df[imgnet_prob_df==""] = NA
                
                
                # Decrease 
                imgnet_prob_df_adjusted =  t(sapply(1:nrow(imgnet_prob_df), FUN = function(x) imgnet_prob_df[x,] / noofduplicates_v[x]))
                
                
                # counting weighted PUD
                weightedPUD = sum(unlist(imgnet_prob_df_adjusted))
                
                # return(c(cluster_tmp, weightedPUD))
                
                return(c(weightedPUD))
            }
            
            
            # if (is.null(dim(subcell_apud_annual)) || dim(subcell_apud_annual)[2] ==1) { 
            #     apud_df[match(subcell_v[j], sdg_pud_1km$CELL_ID_),  ] = c(subcell_apud_annual, sum(subcell_apud_annual))
            # } else { 
            #     
            subcell_apud_annual = as.numeric(subcell_apud_annual)
            
            # sdg_pud_1km@data[match(subcell_v, sdg_pud_1km$CELL_ID_),"PUD"] = subcell_pud_df
            apud_df[match(subcell_v[j], sdg_pud_1km$CELL_ID_),  ] = c(subcell_apud_annual, sum(subcell_apud_annual, na.rm=T))
            # }
        }
        
        
        # subcell_cluster_arr = abind::abind(subcell_cluster_l, along = 0)
        
        # return(subcell_cluster_arr)
    } 
    
    
    saveRDS(apud_df, file = paste0(path_out_final, "apud_df.Rds"))
} else {
    apud_df = readRDS(file = paste0(path_out_final, "apud_df.Rds"))
}
sdg_apud_1km = as_Spatial(sdg_aoi_1km)
colnames(apud_df) = apud_columns # c(paste0("AP", years), "APTotal")

sdg_apud_1km$CELL_ID_ = NULL

# sdg_apud_1km$APUD = apud_df$APUD_Total
sdg_apud_1km@data = cbind(sdg_apud_1km@data, apud_df)



# plot(st_as_sf(sdg_pud_1km))
# library(terra)

writeOGR(sdg_apud_1km, dsn = paste0(path_out_final),layer = paste0( "FlickrSDG_APUD_1km_", Sys.Date(), ".shp"), driver = "ESRI Shapefile", overwrite_layer = T)

# write_sf(st_as_sf(sdg_apud_1km), dsn = paste0(path_out_final), layer = paste0( "FlickrSDG_APUD_1km_22July2022"), driver = "gpkg", overwrite_layer = T)
# 
# st_write(st_as_sf(sdg_apud_1km), dsn = "nc1.shp", layer = "nc.shp", driver = "ESRI Shapefile")

## rasterize
dummy_r = raster(paste0(path_data, "GIS/sdg_r_dummy_1km.tif"))



sdg_apud_1km_rs =  foreach(apud_column = apud_columns) %do% { 
    print(apud_column)
    raster::rasterize(sdg_apud_1km[, apud_column], field=apud_column, y = dummy_r)
}

names(sdg_apud_1km_rs) = apud_columns

sdg_apud_1km_rs$APUD_Total = sum(sdg_apud_1km_rs[[apud_columns[1:18]]], na.rm=T)

sdg_apud_1km_rs = stack(sdg_apud_1km_rs)
# sdg_pud_1km_rs

plot(sdg_apud_1km_rs)

writeRaster(sdg_apud_1km_rs, filename = paste0(path_out_final, "/FlickrSDG_APUD_1km_22July2022.tif"), bylayer= T, suffix = apud_columns, overwrite=T, format="GTiff")

# 
# ### 
# 
# ###
# ### Creating gridded APUD shapefiles###
# ### Requires: AOI and output from `addingUserData.R`
# 
# library(tidyverse)
# library(sf)
# library(lubridate)
# 
# 
# # reproject both to WGS84 UTM10N
# photos_10 <- st_transform(photos, crs = 32610)
# aoi_10 <- st_transform(aoi, crs = 32610)
# 
# top1_pts <- photos_10 %>% dplyr::select(Top1, userid, date_taken)
# 
# # drop out noactivity and NA, and flooding
# # as of 5/18/21, also removing "fishing" and "trailrunning" because of poor model performance
# # and "horseriding" on 7/15/21
# top1_pts_sub <- top1_pts %>% 
#     filter(!Top1 %in% c("noactivity", "pplnoactivity", "flooding", "fishing", "trailrunning", "horseriding"),
#            !is.na(Top1))
# 
# # make grid
# aoi_hex_sfc <- st_make_grid(aoi_10, cellsize = 2000, square = TRUE) 
# # assign cids (cell ids)
# cids <- 1:length(aoi_hex_sfc)
# aoi_hex <- st_sf(cid = cids, geometry = aoi_hex_sfc)
# 
# #ggplot(aoi_hex_in) +geom_sf()
# 
# # remove hexes outside of aoi
# aoi_hex_in <- aoi_hex[aoi_10, op = st_intersects]
# 
# # intersect with grid
# top1_int <- st_intersection(top1_pts_sub, aoi_hex_in)
# 
# # calculate APUD per grid cell
# top1_unique <- top1_int %>%
#     st_drop_geometry() %>%
#     mutate(date = date(date_taken)) %>%
#     dplyr::select(-date_taken) %>%
#     distinct()
# 
# daily_apud <- top1_unique %>%
#     group_by(Top1, cid, date) %>%
#     summarise(APUD = n())
# daily_apud
# 
# annual_apud <- daily_apud %>%
#     group_by(Top1, cid, year = year(date)) %>%
#     summarise(APUD = sum(APUD))
# 
# total_apud <- daily_apud %>%
#     group_by(Top1, cid) %>%
#     summarise(APUD = sum(APUD))
# total_apud
# 
# 
# # now, make sure that I have every cid*activity combo
# total_apud_com <- total_apud %>%
#     complete(Top1, cid = aoi_hex_in$cid, fill = list(APUD = NA))
# total_apud_com
# 
# # make total_apud spatial again
# total_apud_sf <- aoi_hex %>%
#     left_join(total_apud_com)
# total_apud_sf
# 
# ########### How about diversity of activities? #########
# ## Calculate number of different activities occuring in each cid (equivalent to activity "richness", ala species richness)
# activities_cid <- total_apud_com %>%
#     group_by(cid) %>%
#     summarise(activities = sum(!is.na(APUD)),
#               APUD = sum(APUD, na.rm = TRUE),
#               act_p_APUD = activities/log1p(APUD))
# activities_cid
# 
# activities_sf <- aoi_hex %>% 
#     left_join(activities_cid)
# 
# num_act_plot <- ggplot(activities_sf) + # %>% filter(activities != 0)) +
#     #geom_sf(data = aoi_hex) +
#     geom_sf(aes(fill = activities), size = .1) +
#     scale_fill_viridis_c() +
#     labs(title = paste(location, "Number of Distinct Activities"))
# num_act_plot
# 
# # Ok. Let's examine this a bit, and write it out
# activities_sf %>%
#     arrange(desc(activities))
# # cid 45 & 141 (mtnloop) have 10 distinct activities. I think this is out of 11 possible - let's check
# # middlefork: cid 45 has 11, cids 10 and 149 each have 10 activities
# 
# total_apud_com %>% filter(cid == "45") #hmm. actually out of 12. None for fishing or trailrunning
# total_apud_com %>% filter(cid == "141") # missing the same 2
# 
# # write out the diversity grid
# st_write(activities_sf, paste0("GIS/", location, "_activities_by_grid_", dddd, ".geojson"))
# 
# 
# 
# 
# # let's at least look at the correlation between number activities and total apud
# ggplot(activities_cid) +
#     geom_point(aes(y = activities, x = log(APUD)))
# 
# 











