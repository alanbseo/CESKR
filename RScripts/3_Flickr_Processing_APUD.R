# install.packages("devtools")
# library(devtools)
# install_github("DougLuke/UserNetR")

library(bigmemory)
library(tidyverse)

library(UserNetR)
library(stringr)
library(rgexf)
library(openxlsx)
library(igraph)
library(rgdal)
library(scales)
library(gplots)
library(reshape2)
library(raster)
library(plyr)
library(doSNOW)
library(abind)

library(sf)



# 1. Load data

# Mac Alan 
proj4.LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


path_base = "~/Dropbox/KIT/CES_SEOUL/CESKR/"
path_out_csv = "~/Dropbox/KIT/CES_SEOUL/CESKR/FlickrSDG_result/MergedCSV/"

path_SDG_xls = "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_download/July2022_V1/Xlsx/"
path_out_network =  "~/Dropbox/KIT/CES_SEOUL/CESKR/FlickrSDG_result/Network/"


path_out_final =  "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_result/Final/"

years = 2005:2022

setwd(path_base)

proj4.LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468
places_all_m = readRDS( "Data/places_all_m.Rds")


imgnet_all_m = readRDS("Data/imagenet_all_m.Rds")
imgnet_all_m2 = readRDS(file = "Data/imagenet_all_reduced_m2.rds")
imgnet_all_m3 = readRDS(file = "Data/imagenet_all_reduced_m3.rds")


imgnet_all_id_v = colnames(imgnet_all_m)
imgnet_occurred = colnames(imgnet_all_m3)

cw = readRDS("Data/cw_gte5_reduced.Rds")

k_v = 10:200
cluster_df = sapply(k_v, FUN = function(x) cut_at(cw, no = x))

k_v[98]
summary(as.numeric(table(cluster_df[,98])))
tb1 = table(cluster_df[,98])
length(tb1[tb1>9])


# cls_tmp = read.xlsx( paste0("Data/ClusterInfo_", dt_name, "_gte5.xlsx"))



imgnet21k_tags_ids = readLines("Data/imagenet21k_wordnet_ids.txt") %>% as.vector
imgnet21k_tags_verbose = readLines("Data/imagenet21k_wordnet_lemmas.txt")
imgnet21k_tags = as.vector(imgnet21k_tags_verbose)

imgnet21k_tags_df = data.frame(ID = imgnet21k_tags_ids, Tag = imgnet21k_tags) 


imgnet21k_tags_unique = unique(imgnet21k_tags)

tag_cluster_v = (cluster_df[,98])
names(tag_cluster_v) =imgnet_occurred
tag_cluster_v
tag_cluster_id_v = imgnet21k_tags_df$ID[match( names(tag_cluster_v), imgnet21k_tags_df$Tag)]


imgnet_all_tag_v = imgnet21k_tags_df$Tag[match( imgnet_all_id_v, imgnet21k_tags_df$ID)]


tag_occured_df = cbind(ID =tag_cluster_id_v, Cluster = as.numeric(tag_cluster_v), Tag = names(tag_cluster_v))


# cluster look-up table

imgnet_all_df = data.frame(ID = imgnet_all_id_v, Tag = imgnet_all_tag_v)

# imgnet_all_df$Cluster[match( imgnet_all_tag_v, names(tag_cluster_v))] = as.numeric(tag_cluster_v)[match(  imgnet_all_tag_v, names(tag_cluster_v))]

imgnet_all_df2 = left_join(imgnet_all_df, tag_occured_df, by = "ID", copy=TRUE, keep=F)
imgnet_all_df2$Tag.y = NULL
imgnet_all_df2$Tag = imgnet_all_df2$Tag.x
imgnet_all_df2$Tag.x = NULL

head(imgnet_all_df2)

if (FALSE) { 
    
    
    tb1 = (sort(table(tag_cluster_v), decreasing = F))
    tb1_gt10 = rev(tb1[tb1>=10])
    
    summary(as.numeric(tb1))
    
    
    head(tag_cluster_v[order(tag_cluster_v, decreasing = T)])
    
    
    tags_by = by(imgnet_occurred, INDICES = tag_cluster_v, FUN = function(x) x)
    
    tags_by2 = tags_by[names(tb1_gt10)]
    
    tags_gt10 = t(sapply(tags_by2, FUN = function(x) x[1:50]))
    
    tags_gt10 = data.frame(tags_gt10)
    rownames(tags_gt10) = paste0("Cluster_", 1:nrow(tags_gt10))
    colnames(tags_gt10) = paste0("Tag_", 1:50)
    tags_gt10 = data.frame(t(tags_gt10))
    
    write.xlsx((tags_gt10), file = "Data/imagenet21k_28clusters_50tags.xlsx")
    
    
}


### Activity PUD


### imgnet 21k tags


# registerDoMC(n_thread)
path_SDG_xls = "~/Dropbox/KIT/CES_SEOUL/FlickrSDG_download/July2022_V1/Xlsx/"
path_out_network =  "~/Dropbox/KIT/CES_SEOUL/CESKR/FlickrSDG_result/Network/"




# meta data files
xls_SDG_V = list.files(path_SDG_xls, pattern="\\.xlsx$", full.names = T)


n_points = length(xls_SDG_V)

# idxs = sample(10000:20000)
idxs = 1:n_points



sdg_aoi_1km = read_sf(paste0(path_data, "/GIS/FlickrSDG_AOI_1km_19July2022.shp" ))
sdg_aoi_1km$CELL_ID_1km = sdg_aoi_1km$CELL_ID_
sdg_pud_1km = sdg_aoi_1km



if (FALSE) { 
    # idxs = 19053
    
    # write cluster information for each photo
    foreach (i =rev(idxs), .errorhandling = "stop") %do% {
        # for (i in (idxs)) {
        
        if ((i %% 1 ) ==0) {
            cat("AOI_", i, ">")
        }
        in_filename_tmp = paste0(path_out_csv, "/", basename(xls_SDG_V[i])) 
        out_filename_tmp = paste0(path_out_final, "/", basename(xls_SDG_V[i])) 
        
        
        if (file.exists(out_filename_tmp)) {
            # next
            return()
        }
        
        
        # aoi_tmp = str_extract(xls_SDG_V[i], pattern = "Poly\\_[0-9]*")
        
        # aoi_tmp = paste0("AOI_", as.numeric(substr(aoi_tmp, start = 6, 11)))
        
        aoi_tmp = str_extract(xls_SDG_V[i], pattern = "CellID\\_[0-9]*")
        aoi_tmp = paste0("AOI_", aoi_tmp)
        cell_id_tmp = as.numeric(str_sub(aoi_tmp,12, 17))
        
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
        
        
        dt$FlickrClusterK107 = cluster_tmp_v
        
        # for each photo
        write.xlsx(dt, file = out_filename_tmp)
        
        
        
    }
}


### ES/non-ES

ES_tb = read.xlsx("Data/imagenet21k_28clusters_50tags.xlsx", 1)

ES_tb$`ES/non-ES`

ES_tb = ES_tb[ES_tb$`ES/non-ES` == "ES",]
ES_tb$Cluster


i = 100 

# idxs = 19053



##### APUD and Cluster


# idxs = 19053

apud_columns = c(paste0("APUD_", years), "APUD_Total")



if (FALSE) { 
    apud_df = matrix(data = 0, nrow = nrow(sdg_aoi_1km), ncol = length(apud_columns) ) %>% data.frame
    colnames(apud_df) = apud_columns
    # library(doMC)
    # registerDoMC(1)
    
    # i= 100 
    # write cluster information for each photo
    res_tmp = foreach (i = (idxs)[101:198], .errorhandling = "stop") %do% {
        # for (i in (idxs)) {
        
        if ((i %% 1 ) ==0) {
            cat("AOI_", i, ">")
        }
        in_filename_tmp = paste0(path_out_final, "/", basename(xls_SDG_V[i])) 
        # out_filename_tmp = paste0(path_out_final, "/", basename(xls_SDG_V[i])) 
        # 
        # 
        # if (file.exists(out_filename_tmp)) {
        #     # next
        #     return()
        # }
        
        
        aoi_tmp = str_extract(xls_SDG_V[i], pattern = "CellID\\_[0-9]*")
        aoi_tmp = paste0("AOI_", aoi_tmp)
        cell_id_tmp = as.numeric(str_sub(aoi_tmp,12, 17))
        
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
        spdf_tmp = SpatialPointsDataFrame(cbind(as.numeric(dt$Longitude), as.numeric(dt$Latitude)), proj4string = crs(proj4.LL), data = dt)
        spdf_tmp = spTransform(spdf_tmp, CRSobj = crs(proj4.UTM52N))
        
        # spdf_tmp$Owner = factor(spdf_tmp$Owner)
        # spplot(spdf_tmp, "Owner")
        
        sdg_aoi_1km_tmp = sdg_aoi_1km[sdg_aoi_1km$CELL_ID %in% cell_id_tmp,]
        
        
        sdg_join = sf::st_join(st_as_sf(sdg_aoi_1km_tmp), st_as_sf(spdf_tmp))
        sdg_join = sdg_join[!is.na(sdg_join$PhotoID),]
        
        subcell_v = unique(sdg_join$CELL_ID_1km)
        
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

writeOGR(sdg_apud_1km, dsn = paste0(path_out_final),layer = paste0( "FlickrSDG_APUD_1km_22July2022.shp"), driver = "ESRI Shapefile", overwrite_layer = T)

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











