library(bigmemory)
library(doMC)
library(stringr)
library(readxl)
library(openxlsx)

library(dplyr)
library(tidyr)

proj4.LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


path_base = "~/Dropbox/KIT/CES_SEOUL/CESKR/"
path_out_csv = "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/MergedCSV/"

path_xls = "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_download/Sep2023_Korea_V2/Xlsx/"
path_out_network =  "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/Network/"




# meta data files
xls_V = list.files(path_xls, pattern="\\.xlsx$", recursive = T, full.names = T)


n_thread = detectCores()


# Places 365
places365_tags_raw = read.table("Data/categories_places365.txt")
places365_tags_raw$V2 = NULL
# str(places365_tags_raw)

places365_tags_raw = as.character(places365_tags_raw$V1)

places365_tags = str_extract(places365_tags_raw, pattern = "(?<=[a-z]/).*")


# IMGNET tags
imgnet1000_tags_verbose = jsonlite::fromJSON("Data/imagenet_class_index.json", simplifyDataFrame = T)

imgnet1000_tags_df = data.frame(do.call(rbind, imgnet1000_tags_verbose))
imgnet1000_tags_df$X1 = NULL
imgnet1000_tags = as.vector(imgnet1000_tags_df$X2)
imgnet1000_tags = unique(imgnet1000_tags)

# 
imgnet21k_tags_ids = readLines("Data/imagenet21k_wordnet_ids.txt") %>% as.vector
imgnet21k_tags_verbose = readLines("Data/imagenet21k_wordnet_lemmas.txt")
imgnet21k_tags = as.vector(imgnet21k_tags_verbose)

imgnet21k_tags_df = data.frame(ID = imgnet21k_tags_ids, Tag = imgnet21k_tags) 


imgnet21k_tags_unique = unique(imgnet21k_tags)


setwd(path_base)

if (!dir.exists(path_out_csv)) { 
    dir.create(path_out_csv, recursive = T)
}

if (!dir.exists(path_out_network)) { 
    dir.create(path_out_network, recursive = T)
}

# i = 1 

# cl = makeCluster(16)
# registerDoSNOW(cl)

reprocessCSV = F

if (reprocessCSV) {
    
    # places
    path_places =  "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/Flickr/Tags/places365_resnet50/"
    
    # Imagenet tag
    path_EFFV2XL_21k =  "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/Flickr/Tags/EfficientNetV2XL_21k/"
    path_EFFV2L_1k =   "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/Flickr/Tags/EfficientNetV2L_1k/"
    
    
    res_l = foreach (i = 1:length(xls_V), .packages = c("stringr", "readxl", "doSNOW"), .errorhandling = "stop") %do% { 
        
        aoi_tmp = str_extract(xls_V[i], pattern = "CellID\\_[A-Z0-9]*")
        
        aoi_tmp = paste0("AOI_", aoi_tmp)
        
        cellregion_tmp = str_extract(aoi_tmp, pattern = "[A-Z].(?=[0-9])")
        
        # tagfile_name = str_extract(basename(xls_SDG_V[i]), pattern = ".*(?=.xlsx)")
        tagfile_name = str_extract(basename(xls_V[i]), pattern = ".*(?=_n)") # without the number of the photos
        
        tagfile_name =  str_extract(tagfile_name, pattern = "[^0-9]*")
        
        out_filename_tmp = paste0(path_out_csv, "/", basename(xls_V[i])) 
        
        if (file.exists(out_filename_tmp)) {
            return(NULL)
        }
        print(i)
        
        
        
        nphotos_tmp = str_extract(basename(xls_V[i]), pattern = "(?<=_n)[0-9].")
        
        # No photos
        if (nphotos_tmp == 0) {
            
            return(NULL)
        }
        
        # Read meta data
        
        
        tryCatch({
            xl_df = read_xlsx  (xls_V[i])
        }
        , error= function(e) 
        {
            print(e); print("XLSX is null"); return(NULL)
        }
        )
        
        
        
        
        if (nrow(xl_df)==0) { 
            print("no data")
            return(NULL)
            
        }
        
        colnames(xl_df)
        
        xl_df = xl_df[!duplicated(xl_df$PhotoID),]
        
        
        # unique(xl_df$PhotoID)
        
        places_df_name = paste0(path_places, "/", tagfile_name, ".csv")
        
        # Places365
        places_df = read.csv(places_df_name)  
        photoid_tmp = str_extract(places_df$Filename, pattern="(?<=_)[0-9]*")
        places_df$Filename = NULL
        
        colnames(places_df) = paste0("PLACES365_", colnames(places_df))
        places_df$PhotoID = photoid_tmp
        
        # if (!file.exists(places_df_name)) { 
        #     return(NULL)
        # }
        
        
        imgnet_name = paste0(path_EFFV2XL_21k, "/", tagfile_name, ".csv")
        # imgnet_name = paste0(path_EFFV2L_1k, "/", tagfile_name, ".csv") 
        
        
        imgnet_df = read.csv(imgnet_name)  
        imgnet_df$Year = NULL
        
        
        photoid_tmp2 = str_extract(imgnet_df$Filename, pattern="(?<=_)[0-9]*")
        imgnet_df$Filename = NULL
        
        colnames(imgnet_df) = paste0("IMGNET_21k_", colnames(imgnet_df))
        
        imgnet_df$PhotoID = photoid_tmp2
        
        
        
        tag_df = merge(imgnet_df, places_df, by = "PhotoID", all = T)
        rm(places_df, imgnet_df)
        
        
        ### remove empty tags (using places tag)
        
        # tag_df = tag_df[!is.na(tag_df$Places365_Top1),]
        
        
        # photoid_tmp[match(xl_df$PhotoID, photoid_tmp)][1]
        merged_df = merge(xl_df, tag_df, by = "PhotoID", all.x = T)
        rm(xl_df, tag_df)
        
        
        write.xlsx(merged_df, file = out_filename_tmp)
        
    }
    
    
    
} 


createMatrix = TRUE 

if (createMatrix) { 
    ### Create 1-mode matrix
    
    
    
    ### delete matchstick tags
    # cl = makeCluster(16)
    # registerDoSNOW(cl)
    
    # i = 1
    
    
    myFunc = function(x, prob, lev) { 
        # print(x)
        # print(prob)

                fac = factor(x, levels = lev, ordered = TRUE)
        wt = numeric(length(lev))
        wtsum = tapply(prob, INDEX = as.numeric(fac), FUN = sum, na.rm=T)
        wt[as.numeric(names(wtsum))] = wtsum
        tb = tabulate(fac, nbins = length(lev))
        return( tb * wt )
        
    }
    
    weighted1modematrix = function (df_in, prob_in = NULL) {
        
        lev <- sort(unique(unlist(df_in)))
        
        dat <- do.call(rbind, lapply(1:nrow(df_in), FUN = function(x) myFunc(df_in[x,], as.numeric( prob_in[x,]), lev)))
        
        colnames(dat) <- lev
        t(data.frame(dat, check.names = FALSE))
    }
    # 
    # df_in = imgnet_df # [1:5, 1:5]
    # prob_in = imgnet_prob_df# [1:5, 1:5]
    # 
    # weighted1modematrix(df_in, prob_in)
    
    
    # 
    # make1modematrix <- function(keyword.vec.in, keywords.vec.ref) {
    #     matched <- match(keyword.vec.in, keywords.vec.ref)
    #     # matched.nona <- matched[!is.na(matched )]
    #     
    #     matched.tb <- table(matched)
    #     
    #     res.tmp <- numeric(length(keywords.vec.ref))
    #     res.tmp[as.numeric(names(matched.tb))] <-  (matched.tb)
    #     return(res.tmp)
    # }
    
    
    # print("Making the 1-mode matrix")
    
    # 
    # res.1modematrix <- apply(keyword.in.reduced, MARGIN = 2, function(x) make1modematrix(x, keywords.occurred))
    # apply(res.1modematrix, MARGIN = 1, FUN = table)
    
    
    # matrix_in  = places_mt
    # keywords.vec.ref = sort(unique(unlist(places_mt)))
    
    makefinalmatrix <- function(matrix_in, keywords.vec.ref) {
        
        
        print(paste0("N of keywords: ", length(keywords.vec.ref)))
        
        final.m <- matrix(0, nrow = length(keywords.vec.ref), ncol =  length(keywords.vec.ref))
        
        # system.time({ 
        for (i in 1:length(keywords.vec.ref)) {
            
            # if ((i %% 20) ==0) {
            #     cat(i, ">")
            # }
            
            imat_tmp <- matrix_in[i,]
            imat_idx_tmp = imat_tmp > 0
            
            for (j in 1:length(keywords.vec.ref)) {
                
                cooccur.idx <- which(imat_idx_tmp &matrix_in[j,] > 0)
                
                if (length(cooccur.idx)> 0) {
                    final.m[i,j] <- sum(imat_tmp[cooccur.idx] * matrix_in[j,][cooccur.idx], na.rm=T)
                }
                
            }
            
            diag(final.m) <- rowSums(matrix_in)
            
        } 
        # })
        
        colnames(final.m) <- rownames(final.m) <- keywords.vec.ref
        return(final.m)
    } 
    
    
    # i = 8245
    
    # stopCluster(cl)
    
    registerDoMC(n_thread)
    
    
    n_points = length(xls_V)
    
    # idxs = sample(10000:20000)
    idxs = 1:n_points
    
    
    # idxs = 19053
    
    foreach (i =(idxs), .errorhandling = "stop") %dopar% {
        # for (i in (idxs)) {
        
        if ((i %% 1 ) ==0) {
            cat("AOI_", i, ">")
        }
        in_filename_tmp = paste0(path_out_csv, "/", basename(xls_V[i])) 
        
        
        if (!file.exists(in_filename_tmp)) {
            # next
            return()
        }
        
        
 
        
        aoi_tmp = str_extract(xls_V[i], pattern = "CellID\\_[A-Z0-9]*")
        
        aoi_tmp = paste0("AOI_", aoi_tmp)
        
 
        
        
        dt = NULL    
        
        pl_name = paste0(path_out_network, "/places_m_", aoi_tmp, ".csv")
        
        if (!file.exists(pl_name)) {
            
            if (is.null(dt)) { 
                dt = read.xlsx(in_filename_tmp)
            }
            
            # places 
            places_df = dt[, paste0("PLACES365_Top", 1:10)] 
            places_df[places_df==""] = NA
            
            places_prob_df = dt[, paste0("PLACES365_Prob", 1:10)] 
            places_prob_df[places_prob_df==""] = NA
            
            
            # print("Making the 1-mode matrix")
            # places_mt <- t(mtabulate(places_df)) # non-weighted
            places_mt <- weighted1modematrix(places_df, places_prob_df)
            
            # print("Making the final matrix")
            places_1mode <- makefinalmatrix(places_mt,  sort(unique(unlist(places_df))))
            write.csv(places_1mode, file = pl_name)
            
        }
        
        img_name = paste0(path_out_network, "/imgnet_m_", aoi_tmp, ".csv")
        
        if (!file.exists(img_name)) {
            
            if (is.null(dt)) { 
                dt = read.xlsx(in_filename_tmp)
            }
            
            # Imagenet 
            imgnet_df = dt[, paste0("IMGNET_21k_Top", 1:10)] 
            imgnet_df[imgnet_df==""] = NA
            
            
            imgnet_ID_df = lapply(imgnet_df,FUN = function(x) imgnet21k_tags_df$ID[match(x, imgnet21k_tags_df$Tag)]) %>% data.frame
            
            
            
            imgnet_prob_df = dt[, paste0("IMGNET_21k_Prob", 1:10)] 
            imgnet_prob_df[imgnet_prob_df==""] = NA
            
            
            
            imgnet_mt <- weighted1modematrix(imgnet_ID_df, imgnet_prob_df)
            
            imgnet_1mode <- makefinalmatrix(imgnet_mt,   sort(unique(unlist(imgnet_ID_df))))
            write.csv(imgnet_1mode, file = img_name)
            
        }
        
        
    }
    
}



## aggregate csvs


places_all_m <- data.frame(matrix(data = 0, nrow = length(places365_tags), ncol = length(places365_tags)))
dimnames(places_all_m) <- list( places365_tags, places365_tags)


for (i in 1:length(xls_V))  {
    
    
 
    aoi_tmp = str_extract(xls_V[i], pattern = "CellID\\_[A-Z0-9]*")
    
    aoi_tmp = paste0("AOI_", aoi_tmp)
    
    
    
    
    
    pl_name = paste0(path_out_network, "/places_m_", aoi_tmp, ".csv")
    
    print(pl_name)
    
    if (file.exists(pl_name)) { 
        places_1mode = read.csv(file = pl_name)
        places_1mode$X = NULL
        
        colnames(places_1mode) = str_replace(colnames(places_1mode), pattern = "\\.", replacement = "/")
        
        rownames(places_1mode) = colnames(places_1mode)
        places_all_m[rownames(places_1mode ), colnames(places_1mode)] = places_all_m[rownames(places_1mode ), colnames(places_1mode)] +  places_1mode
        
    } else {
        print("skips")   
    }
    
}



write.csv(places_all_m, file = "Data/places_all_m.csv")
saveRDS(places_all_m, file = "Data/places_all_m.Rds")



### imgnet 21k tags

imgnet21k_tags_occurred = c()

for (i in 1:length(xls_V))  {
    
    # aoi_tmp = str_extract(xls_SDG_V[i], pattern = "Poly\\_[0-9]*")
    # aoi_tmp = paste0("AOI_", as.numeric(substr(aoi_tmp, start = 6, 11)))
    aoi_tmp = str_extract(xls_V[i], pattern = "CellID\\_[A-Z0-9]*")
    aoi_tmp = paste0("AOI_", aoi_tmp)
    
    img_name = paste0(path_out_network, "/imgnet_m_", aoi_tmp, ".csv")
    print(img_name)
    if (file.exists(img_name)) { 
        imgnet_1mode = read.csv(file = img_name)
        
    }
    imgnet_1mode$X = NULL
    tags_tmp = colnames(imgnet_1mode)
    
    imgnet21k_tags_occurred = unique(c(imgnet21k_tags_occurred, tags_tmp))
    
}

writeLines(imgnet21k_tags_occurred, con = "Data/imgnet_tags_occurred.txt")



imgnet21k_tags_occurred = readLines("Data/imgnet_tags_occurred.txt")



# imgnet_all_m <- data.frame(matrix(data = 0, nrow = length(imgnet21k_tags_occurred), ncol = length(imgnet21k_tags_occurred)))
# dimnames(imgnet_all_m) <- list( imgnet21k_tags_occurred, imgnet21k_tags_occurred)


imgnet_all_m <- big.matrix(nrow = length(imgnet21k_tags_occurred), ncol = length(imgnet21k_tags_occurred), dimnames = list(imgnet21k_tags_occurred,imgnet21k_tags_occurred))

imgnet_all_m[,]= 0




# colnames(imgnet_1mode)[!colnames(imgnet_1mode) %in% imgnet1000_tags]

for (i in 1:length(xls_V))  {
    
    # aoi_tmp = str_extract(xls_SDG_V[i], pattern = "Poly\\_[0-9]*")
    
    # aoi_tmp = paste0("AOI_", as.numeric(substr(aoi_tmp, start = 6, 11)))
    aoi_tmp = str_extract(xls_V[i], pattern = "CellID\\_[A-Z0-9]*")
    
    aoi_tmp = paste0("AOI_", aoi_tmp)
    
    
    
    img_name = paste0(path_out_network, "/imgnet_m_", aoi_tmp, ".csv")
    print(img_name)
    
    if (file.exists(img_name)) { 
        imgnet_1mode = read.csv(file = img_name)
        imgnet_1mode$X = NULL
        # colnames(imgnet_1mode) = str_replace_all(colnames(imgnet_1mode), pattern = "\\.", replacement = "-")
        # colnames(imgnet_1mode) =  str_replace(colnames(imgnet_1mode), pattern = "potter-s_wheel", replacement =    "potter's_wheel")
        # colnames(imgnet_1mode) =  str_replace(colnames(imgnet_1mode), pattern = "jack-o--lantern", replacement = "jack-o'-lantern")
        # 
        # colnames(imgnet_1mode) =  str_replace(colnames(imgnet_1mode), pattern = "carpenter-s_kit", replacement = "carpenter's_kit")
        # colnames(imgnet_1mode) =  str_replace(colnames(imgnet_1mode), pattern =  "yellow_lady-s_slipper", replacement = "yellow_lady's_slipper")
        # 
        
        
        rownames(imgnet_1mode) = colnames(imgnet_1mode)
        
        
        imgnet_all_m[rownames(imgnet_1mode ), colnames(imgnet_1mode)] = imgnet_all_m[rownames(imgnet_1mode ), colnames(imgnet_1mode)] + as.matrix(imgnet_1mode)
        
    }   else {
        print("skips")   
    }
}

write.csv(as.matrix(imgnet_all_m), file = "Data/imgnet_all_m.csv")
saveRDS(as.matrix(imgnet_all_m), file = "Data/imagenet_all_m.Rds") # didn't work
write.big.matrix(imgnet_all_m, filename = "Data/imagenet_all_m_bigmatrix.txt", col.names=TRUE, 
                 row.names=TRUE)



stop("ends here")



joint_tags =  c(places365_tags, imgnet1000_tags)
joint_tags = unique(joint_tags)


joint_all_m = data.frame(matrix(data = 0,  nrow = length(joint_tags), ncol = length(joint_tags)))
dimnames(joint_all_m) <- list(joint_tags, joint_tags)





joint_find = c("bicycle/built/for/two", "curly/coated_retriever", "flat/coated_retriever", "four/poster", "go/kart","hand/held_computer","hen/of/the/woods","red/backed_sandpiper","red/breasted_merganser","sulphur/crested_cockatoo", "ping/pong_ball", "pay/phone","potter/s_wheel","soft/coated_wheaten_terrier", "potter-s_wheel",  "carpenter/s_kit",  "yellow_lady/s_slipper", "jack/o//lantern", "Shih/Tzu", "German_short/haired_pointer", "three/toed_sloth", "long/horned_beetle", "wire/haired_fox_terrier", "black/footed_ferret", "black/and/tan_coonhound")


joint_replaced = c("bicycle-built-for-two", "curly-coated_retriever", "flat-coated_retriever", "four-poster", "go-kart","hand-held_computer","hen-of-the-woods","red-backed_sandpiper","red-breasted_merganser","sulphur-crested_cockatoo", "ping-pong_ball", "pay-phone","potter-s_wheel","soft-coated_wheaten_terrier",  "potter's_wheel",  "carpenter's_kit", "yellow_lady's_slipper", "jack-o'-lantern", "Shih-Tzu", "German_short-haired_pointer", "three-toed_sloth", "long-horned_beetle", "wire-haired_fox_terrier", "black-footed_ferret", "black-and-tan_coonhound")

for (i in 1:length(xls_SDG_V))  {
    
    
    aoi_tmp = str_extract(xls_SDG_V[i], pattern = "Poly\\_[0-9]*")
    aoi_tmp = paste0("AOI_", as.numeric(substr(aoi_tmp, start = 6, 11)))
    
    jt_name = paste0(path_out_network, "/joint_m_", aoi_tmp, ".csv")
    print(jt_name)
    
    if (file.exists(jt_name)) { 
        
        joint_1mode = read.csv(file = jt_name)
        joint_1mode$X = NULL
        
        colnames(joint_1mode) = str_replace_all(colnames(joint_1mode), pattern = "\\.", replacement = "/")
        
        for (jt in 1:length(joint_replaced)) { 
            
            colnames(joint_1mode) =  str_replace(colnames(joint_1mode), pattern = joint_find[jt], replacement =joint_replaced[jt])
        }
        
        print(colnames(joint_1mode)[!colnames(joint_1mode) %in% c(places365_tags, imgnet1000_tags)])
        
        
        
        
        # 
        rownames(joint_1mode) = colnames(joint_1mode)
        
        
        
        joint_all_m[rownames(joint_1mode ), colnames(joint_1mode)] =  joint_all_m[rownames(joint_1mode ), colnames(joint_1mode)]+ joint_1mode
        
    }   else {
        print("skips")   
    }
    
}


write.csv(joint_all_m, file = "Data/joint_all_m.csv")
saveRDS(joint_all_m, file = "Data/joint_all_m.Rds")

# save.image("~/Downloads/FlickrEU_June8_2021.RData")




# res_err_cnt = sapply(res_l, FUN = c)
# table(res_err_cnt > 10 )
# summary(res_err_cnt)
# 
# 
# boxplot(res_l)



# # str(res_l)
# res_big_df = do.call("rbind", res_l)
# nrow(res_big_df)
# 
# res_big_df = res_big_df[!duplicated(res_big_df$PhotoID),]
# nrow(res_big_df)
# 
# 
# write.xlsx(res_big_df, file = "../ESP2021/FlickrEU_Bayern_tags.xlsx")
# saveRDS(res_big_df, file = "../ESP2021/FlickrEU_Bayern_tags.Rds")
# 
# res_small_df = res_big_df
# 
# res_small_df$Latitude = NULL
# res_small_df$Longitude = NULL
# res_small_df$URL = NULL
# res_small_df$Geocontext = NULL
# 
# library(rgdal)
# 
# br_spdf = SpatialPointsDataFrame(cbind(res_big_df$Longitude, res_big_df$Latitude), data = res_small_df, proj4string = CRS( proj4.LL))
# 
# 
# plot(br_spdf, col = factor(br_spdf$Places365_Top1), pch=15, cex=0.1)
# 
# writeOGR(br_spdf, dsn = "../ESP2021/", layer = "FlickrEU_Bayern_tags", driver = "ESRI Shapefile", overwrite_layer = T)
# 
# 
# 
# table(br_spdf$Places365_Top1)

