


#### Read predicted tags
 


plot(costarica_ext, xlab= "Lon", ylab="Lat")

plot(costarica_ext, xlab= "Lon", ylab="Lat", border=NA, col=NA)


nphotos_perct = quantile(dt$NPHOTOS_UNIQUE, probs= seq(0, 1, 0.1))
nphotos_perct = unique(nphotos_perct)

nphotos_cut = cut(dt$NPHOTOS_UNIQUE, breaks= as.numeric(nphotos_perct))


# levels(nphotos_cut)
aoi_col = rev(topo.colors(length(nphotos_perct)))[as.numeric(nphotos_cut)]
plot(costarica_ext, xlab= "Lon", ylab="Lat", border=NA, col=NA)

plot(dt, col = aoi_col, add=T, border="grey")
plot(costarica_adm2_ll, add=T, col=NA, border="grey")
plot(costarica_natpark, add=T, col=NA, border='red')

legend("bottomright", title = "# of Flickr photos", legend = levels(nphotos_cut), col = rev(topo.colors(length(nphotos_perct))), pch=15, bty="n")

# plot(costarica_aoi, add=T)





options(warn=2)
# options(warn=1)


processFlickrList <- function(x) {
    print(x)
    
    
    # xin <- flickrphotos_metadata_specific_df_l[[x]]
    # plen <- length(xin) 
    
    if (plen > 1 ) {
        # x3_l <- lapply(xin, FUN = function(x2) (
        #     if (is.null(x2) || nrow(x2)==1 ) {
        #         return(x2)
        #     } else {
        #         return(x2)
        #         
        #         # return(do.call(cbind, x2))
        #     }
        # ))
        
        x3_l <- xin[!sapply(xin, FUN = is.null)]
        x3 <- do.call(rbind, x3_l)
        
        
    } else if (plen == 1) {
        x3 <- do.call(cbind, xin)
    } else {
        return(NULL)   
    }
    
    
    if (is.null(x3) || (nrow(x3) == 0 )) { 
        return (NULL) 
    } else {
        x4 <- x3[!is.na(x3[,1]),]
        
        return (x4)
    }
    
    
}

flickrphotos_metadata_specific_df_l_df <-lapply(1:n_points, FUN = processFlickrList)
# processFlickrList(5812)
# processFlickrList(995)
# processFlickrList(490)
# processFlickrList(3278)
# 
# processFlickrList(1046)

poly.pages_idx <- which(sapply(flickrphotos_metadata_specific_df_l_df, FUN = function(x) !is.null(x)))

str(flickrphotos_metadata_specific.final_df <- do.call(rbind, flickrphotos_metadata_specific_df_l_df[poly.pages_idx]))
length(table(unique(flickrphotos_metadata_specific.final_df$PhotoID)))
length(( (flickrphotos_metadata_specific.final_df$PhotoID)))

which.max(sort(table( (flickrphotos_metadata_specific.final_df$PhotoID)), T))
sort(table((flickrphotos_metadata_specific.final_df$PhotoID)), T)

# flickrphotos_metadata_specific.final_df[(flickrphotos_metadata_specific.final_df$PhotoID == 31573925952), ]

unique.photoids <- (unique(flickrphotos_metadata_specific.final_df$PhotoID))

flickrphotos_metadata_specific.final_df_unique <- flickrphotos_metadata_specific.final_df[!(duplicated(flickrphotos_metadata_specific.final_df$PhotoID)),]
summary(table(unique(flickrphotos_metadata_specific.final_df_unique$PhotoID)))

unique.photoids

# 
load(file = "Rdata/clarifai_final_workspace_by_12635_2017-01-02.RData")

flickr_final_df <- read.xlsx(xlsxFile  = "Data/Flickr_ClarifaiTags_2017-01-09.xlsx", sheet =  1)



table(flickr_final_df[,2] %in% unique.photoids)
# photoids.overlap_idx <- match(flickr_final_df[,2], unique.photoids)
# photoids.overlap.rev_idx <- match( unique.photoids, flickr_final_df[,2])

# yesinfo_idx <- which(!is.na(photoids.overlap_idx ) )
# noinfo_idx <- which(is.na(photoids.overlap_idx ) )

# photoids.overlap_idx[!is.na(photoids.overlap_idx)]


flickr_final_df_merged <- merge(x = flickr_final_df, y = flickrphotos_metadata_specific.final_df_unique[, c("PhotoID", "Latitude", "Longitude", "Ntag")], all.x = TRUE, by.x = "Photo_ID", by.y = "PhotoID")

str(flickr_final_df_merged)

flickr_final_df_merged$Poly_ID <- as.numeric(as.character(flickr_final_df_merged$Poly_ID))

flickr_final_df_merged$Photo_ID <- as.numeric(as.character(flickr_final_df_merged$Photo_ID))
flickr_final_df_merged$Poly_ID <- as.numeric(as.character(flickr_final_df_merged$Poly_ID))

flickr_final_df_merged$Year <- as.numeric(as.character(flickr_final_df_merged$Year))
flickr_final_df_merged$Date <-  (as.character(flickr_final_df_merged$Date))

flickr_final_df_merged$Latitude <- as.numeric(as.character(flickr_final_df_merged$Latitude))
flickr_final_df_merged$Longitude <- as.numeric(as.character(flickr_final_df_merged$Longitude))
flickr_final_df_merged$Ntag <- as.numeric(as.character(flickr_final_df_merged$Ntag))



noinfo_idx <- which(is.na(flickr_final_df_merged$Latitude))
flickr_final_df_merged[noinfo_idx,]




noinfo.photoids <- flickr_final_df_merged[noinfo_idx, "Photo_ID"] 


library(rjson)
library(RCurl)

n.noinfo.photos <- length(noinfo.photoids)

ntag <- longitude <- latitude <- numeric(n.noinfo.photos)

flickr_photo.noinfo_l <- vector("list", n.noinfo.photos)

for (i in 1:n.noinfo.photos) {
    cat(i,",")
    id <- noinfo.photoids[i]
    query.info <- paste("https://api.flickr_com/services/rest/?method=flickr_photos_getInfo&format=json&api_key=", api.key, "&nojsoncallback=1&photo_id=", id, sep="")
    
    info.json <- getURL(query.info, ssl_verifypeer = FALSE, .opts = geturl.opts)
    info_l <- fromJSON(info.json, unexpected.escape="skip", method="C")
    
    
    if (info_l$stat == "ok") {
        longitude[i]  <-  info_l$photo$location$longitude
        latitude[i]  <-  info_l$photo$location$latitude
        ntag[i] <- length(info_l$photo$tags$tag)
    }
    
    flickr_photo.noinfo_l[[i]] <- info_l
    
}


flickr_final_df_merged[noinfo_idx, c("Latitude", "Longitude", "Ntag")] <- cbind(latitude, longitude, ntag)




flickr_final_df_merged$Landcover <- aoi_poly_in$LN[flickr_final_df_merged$Poly_ID]

table(flickr_final_df_merged$Landcover)


library(rgeos)
aoi_poly_in$AreaKM2 <- gArea(lu.mulde.sachsen.nonurban, byid = T) / 1E6

flickr_final_df_merged$AreaKM2 <- aoi_poly_in$AreaKM2[flickr_final_df_merged$Poly_ID]


#  
# 
write.xlsx(flickr_final_df_merged, file = paste0("Data/Flickr_Clarifai_Final_Results_n12635_", Sys.Date(), ".xlsx"))
# 

