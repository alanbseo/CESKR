library(data.table)

source("RScripts/0_Flickr_Common.R")

library(readxl)


 

# Retreiving the data 

aois_done <- list.files(paste0(workdir, "/", savedir, "/Xlsx"), pattern = "^AOI\\_CellID\\_.*\\.xlsx$")
aois_done_v <- as.numeric(sapply(aois_done, FUN = function(x)  (str_split(x, pattern = "_")[[1]][3])))
years = 2005:2023
n_years = length(years)

nphotos_done_v <- as.numeric(sapply(aois_done, FUN = function(x)  str_extract(str_split(x, pattern = "_")[[1]][5], "[0-9]+")))

summary(nphotos_done_v)

hist(nphotos_done_v, nclass=30, xlab="# of photos per cell")

nphotos_true_v = numeric(length = length(nphotos_done_v))



sdg_aoi_1km = readOGR(paste0(path_data, "/GIS/FlickrSDG_AOI_1km_19July2022.shp" ))

sdg_pud_1km = sdg_aoi_1km
 


pud_columns = c(paste0("PUD_", years), "PUD_Total")

pud_df = matrix(data = 0, nrow = nrow(sdg_aoi_1km), ncol = length(pud_columns) ) %>% data.frame
colnames(pud_df) = pud_columns

for (i in 1:length(aois_done)) { 
    cat(">")
    aoi_tmp <- aois_done[i]
    cell_id_tmp = as.numeric(substr(aoi_tmp, 12, 17))
    
    # Sys.setlocale("UTF-8")
    tryCatch(
        aoi_dt_raw <- data.frame(read.xlsx(paste0(workdir, savedir, "/Xlsx/", aoi_tmp), sheet = 1)) 
    )      
    
    if (nrow(aoi_dt_raw)==0) { 
        next()
    }
    
    spdf_tmp = SpatialPointsDataFrame(cbind(as.numeric(aoi_dt_raw$Longitude), as.numeric(aoi_dt_raw$Latitude)), proj4string = crs(proj4.LL), data = aoi_dt_raw)
    spdf_tmp2 = spTransform(spdf_tmp, CRSobj = crs(proj4.UTM52N))
    
    # spdf_tmp2$Owner = factor(spdf_tmp2$Owner)
    # 
    # spplot(spdf_tmp2, "Owner")
    
    sdg_aoi_1km_tmp = sdg_aoi_1km[sdg_aoi_1km$CELL_ID %in% cell_id_tmp,]
    
    
    sdg_join = sf::st_join(st_as_sf(sdg_aoi_1km_tmp), st_as_sf(spdf_tmp2))
    sdg_join = sdg_join[!is.na(sdg_join$PhotoID),]
    
    subcell_v = unique(sdg_join$CELL_ID_)
    
    
    subcell_pud_df = foreach (j = 1:length(subcell_v), .combine = "rbind") %do% { 
        # print(j)    
        
        
        subcell_pud_annual = foreach (year_tmp = years, .combine = c) %do% { 
            sdg_subcell_tmp = sdg_join[(sdg_join$CELL_ID_ == subcell_v[j]) & (sdg_join$Year == year_tmp), ]
            
            subcell_by = by(sdg_subcell_tmp, INDICES = sdg_subcell_tmp$Date, FUN = function(x) length(unique(x$Owner)))
            # length(subcell_by)
            
            return(sum(as.numeric(subcell_by), na.rm=T))
        }
        
        
    }
    
    if (is.null(dim(subcell_pud_df))) { 
        pud_df[match(subcell_v, sdg_pud_1km$CELL_ID_),  ] = c(subcell_pud_df, sum(subcell_pud_df))
    } else { 
        # sdg_pud_1km@data[match(subcell_v, sdg_pud_1km$CELL_ID_),"PUD"] = subcell_pud_df
        pud_df[match(subcell_v, sdg_pud_1km$CELL_ID_),  ] = cbind(subcell_pud_df, rowSums(subcell_pud_df))
    }
}

sdg_pud_1km@data = cbind(sdg_pud_1km@data, pud_df)

# plot(st_as_sf(sdg_pud_1km))
writeOGR(sdg_pud_1km, dsn = paste0(path_data, "GIS"), background=NA, layer = "FlickrKR_AnnualPUD_1km_22July2022", driver = "ESRI Shapefile", overwrite_layer = T)



## rasterize
dummy_r = raster(paste0(path_data, "GIS/sdg_r_dummy_1km.tif"))



sdg_pud_1km_rs =  foreach(pud_column = pud_columns) %do% { 
    print(pud_column)
    raster::rasterize(sdg_pud_1km[, pud_column], field=pud_column, y = dummy_r)
}
sdg_pud_1km_rs = stack(sdg_pud_1km_rs)
sdg_pud_1km_rs



writeRaster(sdg_pud_1km_rs, filename = paste0(path_data, "GIS", "/FlickrKR_1km_22July2022.tif"), bylayer= T, suffix = pud_columns, overwrite=T, driver="GeoTiff")

# i = 527 
# aoi_dt.raw[aoi_dt.raw$PhotoID == 32570535417,]  # photos duplicated


plot(nphotos_done_v, nphotos_true_v, xlab = "# of Flickr photos returned by API", ylab = "# of unique Flickr photos", main = "Large number of duplicated Flickr photos (Seoul, 2005-2022")

smr = round(summary(nphotos_true_v)[c(1,3,4,6)],1)

hist(nphotos_true_v, nclass=20)

barplot(sort(nphotos_true_v), xlab = "Cell", ylab = "# of Flickr photos", ylim=c(0, 4100))
legend("topleft", legend = paste0(names(smr), ":", smr))

aoi_poly_in$NPHOTOS = nphotos_true_v
writeOGR(aoi_poly_in, dsn =  paste0(path_data, "/GIS"), layer = "FlickrKR_AOI_Nphotos.shp", driver = "ESRI Shapefile", overwrite_layer = T)

col_nphotos = rev(heat.colors(n = max(nphotos_true_v)+1))[aoi_poly_in$NPHOTOS+1]

leg = seq(0, 4000, 500)

col_leg = rev(heat.colors(n = length(leg)))


plot(aoi_poly_in, col = col_nphotos) # "NPHOTOS")

legend("bottomleft", legend = leg, col = col_leg, pch=15, title = "# of Flickr photos", bty="n")



nphotos_annual_m = matrix(0, nrow =length(aois_done), ncol = n_years)
nphotos_monthly_m = matrix(0, nrow =length(aois_done), ncol = 12)

### Annual number of photos

for (i in 1:length(aois_done)) { 
    # cat(i)
    aoi_tmp <- aois_done[i]
    # Sys.setlocale("UTF-8")
    tryCatch(
        aoi_dt_raw <- data.frame(read.xlsx(paste0(workdir, savedir, "/Xlsx/", aoi_tmp), sheet = 1)) 
    )      
    
    if (nrow(aoi_dt_raw)==0) { 
        next()
    }
    
    
    month_v = as.numeric(substr(aoi_dt_raw$Date, start = 6, 7))
    
    nphotos_annual_m[i, ] = sapply(years, FUN = function(x) max(0, length(which(aoi_dt_raw$Year == x))))
    nphotos_monthly_m[i, ] = sapply(1:12, FUN = function(x) max(0, length(which(month_v == x))))
    
}

nphotos_per_year = colSums(nphotos_annual_m)
names(nphotos_per_year) = years

nphotos_per_month = colSums(nphotos_monthly_m)
names(nphotos_per_month) = 1:12


par(mfrow=c(2,1))
barplot(nphotos_per_year, type="l", ylab="# of Flickr photos", xlab = "Year", las=1)

barplot(nphotos_per_month, type="l", ylab="# of Flickr photos", xlab = "Month", las=1)


### Annual PUD

for (i in 1:length(aois_done)) { 
    # cat(i)
    aoi_tmp <- aois_done[i]
    # Sys.setlocale("UTF-8")
    tryCatch(
        aoi_dt_raw <- data.frame(read.xlsx(paste0(workdir, savedir, "/Xlsx/", aoi_tmp), sheet = 1)) 
    )      
    
    if (nrow(aoi_dt_raw)==0) { 
        next()
    }
    
    aoi_pud_dt = by(aoi_dt_raw, aoi_dt_raw[, c("Date")], FUN = function(x) rbind(x))
    sapply(aoi_pud_dt, FUN = function(x) length(unique(x$Owner)))
    
    month_v = as.numeric(substr(aoi_dt_raw$Date, start = 6, 7))
    
    nphotos_annual_m[i, ] = sapply(years, FUN = function(x) max(0, length(which(aoi_dt_raw$Year == x))))
    nphotos_monthly_m[i, ] = sapply(1:12, FUN = function(x) max(0, length(which(month_v == x))))
    
}






