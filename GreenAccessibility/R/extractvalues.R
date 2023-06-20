library(doMC)
library(stars)
library(sf)
library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(readxl)
library(doSNOW)

path_data = "GreenAccessibility/R/R_input/"


path_pop = "/Users/seo-b/Dropbox/KIT/CES_SEOUL/CESKR/GreenAccessibility/GIS/pop/Pop_jeju.shp"

# path_raster = "/Users/seo-b/Dropbox/KIT/CES_SEOUL/CESKR/GreenAccessibility/GIS/Cost/"
path_raster = "/Users/seo-b/Downloads/test619/"

pop_shp = readOGR(path_pop)
pop_sf = st_as_sf(pop_shp)


r_idxs = 2:13902
n_raster = length(r_idxs)

UPPLIM_VAL = 3.400e+30

cl = makeCluster(8)
registerDoSNOW(cl)
val_l = foreach (r_idx = r_idxs, .errorhandling = "stop", .packages = c("sf", "stars")) %dopar% { 
    cat(">", r_idx)
    r_tmp = read_stars(paste0(path_raster, r_idx, ".tif"))
    # r_tmp = read_stars(paste0(path_raster, r_idx, ""))
    
    # sf::st_crs(pop_sf)
    # sf::st_crs(r_tmp)
    
    st_crs(pop_sf ) = sf::st_crs(r_tmp)
    
    
    val_ret = st_extract(r_tmp, pop_sf)[[1]]
    val_ret[val_ret> UPPLIM_VAL] = NA
    
    return(val_ret)
}


val_df = do.call("cbind", val_l)
str(val_df)



### write an excel file

colnames(val_df) = paste0("Raster_", r_idxs)
rownames(val_df) = paste0("ORIG_FID_", 1:nrow(pop_sf))

write.csv(val_df, file = "Jeju_val_df_col13526_row13901.csv", quote = F, row.names = T, na = "")


