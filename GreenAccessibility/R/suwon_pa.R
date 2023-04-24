library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(readxl)
# library(doParallel)

path_data = "GreenAccessibility/R/R_input/"


costd <- read.csv(paste0(path_data, "suwon_pa_0116_filter.csv"))
# str(costd)

biotope <- read.csv(paste0(path_data, "/biotope_db2.csv"),fileEncoding = "euc-kr")

biotope_class <- c(1:977)
biotope_df <- data.frame(biotope, biotope_class)


biotope_df[,6] = 400

cd_only <- costd[, -c(3:4, 6:8)] # FID, Population, Near DIST, F1...
# str(cd_only)s

colnames(cd_only)[1] <- 'ORIG_FID'

biotope_order <- biotope_df[,c(1,3,6)]

# FID and area size
area <- biotope_order[,c(1,2)]

area[is.na(area)] <- 0



n_row = nrow(cd_only)
n_col = 977 # ?
# cost distance and the row ids

# time consuming

# cd_dist <- cd_only[,1:ncol(cd_only)]
# cd_dist <- cd_dist[,-1] # Remove FID
# str(cd_dist) # Population, Dist, F1 ... 
# 
# for (i in 1:nrow(cd_dist)){
#   for (j in 3:ncol(cd_dist)){
#     cd_dist[i,j] <- cd_only[i,j+1] + cd_only[i,3] # add near dist
#   }
# }

# distance + near distance
cd_only_plusoffset = cd_only[, 4:ncol(cd_only)] + cd_only[,3]

cd_dist = cbind(cd_only[, 2:3 ], cd_only_plusoffset)


#Calculate population under 400m

pop_400 <- 0
cd_400 <- cd_dist

#remove N/A
# for (i in 1:n_row){
#   for (j in 3:979){
#     if (is.na(cd_dist[i,j]) == TRUE){
#       cd_400[i,j] <- 500
#     }  
#   }
# }

cd_only_plusoffset_NAmarked = cd_only_plusoffset
cd_only_plusoffset_NAmarked[is.na(cd_only_plusoffset_NAmarked)] = 500 # minimum size 500 m (Vogt et al., 2015.. Voigt?)

cd_400[,3:ncol(cd_400)] = cd_only_plusoffset_NAmarked


# if very close, assign population
# for (i in 1:nrow(cd_dist)){
#   if (min(cd_dist[i, c(-1,-2)]) <= 400){
#     pop_400 <- pop_400 + cd_dist[i,1]
#   }
# }

row_min_values = apply(cd_only_plusoffset_NAmarked, MARGIN = 2, FUN = min, na.rm=T)
table(row_min_values <= 400)

# sum all population closer to green area less or equal than 400 m 
pop_400 = sum(cd_dist$TT_Pop[row_min_values <= 400], na.rm=T)
pop_400 
# 847330


# Population
pop_sum <- sum(cd_dist[,1])
pop_400_percentage <- pop_400 / sum(cd_dist[,1]) * 100



obj_id <- cd_only[,1]
SW <- c(1:nrow(cd_dist))
LS <- c(1:nrow(cd_dist))


#df <- data.frame(obj_id, to_park, NDVI_Dist, near_park, Dist, SW)


#########################

SWDF <- cd_dist
SWDF_fa <- SWDF
str(cd_dist)
i <- 1
j <- 1
#area_sum <- sum(area[,2])

#recommended green space per capita (20 m2)
area_mean <- 20

#     #all open space_400m
#     for (j in 1:n_row){
#         cat(j)
#         for (i in 1:977){
#             
#             # area >= 5000 
#             if (is.na(cd_dist[j, i+2]) == FALSE & area[i,2] >= 5000){
#                 
#                 # dist == 0, then just the area
#                 if (cd_dist[j, i+2] == 0){
#                     SWDF[j,i+2] <- 1 * area[i,2]
#                     
#                     # dist > 0  
#                 } else {
#                     # area > 1E5
#                     if (area[i,2] > 100000){
#                         SWDF[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * 100000
#                         
#                         # area <= 1E5
#                     } else {
#                         SWDF[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * area[i,2]
#                     }
#                 }
#             }
#         }
#     }
# 
# #forest & artificial green area
# for (j in 1:n_row){
#     for (i in 1:977){
#         if (is.na(cd_dist[j, i+2] & area[i,2] >= 5000) == FALSE){
#             if (biotope_df[i,2] == "산림지" | biotope_df[i,2] == "조경녹지"){
#                 if (cd_dist[j, i+2] == 0){
#                     SWDF_fa[j,i+2] <- 1 * area[i,2]
#                 } else {
#                     if (area[i,2] > 100000){
#                         SWDF_fa[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * 100000
#                     } else {
#                         SWDF_fa[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * area[i,2]
#                     }
#                 }
#             } else {
#                 SWDF_fa[j, i+2] <- 0
#             }
#         }
#     }
# }

### Matrix calculation
SWDF = matrix(0, nrow = n_row, ncol = n_col)

# Create an area matrix. Replicate the area vector
Area_matrix = rep(area$area, n = n_row) %>% matrix(nrow = n_col, ncol =  n_row) %>% t


# mean spatial weight 
msw = 0.05 # following usual alpha (5%)
ers = 400 # effective range of service

# 
Area_corrected = Area_matrix
# max effective area size (10 ha)
Area_corrected[Area_matrix > 1E5 ] = 1E5
 
# Spatial Weight
SWDF = exp(cd_only_plusoffset * log(msw) / ers) * Area_corrected


# Area >= 5000 and Dist == 0
SWDF[Area_corrected==0 ] = Area_matrix[Area_corrected==0]

# Area < 5 ha
SWDF[Area_matrix < 1E5] = Area_matrix[Area_matrix < 1E5] # zero benefit 


# Forest & artificial green area
forest_idx = (biotope_df[i,2] == "산림지" | biotope_df[i,2] == "조경녹지")

SWDF_fa = SWDF
SWDF_fa[,!forest_idx] = 0 # mask all non-forest patches


#NA data to 0
SWDF[is.na(SWDF)] = 0
SWDF_fa[is.na(SWDF_fa)] = 0


 
#Level of Service Data Frame
LSDF = rowSums(SWDF) / area_mean - costd$TT_Pop 
LSDF_fa = rowSums(SWDF_fa) / area_mean - costd$TT_Pop 


 

LSDF_final <- data.frame(obj_id, LSDF, costd$TT_Pop )
LSDF_fa_final <- data.frame(obj_id, LSDF_fa, costd$TT_Pop )




#save csv file
write.csv(LSDF_final, "./LSDF_Suwon_os_0208_400.csv_repl", row.names = F)
write.csv(LSDF_fa_final, "./LSDF_Suwon_0208_fa_400.csv_repl", row.names = F)




