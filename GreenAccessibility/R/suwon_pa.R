library(raster)
library(sp)
library(rgdal)
library(readxl)
#library(doParallel)

path_data = "GreenAccessibility/R/R_input/"


costd <- read.csv(paste0(path_data, "suwon_pa_0116_filter.csv"))
str(costd)

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
cd_only_plusoffset_NAmarked[is.na(cd_only_plusoffset_NAmarked)] = 500 # 500 m 

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

#recommended green space per capita
area_mean <- 20

#all open space_400m
for (j in 1:n_row){
  for (i in 1:977){
    if (is.na(cd_dist[j, i+2]) == FALSE & area[i,2] >= 5000){
      if (cd_dist[j, i+2] == 0){
        SWDF[j,i+2] <- 1 * area[i,2]
      } else {
        if (area[i,2] > 100000){
          SWDF[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * 100000
        } else {
          SWDF[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * area[i,2]
        }
      }
    }
  }
}







#forest & artificial green area
for (j in 1:n_row){
  for (i in 1:977){
    if (is.na(cd_dist[j, i+2] & area[i,2] >= 5000) == FALSE){
      if (biotope_df[i,2] == "?긲??" | biotope_df[i,2] == "��??????"){
        if (cd_dist[j, i+2] == 0){
          SWDF_fa[j,i+2] <- 1 * area[i,2]
        } else {
          if (area[i,2] > 100000){
            SWDF_fa[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * 100000
          } else {
            SWDF_fa[j,i+2] <- exp(cd_dist[j,i+2] * log(0.05) / as.integer(biotope_order[i,3])) * area[i,2]
          }
        }
      } else {
        SWDF_fa[j, i+2] <- 0
      }
    }
  }
}



pop <- costd[,2]
LSDF <- data.frame(obj_id, LS, pop)
LSDF_fa <- data.frame(obj_id, LS, pop)



#NA data to 0
for (j in 1:n_row){
  for (i in 1:977){
    if(is.na(SWDF[j,i+2]) == TRUE){
      SWDF[j,i+2] = 0
    }
  }
}

for (j in 1:n_row){
  for (i in 1:977){
    if(is.na(SWDF_fa[j,i+2]) == TRUE){
      SWDF_fa[j,i+2] = 0
    }
  }
}



#Level of Service Data Frame
for (j in 1:n_row){
  LSDF[j,2] <- (sum(SWDF[j,c(-1,-2)]) / area_mean) - costd[j,2]
}




for (j in 1:n_row  LS _fa[j,2] <- (sum(SWDF_fa[j,c(-1,-2)]) / area_mean) - costd[j,2]
}





#save csv file
write.csv(LSDF, "./LSDF_Suwon_os_0208_400.csv_repl", row.names = F)

write.csv(LSDF_fa, "./LSDF_Suwon_0208_fa_400.csv_repl", row.names = F)




