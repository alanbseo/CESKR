library(raster)
library(sp)
library(rgdal)
library(readxl)

setwd("./GreenAccessibility/R/R_input/costd <- read.csv("./ansan_pa_0116_filter.csv")
biotope <- read.csv("./biotope_db2.csv",fileEncoding = "euc-kr")

biotope_class <- c(1:1135)
biotope_df <- data.frame(biotope, biotope_class)

biotope_df[,7] = 400


cd_only <- costd[,c(-3,-4,-5,-7,-8)]

colnames(cd_only)[1] <- 'ORIG_FID'

biotope_order <- biotope_df[,c(2,3,7)]

area <- biotope_order[,c(1,2)]

area[is.na(area)] <- 0

cd_dist <- cd_only[,1:ncol(cd_only)]
cd_dist <- cd_dist[,-1]

for (i in 1:3066){
  for (j in 3:1137){
    cd_dist[i,j] <- cd_only[i,j+1] + cd_only[i,3]
  }
}

obj_id <- cd_only[,1]
SW <- c(1:nrow(cd_dist))
LS <- c(1:nrow(cd_dist))

#########################

SWDF <- cd_dist
SWDF_fa <- SWDF


#recommended green area per capita
area_mean <- 20

#forest & artificial green area
for (j in 1:3066){
  for (i in 1:1135){
    if (is.na(cd_dist[j, i+2]) == FALSE & area[i,2] >= 5000){
      if (biotope_df[i,2] == "?ê¸²" | biotope_df[i,2] == "Á¶??????"){
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


# all green open space
for (j in 1:3066){
  for (i in 1:1135){
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


pop <- costd[,2]
LSDF <- data.frame(obj_id, LS, pop)
LSDF_fa <- LSDF


#Spatial weight data frame
for (j in 1:3066){
  for (i in 1:1135){
    if(is.na(SWDF[j,i+2]) == TRUE){
      SWDF[j,i+2] = 0
    }
  }
}


for (j in 1:3066){
  for (i in 1:1135){
    if(is.na(SWDF_fa[j,i+2]) == TRUE){
      SWDF_fa[j,i+2] = 0
    }
  }
}


#Level of service data frame
for (j in 1:3066){
  LSDF[j,2] <- (sum(SWDF[j,c(-1,-2)]) / area_mean) - costd[j,2]
}


for (j in 1:3066){
  LSDF_fa[j,2] <- (sum(SWDF_fa[j,-1]) / area_mean) - costd[j,2]
}





write.csv(LSDF, "./LSDF_ansan_os_0208_400.csv", row.names = F)

write.csv(LSDF_fa, "./LSDF_ansan_fa_0208_400.csv", row.names = F)


