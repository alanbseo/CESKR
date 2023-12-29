# install_github("DougLuke/UserNetR")
# library(UserNetR)

library(bigmemory)

library(rgexf)
library(openxlsx)
library(igraph)
library(rgdal)
library(scales)
library(gplots)
library(reshape2)
library(plyr)


library(fclust)
library(cluster)



# 1. Load data

# Mac Alan 
proj4.LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"


path_base = "~/Dropbox/KIT/CES_SEOUL/CESKR/"
path_out_csv = "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/MergedCSV/"

path_xls = "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_download/Sep2023_Korea_V2/Xlsx/"
path_out_network =  "~/Dropbox/KIT/CES_SEOUL/FlickrKOR_result/Network/"




setwd(path_base)

proj4.LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

proj4.DHDN <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=1 +x_0=4500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs" # epsg:31468


places_all_m = readRDS(paste0(path_out_network, "/places_all_m.Rds"))

# imgnet_all_m = read.csv("Data/imgnet_all_m.csv")
# imgnet_all_m$X.1 = NULL
# imgnet_all_m$X = NULL
# 
# 
# colnames(imgnet_all_m) = imgnet_all_m[1,]
# imgnet_all_m = imgnet_all_m[-1,]
# rownames(imgnet_all_m) = colnames(imgnet_all_m)

imgnet_all_m = readRDS(paste0(path_out_network, "/imagenet_all_m.Rds"))



# write.big.matrix( "Data/imagenet_all_m_bigmatrix.txt")

# imgnet_all_bm = read.big.matrix( "Data/imagenet_all_m_bigmatrix.txt") # didn't work well

# Use absolute path (again didn't work)
# imgnet_all_bm = read.big.matrix( normalizePath("Data/imagenet_all_m_bigmatrix.txt"), type="double", header=TRUE)


# joint_all_m = readRDS( "Data/joint_all_m.Rds")


# summary(as.numeric(imgnet_all_m ))
# quantile(as.numeric(imgnet_all_m ), probs = c(0.05, 0.1, 0.2, 0.999))

imgnet_v = as.numeric(imgnet_all_m )
# table(imgnet_v > 1E-3)
# table(imgnet_v > 1E-2)
# table(imgnet_v > 1E-4)

imgnet_rowmed_v = apply(imgnet_all_m, MARGIN = 1, FUN = median, na.rm=T)
imgnet_rowsavg_v = apply(imgnet_all_m, MARGIN = 1, FUN = mean, na.rm=T)

# summary(imgnet_rowsavg_v)
# table(imgnet_rowsavg_v > 1E-1)

thr = 1E-5
# table(imgnet_rowsavg_v>thr)

imgnet_all_m2 = imgnet_all_m[imgnet_rowsavg_v>thr, imgnet_rowsavg_v>thr]

imgnet_occurred = colnames(imgnet_all_m2)

imgnet21k_tags_ids = readLines("Data/imagenet21k_wordnet_ids.txt") %>% as.vector
imgnet21k_tags_verbose = readLines("Data/imagenet21k_wordnet_lemmas.txt")
imgnet21k_tags = as.vector(imgnet21k_tags_verbose)

imgnet21k_tags_df = data.frame(ID = imgnet21k_tags_ids, Tag = imgnet21k_tags) 


imgnet21k_tags_unique = unique(imgnet21k_tags)



imgnet_occurred_lemmas = imgnet21k_tags_df$Tag[match(imgnet_occurred, imgnet21k_tags_df$ID)]

imgnet_all_m3 = imgnet_all_m2
colnames(imgnet_all_m3) = rownames(imgnet_all_m3) = imgnet_occurred_lemmas

summary(diag(imgnet_all_m3))

saveRDS(imgnet_all_m2, file = paste0(path_out_network,"/imagenet_all_reduced_m2.rds"))

saveRDS(imgnet_all_m3, file = paste0(path_out_network,"/imagenet_all_reduced_m3.rds"))

tags_l = list(places_all_m, imgnet_all_m3)#, joint_all_m)
dt_names = c("Places365", "IMAGENET")#, "Hybrid")

# Walk-trap steps
N_STEPS = as.integer(c(365/10, 2.1E3 / 10 ))



idx = 1





for (idx in seq_along(dt_names)) { 
    
    N_STEP = N_STEPS[idx]
    
    tags_dt = tags_l[[idx]]
    dt_name = dt_names[[idx]]
    # dimnames(tags.dt)
    tags_v <- colnames(tags_dt)
    tags_m <- (as.matrix(data.frame(tags_dt)))
    dimnames(tags_m) <- list( tags_v, tags_v)
    total_occur <- colSums(tags_m)
    
    tags_m[lower.tri(tags_m, diag=T)] <- 0
    max(tags_m) # previously 2460
    
    
    
    
    
    
    # 2. Create weighted graph 
    fl_graph <- graph.adjacency(tags_m,
                                weighted=TRUE,
                                mode="undirected",
                                diag=TRUE)
    
    
    
    # Cluster
    cw <-  cluster_walktrap(fl_graph, steps=N_STEP, membership = T, weights = E(fl_graph)$weight)  # step: the length of the random walks to perform. 
    pdf(paste0(paste0(path_out_network, "/",dt_name, "_Modularity.pdf")), width=30, height = 20)
    
    mod_v = sapply(1:300, FUN = function(x)  modularity(fl_graph,  cut_at(cw, no = x)))
    
    plot(mod_v, type="o", xlab= "K", ylab= "Modularity", main = paste0("Modularity changing with K (Walktrap) for ", dt_name))
    # sapply(1:50, FUN = function(x)  modularity(fl_graph,  cut_at(cw, no = x)))
    dev.off()
    
    which.max(mod_v)
    
    write.xlsx(data.frame(K = 1:300, Modularity =mod_v), file = paste0(path_out_network, "/cw_modularity_", dt_name, ".xlsx"))
    
    # plot(fl_graph)
    
    # Fuzzy clustering
    
    # One of the main drawbacks of fuzzy clustering algorithms is their sensitivity to the choice of parameters, particularly the fuzziness parameter. In fuzzy clustering, each data point can belong to multiple clusters with different degrees of membership. The degree of membership is controlled by the fuzziness parameter (typically denoted by "m" in algorithms like Fuzzy C-Means).
    # Fuzzy clustering may be more sensitive to noise in the data.
    # Fuzzy 클러스터링이 더 좋지는 않다
    
    
    
    
    pdf(paste0(paste0(path_out_network, "/",dt_name, "_dendrogram_reduced.pdf")), width=30, height = 20)
    # igraph::plot_dendrogram(cw, cex=0.2, mode = "phylo") # requires 'ape'
    igraph::plot_dendrogram(cw, cex=0.2, mode = "hclust")
    # igraph::plot_dendrogram(cw, cex=0.3, mode = "dendrogram")
    
    dev.off()
    
    
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K3", value = cut_at(cw, no = 3))
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K5", value = cut_at(cw, no = 5))
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K7", value = cut_at(cw, no = 7))
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K9", value = cut_at(cw, no = 9))
    
    for(k in 11:30) { 
        fl_graph <- set_vertex_attr(fl_graph, paste0("cluster_K", k), value = cut_at(cw, no = k))
        
    }
    
    opt_k = which.max(mod_v)
    
    
    fl_graph <- set_vertex_attr(fl_graph, paste0("cluster_K", opt_k, "opt"), value = cut_at(cw, no = opt_k))
    
    
    
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K50", value = cut_at(cw, no = 50))
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K60", value = cut_at(cw, no = 60))
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K100", value = cut_at(cw, no = 100))
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K107", value = cut_at(cw, no = 107))
    fl_graph <- set_vertex_attr(fl_graph, "cluster_K200", value = cut_at(cw, no = 200))
    # fl_graph <- set_vertex_attr(fl_graph, "cluster_K300", value = cut_at(cw, no = 300))
    # fl_graph <- set_vertex_attr(fl_graph, "cluster_K400", value = cut_at(cw, no = 400))
    # fl_graph <- set_vertex_attr(fl_graph, "cluster_K500", value = cut_at(cw, no = 500))
    # 
    # fl_graph <- set_vertex_attr(fl_graph, "cluster_K5000", value = cut_at(cw, no = 5000))
    # fl_graph <- set_vertex_attr(fl_graph, "cluster_K10000", value = cut_at(cw, no = 10000))
    
    # write.graph(fl_graph, file = "cw5.dl", format = "pajek")
    
    # g1.gexf <- igraph.to.gexf(fl_graph)
    write.graph(fl_graph, file = paste0(path_out_network, "/cw_", dt_name, "_reduced.gml"), format = "gml")
    
    saveRDS(cw, paste0(path_out_network, "/cw_", dt_name, "_reduced.Rds"))
    
    k_v = 1:200
    cluster_df = sapply(k_v, FUN = function(x) cut_at(cw, no = x))
    
    rownames(cluster_df) = tags_v
    colnames(cluster_df) = paste0("Cluster_", k_v)
    
    
    round(sapply(1:length(k_v), FUN = function(x) mean(table(cluster_df[,x])[1:9])))
    round(sapply(1:length(k_v), FUN = function(x) max(table(cluster_df[,x]))))
    
    plot(sapply(1:length(k_v), FUN = function(x) max(table(cluster_df[,x]))), type="l", ylab="Max. freq. of a single cluster", xlab="Number of Clusters")
    
    
    cls_tmp = data.frame(Tag = tags_v,  cluster_df[,])
    rownames(cls_tmp) = tags_v
    
    
    write.xlsx(cls_tmp, file = paste0(path_out_network, "/ClusterInfo_", dt_name, ".xlsx"))
    
    
    
    # write.xlsx(tags_m, file = paste0("Data/CooccurenceMatrix_", dt_name, ".xlsx"))
    
}


stop("ends here")

cw_5 <- igraph::cut_at(cw, no = 5)
table(cw$membership)
table(cw_5)
# igraph::plot_hierarchy(cw_5, fl_graph)
igraph::plot_dendrogram(cw)

# igraph::plot_hierarchy(cw, fl_graph)



# modularity(fl_graph, membership = cw$membership)
# modularity(fl_graph, membership = cw_5)
#  

# cfg <- cluster_fast_greedy(fl_graph)
# system.time(    clb <- cluster_label_prop(fl_graph, weights = E(fl_graph)$weight))
# system.time(    clv <- cluster_louvain(fl_graph, weights = E(fl_graph)$weight))
# system.time(    ci <- cluster_infomap(fl_graph))


plot(fl_graph, col = cw_5)





# plot(cw_5, fl_graph)


# V(fl_graph)$membership <- tmp$membership
# str(vertex_attr(fl_graph))

k_v <- 3:20
n_k <- length(k_v)



algorithm.names <- c( "Walktrap") # , "FastGreedy")
n.algorithm <- length(algorithm.names)


s.l <- s.major.l <-  in.dt.l <- vector("list", n.algorithm) 



for (idx in 1:3) { 
    
    algorithm.l <- list(cw) # , cfg)  # walktrop, groups: 5, mod: 0.26 <-? different from the plot. 
    
    for (al.idx in 1:n.algorithm) {
        
        cluster.tmp <- algorithm.l[[al.idx]]
        al.name <- algorithm.names[al.idx]
        
        s.l[[al.idx]] <- s.major.l[[al.idx]] <-in.dt.l[[al.idx]] <- vector("list", n.k) 
        
        for (k.idx in 1:n.k) {
            
            k.tmp <- k.v[k.idx]
            
            cluster.tmp$membership <- cut_at(cluster.tmp, no = k.tmp)
            lookup.dt.tmp <- data.frame(tags.v, cluster.tmp$membership )
            
            lookup.dt.tmp.by <- by(lookup.dt.tmp$tags.v, INDICES = lookup.dt.tmp$cluster.tmp.membership, FUN = as.character)
            
            lookup.dt.tmp.by.table <- t(rbind.fill( lapply(lookup.dt.tmp.by, FUN = function(x) data.frame(t(x)))))
            colnames(lookup.dt.tmp.by.table) <- paste0("Cluster_", 1:k.tmp)
            
            #write.xlsx(lookup.dt.tmp.by.table, file = paste0("tmp/", al.name, "_tag_clusters_k", k.tmp, ".xlsx"))
            
            
            clusters.dt.tmp <-  apply(rawdata.dt[, ], MARGIN = 1, FUN = function(x) {
                lookup.dt.tmp[match(x[1:20], lookup.dt.tmp$tags.v), 2]
            })
            
            major.clusters.dt.tmp <-  apply(rawdata.dt[, ], MARGIN = 1, FUN = function(x) {
                x.tb <- lookup.dt.tmp[match(x[1:20], lookup.dt.tmp$tags.v), 2]
                # x.tb[is.na(x.tb)] <- "NoCluster"
                x.tb[is.na(x.tb)] <- k.tmp + 1 
                
                names(x.tb) <- x.tb
                x.wt <- x[21:40]
                x.weighted.cluster <- tapply(as.numeric(x.wt), INDEX = x.tb, FUN = sum)
                return(names(x.weighted.cluster)[which.max(x.weighted.cluster)])
            })
            
            
            s.l[[al.idx]][[k.idx]] <- apply(clusters.dt.tmp, MARGIN = 2, FUN = function(x) {x.f <- factor(x, levels = 1:k.tmp, labels = 1:k.tmp); x.t <- table(x.f, useNA = "always"); x.t / sum(x.t)})
            in.dt.l[[al.idx]][[k.idx]]  <- cbind(names(rawdata.dt), rawdata.dt[, ], t(clusters.dt.tmp))
            
            s.major.l[[al.idx]][[k.idx]] <-  major.clusters.dt.tmp
            
            
        }
        
    }
    
}

### 
cw_clusters_num <- do.call(cbind, lapply(3:20, FUN = function(x) cut_at(cw, no = x)))
cw_clusters <- cbind(cw$names, cw_clusters_num)



# cfg.clusters.num <-  do.call(cbind, lapply(3:20, FUN = function(x) cut_at(cfg, no = x)))
# cfg.clusters <- cbind(cfg$names,cfg.clusters.num)


cw.clusters.prop <-  sapply(1:n_k, FUN = function(x) {
    x2 <- numeric(max(k_v)); 
    x2[1:k_v[x]] <- table(cw_clusters_num[,x])/sum(table(cw_clusters_num[,x])) * 100 ; 
    return(x2)}
)

# 
# cfg.clusters.prop <-  sapply(1:n.k, FUN = function(x) {
#     x2 <- numeric(max(k.v)); 
#     x2[1:k.v[x]] <- table(cfg.clusters.num[,x])/sum(table(cfg.clusters.num[,x])) * 100 ; 
#     return(x2)}
# )


write.xlsx(cw_clusters, file = "Data/cw.clusters_3to20.xlsx")


# dummy.sp <- in.sp 
# 
# in.sp <- SpatialPointsDataFrame(cbind(as.numeric(rawdata.dt$Longitude), as.numeric(rawdata.dt$Latitude)), proj4string = CRS(proj4.LL), data = rawdata.dt)
# 
# dummy.250m.r.ll <- projectRaster(usdyav, crs = proj4.LL)
# dummy.250m.r.ll <- crop(dummy.250m.r.ll, in.sp)
# 
# dummy.250m.r.ll <- setValues(dummy.250m.r.ll, 0)
# dummy.1000m.r.ll <- setValues(aggregate(dummy.250m.r.ll, 4, fun = mean), 0)
# dummy.2000m.r.ll <- setValues(aggregate(dummy.250m.r.ll, 8, fun = mean), 0)
# 
# dummy.1000m.p <- (rasterToPolygons(dummy.1000m.r.ll, dissolve = F))
# dummy.2000m.p <- (rasterToPolygons(dummy.2000m.r.ll, dissolve = F))
# 
# 
# mulde.sachsen.p <- readOGR("Data", layer = "Mulde_boundary_Sachsen")
# mulde.sachsen.p.ll <- spTransform(mulde.sachsen.p, CRSobj = proj4.LL)
# 
# srtm <- raster("Data/SRTM_Mulde_DHDN.tif")
# srtm.ll <- projectRaster(srtm, crs = proj4.LL)
# 
# srtm.ll.crop <- crop(srtm.ll, mulde.sachsen.p.ll)
# 
# 
# 
# ## Functions
# getMoltSP <- function(in.dt, k) {
#     
#     molt.dt <- melt(in.dt, id.vars = c(1:11), measure.vars = c(12:31))
#     molt.sp <- SpatialPointsDataFrame(cbind(as.numeric(molt.dt$Longitude), as.numeric(molt.dt$Latitude)), proj4string = CRS(proj4.LL), data = molt.dt)
#     molt.sp$value <- as.numeric(molt.dt$value)
#     molt.sp$color <- rainbow(k)[ molt.dt$value]
#     return(molt.sp)
# }
# 
# save.image(file = paste0( "tmp/flickr_plotting_workspace", Sys.Date(), ".RData")    )
# 
# 
# stop("ends here")



