suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(missMDA))
suppressPackageStartupMessages(library(rnaturalearth))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(cluster))
suppressPackageStartupMessages(library(fpc))
suppressPackageStartupMessages(library(lingtypology))
suppressPackageStartupMessages(library(ape))

convertToWide <- function(valuescsv) {
  wider_data <- valuescsv %>% 
    dplyr::select(c('LanguageID', 'ParameterID', 'Value', 'Frequency', 'Remark')) %>% 
    group_by(LanguageID, ParameterID, Frequency, Remark) %>% 
    pivot_wider(names_from = ParameterID,
                values_from = c(Value, Frequency, Remark), 
                names_glue = "{ParameterID}.{.value}", names_vary = "slowest",
                values_fn = function(x) paste(x, collapse=";;")) %>%
    rename_at(vars(contains(".Value")), list(~ sub('.Value','',.)))
  wider_metadata <- valuescsv %>% 
    dplyr::select(c('LanguageID', 'ParameterID', 'Value', 'Source','Coder')) %>% 
    group_by(LanguageID, ParameterID, Source, Coder) %>% 
    distinct(Value) %>%
    pivot_wider(names_from = ParameterID,
                values_from = c(Value, Source, Coder), 
                names_glue = "{ParameterID}.{.value}", names_vary = "slowest",
                values_fn = function(x) paste(unique(x), collapse=";;")) %>%
    select(-contains("Value"))
  wider <- merge(wider_data, wider_metadata, by="LanguageID")
  wider <- wider %>% select(order(colnames(wider))) %>% select(LanguageID, everything())
  return(wider)
}

cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}

project_data <-  function(
    df,  # a data frame with Longitude, Latitude, and data
    world_map_initial = world_map_initial, # a natural earth map or overlays of several maps
    projection = 8859, # 8859 is the crs code for equal earth WGS84 with 150° as center meridian
    graticules_gap = 20, # gap between single graticule lines in ° (1, 5, 10, 15, 20, 30 are possible) (default 20)
    xmin = -180, # minimum longitude of desired extent (in ° between -180 and 180) (default -180)
    xmax = 180, # maximum longitude of desired extent (in ° between -180 and 180) (default 180)
    ymin = -60, # minimum latitude of desired extent (in ° between -90, 90) (default -60)
    ymax = 85, # maximum latitude of desired extent (in ° between -90, 90) (default 85)
    labels_lat = -175, # position of latitude labels on longitude axis (in ° long) (default -175)
    labels_long = -52.5 # position of latitude labels on latitude axis (in ° lat) (default -52.5)
) {
  library(tidyverse)
  library(dplyr)
  library(sf)
  library(rnaturalearth)
  library(ggplot2)
  
  #### Settings
  # target projection
  df_projection <- 4326 # projection code of the data frame (e.g. 4326 for WGS84) (default 4326)
  # meridian where world map is split up
  split_meridian <- -30
  # deactivating s2 spherical geometry to make following map crops possible
  sf_use_s2(FALSE)
  
  # duplicate base_map for further processing
  base_map <- world_map_initial
  
  # create "split line" to split polygons/linestrings that cross the splitting meridian
  split_line <- st_linestring(x = cbind(split_meridian,c(-90,90)), dim = "XY")
  split_line <- st_geometry(split_line) # makes it possible to assign crs
  st_crs(split_line) <- st_crs(base_map) # assign crs from base map to line
  
  # intersect line with continent polygons/linestrings to identify the ones that cross splitting meridian
  base_map$intersects <- suppressMessages(st_intersects(base_map, split_line, sparse = F))
  base_map_intersects <- filter(base_map, intersects == T) # map with intersecting polygons
  base_map_cleaned <- filter(base_map, intersects == F) # map without intersecting polygons
  
  # crop polygons/linestrings on both sides of splitting meridian separately
  bbox_left <- c(xmin = -180, xmax = split_meridian-0.000001, ymin = -90, ymax = 90)
  bbox_right <- c(xmin = split_meridian+0.000001, xmax = 180, ymin = -90, ymax = 90)
  # 0.000001 ensures that edges of right and left side do not have the exact same coordinates
  base_map_intersects_left <- suppressMessages(suppressWarnings(st_crop(base_map_intersects, bbox_left)))
  base_map_intersects_right <- suppressMessages(suppressWarnings(st_crop(base_map_intersects, bbox_right)))
  
  # combine all three maps
  base_map <- bind_rows(base_map_cleaned, base_map_intersects_left)
  base_map <- bind_rows(base_map, base_map_intersects_right)
  
  # crop map to desired plotting extent
  bbox_map <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  base_map <- suppressMessages(suppressWarnings(st_crop(base_map, bbox_map)))
  
  # reproject map
  base_map <- st_transform(base_map, crs = projection)
  
  #### Graticules
  # download graticules from naturalearthdata
  if (!exists(paste0("graticules_", graticules_gap))){
    assign(paste0("graticules_", graticules_gap),
           ne_download(scale = 110,
                       type = paste0("graticules_", graticules_gap),
                       category = "physical"),
           pos = 1)
  }
  
  # load the graticules file from global environment
  graticules <- get(paste0("graticules_", graticules_gap), envir = .GlobalEnv)
  
  # crop graticules to desired plotting extent
  bbox_graticule <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  graticules <- suppressMessages(suppressWarnings(st_crop(st_as_sf(graticules), bbox_graticule)))
  
  # reproject graticules
  graticules <- st_transform(graticules, crs = projection)
  
  #### Graticule Labels
  # create data frames with grid labels
  x_labs <- c("-25°", "-20°", "-15°", "-10°", "-5°", "0°", "5°", "10°", "15°", "20°",
              "25°", "30°", "35°", "40°", "45°", "50°", "55°", "60°", "65°", "70°",
              "75°", "80°", "85°", "90°", "95°", "100°", "105°", "110°", "115°", "120°",
              "125°", "130°", "135°", "140°", "145°", "150°", "155°", "160°", "165°",
              "170°", "175°", "180°", "-175°", "-170°", "-165°", "-160°", "-155°", "-150°",
              "-145°", "-140°", "-135°", "-130°", "-125°", "-120°", "-115°", "-110°",
              "-105°", "-100°", "-95°", "-90°", "-85°", "-80°", "-75°", "-70°", "-65°",
              "-60°", "-55°", "-50°", "-45°", "-40°", "-35°")
  y_labs <- c("-85°", "-80°", "-75°", "-70°", "-65°", "-60°", "-55°", "-50°", "-45°",
              "-40°", "-35°", "-30°", "-25°", "-20°", "-15°", "-10°", "-5°", "0°",
              "5°", "10°", "15°", "20°", "25°", "30°", "35°", "40°", "45°", "50°",
              "55°", "60°", "65°", "70°", "75°", "80°", "85°")
  x <- c(seq(-25, 180, by = 5), seq(-175, -35, by = 5))
  long_labels <- data.frame(labs=x_labs, x, y=labels_long)
  y <- c(seq(-85, 0, by = 5), seq(5, 85, by = 5))
  lat_labels <- data.frame(labs=y_labs, x=labels_lat, y=y)
  
  # subset data frames according to graticules gap input feature
  long_labels <- long_labels %>%
    slice(which(x %% graticules_gap == 0))
  lat_labels <- lat_labels %>%
    slice(which(y %% graticules_gap == 0))
  
  # combine data frames into one
  grid_labels <- bind_rows(long_labels, lat_labels)
  
  # create sf object
  grid_labels <- st_as_sf(grid_labels, coords = c("x", "y"), crs = 4326)
  
  # crop graticule labels to desired plotting extent
  bbox_grid <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  grid_labels <- suppressMessages(suppressWarnings(st_crop(st_as_sf(grid_labels), bbox_grid)))
  
  # reproject graticule lables
  grid_labels <- st_transform(grid_labels, crs = projection)
  
  #### data
  # change name of columns to "Longitude" and "Latitude"
  if(any(names(df) %in% 'longitude')) {
    df$Longitude <- df$longitude
    df$Latitude <- df$latitude
  }
  
  if(any(names(df) %in% 'lon')) {
    df$Longitude <- df$lon
    df$Latitude <- df$lat
  }
  
  if(any(names(df) %in% 'long')) {
    df$Longitude <- df$long
    df$Latitude <- df$lat
  }
  
  # create an sf object
  df = st_as_sf(df, coords = c("Longitude", "Latitude"), crs = df_projection)
  
  # crop data frame to desired plotting extent
  bbox_data <- c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  df <- suppressMessages(suppressWarnings(st_crop(df, bbox_data)))
  
  # reproject data frame to desired projection
  df <- st_transform(df, crs = projection)
  
  #### ggplot as base plot with base map
  base_plot <- ggplot() +
    geom_sf(data = base_map, colour = "darkgrey", fill = "transparent", size = .25) +
    scale_size(range = c(10, 10)) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.box.background = element_rect(fill = "white", color = "white"),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank()
    ) +
    guides(color = guide_legend(override.aes = list(size = 6)))
  
  #### objects returned by function
  return(list(base_map = base_map,
              graticules = graticules,
              grid_labels = grid_labels,
              data = df,
              base_plot = base_plot))
}

# get map from Natural Earth data for sf, including islands
world_map <- ne_download(scale = 110, category = "physical", type = "land", returnclass = "sf")
minor_islands <- ne_download(scale = 10, category = "physical", type = "minor_islands", returnclass = "sf")
world_map_initial <- rbind(world_map, minor_islands)

#read raw data
valuescsv <- read.csv("~/Documents/GitHub/ooa-processing/cldf/values.csv", na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)
languagescsv <- read.csv("~/Documents/GitHub/ooa-processing/cldf/languages.csv", na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)
parameterscsv <- read.csv("~/Documents/GitHub/ooa-processing/cldf/parameters.csv", na.strings = c(), colClasses = c("character"), stringsAsFactors = FALSE)


# rename lat, long
names(languagescsv)[names(languagescsv) == "Latitude"] <- "lat"
names(languagescsv)[names(languagescsv) == "Longitude"] <- "lon"

# manipulate data
atlasdata <- convertToWide(valuescsv)
atlasdata[atlasdata=="?"] <- NA
rownames(atlasdata) <- atlasdata$LanguageID
atlasdata[,1] <- NULL
# remove non-data columns
atlasdata <- atlasdata %>% 
  select(-contains(c(".Coder",".Frequency",".Remark",".Source")))
# removes features that are not single-valued
toremove <- parameterscsv$ParameterID[parameterscsv$datatype != '"single-valued"']
atlasdata <- select(atlasdata, -toremove)
# remove derived features
# atlasdata <- atlasdata[, -grep("-\\d+(a|b|c|d|e|f|g|h)$", colnames(atlasdata))]
# remove columns with multiple values
# atlasdata <- atlasdata %>% select(-names(atlasdata)[apply(atlasdata, 2, function(x) any(grepl(";;", x)))])
# remove all dependent features with less than 80% of filled in data
atlasdata <- atlasdata[, colSums(atlasdata=="NA", na.rm=TRUE) < 325/5]
# turn NA into missing data
# atlasdata[atlasdata=="NA"] <- NA
# remove Piraha -- necessary for maps
row.names.remove <- c("pira1253")
atlasdata <- atlasdata[!(row.names(atlasdata) %in% row.names.remove), ]
# make into factors
atlasdata[] <- lapply(atlasdata, factor)

# hclustering: world
atlasdata.renamed <- atlasdata
atlasdata.renamed$Glottocode <- rownames(atlasdata.renamed)
atlasdata.renamed <- merge(atlasdata.renamed, languagescsv, by.x = "Glottocode", by.y="Glottocode")
rownames(atlasdata.renamed) <- atlasdata.renamed$Name
atlasdata.renamed <- atlasdata.renamed %>% select(-names(languagescsv))

gower.dist <- daisy(atlasdata.renamed[ , 1:length(names(atlasdata.renamed))], metric = c("gower"))

# divisive clustering
divisive.clust <- diana(as.matrix(gower.dist), diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

# agglomerative clustering
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c, main = "Agglomerative, complete linkages")

# look for best algorithm, best k

# Elbow
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 20))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 20))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

# Silhouette
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 20))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 20))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

# assuming best k is 6, put cluster into atlasdata.renamed 
clusters <- cutree(aggl.clust.c, 6)
atlasdata.renamed$clusters <- clusters
atlasdata.renamed$clusters <- ifelse(atlasdata.renamed$clusters == 1, "brown2",
                              ifelse(atlasdata.renamed$clusters == 2, "orange",
                              ifelse(atlasdata.renamed$clusters == 3, "yellow",
                              ifelse(atlasdata.renamed$clusters == 4, "springgreen",
                              ifelse(atlasdata.renamed$clusters == 5, "#1A2CEB", "#874AFC")))))

# add back lat/lon
atlasdata.renamed$Name <- rownames(atlasdata.renamed)
atlasdata.renamed <- merge(atlasdata.renamed, languagescsv, by.x = "Name", by.y="Name")
toremove <- names(languagescsv)
toremove <- toremove[!toremove %in% c("lat", "lon", "Glottocode")]
atlasdata.renamed <- atlasdata.renamed %>% select(-toremove)

# get map
rgb_shifted <- project_data(df = atlasdata.renamed, world_map_initial = world_map_initial)

pacific_centered_map <- rgb_shifted$base_plot +
  geom_sf(data = rgb_shifted$data, aes(fill = clusters, color = clusters), shape = 21, size = 1.5) +
  labs(title = "Hierarchical clustering k=6") +
  theme_minimal() +
  scale_fill_identity() +  # use color values as specified in the data
  scale_color_identity() +  # use color values as specified in the data
  guides(fill = "none", color = "none")+
  theme(panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),axis.ticks = element_blank())


dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 6) %>% #, value=c("brown2", "orange", "yellow", "springgreen", "#1A2CEB", "#874AFC")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("black")) %>% 
  set("labels_cex", 0.3)
ggd1 <- as.ggdend(dendro.col)
ggp <- ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 6") +
  theme_void()
ggsave("clustering.png", plot=ggp, width=14, height=16, dpi=500)

#hclustering, americas
atlasdata.americas <- atlasdata
atlasdata.americas$Glottocode <- rownames(atlasdata.americas)
atlasdata.americas <- merge(atlasdata.americas, languagescsv, by = "Glottocode") %>% 
  filter(Macroarea == "North America" | Macroarea == "South America")
rownames(atlasdata.americas) <- atlasdata.americas$Glottocode
atlasdata.americas <- atlasdata.americas %>% select(-names(languagescsv))

gower.dist <- daisy(atlasdata.americas[ , 1:length(names(atlasdata.americas))], metric = c("gower"))

# agglomerative clustering
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c, main = "Agglomerative, complete linkages")

# assuming best k is 5, put cluster into atlasdata.americas 
clusters <- cutree(aggl.clust.c, 5)
atlasdata.americas$clusters <- clusters
atlasdata.americas$clusters <- ifelse(atlasdata.americas$clusters == 1, "brown2",
                               ifelse(atlasdata.americas$clusters == 2, "yellow",
                               ifelse(atlasdata.americas$clusters == 3, "springgreen",
                               ifelse(atlasdata.americas$clusters == 4, "#1A2CEB", "#874AFC"))))

# add back lat/lon
atlasdata.americas$Glottocode <- rownames(atlasdata.americas)
atlasdata.americas <- merge(atlasdata.americas, languagescsv, by = "Glottocode")
toremove <- names(languagescsv)
toremove <- toremove[!toremove %in% c("lat", "lon", "Glottocode")]
atlasdata.americas <- atlasdata.americas %>% select(-toremove)

# get map
rgb_shifted <- project_data(df = atlasdata.americas, world_map_initial = world_map_initial)

pacific_centered_map <- rgb_shifted$base_plot +
  geom_sf(data = rgb_shifted$data, aes(fill = clusters, color = clusters), shape = 21, size = 1.5) +
  labs(title = "Hierarchical clustering k=6") +
  theme_minimal() +
  scale_fill_identity() +  # use color values as specified in the data
  scale_color_identity() +  # use color values as specified in the data
  guides(fill = "none", color = "none")+
  theme(panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),axis.ticks = element_blank())



# do FAMD
# impute
atlasdata.impute <- imputeFAMD(atlasdata, ncp=3, maxiter = 1000000)
# do FAMD
atlasdata.famd <- FAMD(atlasdata, tab.disj = atlasdata.impute$tab.disj, graph=FALSE, ncp=3)
# plot(atlasdata.famd, choix="ind", axes=c(1,2), lab.var=TRUE)
fviz_famd_ind(atlasdata.famd, col.ind = "cos2",
              label=c("ind"), labelsize=3, arrowsize=0,
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = FALSE)

# maps
RGB_mapping <- function(famd.object, taxonomy_matrix, pc1flip=F, pc2flip=F, pc3flip=F) {
  library(scales)
  gg <- as.data.frame(taxonomy_matrix)[ , c('Glottocode','lat', 'lon')]
  df <- data.frame(get_famd(famd.object)$coord, level1=rownames(get_famd(famd.object)$coord))
  
  # map PCs to RGB:
  if(pc1flip){
    pcrange <- range(df$Dim.1)
    pcmid <- mean(pcrange)
    pcflip <- df$Dim.1-2*(df$Dim.1-pcmid)
    df$Dim.1 <- pcflip
  }
  
  if(pc2flip){
    pcrange <- range(df$Dim.2)
    pcmid <- mean(pcrange)
    pcflip <- df$Dim.2-2*(df$Dim.2-pcmid)
    df$Dim.2 <- pcflip
  }
  
  if(pc3flip){
    pcrange <- range(df$Dim.3)
    pcmid <- mean(pcrange)
    pcflip <- df$Dim.3-2*(df$Dim.3-pcmid)
    df$Dim.3 <- pcflip
  }
  
  df$pc.colors1to3 <- with(df, rgb(red=rescale_mid(Dim.1), 
                                   green=rescale_mid(Dim.2), 
                                   blue=rescale_mid(Dim.3), alpha=0.8))
  # df$pc.colors4to6 <- with(df, rgb(red=rescale_mid(Dim.4), 
  #                                  green=rescale_mid(Dim.5), 
  #                                  blue=rescale_mid(Dim.6), alpha=0.8))
  # df$pc.colors7to9 <- with(df, rgb(red=rescale_mid(Dim.7), 
  #                                  green=rescale_mid(Dim.8), 
  #                                  blue=rescale_mid(Dim.9), alpha=0.8))
  geo <- subset(gg, gg[,1] %in% df$level1)
  geo.world <- merge(df, 
                     geo, by.x='level1', by.y=names(gg)[1])
  return(geo.world)
}

grayscale_mapping <- function(famd.object, taxonomy_matrix) {
  library(scales)
  gg <- as.data.frame(taxonomy_matrix)[ , c('Glottocode','lat', 'lon')]
  df <- data.frame(get_famd(famd.object)$coord, level1=rownames(get_famd(famd.object)$coord))
  
  # map dimension to grayscale:
  df$dim.color1 <- with(df, gray(rescale_mid(Dim.1), alpha=0.8))
  df$dim.color2 <- with(df, gray(rescale_mid(Dim.2), alpha=0.8))
  df$dim.color3 <- with(df, gray(rescale_mid(Dim.3), alpha=0.8))
  df$dim.color4 <- with(df, gray(rescale_mid(Dim.4), alpha=0.8))
  df$dim.color5 <- with(df, gray(rescale_mid(Dim.5), alpha=0.8))
  df$dim.color6 <- with(df, gray(rescale_mid(Dim.6), alpha=0.8))
  df$dim.color7 <- with(df, gray(rescale_mid(Dim.7), alpha=0.8))
  df$dim.color8 <- with(df, gray(rescale_mid(Dim.8), alpha=0.8))
  df$dim.color9 <- with(df, gray(rescale_mid(Dim.9), alpha=0.8))
  geo <- subset(gg, gg[,1] %in% df$level1)
  geo.world <- merge(df, 
                     geo, by.x='level1', by.y=names(gg)[1])
  return(geo.world)
}


# plotting RGB map 
rgb_map <- function(rgb_data_frame, world_map_initial, main, plot, size, famd.object){
  main <- paste(main,", Dim1-Dim3 (",round(get_eigenvalue(famd.object)['Dim.3', 'cumulative.variance.percent'],2),"%)",sep="")
  rgb_shifted <- project_data(df = rgb_data_frame, world_map_initial = world_map_initial)
  pacific_centered_map <- rgb_shifted$base_plot +
    geom_sf(data = rgb_shifted$data, aes(fill = pc.colors1to3, color = pc.colors1to3), shape = 21, size = size) +
    labs(title = main) +
    theme_minimal() +
    scale_fill_identity() +  # use color values as specified in the data
    scale_color_identity() +  # use color values as specified in the data
    guides(fill = "none", color = "none")+
    theme(panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
          axis.text = element_blank(),axis.ticks = element_blank())
  if(plot==T){
    print(pacific_centered_map)
  }
  return(pacific_centered_map)
}

gray_map <- function(gray_data_frame, world_map_initial, main, plot, size, dim, famd.object) {
  main <- paste(main,", Dim", dim, sep="")
  gray_shifted <- project_data(df = gray_data_frame, world_map_initial = world_map_initial)
  dimension <- paste0("dim.color",dim)
  pacific_centered_map <- gray_shifted$base_plot +
    geom_sf(data = gray_shifted$data, aes(fill = !!sym(dimension), color = !!sym(dimension)), shape = 21, size = size) +
    labs(title = main) +
    theme_minimal() +
    scale_fill_identity() +  # use color values as specified in the data
    scale_color_identity() +  # use color values as specified in the data
    guides(fill = "none", color = "none")+
    theme(panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5),
          axis.text = element_blank(),axis.ticks = element_blank())
  if(plot==T){
    print(pacific_centered_map)
  }
  return(pacific_centered_map)
}

"all FTT"
"4/5 no Piraha TFF FTF"
"3/4 no Piraha FTF"
atlas.rgb <- RGB_mapping(atlasdata.famd, languagescsv, pc1flip = F, pc2flip = T, pc3flip = F) %>% na.omit()
atlas.rgbmap <- rgb_map(atlas.rgb, world_map_initial, main="ATLAs FAMD", plot=F, size = 2, famd.object = atlasdata.famd)
ggsave("atlas_famd-singlevaluedonly-0.8.png", plot = atlas.rgbmap, width = 9, height = 8, dpi = 500)



# gray maps

atlas.gray <- grayscale_mapping(atlasdata.famd, languagescsv) %>% na.omit()
atlas.graymap1 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 1, famd.object = atlasdata.famd)
atlas.graymap2 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 2, famd.object = atlasdata.famd)
atlas.graymap3 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 3, famd.object = atlasdata.famd)
atlas.graymap4 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 4, famd.object = atlasdata.famd)
atlas.graymap5 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 5, famd.object = atlasdata.famd)
atlas.graymap6 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 6, famd.object = atlasdata.famd)
atlas.graymap7 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 7, famd.object = atlasdata.famd)
atlas.graymap8 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 8, famd.object = atlasdata.famd)
atlas.graymap9 <- gray_map(atlas.gray, world_map_initial, main="ATLAs FAMD", plot=F, size = 1.2, dim = 9, famd.object = atlasdata.famd)


# plot FAMD onto families

families_more_than_4 <- c()
for (family in unique(languagescsv$Family_Name)) {
  if (family != "" & length(languagescsv$Glottocode[languagescsv$Family_Name == family]) > 4) {
    families_more_than_4 <- c(families_more_than_4, family)
  }
}

tree <- read.nexus("../../consonant-stability/code/global-language-tree-MCC-labelled.tree")
taxa <- tree$tip.label
# split each taxon at the first underscore and keep the first part
taxa <- sapply(strsplit(taxa, "_"), function(x) x[1])
tree$tip.label <- taxa


ua <- drop.tip(tree, setdiff(tree$tip.label, languagescsv$Glottocode[languagescsv$Family_Name == "Athabaskan-Eyak-Tlingit"]))
plot(ua)
ua_colors <- atlas.rgb %>% filter(level1 %in% languagescsv$Glottocode[languagescsv$Family_Name == "Athabaskan-Eyak-Tlingit"])
rownames(ua_colors) <- ua_colors$level1
ua_colors <- as.matrix(ua_colors)[,"pc.colors1to3"]
dotTree(ua, ua_colors, colors=setNames(as.data.frame(ua_colors)[,"ua_colors"], as.data.frame(ua_colors)[,"ua_colors"]))