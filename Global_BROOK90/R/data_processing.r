##### Data processing and preparation for BROOK90 initial input #####
### 22/06/2020 update
### Ivan Vorobevskii
### Germany, TU Dresden, Institute of Hydrology and Meteorology, Chair of Meteorology

data_processing = function (catchment_path,
                            dem_dir,
                            soil_dir,
                            landcover_dir,
                            meteo_dir,
                            model_output_dir,
                            hourly_P,
                            meteo_averaging) {

### load catchment ###
catchment = readOGR(catchment_path, verbose=F)
catchment_centre = round(coordinates(gCentroid(catchment)),2)

### create regular grid for catchment (res 50 m for soil/landuse, 30 m for DEM), catchment mean elevation, slope, center ###
lon_length = cos(abs(catchment_centre[2])*pi/180)*111.321 # [km]
lat_length = 111 # [km]
lon_cellsize = 1/lon_length/(1000/50) # in degree 50 m
lat_cellsize = 1/lat_length/(1000/50) # in degree 50 m
lon_cellsize2 = 1/lon_length/(1000/30) # in degree 30 m
lat_cellsize2 = 1/lat_length/(1000/30) # in degree 30 m
catchment_buff = suppressWarnings(gBuffer(catchment, width=lon_cellsize, quadsegs=20))
grid_points = makegrid(catchment_buff, cellsize = c(lon_cellsize,lat_cellsize))
grid = SpatialPoints(grid_points, proj4string = CRS(proj4string(catchment)))
grid = SpatialPixels(grid[catchment,])
grid_points2 = makegrid(catchment_buff, cellsize = c(lon_cellsize2,lat_cellsize2))
grid2 = SpatialPoints(grid_points2, proj4string = CRS(proj4string(catchment)))
grid2 = SpatialPixels(grid2[catchment,])
#grid = SpatialPointsDataFrame(grid, data.frame(ID=1:length(grid)))
#writeOGR(grid, ".", "filename", driver="ESRI Shapefile")
model_grid_pic = ggplot() +
  suppressMessages(geom_polygon(data=catchment, aes(x=long, y=lat, group=group), colour="red", fill='red')) +
  suppressMessages(geom_point(data=as.data.frame(grid), aes(x=x1, y=x2), colour="black", shape=3)) +
  theme_bw() +
  labs(title='Catchment model grid (30m)', x='Longitude', y='Latitude')
setwd(model_output_dir)
suppressMessages( ggsave('catchment_model_50m_grid.png', model_grid_pic) )

### calculate elevation and slope
setwd(dem_dir)
dem_raster = raster('DEM.tif')
mean_catchment_evel = round(mean(extract(dem_raster, grid2), na.rm=T), 2)
terrain_raster = terrain(dem_raster, opt="slope", unit="degrees")
mean_catchment_slope_deg = round(mean(extract(terrain_raster, grid2), na.rm=T), 2)
mean_catchment_slope_rad = round(mean_catchment_slope_deg*pi/180, 4)
print('      catchment slope and grid -> completed')

### get soil texture [-], coarse fragments [%] for 7 layers and depth to bedrock [cm] ###
setwd(soil_dir)
st1_r = raster('soil.texture_1.tif')
st2_r = raster('soil.texture_2.tif')
st3_r = raster('soil.texture_3.tif')
st4_r = raster('soil.texture_4.tif')
st5_r = raster('soil.texture_5.tif')
st6_r = raster('soil.texture_6.tif')
st7_r = raster('soil.texture_7.tif')
scf1_r = raster('soil.coarse_fragments_1.tif')
scf2_r = raster('soil.coarse_fragments_2.tif')
scf3_r = raster('soil.coarse_fragments_3.tif')
scf4_r = raster('soil.coarse_fragments_4.tif')
scf5_r = raster('soil.coarse_fragments_5.tif')
scf6_r = raster('soil.coarse_fragments_6.tif')
scf7_r = raster('soil.coarse_fragments_7.tif')
sb_r = raster('soil.bedrock_depth.tif')
st1 = extract(st1_r, grid)
st2 = extract(st2_r, grid)
st3 = extract(st3_r, grid)
st4 = extract(st4_r, grid)
st5 = extract(st5_r, grid)
st6 = extract(st6_r, grid)
st7 = extract(st7_r, grid)
scf1 = extract(scf1_r, grid)
scf2 = extract(scf2_r, grid)
scf3 = extract(scf3_r, grid)
scf4 = extract(scf4_r, grid)
scf5 = extract(scf5_r, grid)
scf6 = extract(scf6_r, grid)
scf7 = extract(scf7_r, grid)
sb = extract(sb_r, grid)
sb = cut(sb, seq(0,200,10), labels=seq(10,200,10)) # reclassify depth to bedrock by 10 cm to reduce unique hydrotops
sb = as.numeric(levels(sb)[sb])
print('      raw soil data -> completed')

### get land cover type ###
#files are downloaded 20*20 grad - possible issue if catchment is on boundary - not solved
setwd(landcover_dir)
land_cover_r = raster('lc.tif')
lc = extract(land_cover_r, grid)
print('      raw land cover data -> completed')

### locate unique hydrotops in catchment ###
soil_class_name = list('Clay'=1,'Silty clay'=2,'Sandy clay'=3,'Clay loam'=4,'Silty clay loam'=5,'Sandy clay loam'=6,'Loam'=7,'Silt loam'=8,'Sandy loam'=9,'Silt'=10,'Loamy sand'=11,'Sand'=12)
lc_class_name = list('Closed forest, evergreen needle leaf'=111,'Closed forest, deciduous needle leaf'=113,
                     'Closed forest, evergreen, broad leaf'=112,'Closed forest, deciduous broad leaf'=114,
                     'Closed forest, mixed'=115,'Closed forest, unknown'=116,'Open forest, evergreen needle leaf'=121,
                     'Open forest, deciduous needle leaf'=123,'Open forest, evergreen broad leaf'=122,
                     'Open forest, deciduous broad leaf'=124,'Open forest, mixed'=125,'Open forest, unknown'=126,
                     'Shrubs'=20,'Herbaceous vegetation'=30,'Herbaceous wetland'=90,'Moss and lichen'=100,
                     'Bare / sparse vegetation'=60,'Cultivated and managed vegetation/agriculture (cropland)'=40,
                     'Urban / built up'=50,'Snow and Ice'=70,'Permanent water bodies'=80,'Open sea'=200,'No data'=0)
hydrotops1 = data.frame(st1,st2,st3,st4,st5,st6,st7,scf1,scf2,scf3,scf4,scf5,scf6,scf7,sb,lc)
hydrotops = data.frame(total_number=1:length(st1),st1,st2,st3,st4,st5,st6,st7,sb,lc)
hydrotops = aggregate(total_number ~ ., hydrotops, FUN=length)
hydrotops = cbind(hydrotops, round(aggregate(scf1~st1+st2+st3+st4+st5+st6+st7+sb+lc, data=hydrotops1, FUN='mean')['scf1'], 0))
hydrotops = cbind(hydrotops, round(aggregate(scf2~st1+st2+st3+st4+st5+st6+st7+sb+lc, data=hydrotops1, FUN='mean')['scf2'], 0))
hydrotops = cbind(hydrotops, round(aggregate(scf3~st1+st2+st3+st4+st5+st6+st7+sb+lc, data=hydrotops1, FUN='mean')['scf3'], 0))
hydrotops = cbind(hydrotops, round(aggregate(scf4~st1+st2+st3+st4+st5+st6+st7+sb+lc, data=hydrotops1, FUN='mean')['scf4'], 0))
hydrotops = cbind(hydrotops, round(aggregate(scf5~st1+st2+st3+st4+st5+st6+st7+sb+lc, data=hydrotops1, FUN='mean')['scf5'], 0))
hydrotops = cbind(hydrotops, round(aggregate(scf6~st1+st2+st3+st4+st5+st6+st7+sb+lc, data=hydrotops1, FUN='mean')['scf6'], 0))
hydrotops = cbind(hydrotops, round(aggregate(scf7~st1+st2+st3+st4+st5+st6+st7+sb+lc, data=hydrotops1, FUN='mean')['scf7'], 0))
hydrotops = hydrotops[which(!is.na(hydrotops$st1)),] # NA in 1 layer of soil
hydrotops = hydrotops[which(!is.na(hydrotops$sb)),] # NA in bedrock depth
hydrotops = hydrotops[which(!(hydrotops$lc %in% c(0,80,200))),] # NA
for (r in 1:nrow(hydrotops)) {
  hydrotops[r,9] = names(which(lc_class_name==hydrotops[r,9]))
  for (c in 1:7) {
    hydrotops[r,c] = names(which(soil_class_name==hydrotops[r,c]))
  }
}
hydrotops$weights = hydrotops$total_number / sum(hydrotops$total_number)
hydrotops = hydrotops[,c(10,18,9,8,1:7,11:17)]
print('      unique hydrotops subset -> completed')

### initial soil parameters (Clapp and Hornberger (1978)) ###
# for silt silt-loam pars are assigned
soil.pars = list(class_index=names(soil_class_name),
                 PSIF=c(-7.7,-6.5,-3.7,-14.8,-6,-6.3,-8.5,-25,-7.9,-25,-3.8,-7),
                 THETAF=c(0.425,0.433,0.358,0.402,0.397,0.317,0.324,0.365,0.266,0.365,0.203,0.188),
                 THSAT=c(0.482,0.492,0.426,0.476,0.477,0.42,0.451,0.485,0.435,0.485,0.41,0.395),
                 BEXP=c(11.4,10.4,10.4,8.52,7.75,7.12,5.39,5.3,4.9,5.3,4.38,4.05),
                 KF=c(4.3,4.2,2.9,7.3,4.9,4.2,6.3,13.1,5.5,13.1,3.5,4),
                 WETINF=c(0.94,0.93,0.93,0.92,0.92,0.92,0.92,0.92,0.92,0.92,0.92,0.92))

### initial vegetation parameters (ref on sourse below) ###
veg.pars = list(class_index=names(lc_class_name[which(lc_class_name%in%c(111,113,112,114,115,116,121,123,122,124,125,126,20,30,90,100,60,40,50,70,80))]) ,
                RELHT_N = list(c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,0.1,100,0.1,120,1,300,1,366,0.1,rep(0,10)),
                             c(1,0.1,100,0.1,120,1,300,1,366,0.1,rep(0,10)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,0,100,0,180,1,270,1,310,0,366,0,rep(0,8)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)) ), #rbrook+own
                RELLAI_N = list(c(1,1,366,1,rep(0,16)),c(1,0,60,0,90,1,300,1,330,0,366,0,rep(0,8)),
                              c(1,1,366,1,rep(0,16)),c(1,0,60,0,90,1,300,1,330,0,366,0,rep(0,8)),
                              c(1,0.5,60,0.5,90,1,300,1,330,0.5,366,0.5,rep(0,8)),c(1,0.5,60,0.5,90,1,300,1,330,0.5,366,0.5,rep(0,8)),
                              c(1,1,366,1,rep(0,16)),c(1,0,70,0,90,1,300,1,320,0,366,0,rep(0,8)),
                              c(1,1,366,1,rep(0,16)),c(1,0,70,0,90,1,300,1,320,0,366,0,rep(0,8)),
                              c(1,0.5,70,0.5,90,1,300,1,320,0.5,366,0.5,rep(0,8)),c(1,0.5,70,0.5,90,1,300,1,320,0.5,366,0.5,rep(0,8)),
                              c(1,0,70,0,90,1,300,1,320,0,366,0,rep(0,8)),c(1,0,100,0,150,1,300,1,366,0,rep(0,10)),
                              c(1,0.1,100,0.1,150,1,300,1,366,0.1,rep(0,10)),c(1,0.5,250,1,366,0.5,rep(0,14)),
                              c(1,0.5,250,1,366,0.5,rep(0,14)),c(1,0,100,0,200,1,270,1,310,0,366,0,rep(0,8)),c(1,0.5,250,1,366,0.5,rep(0,14)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)) ),  #rbrook+own
                RELHT_S = list(c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,120,1,180,0.1,300,0.1,330,1,366,1,rep(0,8)),
                             c(1,1,120,1,180,0.1,300,0.1,330,1,366,1,rep(0,8)),c(1,1,366,1,rep(0,16)),
                             c(1,1,366,1,rep(0,16)),c(1,1,90,1,130,0,280,0,366,1,rep(0,10)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)) ), #rbrook+own
                RELLAI_S = list(c(1,1,366,1,rep(0,16)),c(1,1,120,1,150,0,240,0,270,1,366,1,rep(0,8)),
                              c(1,1,366,1,rep(0,16)),c(1,1,120,1,150,0,240,0,270,1,366,1,rep(0,8)),
                              c(1,1,120,1,150,0.5,240,0.5,270,1,366,1,rep(0,8)),c(1,1,120,1,150,0.5,240,0.5,270,1,366,1,rep(0,8)),
                              c(1,1,366,1,rep(0,16)),c(1,0,70,0,90,1,300,1,320,0,366,0,rep(0,8)),
                              c(1,1,366,1,rep(0,16)),c(1,0,70,0,90,1,300,1,320,0,366,0,rep(0,8)),
                              c(1,1,120,1,140,0.5,250,0.5,270,1,366,1,rep(0,8)),c(1,1,120,1,140,0.5,250,0.5,270,1,366,1,rep(0,8)),
                              c(1,0,70,0,90,1,300,1,320,0,366,0,rep(0,8)),c(1,0,100,0,150,1,300,1,366,0,rep(0,10)),
                              c(1,1,110,1,180,0.1,280,0.1,330,1,366,1,rep(0,8)),c(1,0.8,60,1,180,0.5,366,0.8,rep(0,12)),
                              c(1,0.8,60,1,180,0.5,366,0.8,rep(0,12)),c(1,1,80,1,120,0,180,0,366,0.9,rep(0,10)),c(1,0.8,60,1,180,0.5,366,0.8,rep(0,12)),c(1,1,366,1,rep(0,16)),c(1,1,366,1,rep(0,16)) ),  #rbrook+own
                ALB = c(0.1,0.13,0.15,0.18,0.14,0.14,0.13,0.16,0.18,0.21,0.17,0.17,0.18,0.2,0.22,0.2,0.2,0.22,0.2,0.6,0.1), #brook+rbrook+own
                ALBSN = c(0.28,0.3,0.2,0.22,0.25,0.25,0.3,0.32,0.22,0.24,0.27,0.27,0.35,0.45,0.5,0.5,0.4,0.5,0.2,0.6,0.6), #brook+rbrook+own
                KSNVP = c(0.3,0.3,0.3,0.3,0.3,0.3,0.5,0.5,0.5,0.5,0.5,0.5,0.9,1,1,1,1,1,1,1,1), #brook+own
                Z0G = c(0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.02,0.01,0.005,0.005,0.005,0.005,0.02,0.005,0.005), #brook+own
                MAXHT = c(30,30,30,30,30,30,25,25,25,25,25,25,3,0.5,0.3,0.1,0.1,1.5,0.1,0.001,0.001), #own
                MAXLAI = c(6,5,8,7,6,6,5,4,7,6,5,5,4,2.5,3,2,1,4,2,0.001,0.001), #brook+own+G.B.Bonan 2002
                MXRTLN = c(3100,3000,3300,3200,3200,3200,2600,2500,2800,2700,2700,2700,3400,1000,1100,860,400,110,300,1,1), #brook+own
                FXYLEM = c(0.5,0.5,0.5,0.5,0.5,0.5,0.4,0.4,0.4,0.4,0.4,0.4,0.3,0,0,0,0,0,0,0,0), #brook
                GLMAX = c(0.34,0.34,0.45,0.45,0.4,0.4,0.4,0.4,0.5,0.5,0.45,0.45,0.53,0.8,0.8,0.66,0.66,1.1,0.66,0.2,0.2), #brook+rbrook+own
                LWIDTH = c(0.002,0.002,0.07,0.07,0.04,0.04,0.01,0.01,0.05,0.05,0.03,0.03,0.02,0.01,0.01,0.005,0.01,0.05,0.01,0.001,0.001), #brook+own
                CR = c(0.5,0.5,0.6,0.6,0.55,0.55,0.5,0.5,0.6,0.6,0.55,0.55,0.65,0.7,0.7,0.7,0.7,0.7,0.7,0.7,0.7), #brook
                ROOTDEN = list(c(100,0.27,100,0.195,100,0.14,100,0.1,100,0.075,100,0.065,100,0.04,100,0.03,100,0.025,100,0.015,100,0.015,100,0.01,rep(c(100,0.005),7),rep(c(100,0),6)),
                               c(100,0.305,100,0.215,100,0.15,100,0.1,100,0.07,100,0.05,100,0.045,100,0.025,100,0.02,100,0.015,100,0.01,100,0.01,100,0.005,100,0.005,rep(c(100,0),11)),
                               c(100,0.27,100,0.195,100,0.14,100,0.1,100,0.075,100,0.065,100,0.04,100,0.03,100,0.025,100,0.015,100,0.015,100,0.01,rep(c(100,0.005),7),rep(c(100,0),6)),
                               c(100,0.305,100,0.215,100,0.15,100,0.1,100,0.07,100,0.05,100,0.045,100,0.025,100,0.02,100,0.015,100,0.01,100,0.01,100,0.005,100,0.005,rep(c(100,0),11)),
                               c(100,0.29,100,0.205,100,0.145,100,0.1,100,0.075,100,0.055,100,0.04,100,0.03,100,0.025,100,0.015,100,0.013,100,0.01,rep(c(100,0.005),4),rep(c(100,0),9)),
                               c(100,0.29,100,0.205,100,0.145,100,0.1,100,0.075,100,0.055,100,0.04,100,0.03,100,0.025,100,0.015,100,0.013,100,0.01,rep(c(100,0.005),4),rep(c(100,0),9)),
                               c(100,0.27,100,0.195,100,0.14,100,0.1,100,0.075,100,0.065,100,0.04,100,0.03,100,0.025,100,0.015,100,0.015,100,0.01,rep(c(100,0.005),7),rep(c(100,0),6)),
                               c(100,0.305,100,0.215,100,0.15,100,0.1,100,0.07,100,0.05,100,0.045,100,0.025,100,0.02,100,0.015,100,0.01,100,0.01,100,0.005,100,0.005,rep(c(100,0),11)),
                               c(100,0.27,100,0.195,100,0.14,100,0.1,100,0.075,100,0.065,100,0.04,100,0.03,100,0.025,100,0.015,100,0.015,100,0.01,rep(c(100,0.005),7),rep(c(100,0),6)),
                               c(100,0.305,100,0.215,100,0.15,100,0.1,100,0.07,100,0.05,100,0.045,100,0.025,100,0.02,100,0.015,100,0.01,100,0.01,100,0.005,100,0.005,rep(c(100,0),11)),
                               c(100,0.29,100,0.205,100,0.145,100,0.1,100,0.075,100,0.055,100,0.04,100,0.03,100,0.025,100,0.015,100,0.013,100,0.01,rep(c(100,0.005),4),rep(c(100,0),9)),
                               c(100,0.29,100,0.205,100,0.145,100,0.1,100,0.075,100,0.055,100,0.04,100,0.03,100,0.025,100,0.015,100,0.013,100,0.01,rep(c(100,0.005),4),rep(c(100,0),9)),
                               c(100,0.31,100,0.21,100,0.15,100,0.1,100,0.07,100,0.05,100,0.03,100,0.02,100,0.02,100,0.01,100,0.01,100,0.01,100,0.01,rep(c(100,0),12)),
                               c(100,0.44,100,0.25,100,0.14,100,0.08,100,0.04,100,0.02,100,0.02,100,0.01,rep(c(100,0),17)),
                               c(100,0.7,100,0.5,100,0.3,100,0.1,100,0.05,100,0.01,rep(c(100,0),19)),
                               c(100,0.2,100,0.1,100,0.05,rep(c(100,0),22)),
                               c(100,0.25,100,0.2,100,0.15,100,0.1,100,0.05,100,0.02,100,0.01,rep(c(100,0),18)),
                               c(100,0.34,100,0.22,100,0.15,100,0.1,100,0.07,100,0.04,100,0.03,100,0.02,100,0.01,100,0.01,100,0.01,100,0.01,rep(c(100,0),13)),
                               c(100,0.25,100,0.2,100,0.15,100,0.1,100,0.05,100,0.02,100,0.01,rep(c(100,0),18)),
                               c(rep(c(100,0.001),25)),
                               c(rep(c(100,0.001),25)) ), #brook+own
                IMPERV = c(rep(0.01,18),0.9,1,1)
                )

### assign soil and vegetation and other params for unique hydrotops ###
hydrotop.pars = function(ht_id) {
  lc_class = hydrotops[ht_id,'lc']
  s_class1 = hydrotops[ht_id,'st1']
  s_class2 = hydrotops[ht_id,'st2']
  s_class3 = hydrotops[ht_id,'st3']
  s_class4 = hydrotops[ht_id,'st4']
  s_class5 = hydrotops[ht_id,'st5']
  s_class6 = hydrotops[ht_id,'st6']
  s_class7 = hydrotops[ht_id,'st7']
  s_br = hydrotops[ht_id,'sb']
  LAT = round(catchment_centre[2]*pi/180, 3)
  DTIMAX = 0.5
  ESLOPE = mean_catchment_slope_rad
  ESLOPED = mean_catchment_slope_deg
  if (hourly_P == TRUE) {
    SUBDAYDATA = T
    NPINT = 24
  } else {
    SUBDAYDATA = F
    NPINT = 1
  }
  if(LAT>=0) {RELHT=veg.pars$RELHT_N[[which(veg.pars$class_index==lc_class)]] }else{ RELHT=veg.pars$RELHT_S[[which(veg.pars$class_index==lc_class)]]}
  if(LAT>=0) {RELLAI=veg.pars$RELLAI_N[[which(veg.pars$class_index==lc_class)]] }else{ RELLAI=veg.pars$RELLAI_S[[which(veg.pars$class_index==lc_class)]]}
  ALB = veg.pars$ALB[[which(veg.pars$class_index==lc_class)]]
  ALBSN = veg.pars$ALBSN[[which(veg.pars$class_index==lc_class)]]
  KSNVP = veg.pars$KSNVP[[which(veg.pars$class_index==lc_class)]]
  Z0G = veg.pars$Z0G[[which(veg.pars$class_index==lc_class)]]
  MAXHT = veg.pars$MAXHT[[which(veg.pars$class_index==lc_class)]]
  MAXLAI = veg.pars$MAXLAI[[which(veg.pars$class_index==lc_class)]]
  MXRTLN = veg.pars$MXRTLN[[which(veg.pars$class_index==lc_class)]]
  FXYLEM = veg.pars$FXYLEM[[which(veg.pars$class_index==lc_class)]]
  GLMAX = veg.pars$GLMAX[[which(veg.pars$class_index==lc_class)]]
  LWIDTH = veg.pars$LWIDTH[[which(veg.pars$class_index==lc_class)]]
  CR = veg.pars$CR[[which(veg.pars$class_index==lc_class)]]
  ROOTDEN = veg.pars$ROOTDEN[[which(veg.pars$class_index==lc_class)]]
  IMPERV = veg.pars$IMPERV[[which(veg.pars$class_index==lc_class)]]
  NLAYER = 7
  THICK = c(25,75,125,225,350,700,500, rep(0,18))
  thick_cumul = c(0,2.5,10,22.5,45,80,150,200)
  STONEF = c(as.numeric(hydrotops[ht_id,c('scf1','scf2','scf3','scf4','scf5','scf6','scf7')])/100, rep(0,18))
  PSIF = c(soil.pars$PSIF[[which(soil.pars$class_index==s_class1)]],soil.pars$PSIF[[which(soil.pars$class_index==s_class2)]],
           soil.pars$PSIF[[which(soil.pars$class_index==s_class3)]],soil.pars$PSIF[[which(soil.pars$class_index==s_class4)]],
           soil.pars$PSIF[[which(soil.pars$class_index==s_class5)]],soil.pars$PSIF[[which(soil.pars$class_index==s_class6)]],
           soil.pars$PSIF[[which(soil.pars$class_index==s_class7)]], rep(0,18))
  THETAF = c(soil.pars$THETAF[[which(soil.pars$class_index==s_class1)]],soil.pars$THETAF[[which(soil.pars$class_index==s_class2)]],
             soil.pars$THETAF[[which(soil.pars$class_index==s_class3)]],soil.pars$THETAF[[which(soil.pars$class_index==s_class4)]],
             soil.pars$THETAF[[which(soil.pars$class_index==s_class5)]],soil.pars$THETAF[[which(soil.pars$class_index==s_class6)]],
             soil.pars$THETAF[[which(soil.pars$class_index==s_class7)]], rep(0,18))
  THSAT =  c(soil.pars$THSAT[[which(soil.pars$class_index==s_class1)]],soil.pars$THSAT[[which(soil.pars$class_index==s_class2)]],
             soil.pars$THSAT[[which(soil.pars$class_index==s_class3)]],soil.pars$THSAT[[which(soil.pars$class_index==s_class4)]],
             soil.pars$THSAT[[which(soil.pars$class_index==s_class5)]],soil.pars$THSAT[[which(soil.pars$class_index==s_class6)]],
             soil.pars$THSAT[[which(soil.pars$class_index==s_class7)]], rep(0,18))
  BEXP = c(soil.pars$BEXP[[which(soil.pars$class_index==s_class1)]],soil.pars$BEXP[[which(soil.pars$class_index==s_class2)]],
           soil.pars$BEXP[[which(soil.pars$class_index==s_class3)]],soil.pars$BEXP[[which(soil.pars$class_index==s_class4)]],
           soil.pars$BEXP[[which(soil.pars$class_index==s_class5)]],soil.pars$BEXP[[which(soil.pars$class_index==s_class6)]],
           soil.pars$BEXP[[which(soil.pars$class_index==s_class7)]], rep(0,18))
  KF = c(soil.pars$KF[[which(soil.pars$class_index==s_class1)]],soil.pars$KF[[which(soil.pars$class_index==s_class2)]],
         soil.pars$KF[[which(soil.pars$class_index==s_class3)]],soil.pars$KF[[which(soil.pars$class_index==s_class4)]],
         soil.pars$KF[[which(soil.pars$class_index==s_class5)]],soil.pars$KF[[which(soil.pars$class_index==s_class6)]],
         soil.pars$KF[[which(soil.pars$class_index==s_class7)]], rep(0,18))
  WETINF = c(soil.pars$WETINF[[which(soil.pars$class_index==s_class1)]],soil.pars$WETINF[[which(soil.pars$class_index==s_class2)]],
             soil.pars$WETINF[[which(soil.pars$class_index==s_class3)]],soil.pars$WETINF[[which(soil.pars$class_index==s_class4)]],
             soil.pars$WETINF[[which(soil.pars$class_index==s_class5)]],soil.pars$WETINF[[which(soil.pars$class_index==s_class6)]],
             soil.pars$WETINF[[which(soil.pars$class_index==s_class7)]], rep(0,18))
  if (s_br<200) { # cut soil profile and pars to depth_to_bedrock value
    NLAYER = which(s_br %between% list(c(0,2.5,10,22.5,45,80,150),c(2.5,10,22.5,45,80,150,200)) == T)[[1]]
    THICK[NLAYER] = THICK[NLAYER] + (s_br - thick_cumul[NLAYER+1])*10
    THICK[NLAYER+1:20] = 0
    STONEF = c(STONEF[1:NLAYER], rep(0,20-NLAYER))
    PSIF = c(PSIF[1:NLAYER], rep(0,20-NLAYER))
    THETAF = c(THETAF[1:NLAYER], rep(0,20-NLAYER))
    THSAT = c(THSAT[1:NLAYER], rep(0,20-NLAYER))
    BEXP = c(BEXP[1:NLAYER], rep(0,20-NLAYER))
    KF = c(KF[1:NLAYER], rep(0,20-NLAYER))
  }
  return(list(LAT=LAT,DTIMAX=DTIMAX,ESLOPE=ESLOPE,ESLOPED=ESLOPED,SUBDAYDATA=SUBDAYDATA,NPINT=NPINT,
              RELHT=RELHT,RELLAI=RELLAI,ALB=ALB,ALBSN=ALBSN,KSNVP=KSNVP,Z0G=Z0G,MAXHT=MAXHT,MAXLAI=MAXLAI,
              MXRTLN=MXRTLN,FXYLEM=FXYLEM,GLMAX=GLMAX,LWIDTH=LWIDTH,CR=CR,ROOTDEN=ROOTDEN,IMPERV=IMPERV,NLAYER=NLAYER,
              THICK=THICK,STONEF=STONEF,PSIF=PSIF,THETAF=THETAF,THSAT=THSAT,BEXP=BEXP,KF=KF,WETINF=WETINF)) # here list of pars
}
hydrotops.pars = list()
for (i in 1:nrow(hydrotops)) {
  hydrotops.pars[[i]] = hydrotop.pars(i)
}
print('      soil and land cover pars for hydrotops -> completed')

### prepare meteo dataframe(s) ###
    # ERA5 variables:
    #   - surface solar radiation (ssr [J/m2])
    #   - wind speed 10m v and u directions (v10,u10 [m/s])
    #   - air temperature 2m (t2m [K])
    #   - precipitation (tp [m])
    # brook90 variables:
    #   - year month day
    #   - daily solar radiation sum (SR [MJ/m2])
    #   - daily maximum and minimum air temperature (Tmax, Tmin [C])
    #   - daily actual vapour pressure (actual_VP [KPa])
    #   - daily mean wind speed W [m/s]
    #   - daily/hourly precipitation (P [mm])
    #   - daily mean observed runoff (Flow[mm])
setwd(meteo_dir)
meteo_nc_list = list.files(meteo_dir)
P = c()
SR = c()
Tk = c()
Wv = c()
Wu = c()
time_nc = c()
nc_processing = function (nc_name) {
  nc = nc_open(nc_name)
  myvar = ncvar_get(nc)
  if (length(nc$dim)==4) {
    era5_na = min(which(is.na(myvar[1,1,1,])))
    myvar[,,1,era5_na:dim(myvar)[4]] = myvar[,,2,era5_na:dim(myvar)[4]]
    myvar = myvar[,,1,]
  }
  lon = round(nc$dim$longitude$vals,1)
  lat = round(nc$dim$latitude$vals,1)
  gr = expand.grid(lon,lat)
  gr = SpatialPoints(gr, proj4string=CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
  gr_poly = list()
  for (i in seq_along(gr)) {
    gr_poly[i]=gBuffer(gr[i], width=0.1/2, quadsegs=1, capStyle="SQUARE")
  }
  gr_poly = do.call(bind, gr_poly)
  if (meteo_averaging == 'mean') {
    var_vec = apply(myvar, length(dim(myvar)), mean)
    myggtitle = 'Meteodata assimilation method: \nmean out of all grids'
  }
  if (meteo_averaging == 'nearest_grid') {
    gr_dist = gDistance(gr,gCentroid(catchment), byid=T)
    nearest_gr = round(coordinates(gr[which(gr_dist==min(gr_dist))]),1)
    var_vec = myvar[which(lon==nearest_gr[1]), which(lat==nearest_gr[2]), ]
    myggtitle = 'Meteodata assimilation method: \nnearest to the catchment mass center grid'
  }
  if (meteo_averaging == 'weighted_mean') {
    gr_poly_intercect = rgeos::gIntersection(catchment, gr_poly, byid=T)
    gr_cat_id = which(gIntersects(gr_poly, catchment, byid=T)==T)
    area_coef = area(gr_poly_intercect)/sum(area(gr_poly_intercect))
    var_list = list()
    for (g in 1:length(gr_cat_id)) {
      coord_gr = round(coordinates(gr_poly[gr_cat_id[g]]),1)
      var_list[[g]] = myvar[which(lon==coord_gr[1]), which(lat==coord_gr[2]), ] * area_coef[g]
    }
    var_vec = Reduce('+', var_list)
    myggtitle = paste0('Meteodata assimilation method: \narea-weighted mean only for intersected grids (',length(gr_cat_id),')')
}
  nc_time = ncvar_get(nc, 'time')
  return(list(var_vec, nc_time, gr_poly, myggtitle))
}
for (f in seq(1,length(meteo_nc_list),5)) {
  suppressWarnings( res <- nc_processing(meteo_nc_list[f]) )
  P = c(P, res[[1]])
  time_nc = c(time_nc, res[[2]])
}
for (f in seq(2,length(meteo_nc_list),5)) {
  suppressWarnings( res <- nc_processing(meteo_nc_list[f]) )
  SR = c(SR, res[[1]])
}
for (f in seq(3,length(meteo_nc_list),5)) {
  suppressWarnings( res <- nc_processing(meteo_nc_list[f]) )
  Tk = c(Tk, res[[1]])
}
for (f in seq(4,length(meteo_nc_list),5)) {
  suppressWarnings( res <- nc_processing(meteo_nc_list[f]) )
  Wu = c(Wu, res[[1]])
}
for (f in seq(5,length(meteo_nc_list),5)) {
  suppressWarnings( res <- nc_processing(meteo_nc_list[f]) )
  Wv = c(Wv, res[[1]])
}

suppressWarnings( gr_poly <- nc_processing(meteo_nc_list[1])[[3]] )
suppressWarnings( myggtitle <- nc_processing(meteo_nc_list[1])[[4]] )
era_gr_pic = ggplot() +
  suppressMessages(geom_polygon(data=gr_poly, aes(x=long, y=lat, group=group), colour="black", fill=NA)) +
  suppressMessages(geom_polygon(data=catchment, aes(x=long, y=lat, group=group), colour="red", fill='red')) +
  geom_point(aes(x=catchment_centre[1], y=catchment_centre[2]), colour="blue", fill='blue') +
  theme_bw() +
  labs(title=myggtitle, x='Longitude', y='Latitude')
setwd(model_output_dir)
suppressMessages( ggsave('catchment_era5grid.png', era_gr_pic) )

Tk[Tk==-32767] = NA  # replace missing era5 values
Wv[Wv==-32767] = NA # replace missing era5 values
Wu[Wu==-32767] = NA # replace missing era5 values
Wv[Wv==0] = 0.1 # brook90 recommendation so that 0 measured speed is not treated as NA
Wu[Wu==0] = 0.1 # brook90 recommendation so that 0 measured speed is not treated as NA
SR[SR==-32767] = NA # replace missing era5 values
P[P==-32767] = NA # replace missing era5 values
P[P<0] = 0 # get rid of small negative P appearing in era5

# get dates and time zone offset to correct era5
utc_offset = tz_offset("2018-06-12", tz_lookup_coords(catchment_centre[2], catchment_centre[1], method="accurate"))
meteo_time_start = meteo_time_end = as.POSIXct('1900-01-01 00:00', tz='GMT')
hour(meteo_time_start) = hour(meteo_time_start) + time_nc[1] + round(utc_offset$utc_offset_h,0)
hour(meteo_time_end) = hour(meteo_time_end) + tail(time_nc, n=1) + round(utc_offset$utc_offset_h,0)
meteo_time_start1 = meteo_time_start
hour(meteo_time_start1) = hour(meteo_time_start1)-1 # shift P by 1 hour (according to ERA5 documentation)
temp_df = data.frame(date=seq.POSIXt(meteo_time_start1, meteo_time_end, by='hour'),
                     Tk=c(NA,Tk), W=c(NA,sqrt((Wu)^2+(Wv)^2)), SR=c(NA,SR), P=c(P,NA))
temp_df = temp_df[complete.cases(temp_df),] # to overcome problem of P and TZ shift

# to overcome problem of incomplete days: subprfileline(IInterValDay) error
if (hour(temp_df$date[1])!=0) {
  temp_df = temp_df[(25-hour(temp_df$date[1])) : length(temp_df$date), ]
}
if (hour(tail(temp_df$date, n=1))!=23) {
  temp_df = temp_df[1 : (length(temp_df$date)-hour(tail(temp_df$date, n=1))-1), ]
}

# AVP = 610.78*exp(17.27*(Tk-273.16)/(Tk-35.86)) Bougeault (1982)
dates = seq.Date(as.Date(temp_df$date[1]), as.Date(tail(temp_df$date, n=1)), by='day')

meteo_df = data.frame(year = year(dates), month = month(dates), day = day(dates),
                      SR = aggregate(SR~day(temp_df$date)+month(temp_df$date)+year(temp_df$date), data=temp_df, FUN=sum)[,4] /10^6,
                      Tmax = aggregate(Tk~day(temp_df$date)+month(temp_df$date)+year(temp_df$date), data=temp_df, FUN=max)[,4] -273.15,
                      Tmin = aggregate(Tk~day(temp_df$date)+month(temp_df$date)+year(temp_df$date), data=temp_df, FUN=min)[,4] -273.15,
                      AVP = 610.78*exp(17.27*(aggregate(Tk~day(temp_df$date)+month(temp_df$date)+year(temp_df$date), data=temp_df, FUN=mean)[,4]-273.15)/(aggregate(Tk~year(temp_df$date)+month(temp_df$date)+day(temp_df$date), data=temp_df, FUN=mean)[,4]-35.86)) /1000,
                      W = aggregate(W~day(temp_df$date)+month(temp_df$date)+year(temp_df$date), data=temp_df, FUN=mean)[,4],
                      P = aggregate(P~day(temp_df$date)+month(temp_df$date)+year(temp_df$date), data=temp_df, FUN=sum)[,4] *1000 ,
                      Q = 0 )

if (hourly_P==T) {
  hourly_P_df = data.frame(year=year(temp_df$date), month=month(temp_df$date), day=day(temp_df$date), NPINT=24,
                           P= temp_df$P*1000, Q=-1 )
  print('      meteo and P dataframes -> completed')
  return (list(hydrotops, hydrotops.pars, meteo_df, dates, hourly_P_df))
} else {
  print('      meteo dataframe -> completed')
  return (list(hydrotops, hydrotops.pars, meteo_df, dates))
}
}




###### END ######
