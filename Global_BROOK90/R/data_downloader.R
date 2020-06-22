##### Data downloader #####
### land cover (Copernicus global land cover), soil (SoilGrids250), meteo (ERA5), DEM (Mapzen Amazon Web Services)
### 15/06/2020 update
### Ivan Vorobevskii
### Germany, TU Dresden, Institute of Hydrology and Meteorology, Chair of Meteorology


# BROOK90 Geoserver request
geourl = function (rastername,ext) {
  myip = '141.76.16.238' # TUD VM IP (Geoserver)
  myurl = paste0('http://',myip,':8080/geoserver/ows?service=WCS&version=2.0.0&request=GetCoverage&coverageId=global_brook90:',rastername,'&format=image/tiff&subset=Lat(',ext[3],',',ext[4],')&subset=Long(',ext[1],',',ext[2],')')
  return(myurl)
}


# Land cover
download.landcover = function (catchment_path, landcover_dir) {
  catchment = readOGR(catchment_path, verbose=F)
  ext = extent(catchment)
  ext[1] = ext[1] - 0.01 # adding a ~ 1 km buffer
  ext[2] = ext[2] + 0.01 # adding a ~ 1 km buffer
  ext[3] = ext[3] - 0.01 # adding a ~ 1 km buffer
  ext[4] = ext[4] + 0.01 # adding a ~ 1 km buffer
  print('Started download of Land cover dataset')
  url = geourl('LC_100',ext)
  download.file(url, destfile=paste0(landcover_dir,'lc.tif'), quiet=T, mode='wb')
  print('Finished download of Land cover dataset')
  # coordinates in geographic format (grad)
  # assess https://lcviewer.vito.be/download , download required zip archive, unzip, delete all except 'discrete-classification'
  #catchment = readOGR(catchment_path, verbose=F)
  #catchment_centre = as.integer(coordinates(gCentroid(catchment)))
  #catchment_centre = coordinates(gCentroid(catchment))
  #coord = c(as.character(dd2dms(catchment_centre[2], NS=T)), as.character(dd2dms(catchment_centre[1], NS=F)))
  #hemisphere1 = str_sub(coord[1],-1,-1)
  #hemisphere2 = str_sub(coord[2],-1,-1)
  #lat = abs(round(catchment_centre[2],0))
  #lon = abs(round(catchment_centre[1],0))

  #if (hemisphere1=='N') { lat.name = (lat%/%20)*20+20 }
  # if (hemisphere1=='S') { lat.name = (lat%/%20)*20 }
  # if (hemisphere2=='E') { lon.name = (lon%/%20)*20 }
  # if (hemisphere2=='W') { lon.name = (lon%/%20)*20+20 }
  # if (lat.name==0) {lat.name = c('00')}
  # if (lon.name==0) {lon.name = c('000')}
  # if (lon.name%in%c(20,40,60,80)) {lon.name = paste0('0',lon.name)}
  # if (catchment_centre[2]>-20 & catchment_centre[2]<0) {hemisphere1='N'}
  #
  # cellname = paste0(hemisphere2,lon.name,hemisphere1,lat.name)  # 'E020N60' 'E020N00'
  #
  # url = paste0('https://s3-eu-west-1.amazonaws.com/vito-downloads/ZIPfiles/',cellname,'_ProbaV_LC100_epoch2015_global_v2.0.1_products_EPSG-4326.zip')
  # print('Started download of Land cover dataset')
  # setwd(landcover_dir)
  # download.file(url, destfile='land_cover.zip', quiet=T, mode='wb', method='libcurl')
  # required_file_name = grep('discrete-classification_EPSG-4326', unzip('land_cover.zip',list=T)$Name,
  #                           ignore.case=T, value=T)
  # unzip('land_cover.zip', files=required_file_name)
  # lc = raster(required_file_name)
  # ext = extent(catchment)
  # ext[1] = ext[1] - 0.01 # adding a ~ 1 km buffer
  # ext[2] = ext[2] + 0.01 # adding a ~ 1 km buffer
  # ext[3] = ext[3] - 0.01 # adding a ~ 1 km buffer
  # ext[4] = ext[4] + 0.01 # adding a ~ 1 km buffer
  # ext = as(ext, 'SpatialPolygons')
  # crs(ext) = crs(catchment)
  # writeRaster(crop(lc,ext), 'lc.tif')
  # file.remove(c('land_cover.zip',required_file_name))
  # print('Finished download of Land cover dataset')
}


# soil (texture classes and coarse fragment fraction - 7 layers, depth to bedrock)
download.soil = function (catchment_path, soil_dir) {
  # coordinates in geographic format (grad) lat/lon (negative to western and southern hemisphere)
  # assess https://soilgrids.org/#!/?layer=ORCDRC_M_sl2_250m&vector=1 , download tif
  catchment = readOGR(catchment_path, verbose=F)
  ext = extent(catchment)
  ext[1] = ext[1] - 0.01 # adding a ~ 1 km buffer
  ext[2] = ext[2] + 0.01 # adding a ~ 1 km buffer
  ext[3] = ext[3] - 0.01 # adding a ~ 1 km buffer
  ext[4] = ext[4] + 0.01 # adding a ~ 1 km buffer

  print('Started download of Soil dataset')
  url1 = geourl('TEXMHT_M_sl1_250m_ll',ext)
  url2 = geourl('TEXMHT_M_sl2_250m_ll',ext)
  url3 = geourl('TEXMHT_M_sl3_250m_ll',ext)
  url4 = geourl('TEXMHT_M_sl4_250m_ll',ext)
  url5 = geourl('TEXMHT_M_sl5_250m_ll',ext)
  url6 = geourl('TEXMHT_M_sl6_250m_ll',ext)
  url7 = geourl('TEXMHT_M_sl7_250m_ll',ext)
  url8 = geourl('BDRICM_M_250m_ll',ext)
  url9 = geourl('CRFVOL_M_sl1_250m_ll',ext)
  url10 = geourl('CRFVOL_M_sl2_250m_ll',ext)
  url11 = geourl('CRFVOL_M_sl3_250m_ll',ext)
  url12 = geourl('CRFVOL_M_sl4_250m_ll',ext)
  url13 = geourl('CRFVOL_M_sl5_250m_ll',ext)
  url14 = geourl('CRFVOL_M_sl6_250m_ll',ext)
  url15 = geourl('CRFVOL_M_sl7_250m_ll',ext)

  download.file(url1, destfile=paste0(soil_dir,'soil.texture_1.tif'), quiet=T, mode='wb')
  download.file(url2, destfile=paste0(soil_dir,'soil.texture_2.tif'), quiet=T, mode='wb')
  download.file(url3, destfile=paste0(soil_dir,'soil.texture_3.tif'), quiet=T, mode='wb')
  download.file(url4, destfile=paste0(soil_dir,'soil.texture_4.tif'), quiet=T, mode='wb')
  download.file(url5, destfile=paste0(soil_dir,'soil.texture_5.tif'), quiet=T, mode='wb')
  download.file(url6, destfile=paste0(soil_dir,'soil.texture_6.tif'), quiet=T, mode='wb')
  download.file(url7, destfile=paste0(soil_dir,'soil.texture_7.tif'), quiet=T, mode='wb')
  download.file(url8, destfile=paste0(soil_dir,'soil.bedrock_depth.tif'), quiet=T, mode='wb')
  download.file(url9, destfile=paste0(soil_dir,'soil.coarse_fragments_1.tif'), quiet=T, mode='wb')
  download.file(url10, destfile=paste0(soil_dir,'soil.coarse_fragments_2.tif'), quiet=T, mode='wb')
  download.file(url11, destfile=paste0(soil_dir,'soil.coarse_fragments_3.tif'), quiet=T, mode='wb')
  download.file(url12, destfile=paste0(soil_dir,'soil.coarse_fragments_4.tif'), quiet=T, mode='wb')
  download.file(url13, destfile=paste0(soil_dir,'soil.coarse_fragments_5.tif'), quiet=T, mode='wb')
  download.file(url14, destfile=paste0(soil_dir,'soil.coarse_fragments_6.tif'), quiet=T, mode='wb')
  download.file(url15, destfile=paste0(soil_dir,'soil.coarse_fragments_7.tif'), quiet=T, mode='wb')
  print('Finished download of Soil dataset')
}


# meteo data (ERA5: P, Tmean, DR, W10 - raw hourly data from API request)

# add UID and API Key (after registration=>User Profile=>View (bottom))
# https://cds.climate.copernicus.eu/user/login?destination=%2F%23!%2Fhome

download.meteo = function (cds_user=NA, cds_key=NA, catchment_path, meteo_dir,
                          time_start=NA, time_end=NA) {
  catchment = readOGR(catchment_path, verbose=F)
  suppressMessages(wf_set_key(user=cds_user, key=cds_key, service='cds'))
  dates = seq.Date(time_start, time_end, by='day')
  cat_ext = round(extent(catchment),1)
  request_extend = paste0(cat_ext[4]+0.1,'/',cat_ext[1]-0.1,'/',cat_ext[3]-0.1,'/',cat_ext[2]+0.1)

  request_builder = function(my_dates, my_var, my_time, my_area, my_target) {
    request = list('dataset_short_name'='reanalysis-era5-single-levels','product_type'='reanalysis','grid'='0.1/0.1',
                   'variable'=my_var,'date'=my_dates,'time'=my_time,'area'=my_area,
                   'format'='netcdf','target'=my_target)
    return (request)
  }
  r_hours = c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23')

  if ((time_end-time_start) > 4999) { # if request is > 5000 days (120000/24) need to split: max 3 date-chunks
    periods = length(dates) %/% 4999
    dates = list()
    requests = list()
    dates[[1]] = seq.Date(time_start, time_start+4999, by='day')
    for (p in 1:periods) {
      time_start_new = time_start+1 + p*4999
      time_end_new = time_start_new-1 + 4999
      if (time_end_new > time_end) {time_end_new = time_end}
      dates[[p+1]] = seq.Date(time_start_new, time_end_new, by='day')
    }
    for (p in 1:(periods+1)) {
      requests = append(requests, list(request_builder(dates[[p]], '2m_temperature', r_hours, request_extend, paste0('era5_',head(dates[[p]])[1],'_',tail(dates[[p]],n=1),'_T.nc'))))
      requests = append(requests, list(request_builder(dates[[p]], '10m_v_component_of_wind', r_hours, request_extend, paste0('era5_',head(dates[[p]])[1],'_',tail(dates[[p]],n=1),'_Wv.nc'))))
      requests = append(requests, list(request_builder(dates[[p]], '10m_u_component_of_wind', r_hours, request_extend, paste0('era5_',head(dates[[p]])[1],'_',tail(dates[[p]],n=1),'_Wu.nc'))))
      requests = append(requests, list(request_builder(dates[[p]], 'surface_net_solar_radiation', r_hours, request_extend, paste0('era5_',head(dates[[p]])[1],'_',tail(dates[[p]],n=1),'_SR.nc'))))
      requests = append(requests, list(request_builder(dates[[p]], 'total_precipitation', r_hours, request_extend, paste0('era5_',head(dates[[p]])[1],'_',tail(dates[[p]],n=1),'_P.nc'))))
    }
  }else{
    requests = list(request_builder(dates, '2m_temperature', r_hours, request_extend, paste0('era5_',head(dates)[1],'_',tail(dates,n=1),'_T.nc')),
                    request_builder(dates, '10m_v_component_of_wind', r_hours, request_extend, paste0('era5_',head(dates)[1],'_',tail(dates,n=1),'_Wv.nc')),
                    request_builder(dates, '10m_u_component_of_wind', r_hours, request_extend, paste0('era5_',head(dates)[1],'_',tail(dates,n=1),'_Wu.nc')),
                    request_builder(dates, 'surface_net_solar_radiation', r_hours, request_extend, paste0('era5_',head(dates)[1],'_',tail(dates,n=1),'_SR.nc')),
                    request_builder(dates, 'total_precipitation', r_hours, request_extend, paste0('era5_',head(dates)[1],'_',tail(dates,n=1),'_P.nc')) )
  }

  print('Started download of Meteo dataset')
  submitted_requests = list()
  for (i in 1:length(requests)) { # send silent requests
    submitted_requests[[i]] = suppressMessages(wf_request(user=cds_user, request=requests[[i]], transfer=F, path=meteo_dir, verbose=F, time_out=5*3600))
  }
  for (i in 1:length(requests)) { # download finished requests
    wf_transfer(submitted_requests[[i]]$request_id, user=cds_user, service='cds', path=meteo_dir, filename=requests[[i]]$target, verbose=T)
  }
  # temporary solution: check every 10 min for completed requests and download them
  while (length(requests) > length(list.files(meteo_dir))) {
    print('checking for completed ERA5 requests')
    for (i in 1:length(requests)) { # download finished requests
      temp = try(wf_transfer(submitted_requests[[i]]$request_id, user=cds_user, service='cds', path=meteo_dir, filename=requests[[i]]$target, verbose=T) , silent=T)
      if('try-error' %in% class(temp)) { # if request fail (mars server)
        submitted_requests[[i]] = suppressMessages(wf_request(user=cds_user, request=requests[[i]], transfer=F, path=meteo_dir, verbose=F, time_out=5*3600))
      }
    }
    if (length(requests) > length(list.files(meteo_dir))) {
      Sys.sleep(300)
    }
  }
  print('Finished download of Meteo dataset')
}


# DEM data (SRTM30 from Mapzen Amazon Web Services)
download.dem = function (catchment_path, dem_dir) {
  print('Started download of DEM')
  catchment = readOGR(catchment_path, verbose=F)
  ext = extent(catchment)
  ext[1] = ext[1] - 0.01 # adding a ~ 1 km buffer
  ext[2] = ext[2] + 0.01 # adding a ~ 1 km buffer
  ext[3] = ext[3] - 0.01 # adding a ~ 1 km buffer
  ext[4] = ext[4] + 0.01 # adding a ~ 1 km buffer
  ext = as(ext, 'SpatialPolygons')
  crs(ext) = crs(catchment)
  dem_raster = suppressMessages(get_elev_raster(ext, z=14, clip='locations', verbose=F))
  writeRaster(dem_raster, paste0(dem_dir,'DEM.tif'))
  print('Finished download of DEM')
}


### end
