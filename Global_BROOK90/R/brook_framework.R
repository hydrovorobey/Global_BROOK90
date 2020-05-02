##### Global BROOK90 framework #####
### 25/03/2020 update
### Ivan Vorobevskii
### Germany, TU Dresden, Institute of Hydrology and Meteorology, Chair of Meteorology

brook90.framework = function  (catchment_path,
                               model_dir = NA,
                               cds_user, cds_key,
                               output_variables = c('swatt','floww','precc','evpp'),
                               cut_warmup_period = 30,
                               meteo_averaging = 'weighted_mean',
                               time_start = as.Date('02/01/1979', tryFormats='%d/%m/%Y'),
                               time_end = as.Date('31/12/2019', tryFormats='%d/%m/%Y') ) {

  pacman::p_load("raster","elevatr","rgdal","sp","ecmwfr","keyring","lubridate","rgeos","stringr",
                 "plyr","data.table","ncdf4","lutz","ggplot2", "rstudioapi")

  ### create model subdir ###
  if (is.na(model_dir)) { model_dir = dirname(catchment_path) }
  suppressWarnings(dir.create(file.path(model_dir,'/dem')))
  suppressWarnings(dir.create(file.path(model_dir,'/soils')))
  suppressWarnings(dir.create(file.path(model_dir,'/land_cover')))
  suppressWarnings(dir.create(file.path(model_dir,'/meteo')))
  suppressWarnings(dir.create(file.path(model_dir,'/model_output')))
  dem_dir =  paste0(model_dir,'/dem/')
  soil_dir = paste0(model_dir,'/soils/')
  landcover_dir = paste0(model_dir,'/land_cover/')
  meteo_dir = paste0(model_dir,'/meteo')
  model_output_dir = paste0(model_dir,'/model_output/')

  ### download initial data ###
  time.benchmark1 = Sys.time()
  time_start = as.Date(time_start, tryFormats='%d/%m/%Y')
  time_end = as.Date(time_end, tryFormats='%d/%m/%Y')
  if (length(list.files(dem_dir))==0) {
    download.dem(catchment_path, dem_dir) # 1-5 min depending on internet speed
  } else {print('Elevation data is already in the folder. Download is skipped')}
  if (length(list.files(soil_dir))==0) {
    download.soil(catchment_path, soil_dir) # 5-10 seconds depending on internet speed
  } else {print('Soil data is already in the folder. Download is skipped')}
  if (length(list.files(landcover_dir))==0) {
    download.landcover(catchment_path, landcover_dir)  # 5-30 min depending on internet speed
  } else {print('Land cover data is already in the folder. Download is skipped')}
  if (length(list.files(meteo_dir))==0) {
    download.meteo(cds_user, cds_key, catchment_path, meteo_dir, time_start, time_end) # ~5-10 hours (15 requests)
  } else {print('Some meteo data is already in the folder. Download is skipped')}

  ### data processing for brook input ###
  time.benchmark2 = Sys.time()
  print('Data processing:')
  model_initials = data_processing (catchment_path, dem_dir, soil_dir, landcover_dir, meteo_dir,
                                    model_output_dir, hourly_P = TRUE, meteo_averaging)
  hydrotops = model_initials[[1]]
  hydrotops.pars = model_initials[[2]]
  meteo_df = model_initials[[3]]
  dates = model_initials[[4]]
  if (length(model_initials)==5) { p_df = model_initials[[5]] } else { p_df = NA }

  ### run model for each hydrotop, get weighting average, cut warm-up period ###
  time.benchmark3 = Sys.time()
  print('Applying BROOK90 for each uniue hydrotop')
  result_list_of_df = list()

  for (i in 1:length(output_variables)) {
    var_df = data.frame(matrix(nrow=length(dates), ncol=(nrow(hydrotops)+2)))
    colnames(var_df) = c('date','weighted_average',1:nrow(hydrotops))
    var_df[,1] = dates
    result_list_of_df[[i]] = var_df
  }
  for (n in 1:nrow(hydrotops)) {
    res = brook90.run.subcatchment (params=hydrotops.pars[[n]], df.meteoFile=meteo_df, df.precFile=p_df, ts.results=output_variables)
    for (i in 1:length(output_variables)) {
      result_list_of_df[[i]][,n+2] = res[[i]]
    }
    print(paste0('      hydrotop ', n, '/', nrow(hydrotops),' -> completed'))
  }
  cols_to_remove = which(tail(result_list_of_df[[1]],1) == 0) # remove hydrotops with numerical problems in swat
  if (length(cols_to_remove)>0) {
    hydrotops = hydrotops[-c(cols_to_remove-2),]
    hydrotops$weights = hydrotops$total_number / sum(hydrotops$total_number)
    for (i in 1:length(output_variables)) {
      result_list_of_df[[i]] = result_list_of_df[[i]][,-c(cols_to_remove)]
    }
  }
  for (i in 1:length(output_variables)) {
    result_list_of_df[[i]] = result_list_of_df[[i]][cut_warmup_period:length(dates),]
    result_list_of_df[[i]][,'weighted_average'] = apply(result_list_of_df[[i]][,3:(2+nrow(hydrotops))], 1, function(x) sum(hydrotops$weights*x))
  }

  ### save and plot results ###
  print('Saving results and plots')
  time.benchmark4 = Sys.time()
  setwd(model_output_dir)
  write.csv(hydrotops, 'unique_hydrotops.csv')
  for (i in 1:length(output_variables)) {
    write.csv(result_list_of_df[[i]], paste0(output_variables[i],'.csv'))
  }
  brook_vars = c('swatt','floww','precc','evpp','rnett','ptrann','irvpp','isvpp','snoww',
                 'pintt','snvpp','slvpp','trandd','mesfld','smltd','slfld','rfald','sfald',
                 'sintdd','rintdd','rthrd','sthrd','rsnod')
  names(brook_vars) = c('soil water volume (total)','total streamflow','precipitation from input',
                        'evaporation','net precipitaiton','potential transpiration',
                        'evaporation rate of intercepted rain','evaporation rate of intercepted snow',
                        'water equivalent of snow on ground','potential interception','evaporation rate from snow',
                        'evaporation rate from soil','transpiration rate','flow from input',
                        'melt drainage rate from snowpack','input rate to soil surface','rainfall rate',
                        'snowfall rate','snowfall catch rate','rainfal catch rate','rain throughfall rate',
                        'snow throughfall rate','rain added to snowpack')
  for (i in 1:length(output_variables)) {
    temp_df = result_list_of_df[[i]]
    temp_df$min = apply(temp_df[,3:ncol(temp_df)],1,FUN=min)
    temp_df$max = apply(temp_df[,3:ncol(temp_df)],1,FUN=max)
    varname = names(brook_vars)[which(brook_vars==output_variables[i])]
    pic = ggplot() +
      geom_ribbon(data=temp_df, aes(x=date, ymin=temp_df$min, ymax=temp_df$max, fill='min/max range \n(all hydrotops)'), alpha=0.2, na.rm=T) +
      geom_line(data=temp_df, aes(x=date, y=temp_df$weighted_average, colour='weighted average'), size=0.3, na.rm=T) +
      theme_bw() + theme(legend.position='bottom') +
      scale_color_manual(values=c('weighted average'='black')) +
      scale_fill_manual(values=c('min/max range \n(all hydrotops)'='black')) +
      scale_x_date(expand=c(0,0), minor_breaks='year') +
      labs(x='', y=paste0(varname,' [mm]'), fill='', colour='', clip='off')
    suppressMessages(ggsave(paste0(output_variables[i],'.png'), plot=pic, dpi=300))
  }
  time.benchmark5 = Sys.time()

  ### print time benchmarks ###
  print(paste0('Starting data download ', time.benchmark1))
  print(paste0('Starting data processing ', time.benchmark2))
  print(paste0('BROOK90 for each hydrotop ', time.benchmark3))
  print(paste0('Saving results ', time.benchmark4))
  print(paste0('End ', time.benchmark5))

}



#####   end   #####
