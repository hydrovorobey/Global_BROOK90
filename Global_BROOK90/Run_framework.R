### 25/03/2020 update

### Mandatory set-ups ###
catchment_path = 'XXXXX'
time_start = 'XXXXX' # min: 02/01/1979
time_end = 'XXXXX' # max: current date minus 5-7 days

cds_user = 'XXXXX' # ERA5 user ID
cds_key = 'XXXXX' # ERA5 key (pass)

### Run framework ###
brook90.framework (catchment_path=catchment_path,
                   cds_user=cds_user, cds_key=cds_key,
                   time_start=time_start, time_end=time_end,
                   # optional defaults
                   model_dir = NA,                                        # folder to put results
                   output_variables = c('swatt','floww','precc','evpp'),  # p.s. see full list below
                   cut_warmup_period = 30,                                # in days
                   meteo_averaging = 'weighted_mean',                     # weighted_mean/mean/nearest_grid
                   check_packages_updates = FALSE  )                       # reinstall add used packages to uptodate versions

0### List of all variables:
# 'swatt'  # soil water volume (total)
# 'floww'  # total streamflow
# 'precc'  # precipitation from input
# 'evpp'   # evaporation
# 'rnett'  # net precipitation
# 'ptrann' # potential transpiration
# 'irvpp'  # evaporation rate of intercepted rain
# 'isvpp'  # evaporation rate of intercepted snow
# 'snoww'  # water equivalent of snow on ground
# 'pintt'  # potential interception
# 'snvpp'  # evaporation rate from snow
# 'slvpp'  # evaporation rate from soil
# 'trandd' # transpiration rate
# 'smltd'  # melt drainage rate from snowpack
# 'slfld'  # input rate to soil surface
# 'rfald'  # rainfall rate
# 'sfald'  # snowfall rate
# 'sintdd' # snowfall catch rate
# 'rintdd' # rainfall catch rate
# 'rthrd'  # rain throughfall rate
# 'sthrd'  # snow throughfall rate
# 'rsnod'  # rain added to snowpack


# output results in 'model_output' folder:
# csv files for each of variables (weighted average, min/max and each hydrotop)
# csv with hydrotops (soil and land cover parameter combinations)
# basic plots for each of variables

###
