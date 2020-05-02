### 25/03/2020 update

### Mandatory set-ups ###
catchment_path = 'D:/WORK/brook90_improvement/validation_discharge/catchment_123/catchment_123.shp'
time_start = '01/01/2017' # min: 02/01/1979
time_end = '15/04/2020' # max: current date minus 5-7 days

cds_user = '28735' # ERA5 user ID
cds_key = '20901d6e-0f08-4d25-8eac-5d2130197dfb' # ERA5 key (pass)
cds_user = '33256' # rico
cds_key = '2cab8392-b91e-4d2f-9809-f3ece1ba74d6' # rico
cds_user = '33968' # kate
cds_key = 'c7330c02-6558-4dbb-922f-6fd78ac4bd70' # kate
cds_user = '41532' # my2
cds_key = '5546bfb3-e53c-4678-a382-315c5f203536' # my2
cds_user = '41540' # kate2
cds_key = '30a814c0-395a-4a7e-9582-624ff3c8f70e' # kate2


### Run framework ###
brook90.framework (catchment_path=catchment_path,
                   cds_user=cds_user, cds_key=cds_key,
                   time_start=time_start, time_end=time_end,
                   # optional defaults
                   model_dir = NA,                                        # folder to put results
                   output_variables = c('swatt','floww','precc','evpp'),  # p.s. see full list below
                   cut_warmup_period = 30,                                # in days
                   meteo_averaging = 'weighted_mean' )                    # weighted_mean/mean/nearest_grid

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
