#create configuration environmet for BROOK90 variables
brook90.environment <- new.env(parent=baseenv())

#Author: R.Kronenberg [06112017]
#
#Parameter from files
#
##CTemperateDeciduousForest.txt
#SCscl.txt
#LMcu.txt
#IDefault.txt
#Xdefault.txt
#FUni50.txt

#
#   * indicates an input from the parameter file
#  ** indicates a variable in the data file or precip input file
# *** indicates a constant
#**** indicates a variable initialized or parameter set in the data file
#   & indicates a variable initialized in the parameter file
#   @ indicates a calculated parameter (constant for run)
#     all others are variables
# at end of variable name indicates integer variable
#& at end of variable name indicates long integer variable
#$ at end of variable name indicates string variable
#xxxxP      flow for precipitation interval, mm
#xxxxD      flow for day, mm
#xxxxM      flow for month, mm
#xxxxY      flow for year, mm



brook90.environment$ML <- 25    #number of values in layer parameter arrays, set to 25 for B90V4
brook90.environment$gctMaxoutvars <- 60 # maximum number of output variables, an array dimension
brook90.environment$gctGenoutv <- 19 # number of variables in GENOUT form
brook90.environment$gctIntoutv <- 22 # number of variables in INTRNOUT form
brook90.environment$gctLayoutv <- 12 # number of variables in LAYOUT form
brook90.environment$gvals <- 8 # max number of graph output values
brook90.environment$maxgraphs <- 100
#
brook90.environment$A <- numeric(brook90.environment$ML) # parsed values from INSTRNG
#aparsed  # parsed values from gsbparsestring, first subscript is 0
#bad      # error indicator passed back to calling routine
#B90path$  # path for running program, used for .INI and .TMP
#cancelsave As Boolean #true if save or print should be cancelled
#chkout(1 To gctMaxoutvars, 1 To 5) As Integer #1 for output, 5 intervals
#        indicates which variables/interval selected
#        stored in .INI
#commadec As Boolean
#dfiletype    #data file type, 1-csv,2-decimal period, 3-decimal comma
brook90.environment$dum <- 0   # dummy variable
brook90.environment$errnum <- 0   # error number from ERR
#EV(1 To 25) As New frmevalout # EVAL windows
brook90.environment$evalnumber <- 0 # number of EVAL screens
#FG(1 To maxgraphs) As New frmgraph  # multiple instances of graph
#gnumber   # graph number
#graphon    # true if graph has been initialized
brook90.environment$INIDAYS <- 0  # number of days to initialize run
#inoutdir$  # latest input-output directory
#intrvl     #output interval index, 1 ANN, 2 MON, 3 DAY, 4 PRE, 5 ITR
#ivar(1 To gctMaxoutvars), pvar(1 To gctMaxoutvars), dvar(1 To gctMaxoutvars), mvar(1 To gctMaxoutvars), yvar(1 To gctMaxoutvars) # list of var numbers for output, does not count mulltiple soil layers
brook90.environment$lstart <- 0# starting output layer
brook90.environment$lend <- 0  #ending output layer
brook90.environment$lstep <- 0 #step for output layers
#msg As String#
#noenderror As Integer  # 0 if no END in parameter file, -1 if END is found
#noruncontinue #1 to prevent Run-Continue after soil parameter changes
#NLN As String  # new line
#outfilestatus(1 To 5) As Integer  #0 if no output, 1 if output, subscript 1 ann, 2 mon, 3 day, 4 pre, 5 itr
##        indicates presence of .TMP output files, controls mnuview???.Enabled
#outselect(1 To 5)  As Integer # True(-1) if output selected for that time interval, False(0) if not
#        controls checking in Select Output menu
brook90.environment$parserr <- 0  #true if parse error in Soil parameters
brook90.environment$prfiletype  <- 0  #precip file type, 1-csv,2-decimal period, 3-decimal comma
brook90.environment$rerunnumber <- 0 # 0 for new run, 1 to 9 for each successive add run
brook90.environment$rstop <- 0 # 1 if run should stop at end of day, 2 if EOF and date check on, 3 if crash error in run or immediate stop request, 4 if max number of graphs
brook90.environment$RUNDAYS <- 0  # number of output days to run after initialize
brook90.environment$runflag <- 0 # 0 for new run, 1 for add run, 2 for continue run
#strng$
#Title$(1 To gctMaxoutvars)   # column titles for gctMaxoutvars variables
#txt$(1 To 30) # text strings for help
#userfont As String
#varno # output variable number, from 1 to gctMaxoutvars, always locally used
#yvars, mvars, dvars, pvars, ivars  # number of vars/cols in output

#DT  time step for DFILE interval,  must be 1 d
brook90.environment$DT <- 1
#WTOMJ  (MJ m-2 d-1)/(watt/m2) = 86400 s/d * .000001 MJ/J
brook90.environment$WTOMJ <- 0.0864
#ETOM  (mm water)/(MJ/m2) using Lv 2448 MJ/Mg and density of water 1 Mg/m3
#= 1E3 mm/m / (2448 MJ/Mg * 1 Mg/m3)
brook90.environment$ETOM <- 0.4085
#CPRHO - volumetric heat capacity of air, J m-3 K-1)
brook90.environment$CPRHO <- 1240#
#GAMMA - psychrometer constant, kPa/K
brook90.environment$GAMMA <- 0.067
#CVLQ - volumetric heat capacity of water, MJ m-2 mm-1 K-1
brook90.environment$CVLQ <- 0.00418
#CVICE - volumetric heat capacity of ice, MJ m-2 mm-1 K-1
brook90.environment$CVICE <- 0.00192
#LF  heat of fusion of water, MJ m-2 mm-1
brook90.environment$LF <- 0.335
#LS  latent heat of sublimation of snow, MJ m-2 mm-1
brook90.environment$LS <- 2.824
#RHOWG  density of water times gravity acceleration, MPa/m or kPa/mm
brook90.environment$RHOWG <- 0.00981
#SIGMA  Stefan-Boltzmann constant, W m-2 K-4)
brook90.environment$SIGMA <- 0.0000000567
#SC  solar constant, value from Lean (1991), W/m2
brook90.environment$SC <- 1367#
#K  vonKarman constant
brook90.environment$K <- 0.4
#PI  pi
brook90.environment$PI <- 3.1416 #pi

brook90.environment$AA  <- 0              # average available energy over daytime or nighttime, W/m2
brook90.environment$ADEF <- 0              # available water deficit in root zone, mm, output only
brook90.environment$ALB <- 0.18            # * albedo with no snow
brook90.environment$ALBEDO <- 0            # albedo
brook90.environment$ALBSN <- 0.23            # * albedo with snow on the ground
brook90.environment$ALPHA <- numeric(brook90.environment$ML)   # modified Cowan alpha, MPa
brook90.environment$ASPECT <-0          # aspect, radians through east from north
brook90.environment$ASPECTD <-0          # * aspect, degrees through east from north
brook90.environment$ASUBS <- 0          # average avail. energy at ground over day or night, W/m2
brook90.environment$ATR <- numeric(2)           #actual transpiration rate for daytime or night, mm/d
brook90.environment$ATRANI <- numeric(brook90.environment$ML)  #actual transp.rate from layer for daytime or night,mm/d
brook90.environment$ATRI <- matrix(0, 2, brook90.environment$ML) #actual transp.rate from layer for daytime and night,mm/d
brook90.environment$AWAT <- 0          # available soil water in root zone, mm, output only
brook90.environment$BALERD <- 0
brook90.environment$BALERM <- 0
brook90.environment$BALERY <- 0
# error in water balance. mm
brook90.environment$BEXP <- rep(7.12, brook90.environment$ML)    #* exponent for psi-theta relation
brook90.environment$BYFL <-0           #bypass flow rate from all layers for iteration, mm/d
brook90.environment$BYFLI <- numeric(brook90.environment$ML)   #bypass flow rate from layer, mm/d
brook90.environment$BYFLPI <- numeric(brook90.environment$ML)
brook90.environment$BYFLDI <- numeric(brook90.environment$ML)
brook90.environment$BYFLMI <- numeric(brook90.environment$ML)
brook90.environment$BYFLYI <- numeric(brook90.environment$ML)
#                         bypass flow from layer, mm
brook90.environment$BYFLP <- rep(0, brook90.environment$ML)
brook90.environment$BYFLD <- rep(0, brook90.environment$ML)
brook90.environment$BYFLM <- rep(0, brook90.environment$ML)
brook90.environment$BYFLY <- rep(0, brook90.environment$ML)  # bypass flow, mm
brook90.environment$BYFRAC <- numeric(brook90.environment$ML)  #fraction of layer infiltration to bypass flow
brook90.environment$BYPAR <- 0             #* 1 to allow BYFL, or 0 to prevent BYFL
#C    <-0             #dummy input string
brook90.environment$C1 <-0.25             #* intercept of relation of solar rad. to sunshine duration
brook90.environment$C2 <- 0.5             #* slope of relation of solar radiation to sunshine duration
brook90.environment$C3 <- 0.2             #* longwave correction factor for overcast sky
brook90.environment$CC <- 0           #cold content of snowpack (positive), MJ m-2
brook90.environment$CCFAC <- 0.3            #* cold content factor, MJ m-2 d-1 K-1
brook90.environment$CHM <- numeric(brook90.environment$ML)     #@ Clapp-Hornberger m parameter, kPa
brook90.environment$CHN <- numeric(brook90.environment$ML)     #@ Clapp-Hornberger n parameter
brook90.environment$CINTRL <- 0.15             #* maximum interception storage of rain per unit LAI, mm
brook90.environment$CINTRS <- 0.15             #* maximum interception storage of rain per unit SAI, mm
brook90.environment$CINTSL <- 0.6             #* maximum interception storage of snow per unit LAI, mm
brook90.environment$CINTSS <- 0.6             #* maximum interception storage of snow per unit SAI, mm
brook90.environment$CR <- 0.6              #* light extinction coefficient for projected LAI + SAI
brook90.environment$CS <- 0.035               #* ratio of projected SAI to canopy height, m-1
brook90.environment$CVPD <- 2              #* vapor pressure deficit at which conductance is halved, kPa
brook90.environment$CZR <- 0.05              #* ratio of roughness to HEIGHT for rough closed canopies
brook90.environment$CZS <- 0.13               #* ratio of roughness to HEIGHT for smooth closed canopies
brook90.environment$DAYLEN <- 0            #daylength in fraction of day, d-1
brook90.environment$DAYMO <- numeric(12)        #* days in month
brook90.environment$DD <- 0              #** day of the month from data files
brook90.environment$DELTA <- 0             #dES/dT at some temperature T, kPa/K
brook90.environment$DENSEF <- 1.0            #* density or thinning multiplier for MAXLAI,CS,RTLEN,RPLANT, not <.001
brook90.environment$DISP <- 0             #zero-plane displacement, m
brook90.environment$DISPC <- 0            #zero-plane displacement for closed canopy of HEIGHT, m
brook90.environment$DOM <- 0             #day of month
brook90.environment$DOY <- 0             #**** first day of the year from DFILE header
brook90.environment$DPSIDW <- numeric(brook90.environment$ML)  #rate of change of total potential with watercontent,kPa/mm
brook90.environment$DPSIMX <- 0.01           #* maximum potential difference considered equal, kPa
brook90.environment$DRAIN <- 1            #* multiplier of VFLUX(n) for drainage to groundwater
brook90.environment$DSFL <- 0             #downslope flow rate from all layers for iteration, mm/d
brook90.environment$DSFLI <- numeric(brook90.environment$ML)   #downslope flow rate from layer, mm/d
brook90.environment$DSFLP <- 0
brook90.environment$DSFLD <- 0
brook90.environment$DSFLM <- 0
brook90.environment$DSFLY <- 0   #downslope flow, mm
brook90.environment$DSFLPI <- numeric(brook90.environment$ML)
brook90.environment$DSFLDI <- numeric(brook90.environment$ML)
brook90.environment$DSFLMI <- numeric(brook90.environment$ML)
brook90.environment$DSFLYI <- numeric(brook90.environment$ML)
#                        #downslope drainage from layer, mm
brook90.environment$DSLOPE <- 0           #slope for DSFL, radians
brook90.environment$DSLOPED <- 0           #* slope for DSFL, degrees
brook90.environment$DSWMAX <- 2          #* maximum change allowed in SWATI, percent of SWATMX(i)
brook90.environment$DTI <- 1             #time step for iteration interval, d
brook90.environment$DTIMAX <- 0.01           #* maximum iteration time step, d
brook90.environment$DTINEW <- 0.05           #second estimate of DTI
brook90.environment$DTP <- 1            #@ time step for precipitation interval, may be <= 1 d
brook90.environment$DTRI <- 0             #time remaining in precipitation interval, d
brook90.environment$DUMM <- rep(0, brook90.environment$gctMaxoutvars)   #dummy array for subroutine calls
brook90.environment$dummy <- 0           # dummy variable for subroutine calls
brook90.environment$DURATN <- c(4,4,5,3,3,2,3,3,4,4,5,5)   #* average duration of daily precip by month, hr
brook90.environment$EA <- 0             #** vapor pressure for the day, kPa
brook90.environment$ES <- 0              #saturated vapor pressure, kPa
brook90.environment$ESLOPE <- 0           #slope for evapotranspiration and snowmelt, radians
brook90.environment$ESLOPED <- 0          #* slope for evapotranspiration and snowmelt, degrees
brook90.environment$EVAPP <- 0
brook90.environment$EVAPD <- 0
brook90.environment$EVAPM <- 0
brook90.environment$EVAPY <- 0  #evapotranspiration
brook90.environment$FARR <- rep(0,366)    #array of simulated daily flow for statistics
brook90.environment$FETCH <- 5000           #* weather station fetch, m"
brook90.environment$FLOWP <- 0
brook90.environment$FLOWD <- 0
brook90.environment$FLOWM <- 0
brook90.environment$FLOWY <- 0   #total flow
brook90.environment$FRINTL <- 0.06           #* intercepted fraction of rain per unit LAI
brook90.environment$FRINTS <- 0.06           #* intercepted fraction of rain per unit SAI
brook90.environment$FSINTL <- 0.04           #* intercepted fraction of snow per unit LAI
brook90.environment$FSINTS <- 0.04           #* intercepted fraction of snow per unit SAI
brook90.environment$FXYLEM <- 0.5           #* fraction of plant resistance in xylem
brook90.environment$GER <- numeric(2)            #ground evaporation rate for daytime or night, mm/d
brook90.environment$GEVP <- 0             #average ground evaporation for day, mm/d
brook90.environment$GIR <- numeric(0)            #ground evap. rate with intercep. for daytime or night,mm/d
brook90.environment$GIVP <- 0              #average ground evaporation for day with interception, mm/d
brook90.environment$GLMAX <- 0            # maximum leaf conductance, m/s
brook90.environment$GLMAXC <- 0.53           # * maximum leaf conductance, cm/s
brook90.environment$GLMIN <- 0            # minimum leaf conductance, m/s
brook90.environment$GLMINC <- 0.03           # * minimum leaf conductance, cm/s
brook90.environment$GRAPH <- 0            #* runtime graphics output, 0-none, 1-continuous, 2-pause
brook90.environment$GRDMLT <- 0.35            #* rate of groundmelt of snowpack, mm/d
brook90.environment$GSC <- 0             #* discharge from GWAT, fraction per day, d-1
brook90.environment$GSP <- 0              #* fraction of discharge to seepage
brook90.environment$GWAT <- 0            #groundwater storage below soil layers, mm
brook90.environment$GWATIN <- 0            #**** initial groundwater storage below soil layers, mm
brook90.environment$GWFL <- 0             #streamflow rate from groundwater, mm/d
brook90.environment$GWFLP <- 0
brook90.environment$GWFLD <- 0
brook90.environment$GWFLM <- 0
brook90.environment$GWFLY <- 0   #groundwater flow, mm
brook90.environment$mnuhalfiter <- TRUE
brook90.environment$HEIGHT <- 0           #canopy height, m
brook90.environment$HR <- 10              #* height above which CZR applies, m
brook90.environment$HS <- 1             #* height below which CZS applies, m
#I    <-0             #index variable for layer number
brook90.environment$I0HDAY <- 0            #potential insolation on horizontal, MJ m-2 d-1
brook90.environment$IDAY <- 0             #day number in run
brook90.environment$II <- 0               #** input precipitation interval number
brook90.environment$ILAYER <- 0           # number of layers over which infiltration is distributed
brook90.environment$IDEPTH <- 500            #* depth over which infiltration is distributed

brook90.environment$IMPERV <- 0.1           #* impervious fraction of area for SRFL
brook90.environment$INFEXP <- 1           #* infiltration exponent, 0-all to top to 1-uniform with depth
brook90.environment$INFRAC <- rep(0, brook90.environment$ML)  #@ fraction of infiltration to each layer
brook90.environment$INFLI <- rep(0, brook90.environment$ML)  #infiltration rate into layer, mm/d
brook90.environment$INFLP <- rep(0, brook90.environment$ML)
brook90.environment$INFLD <- rep(0, brook90.environment$ML)
brook90.environment$INFLM <- rep(0, brook90.environment$ML)
brook90.environment$INFLY <- 0  # infiltration into soil matrix, mm
brook90.environment$INFLPI <- rep(0, brook90.environment$ML)
brook90.environment$INFLDI <- rep(0, brook90.environment$ML)
brook90.environment$INFLMI <- rep(0, brook90.environment$ML)
brook90.environment$INFLYI <- rep(0, brook90.environment$ML)
#                         infiltration to layer, mm
brook90.environment$INTR <- 0            #intercepted rain, mm
brook90.environment$INTRIN <- 0           #& initial intercepted rain, mm
brook90.environment$INTS <- 0            #intercepted snow, mm
brook90.environment$INTSIN <- 0            #& initial intercepted snow, mm
brook90.environment$IRVP <- 0              #evaporation rate of intercepted rain, mm/d
brook90.environment$IRVPD <- 0
brook90.environment$IRVPM <- 0
brook90.environment$IRVPY <- 0  #evaporation of intercepted rain, mm
brook90.environment$ISVP <- 0             #evaporation rate of intercepted snow, mm/d
brook90.environment$ISVPD <- 0
brook90.environment$ISVPM <- 0
brook90.environment$ISVPY <- 0   #evaporation of intercepted snow, mm
brook90.environment$J <- 0              #index variable for day-night separation
brook90.environment$KF <- rep(4.21, brook90.environment$ML)      #* hydraulic conductivity at field capacity, mm/d
brook90.environment$KK <- rep(0, brook90.environment$ML)      #hydraulic conductivity, mm/d
brook90.environment$KSAT <- rep(0, brook90.environment$ML)    #@ saturated hydraulic conductivity, mm/d
brook90.environment$KSNVP <- 0.3             #* multiplier to fix snow evaporation problem
brook90.environment$L1 <- 0               #@ latitude of equivalent slope, radians
brook90.environment$L2 <- 0               #@ time shift of equivalent slope, radians
brook90.environment$LAI <- 0              #leaf area index, m2/m2
brook90.environment$LAIMLT <- 0.2            #* parameter for snowmelt dependence on LAI, Globalensionless
brook90.environment$LAT <- 0             #latitude, radians
brook90.environment$LATD <- 43.23              #**** latitude, degrees
brook90.environment$LENGTH <- 0           #* slope length for DSFL, m
brook90.environment$LPC <- 4              #* minimum LAI defining a closed canopy
brook90.environment$LWIDTH <- 0.1         #* leaf width, m
brook90.environment$MARR <- c(seq(1,366,1))    #array of measured daily flow for statistics
brook90.environment$MAXHT <- 25              #* maximum height for the year, m
brook90.environment$MAXLAI <- 6           #* maximum projected leaf area index for the year, m2/m2
brook90.environment$MAXLQF <- 0.05            #* maximum liquid water fraction of SNOW, Globalensionless
brook90.environment$MELFAC <- 1.5            #* degree day melt factor for open, MJ m-2 d-1 K-1
brook90.environment$MESFL <- 0            #** measured streamflow for day, mm
brook90.environment$MESFLD <- 0
brook90.environment$MESFLM <- 0
brook90.environment$MESFLY <- 0 # measured streamflow, mm
brook90.environment$MESFLP <- 0           #** measured streamflow rate for precip interval, mm/d
brook90.environment$MM <- 0           #** month from data files
brook90.environment$MONTHN <- 0          #month number
brook90.environment$MXKPL <- 8           #* maximum plant conductivity, (mm/d)/MPa
brook90.environment$MXRTLN <- 3000           #* maximum root length per unit land area, m/m2
brook90.environment$N <- 0             #index variable for precipitation interval
brook90.environment$NN <- 2.5              #* wind/diffusivity extinction coefficient
brook90.environment$NDAYS <- 0            #* number of days in run
brook90.environment$NITS <- 0             #number of iterations in precipitation interval
brook90.environment$NITSD <- 0            #total number of iterations for day
brook90.environment$NITSM <- 0            #total number of iterations for month
brook90.environment$NITSY <- 0            #total number of iterations for year
brook90.environment$NITSR <- 0            #total number of iterations for run
brook90.environment$NLAYER <- 20         #* number of soil layers to be used in model, <= ML
brook90.environment$NOOUTF <- 1          #* 1 if no outflow allowed from roots, otherwise 0
brook90.environment$NPINT <- 1           #**** number of precipitation intervals per day
brook90.environment$NTFLI <- rep(0, brook90.environment$ML)   #net flow rate into layer, mm/d
brook90.environment$NTFLPI <- rep(0, brook90.environment$ML)
brook90.environment$NTFLDI <- rep(0, brook90.environment$ML)
brook90.environment$NTFLMI <- rep(0, brook90.environment$ML)
brook90.environment$NTFLYI <- rep(0, brook90.environment$ML)
#                         net flow into layer, mm
brook90.environment$PINT <- 0             #average potential interception for day, mm/d
brook90.environment$PINTD <- 0
brook90.environment$PINTM <- 0
brook90.environment$PINTY <- 0 #  potential interception, mm
brook90.environment$PIR <- c(0,1)            #potential interception rate for daytime or night, mm/d
brook90.environment$PREC <- 0            #precipitation rate, mm/d
brook90.environment$PRECD <- 0
brook90.environment$PRECM <- 0
brook90.environment$PRECY <- 0             #  precipitation, mm
brook90.environment$PREINT <- 1          #** precipitation for precipitation interval, mm
brook90.environment$PRECIN <- 0          #** daily precipitation, mm
brook90.environment$PSICR <- -2           #* minimum plant leaf water potential, MPa
brook90.environment$PSIF <- rep(-6.3, brook90.environment$ML)    #* matric potential at field capacity, kPa
brook90.environment$PSIG <- rep(0, brook90.environment$ML)      #@ gravity potential, kPa
brook90.environment$PSIM <- rep(0, brook90.environment$ML)      #& matric soil water potential for layer, kPa
brook90.environment$PSIMIN <- rep(0, brook90.environment$ML)    # initial PSIM()
brook90.environment$PSITI <- rep(0, brook90.environment$ML)     #total potential, kPa
brook90.environment$PSNVP <- 0            #potential snow evaporation, mm/d
brook90.environment$PTR <- c(0,1)          #potential transpiration rate for daytime or night, mm/d
brook90.environment$PTRAN <- 0           #average potential transpiration rate for day, mm/d
brook90.environment$PTRAND <- 0
brook90.environment$PTRANM <- 0
brook90.environment$PTRANY <- 0            # potential transpiration, mm
brook90.environment$QFFC <- 0.2              #* quick flow fraction (SRFL or BYFL) at field capacity
brook90.environment$QFPAR <- 0.1            #* quick flow parameter (SRFL or BYFL)
brook90.environment$QLAYER <- 1          # number of soil layers for SRFL, 0 to prevent SRFL
brook90.environment$QDEPTH <- 1           #* soil depth for SRFL calculation, 0 to prevent SRFL
brook90.environment$R5 <- 100            #* solar radiation at which conductance is halved, W/m2
brook90.environment$RAA <- 0           #Shuttleworth-Wallace atmosphere aerodynamic resistance,s/m
brook90.environment$RAC <- 0            #Shuttleworth-Wallace canopy aerodynamic resistance, s/m
brook90.environment$RAS <- 0            #Shuttleworth-Wallace ground aerodynamic resistance, s/m
brook90.environment$RELHT <- c(1,0,120,0,213,1,273,1,303,0,366,0,0,0,0,0,0,0,0,0)    #* ten pairs of DOY and relative canopy height
brook90.environment$RELLAI <- c(1,0,120,0,213,1,273,1,303,0,366,0,0,0,0,0,0,0,0,0)   #* ten pairs of DOY and relative LAI
brook90.environment$ROOTDEN <- c(100,0.29,100,0.21,100,.15,100,.07,100,0.05,100,.04,100,.03,100,.02,100,.02,100,.01,100,0.01,100,.01,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0,100,0)  #* 25 pairs of root layer thickness (mm) and relative root density per unit volume
brook90.environment$RELDEN <- rep(0, brook90.environment$ML)  # relative root density per unit volume for soil layers
brook90.environment$RFAL <- 0           #rainfall rate, mm/d
brook90.environment$RFALD <- 0
brook90.environment$RFALM <- 0
brook90.environment$RFALY <- 0            #  rainfall, mm
brook90.environment$RHOTP <- 2            #* ratio of total leaf area to projected area
brook90.environment$RINT <- 0            #rainfall catch rate, mm/d
brook90.environment$RINTD <- 0
brook90.environment$RINTM <- 0
brook90.environment$RINTY <- 0           #  rain interception, mm
brook90.environment$RM <- 1000               #* maximum solar radiation, at which FR = 1, W/m2
brook90.environment$RNET <- 0           #rain reaching soil surface, mm/d
brook90.environment$RNETD <- 0
brook90.environment$RNETM <- 0
brook90.environment$RNETY <- 0            #  rainfall to soil surface, mm
brook90.environment$RPLANT <- 0            #plant resistivity to water flow, MPa d/mm
brook90.environment$RROOTI <- rep(0, brook90.environment$ML)    #root resistance for layer, MPa d/mm
brook90.environment$RTHR <- 0           #rain throughfall rate, mm/d
brook90.environment$RTHRD <- 0
brook90.environment$RTHRM <- 0
brook90.environment$RTHRY <- 0             #  rain throughfall, mm
brook90.environment$RTLEN <- 0            #root length per unit land area, m/m2
brook90.environment$RTRAD <- 0.35            #* average root radius, mm
brook90.environment$RRD <- 0.55
brook90.environment$RSC <- 0              #Shuttleworth-Wallace canopy surface resistance, s/m
brook90.environment$RSNO <- 0             #rain added to snowpack, mm/d
brook90.environment$RSNOD <- 0
brook90.environment$RSNOM <- 0
brook90.environment$RSNOY <- 0            #  rain on snow, mm
brook90.environment$RSS <- 0              #Shuttleworth-Wallace soil surface resistance, s/m
brook90.environment$RSSA <- 500             #* soil evaporation resistance at field capacity, s/m
brook90.environment$RSSB <- 1             #* exponent in relation of soil evap res to water potential
brook90.environment$RSTEMP <- -0.5         #* base temperature for snow-rain transition, ?C
brook90.environment$RXYLEM <- 0           #xylem resistance, MPa d/mm
brook90.environment$SAFRAC <- 0           #source area fraction
brook90.environment$SAI <- 0              #stem area index, m2/m2
brook90.environment$SAIMLT <- 0.5           #* parameter for snowmelt dependence on SAI, Globalensionless
brook90.environment$SEEP <- 0             #deep seepage loss from groundwater, mm/d
brook90.environment$SEEPP <- 0
brook90.environment$SEEPD <- 0
brook90.environment$SEEPM <- 0
brook90.environment$SEEPY <- 0             # seepage loss, mm
brook90.environment$SFAL <- 0           #snowfall rate, mm/d
brook90.environment$SFALD <- 0
brook90.environment$SFALM <- 0
brook90.environment$SFALY <- 0             #  snowfall, mm
brook90.environment$SHEAT <- 0            #@ average soil heat flux for the day, W/m2, fixed at 0
brook90.environment$SINT <- 0             #snowfall catch rate, mm/d
brook90.environment$SINTD <- 0
brook90.environment$SINTM <- 0
brook90.environment$SINTY <- 0# snow interception, mm
brook90.environment$SLFDAY <- 0          #ratio of potential insolation on slope to horizontal, map area
brook90.environment$SLFL <- 0            #input rate to soil surface, mm/d
brook90.environment$SLFLP <- rep(0, brook90.environment$ML)
brook90.environment$SLFLD <- rep(0, brook90.environment$ML)
brook90.environment$SLFLM <- rep(0, brook90.environment$ML)
brook90.environment$SLFLY <- rep(0, brook90.environment$ML)# input to soil surface, mm
brook90.environment$SLFLI <- rep(0, brook90.environment$ML)   #macropore infiltration rate down from layer, mm/d
brook90.environment$SLFLPI <- rep(0, brook90.environment$ML)
brook90.environment$SLFLDI <- rep(0, brook90.environment$ML)
brook90.environment$SLFLMI <- rep(0, brook90.environment$ML)
brook90.environment$SLFLYI <- rep(0, brook90.environment$ML)
#                         vertical macropore infiltration from layer, mm
brook90.environment$SLRAD <- 0            #average solar radiation on slope over daytime, W/m2
brook90.environment$SLRADd <- 0
brook90.environment$SLVP <- 0             #evaporation rate from soil, mm/d
brook90.environment$SLVPD <- 0
brook90.environment$SLVPM <- 0
brook90.environment$SLVPY <- 0             # soil evaporation, mm
brook90.environment$SMLT <- 0            #melt drainage rate from snowpack, mm/d
brook90.environment$SMLTD <- 0
brook90.environment$SMLTM <- 0
brook90.environment$SMLTY <- 0            # snowmelt, mm
brook90.environment$SNODEN <- 0.3         #* snow density, mm/mm
brook90.environment$SNOEN <- 0            #energy flux density to snow surface, MJ m-2 mm-1 d-1
brook90.environment$SNOFRC <- 0           #fraction of precipitation for the day as snow, unitless
brook90.environment$SNOW <- 0             #water equivalent of snow on the ground, mm
brook90.environment$SNOWIN <- 0          #**** initial water equivalent of snow on the ground, mm
brook90.environment$SNOWLQ <- 0            #liquid water content of snow on the ground, mm
brook90.environment$SNVP <- 0            #evaporation rate from snowpack, mm/d
brook90.environment$SNVPD <- 0
brook90.environment$SNVPM <- 0
brook90.environment$SNVPY <- 0            # evaporation from snowpack, mm
brook90.environment$SOLRAD <- 0           #** solar radiation for the day, horizontal surface, MJ/m2
brook90.environment$SOLRADC <- 0           #SOLRAD as corrected if necessary by WEATHER routine, MJ/m2
brook90.environment$SRFL <- 0             #source area flow rate, mm/d
brook90.environment$SRFLP <- 0
brook90.environment$SRFLD <- 0
brook90.environment$SRFLM <- 0
brook90.environment$SRFLY <- 0# source area flow, mm
brook90.environment$STHR <- 0             #snow throughfall rate, mm/d
brook90.environment$STHRD <- 0
brook90.environment$STHRM <- 0
brook90.environment$STHRY <- 0 # snow throughfall, mm
brook90.environment$STONEF <- rep(0.00, brook90.environment$ML) #* stone volume fraction, unitless
brook90.environment$STORD <- 0
brook90.environment$STORM <- 0
brook90.environment$STORY <- 0 # total water storage in system, mm
brook90.environment$STRES <- 0             #TRAN / PTRAN for time period
brook90.environment$STRX <- 0             #string variable to trap q or esc
brook90.environment$SWAT <- 0    #total soil water in all layers, mm
brook90.environment$SWATI <- rep(0, brook90.environment$ML)  #water volume in layer, mm
brook90.environment$SWATMX <- rep(0, brook90.environment$ML)  #maximum water storage for layer, mm
brook90.environment$SWATQF <- 0         #@water storage at field capacity for layers 1 to QLAYER,mm
brook90.environment$SWATQX <- 0           #@ maximum water storage for layers 1 to QLAYER, mm
brook90.environment$T1 <- 10            #* lowest temp. at which stomates not temp. limited, degC
brook90.environment$T2 <- 30            #* highest temp. at which stomates not temp. limited,degC
brook90.environment$TA <- 40             #mean temperature for the day at reference height, degC
brook90.environment$TADTM <- 0            #average daytime temperature at reference height, degC
brook90.environment$TAJ <- 0             #TADTM or TANTM depending on J
brook90.environment$TANTM <- 0            #average nighttime temperature at reference height, degC
brook90.environment$TEMP <- 0           #temporary integer variable - apparently no longer used
brook90.environment$TH <- 0             #* temperature above which stomates are closed, degC
brook90.environment$THETA <- rep(0, brook90.environment$ML)   #water content, mm water / mm soil matrix
brook90.environment$THETAF <- rep(0.318, brook90.environment$ML) #* volumetric water content at field capacity
brook90.environment$THICK <- rep(100, brook90.environment$ML)   #* layer thicknesses, mm
brook90.environment$THSAT <- rep(0.420, brook90.environment$ML)  #* theta at saturation, matrix porosity
brook90.environment$TL <- 0            #* temperature below which stomates are closed, degC
brook90.environment$TMAX <- 0            #** maximum temperature for the day, degC
brook90.environment$TMIN <- 0             #** minimum temperature for the day, degC
brook90.environment$TRANI <- rep(0, brook90.environment$ML)   #average transpiration rate from layer, mm/d
brook90.environment$TRANP <- 0
brook90.environment$TRAND <- 0
brook90.environment$TRANM <- 0
brook90.environment$TRANY <- 0             # transpiration, mm
brook90.environment$TRANPI <- rep(0, brook90.environment$ML)
brook90.environment$TRANDI <- rep(0, brook90.environment$ML)
brook90.environment$TRANMI <- rep(0, brook90.environment$ML)
brook90.environment$TRANYI <- rep(0, brook90.environment$ML)
#                        #layer transpiration, mm
brook90.environment$TSNOW <- 0           #snowpack temperature (isothermal assumed), degC
brook90.environment$UA <- 0           #average wind speed for the day at reference height, m/s
brook90.environment$UADTM <- 0           #average wind speed for daytime at reference height, m/s
brook90.environment$UAJ <- 0            #UADTN or UANTM depending on J
brook90.environment$UANTM <- 0            #average wind speed for nighttime at reference height, m/s
brook90.environment$UW <- 0             #** average wind speed for day at weather station, m/s
brook90.environment$VPD <- 0             #vapor pressure deficit at reference height, kPa
brook90.environment$VRFLI <- rep(0, brook90.environment$ML)   #vertical matrix drainage rate from layer, mm/d
brook90.environment$VRFLPI <- rep(0, brook90.environment$ML)
brook90.environment$VRFLDI <- rep(0, brook90.environment$ML)
brook90.environment$VRFLMI <- rep(0, brook90.environment$ML)
brook90.environment$VRFLYI <- rep(0, brook90.environment$ML)
#                         vertical matrix drainage from layer, mm
brook90.environment$VV <- rep(0, brook90.environment$ML)      #temporary VRFLI
brook90.environment$WETC <- rep(0, brook90.environment$ML)    #@ wetness at PSICR, Globalensionless
brook90.environment$WETF <- rep(0, brook90.environment$ML)   #@ wetness at field capacity, Globalensionless
brook90.environment$WETFR <- 0           #fraction of precipitation interval that canopy is wet
brook90.environment$WETINF <- rep(0.92, brook90.environment$ML)  #* wetness at dry end of near-saturation range
brook90.environment$WETNES <- rep(0, brook90.environment$ML)  #wetness, fraction of saturation
brook90.environment$WNDRAT <- .3           #* ratio of nighttime to daytime wind speed
brook90.environment$XMAX <- 0            #maximum value for x-axis
brook90.environment$XMIN <- 0            #minimum value for x-axis
brook90.environment$YEARN <- 0           #**** first year from DFILE header
brook90.environment$YMAX <- numeric(brook90.environment$gvals)      #* maximum value for y-axis
brook90.environment$YMIN <- numeric(brook90.environment$gvals)      #@ minimum value for y-axis
brook90.environment$YNAME <- numeric(brook90.environment$gvals)    #@ variable name
brook90.environment$YVAL <- numeric(brook90.environment$gvals)      #plot value for y-axis
brook90.environment$YY <- 0            #** year from data files - used only to check dates and to determine end of year for output
brook90.environment$Z0 <- 0             #roughness parameter, m
brook90.environment$Z0C <- 0             #roughness parameter for closed canopy of HEIGHT, m
brook90.environment$Z0G <- 0.02             #* ground surface roughness, m
brook90.environment$Z0GS <- 0            #ground or snow surface roughness, m
brook90.environment$Z0S <- 0.001             #* snow surface roughness, m
brook90.environment$Z0W <- 0.005             #* weather station roughness parameter, m
brook90.environment$ZA <- 0             #reference height for TA, EA, UA, above ground, m
brook90.environment$ZMINH <- 2            #* ZA minus HEIGHT, reference height above canopy top, m
brook90.environment$ZW <- 10              #* weather station measurement height for wind, m"

#load additional variables
brook90.environment$SUBDAYDATA <- FALSE
brook90.environment$SWCHECKED <- FALSE

#load utility functions to environment
#source("./data-raw/brook90.utility.R", local=brook90.environment)

#save configurations
usethis::use_data(brook90.environment, overwrite=T)
