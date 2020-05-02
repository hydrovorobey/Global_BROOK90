
#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [01022018]
#
#   TU Dresden
#   Institut fuer Hydrologie und Meteorologie
#   Professur fuer Meteorologie
#   2017
#
#
#
#
#
#
#
#***********************************************************************
ACCUM <- function(A1, A2, A3, A4, A5, B1, B2, B3, B4, B5){
  # accumulator; adds Aj to Bj
  B1 <- B1 + A1
  B2 <- B2 + A2
  B3 <- B3 + A3
  B4 <- B4 + A4
  B5 <- B5 + A5
  return(list(B1, B2, B3, B4, B5))
}

#***************************************************************************
ACCUMI<-function (N, A1, A2, A3, A4, A5, B1, B2, B3, B4, B5){
  # accumulator for array components; adds Aj(i) to Bj(i) for each i up to n
  B1 <- B1 + A1
  B2 <- B2 + A2
  B3 <- B3 + A3
  B4 <- B4 + A4
  B5 <- B5 + A5
  #for( i in 1:N){
  #  B1[i] <- B1[i] + A1[i]
  #  B2[i] <- B2[i] + A2[i]
  #  B3[i] <- B3[i] + A3[i]
  #  B4[i] <- B4[i] + A4[i]
  #  B5[i] <- B5[i] + A5[i]
  #}
  return(list(B1, B2, B3, B4, B5))
}

#*******************************************************************
ACOSF<-function (T){
  #arc cosine in radians from 0 to pi
  #TA<-0
  #AC<-0
  #TA <- abs(T)
  #if (TA > 1) {#

  #}
  #if (TA < .7) {
  #  AC <- 1.570796 - atan(TA / (1 - TA * TA)^(1/2))
  #}else{
  #  AC <- atan((1 - TA * TA)^(1/2) / TA)
  #}
  #if (T < 0) {
  #  ACOS <- 3.141593 - AC
  #}else{
  #  ACOS <- AC
  #}
  return(acos(T))
}

#**********************************************************
ASINF<-function (temp){
  #arc sine in radians from -pi/2 to pi/2
  #TA<-0
  #TA <- abs(temp)
  #if(TA > 1){
  #
  #}
  #if (TA < 0.7) {
  #  ASIN <- sign(temp) * (atan(TA / (1 - TA * TA)^(1/2)))
  #}else{
  #  ASIN <- sign(temp) * (1.570796 - atan((1 - TA * TA)^(1/2) / TA))
  #}
  return(asin(temp))
}

#**********************************************************
RMAXF <-function(T1, T2){
  #real single precision maximum
  if (T1 < T2) {
    RMAX <- T2
  }else{
    RMAX <- T1
  }
  return(RMAX)
}

#**************************************************
RMINF<-function (T1, T2){
  #real single precision minimum
  if (T1 > T2) {
    RMIN <- T2
  }else{
    RMIN <- T1
  }
  return(RMIN)
}

#************************************************************************
SUMI<-function (N, A1, A2, A3, A4, A5, A6, B1, B2, B3, B4, B5, B6){
  #array summer; sums Aj(i) for i = 1,n with result in Bj
  B<-rep(0,6)
  #ListB<-  ZERO(B1, B2, B3, B4, B5, B6)
  for( i in 1:N){
    B[1] <- B[1] + A1[i]
    B[2] <- B[2] + A2[i]
    B[3] <- B[3] + A3[i]
    B[4] <- B[4] + A4[i]
    B[5] <- B[5] + A5[i]
    B[6] <- B[6] + A6[i]
  }
  return(list(B[1], B[2], B[3], B[4], B[5], B[6]))
}

#***************************************************************************
ZERO<-function (V1, V2, V3, V4, V5, V6){
  #zeroes variables
  V1 <- 0
  V2 <- 0
  V3 <- 0
  V4 <- 0
  V5 <- 0
  V6 <- 0
  return(list(V1, V2, V3, V4, V5, V6))
}

#***************************************************************************
ZEROA <-function(N,A1,A2,A3,A4){
  #zeroes arrays
  #for( i in 1:N){
  A1<-rep(0,ML)
  A2<-rep(0,ML)
  A3<-rep(0,ML)
  A4<-rep(0,ML)
  #}
  return(list(A1,A2,A3,A4))
}

#Code by R.Kronenberg [06112017]
#
#
#
##****************************************************************************
FDPSIDWF<-function(i){
  #d PSI / d WETNES, used in 2nd approximation to iteration timestep
  #input
  #  WETNES     wetness, fraction of saturation
  #  PSIF       matrix potential at field capacity, kPa
  #  BEXP       exponent for psi-theta relation
  #  WETINF     wetness at dry end of near-saturation range
  #  WETF       saturation fraction at field capacity
  #  CHM        Clapp and Hornberger m, kPa
  #  CHN        Clapp and Hornberger n
  #output
  #  FDPSIDW    d PSI / d WETNES, kPa
  #
  if (WETNES[i] < WETINF[i]){
    FDPSIDW <- (-BEXP[i] * PSIF[i] / WETF[i]) * (WETNES[i] / WETF[i]) ^ (-BEXP[i] - 1)
  }else if (WETNES[i] < 1){
    #  in near-saturated range
    FDPSIDW <- CHM[i] * (2 * WETNES[i] - CHN[i] - 1)
  }else{
    #  saturated
    FDPSIDW <- 0
  }
  return(FDPSIDW)
}

#****************************************************************************
FPSIMF<-function(WETNESi, PSIFi, BEXPi, WETINFi, WETFi, CHMi, CHNi){
  #matric potential from wetness
  #input
  #  WETNES     wetness, fraction of saturation
  #  PSIF       matrix potential at field capacity, kPa
  #  BEXP       exponent for psi-theta relation
  #  WETINF     wetness at dry end of near-saturation range
  #  WETF       saturation fraction at field capacity
  #  CHM        Clapp and Hornberger m, kPa
  #  CHN        Clapp and Hornberger n
  #output
  #  FPSIM     matric potential, kPa
  #
  if (WETNESi <= 0){
    #  arbitrary very negative value
    FPSIM <- -10000000000#
  }else if (WETNESi < WETINFi) {
    FPSIM <- PSIFi * (WETNESi / WETFi) ^ (-BEXPi)
  }else if (WETNESi < 1) {
    #  in near-saturated range
    FPSIM <- CHMi* (WETNESi - CHNi) * (WETNESi - 1)
  }else{
    #  saturated
    FPSIM <- 0
  }
  return(FPSIM)
}

#******************************************************************************
SOILPAR<-function(){
  #calculated soil water parameters and initial variables
  #input
  #  NLAYER%  #number of soil layers
  #  THICK() layer thicknesses, mm"
  #  THSAT() theta at saturation, matrix porosity
  #  STONEF()stone volume fraction, unitless"
  #  THETAF()volumetric water content at field capacity"
  #  PSIF()  matric potential at field capacity, kPa
  #  BEXP()  exponent for psi-theta relation
  #  WETINF()wetness at dry end of near-saturation range
  #  PSIM()  matric soil water potential for layer, kPa
  #  KF()    hydraulic conductivity at field capacity, mm/d
  #  PSICR   minimum plant leaf water potential, MPa
  #output
  #  PSIG()  gravity potential, kPa
  #  SWATMX()maximum water storage for layer, mm
  #  WETF()  wetness at field capacity, dimensionless
  #  WETC()  wetness at PSICR, dimensionless
  #  CHM()   Clapp and Hornberger m, kPa
  #  CHN()   Clapp and Hornberger n
  #  WETNES()wetness, fraction of saturation
  #  SWATI() water volume in layer, mm
  #  KSAT()  saturated hydraulic conductivity, mm/d
  #local
  #Dim iii%      #soil layer
  PSIINF<-rep(1,50)
  #          potential at dry end of near saturation range, kPa
  #constant
  #  RHOWG   density of water times acceleration of gravity, kPa/mm
  #
  wetff<-WETF
  psigg<-PSIG
  thickk<-THICK
  SWATMx<-SWATMX
  PSIINf<-  PSIINF
  CHm<-CHM
  CHn<-CHN
  WETNEs<-WETNES
  SWATi<-SWATI
  KSAt<-KSAT
  WETc<-WETC

  for(iii in 1:NLAYER){
    #  gravity potential is negative down from surface
    if (iii == 1) {
      psigg[1] <- -RHOWG * thickk[1] / 2
    }else{
      psigg[iii] <- psigg[iii - 1] - RHOWG * ((thickk[iii - 1] + thickk[iii]) / 2)
    }
    SWATMx[iii] <- thickk[iii] * THSAT[iii] * (1 - STONEF[iii])
    wetff[iii] <- THETAF[iii] / THSAT[iii]
    PSIINf[iii] <- PSIF[iii] * (WETINF[iii] / wetff[iii]) ^ -BEXP[iii]
    CHm[iii] <- (-PSIINf[iii] / (1 - WETINF[iii]) ^ 2) - BEXP[iii] * (-PSIINf[iii]) / (WETINF[iii] * (1 - WETINF[iii]))
    CHn[iii] <- 2 * WETINF[iii] - 1 - (-PSIINf[iii] * BEXP[iii] / (CHm[iii] * WETINF[iii]))
    if (PSIM[iii] > 0) {
      # Stop
    } else if (PSIM[iii] == 0) {
      WETNEs[iii] <- 1
    }else{
      WETNEs[iii]<- wetff[iii] * (PSIM[iii] / PSIF[iii]) ^ (-1 / BEXP[iii])
      if (WETNEs[iii] > WETINF[iii]){
        WETNEs[iii] <- (1 + CHn[iii]) / 2 + 0.5 * (CHn[iii] ^ 2 - 2 * CHn[iii] + 1 + 4 * PSIM[iii] / CHm[iii])^(1/2)
      }
    }
    SWATi[iii] <- WETNEs[iii] * SWATMx[iii]
    KSAt[iii] <- KF[iii] * (1 / wetff[iii]) ^ (2 * BEXP[iii] + 3)
    WETc[iii] <- wetff[iii] * (1000 * PSICR / PSIF[iii]) ^ (-1 / BEXP[iii])
  }

  return(list(PSICR, psigg, SWATMx, wetff, WETc, CHm, CHn, WETNEs, SWATi, KSAt))
}

#**************************************************************************
SOILVAR<-function(){
  #soil water variables
  #input
  #  NLAYER% number of soil layers
  #  PSIG()  gravity potential, kPa
  #  PSIM()  matric soil water potential for layer, kPa
  #  WETNES()wetness, fraction of saturation
  #  THSAT() theta at saturation, matrix porosity
  #  KF()    hydraulic conductivity at field capacity, mm/d
  #  BEXP()  exponent for psi-theta relation
  #  WETF()  wetness at field capacity, dimensionless
  #  SWATI() water volume in layer, mm
  PSITi<-PSITI
  THETa<-THETA
  Kk<-KK
  #output
  #  PSITI() total potential, kPa
  #  THETA() water content, mm water / mm soil matrix
  #  SWAT    total soil water in all layers, mm
  #  KK()    hydraulic conductivity, mm/d
  #local
  #Dim iii%     # soil layer

  SWAt <- 0
  for (iii in 1:NLAYER){
    PSITi[iii] <- PSIM[iii] + PSIG[iii]
    THETa[iii] <- WETNES[iii] * THSAT[iii]
    if(WETNES[iii] > 0.0001){
      Kk[iii] <- KF[iii] * (WETNES[iii] / WETF[iii]) ^ (2 * BEXP[iii] + 3)
    }else{  #extremely dry
      Kk[iii] <- 0.0000000001
    }
    SWAt <- SWAt + SWATI[iii]
  }
  return(c(PSITi, THETa, Kk, SWAt))
}


#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden
#   Institut für Hydrologie und Meteorologie
#   Professur für Meteorologie
#   2017
#
#
#
#
#
#

#******************************************************************************
INTER<-function(RFAL, PINT, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DTP, INTR, RINT, IRVP){
  #rain interception, used when NPINT% > 1
  #same routine is used for snow interception, with different calling variables
  #input
  #   RFAL      rainfall rate, mm/d
  #   PINT      potential interception rate, mm/d
  #   LAI       projected leaf area index, m2/m2
  #   SAI       projected stem area index, m2/m2
  #   FRINTL    intercepted fraction of RFAL per unit LAI
  #   FRINTS    intercepted fraction of RFAL per unit SAI
  #   CINTRL    maximum interception storage of rain per unit LAI, mm
  #   CINTRS    maximum interception storage of rain per unit SAI, mm
  #   DTP       precipitation interval time step, d
  #   INTR      intercepted rain, mm
  #output
  #   RINT      rain catch rate, mm/d
  #   IRVP      evaporation rate of intercepted rain, mm/d
  #local
  INTRMX<-0   #maximum canopy storage for rain, mm
  CATCH<-0    #maximum RINT, mm/d
  NEWINT<-0   #first approximation to new canopy storage (INTR)
  #
  CATCH <- (FRINTL * LAI + FRINTS * SAI) * RFAL
  INTRMX <- CINTRL * LAI + CINTRS * SAI
  NEWINT <- INTR + (CATCH - PINT) * DTP

  if(NEWINT > 0){
    #  canopy is wet throughout DTP
    IRVP <- PINT
    if(NEWINT > INTRMX){
      #     canopy capacity is reached
      RINT <- PINT + (INTRMX - INTR) / DTP
      #     RINT can be negative if INTR exists and LAI or SAI is decreasing over time
    }else{
      #     canopy capacity is not reached
      RINT <- CATCH
    }
  }else{
    #  canopy dries during interval or stays dry
    RINT <- CATCH
    IRVP <- (INTR / DTP) + CATCH
    #  IRVP is < PINT

  }
  return(list(RINT,IRVP));
}

#**************************************************************************
INTER24<-function(RFAL, PINT, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DURATN, INTR, RINT, IRVP, MONTHN){
  #rain interception with duration in hours, used when NPINT% = 1
  #same routine is used for snow interception, with different calling variables
  #input
  #   RFAL      24-hour average rainfall rate, mm/d
  #   PINT      potential interception rate, mm/d
  #   LAI       projected leaf area index, m2/m2
  #   SAI       projected stem area index, m2/m2
  #   FRINTL    intercepted fraction of RFAL per unit LAI
  #   FRINTS    intercepted fraction of RFAL per unit SAI
  #   CINTRL    maximum interception storage of rain per unit LAI, mm
  #   CINTRS    maximum interception storage of rain per unit SAI, mm
  #   DURATN    average storm duration, hr
  #   INTR      intercepted rain storage, mm,
  #   MONTHN    Month of the year
  #output
  #   RINT      rain catch rate, mm/d
  #   IRVP      evaporation rate of intercepted rain, mm/d
  #local
  INTRMX<-0    #maximum canopy storage for rain, mm
  INTRNU<-0    #canopy storage at end of hour, mm
  NEWINT<-0    #first approximation to INTRNU, mm
  RINTHR<-0    #rain catch rate for hour, mm/hr
  CATCH<-0     #maximum RINTHR, mm/hr
  IRVPHR<-0    #evaporation rate for hour, mm/hr
  SMINT<-0     #daily accumulated actual catch, mm
  SMVP<-0      #daily accumulated actual evaporation, mm
  IHD<-0      #half DURATN in truncated integer hours
  #hh<<-0        #hour, 0 to 23
  DTH<-0       #time step, = 1 hr
  #intrinsic
  #   CSNG, INT
  #
  IHD <- as.integer((DURATN[MONTHN] + 0.1) / 2)
  INTRMX <- CINTRL * LAI + CINTRS * SAI
  INTRNU <- INTR
  SMINT <- 0
  SMVP <- 0
  DTH <- 1
  for(i in seq(0,23,1)){
    if((i < (12 - IHD)) || (i >= (12 + IHD))){
      #     before or after rain
      CATCH <- 0
    }else{
      #     during rain, mm/hr is rate in mm/d divided by hr of rain/d
      CATCH <- (FRINTL * LAI + FRINTS * SAI) * RFAL / (2 * IHD)
    }
    NEWINT <- INTRNU + (CATCH - PINT / 24) * DTH
    if (NEWINT > 0.0001) {
      #     canopy is wet throughout hour, evap rate is PINT
      IRVPHR <- PINT / 24
      if (NEWINT > INTRMX) {
        #        canopy capacity is reached
        RINTHR <- IRVPHR + (INTRMX - INTRNU) / DTH
        #        INTRMX - INTRNU can be negative if LAI or SAI is decreasing over time
      }else{
        #        canopy capacity is not reached
        RINTHR <- CATCH
      }
    }else{
      #     canopy dries during hour or stays dry
      RINTHR <- CATCH
      IRVPHR <- INTRNU / DTH + CATCH
      #     IRVPHR for hour is < PI/24
    }
    INTRNU <- INTRNU + (RINTHR - IRVPHR) * DTH
    SMVP <- SMVP + IRVPHR * DTH
    SMINT <- SMINT + RINTHR * DTH
  }
  IRVP <- SMVP
  #           / 1 d
  RINT <- SMINT
  #            / 1 d
  return(list(RINT,IRVP))
}

#******************************************************************************
PLNTRES<-function(NLAYER, THICK, STONEF, RTLEN, RELDEN, RTRAD, RPLANT, FXYLEM, RXYLEM, RROOTI, ALPHA){
  #allocates total plant resistance to xylem and root layers
  #input
  #DIM NLAYER AS INTEGER # number of soil layers (max 50)
  #   THICK() layer thicknesses, mm
  #   STONEF()stone volume fraction, unitless
  #   RTLEN   root length per unit land area, m/m2, MXRTLN * RELHT * DENSEF
  #   RELDEN()relative values of root length per unit volume
  #   RTRAD   average root radius, mm
  #   RPLANT  plant resistance to water flow, MPa d/mm, 1/(KPLANT*RELHT*DENSEF)
  #   FXYLEM  fraction of plant resistance in xylem
  #output
  #   RXYLEM  xylem resistance, MPa d/mm, 1E20 if no roots
  #   RROOTI()root resistance for layer, MPa d/mm, 1E20 if no roots
  #   ALPHA() modified Cowan alpha, MPa
  #local
  #Dim I As Integer # layer counter
  Dic<-c(seq(1,50,1))  #stonefree layer thickness
  SUM<-0     #total relative length, mm
  RTFRAC<-0  #fraction of total root length in layer
  RTDENI<-0  #root density for layer, mm/mm3
  DELT<-0    #root cross-sectional area * LI, dimensionless
  RXYLEm<-0
  RROOTi<-rep(0,ML)
  ALPHa<-rep(0,ML)
  #constants
  #   RHOWG, PI
  #intrinsic function
  #   LOG
  #
  #xylem resistance
  RXYLEm <- FXYLEM * RPLANT
  #
  #SUM = 0
  for( i in seq( 1,NLAYER, 1)){
    Dic[i] <- THICK[i] * (1 - STONEF[i])
    SUM <- SUM + RELDEN[i] * Dic[i]
  }
  for( i in seq( 1,NLAYER,1)){
    if ((RELDEN[i] < 0.00001) || (RTLEN < 0.1)){
      #     no roots in layer
      RROOTi[i] <- 1E+20
      ALPHa[i] <- 1E+20
    }else{
      RTFRAC <- RELDEN[i] * Dic[i] / SUM
      #     root resistance for layer
      RROOTi[i] <- (RPLANT - RXYLEm) / RTFRAC
      #     rhizosphere resistance for layer
      RTDENI <- RTFRAC * 0.001 * RTLEN / Dic[i]
      #                       .001 is (mm/mm2)/(m/m2) conversion
      DELT <- PI * RTRAD ^ 2 * RTDENI
      ALPHa[i] <- (1 / (8 * PI * RTDENI)) * (DELT - 3 - 2 * (log(DELT)) / (1 - DELT))
      ALPHa[i] <- ALPHa[i] * 0.001 * RHOWG / Dic[i]
      #                           .001 is MPa/kPa conversion
    }
  }
  return(c(RXYLEm,RROOTi,ALPHa))

}

#****************************************************************************
TBYLAYER<-function(J, PTR, DISPC, ALPHA, KK, RROOTI, RXYLEM, PSITI, NLAYER, PSICR, NOOUTF){
  #  actual transpiration rate by layers
  #  watch MPa - kPa conversions carefully
  #input
  #   J%             1 for daytime, 2 for nighttime
  #   PTR            average potential transpiration rate over time period, mm/d
  #   DISPC          zero-plane displacement for closed canopy, m
  #   ALPHA()        modified Cowan alpha, MPa
  #   KK()           hydraulic conductivity, mm/d
  #   RROOTI()       root resistance for layer, MPa d/mm
  #   RXYLEM         xylem resistance, MPa d/mm
  #   PSITI()        total soil water potential, kPa
  #   NLAYER%        number of soil layers (max 20)
  #   PSICR          critical potential for plant, MPa
  #   NOOUTF%        1 if no outflow allowed from roots, otherwise 0
  #output
  #   ATR            actual transpiration rate over time period, mm/d
  #   ATRANi()       actual transpiration rate from layer over time period, mm/d
  #local
  #Dim i%             #layer counter
  RI<-rep(0,50)    #root plus rhizosphere resistance, MPa d/mm
  RT<-0             #combined root resistance from unflagged layers, MPa d/mm
  SUM<-0            #sum of layer conductances, (mm/d)/MPa
  TRMIN<-0          #largest negative transpiration loss, mm/d
  PSIT<-0           #weighted average total soil water potential for unflagged layers, kPa
  R<-0              #(2/pi)(SUPPLY/PTR)
  SUPPLY<-0         #soil water supply rate, mm/d
  IDEL<-0          #subscript of flagged layer
  FLAG<-rep(0,50) #1 if layer has no transpiration uptake, otherwise 0
  NEGFLAG<-0       #1 if second iteration is needed
  ATr<-0
  ATRANi<-rep(0,ML)
  #intrinsic function
  #   SIN
  #external function needed
  #   ACOS, RMIN
  #constants
  #   RHOWG          density of water times gravity acceleration, MPa/m
  #   PI
  #
  #flag layers with no roots, indicated by RROOTI = 1E20
  #if outflow from roots is prevented, flag layers with PSITI <= PSICR
  for (i in seq( 1,NLAYER,1)){
    if (RROOTI[i] > 1E+15){
      FLAG[i] <- 1
    } else if((NOOUTF == 1) && (PSITI[i] / 1000 <= PSICR)) {
      FLAG[i] <- 1
    } else {
      FLAG[i]<- 0#  thi slayer has roots
    }
  }
  dfw<-0
  #
  #top of loop for recalculation of transpiration if more layers get flagged
  repeat{
    NEGFLAG <- 0
    SUM <- 0
    for(i in 1:NLAYER){
      if (FLAG[i]== 0){
        RI[i] <- RROOTI[i] + ALPHA[i] / KK[i]
        SUM <- SUM + 1 / RI[i]
      }else{
        #        flagged
        ATRANi[i] <- 0
      }
    }
    if (SUM < 1E-20){
      #     all layers flagged, no transpiration
      ATr <- 0
      PSIT <- -10000000000#
      return(list(ATr,ATRANi))
    }else{
      RT <- 1 / SUM
    }
    #  weighted mean soil water potential
    PSIT <- 0
    for (i in 1:NLAYER){
      if (FLAG[i] == 0){
        PSIT <- PSIT + RT * PSITI[i] / RI[i]
      }
    }
    #  soil water supply rate, assumed constant over day
    SUPPLY <- (PSIT / 1000 - PSICR - RHOWG * DISPC) / (RT + RXYLEM)
    #  transpiration rate limited by either PTR or SUPPLY
    if (J == 1){
      #     daytime, PTR is average of a half sine over daytime
      R <- (2 / PI) * (SUPPLY / PTR)
      if (R <= 0){
        ATr <- 0
      }else if (R < 1){
        ATr <- PTR * (1 + R * ACOSF(R) - sin(ACOSF(R)))
      }else{
        ATr <- PTR
      }
    }else{
      #     nighttime, PTR assumed constant over nighttime
      if ((SUPPLY <= 0) || (PTR <= 0)){
        ATr <- 0
      }else {
        ATr <- RMINF(SUPPLY, PTR)
      }
    }
    #  distribute total transpiration rate to layers
    for (i in 1:NLAYER){
      if (FLAG[i] == 1){
        ATRANi[i] <- 0
      }else{
        ATRANi[i] <- ((PSITI[i]- PSIT) / 1000 + RT * ATr) / RI[i]
        #        check for any negative transpiration losses
        if (ATRANi[i] < -0.000001){
          NEGFLAG <- 1
        }
      }
    }
    dfw<-dfw+1
    if (NOOUTF == 1 && NEGFLAG == 1){
      #     find layer with most negative transpiration and omit it
      IDEL <- 0
      TRMIN <- 0
      for(i in 1:NLAYER){
        if (ATRANi[i] < TRMIN) {
          TRMIN <- ATRANi[i]
          IDEL <- i
        }
      }
      FLAG[IDEL] <- 1
      #     repeat main loop with flagged layers excluded
    }else{
      #     done
      return(list(ATr,ATRANi))
    }
    #
  }
}


#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden
#   Institut für Hydrologie und Meteorologie
#   Professur für Meteorologie
#   2017
#
#
#
#
#
#
#
##**************************************************************************
CANOPY<-function(DOY, MAXHT, RELHT, MAXLAI, RELLAI, SNOW, SNODEN, MXRTLN, MXKPL, CS, DENSEF){
  #canopy parameters
  #input
  #   DOY%    day of year (first day of DFILE and run)"
  #   MAXHT   maximum height for the year, m, minimum of 0.01 m
  #   RELHT() ten pairs of DOY% and relative canopy height
  #   MAXLAI  maximum projected leaf area index for the year, m2/m2
  #   RELLAI()ten pairs of DOY% and relative LAI
  #   SNOW    water equivalent of snow on the ground, mm
  #   SNODEN  snow density, mm/mm
  #   MXRTLN  maximum root length per unit land area, m/m2
  #   MXKPL   maximum plant conductivity, (mm/d)/MPa
  #   CS      ratio of projected SAI to canopy height, m-1
  #   DENSEF  density factor
  #output
  #   HEIGHT  canopy height above any snow, m, minimum of 0.01 m
  #   LAI     leaf area index, m2/m2, minimum of 0.00001
  #   SAI     stem area index, m2/m2
  #   RTLEN   root length per unit land area, m/m2
  #   RPLANT  plant resistivity to water flow, MPa d/mm
  #local
  SNODEP<-0  #snow depth
  HNOSNO<-0  #height of canopy without snow
  HSNO<-0    #height of canopy above snow
  RATIO<-0   #fraction of canopy above snow
  RELHIT<-0  #RELHT for DOY%
  KPL<-0     #plant conductivity, mm d-1 MPa-1
  #intrinsic
  #   CSNG
  #external functions needed
  #   INTERP, RMAX
  #
  RELHIT <- INTERP(10, RELHT, DOY)
  SNODEP <- 0.001 * SNOW / SNODEN
  HNOSNO <- RMAXF(0.01, RELHIT * MAXHT)
  HSNO <- RMAXF(0, HNOSNO - SNODEP)
  RATIO <- HSNO / HNOSNO
  HEIGHT <- RMAXF(0.01, HSNO)
  #
  LAI <- RATIO * DENSEF * INTERP(10, RELLAI, DOY) * MAXLAI
  SAI <- DENSEF * CS * HEIGHT
  if(LAI < 0.00001)  LAI <- 0.00001
  #
  RTLEN <- DENSEF * RELHIT * MXRTLN
  KPL <- DENSEF * RELHIT * MXKPL
  if (KPL < 0.00000001)  KPL <- 0.00000001
  RPLANT <- 1 / KPL
  #
  return(c(HEIGHT, LAI, SAI, RTLEN, RPLANT))
}

#***************************************************************************
ESAT<-function(TA, ES, DELTA){
  #calculates saturated vp and DELTA from temp
  #from Murray J Applied Meteorol 6:203
  #input
  #   TA      air temperature, degC
  #output
  #   ES      saturated vapor pressure at TA, kPa
  #   DELTA   dES/dTA at TA, kPa/K
  #intrinsic
  #   EXP
  Es <- 0.61078 * exp(17.26939 * TA / (TA + 237.3))
  DELTa <- 4098 * Es / (TA + 237.3) ^ 2
  if (TA < 0) {
    Es <- 0.61078 * exp(21.87456 * TA / (TA + 265.5))
    DELTa <- 5808 * Es / (TA + 265.5) ^ 2
  }
  return(c(Es, DELTa))
}

#***************************************************************************
FRSS<-function(RSSA, RSSB, PSIF, PSIM){
  #soil surface resistance to evaporation
  #input
  #  RSSA      soil evaporation resistance at field capacity, s/m
  #  RSSB      exponent in relation of soil evap res to water potential
  #  PSIF      water potential at field capacity, kPa
  #  PSIM      water potential of evaporating layer. kPa
  #output
  #  FRSS      Shuttleworth-Wallace soil surface resistance, s/m
  #
  if (RSSA < 0.0001) {
    FRSs <- 10000000000#
    # forces SLVP=0
  }else{
    FRSs <- RSSA * (PSIM / PSIF) ^ RSSB
  }
  return(FRSs)
}

#****************************************************************************
INTERP<-function(NPAIRS, FUNCT, XVALUE){
  #interpolates between points in data functions
  #input
  #  NPAIRS%         number of pairs of values to be used
  #  FUNCT()         array of pairs of values: x1, y1, x2, y2, ...
  #  XVALUE          x value
  #output
  #  INTERP          y value
  #local
  #Dim I%, J%          #DO indexes
  XX<-c(seq(1,10,1))   #series of x values of FUNCT
  YY<-c(seq(1,10,1))   #series of y values of FUNCT
  #
  #put FUNCT into XX and YY
  i <- 0
  for (J in seq(1,(2 * NPAIRS - 1),2)){
    i <- i + 1
    XX[i] <- FUNCT[J]
    YY[i] <- FUNCT[J + 1]
  }
  #interpolate using XX and YY
  for (J in 1:NPAIRS){
    if (XVALUE == XX[J]){
      INTERp <- YY[J]
      return(INTERp)
    }else if (XVALUE < XX[J]){
      INTERp <- YY[J - 1] + (XVALUE - XX[J - 1]) * (YY[J] - YY[J - 1]) / (XX[J] - XX[J - 1])
      return(INTERp)
    }else{}
  }
  return(INTERp)
}

#***************************************************************************
PM<-function(AA, VPD, DELTA, RA, RC){
  #Penman-Monteith transpiration rate equation
  #input
  #   AA      net energy input, Rn - S, W/m2
  #   VPD      vapor pressure deficit, kPa
  #   DELTA   dEsat/dTair, kPa/K
  #   RA      boundary layer resistance, s/m
  #   RC      canopy resistance, s/m
  #output
  #   PM      Penman-Monteith latent heat flux density, W/m2
  #
  Pm <- (RA * DELTA * AA + CPRHO * VPD) / ((DELTA + GAMMA) * RA + GAMMA * RC)
  return(Pm)
}

#*************************************************************************
ROUGH<-function(HEIGHT, ZMINH, LAI, SAI, CZS, CZR, HS, HR, LPC, CS, Z0GS){
  #closed canopy parameters
  #input
  #   HEIGHT  canopy height, m, minimum of 0.01 m
  #   ZMINH   ZA minus HEIGHT, reference height above canopy top, m
  #   LAI     leaf area index, m2/m2, minimum of 0.00001
  #   SAI     stem area index, m2/m2
  #   CZS     ratio of roughness to height for smooth closed canopies
  #   CZR     ratio of roughness to height for rough closed canopies
  #   HS      height below which CZS applies, m
  #   HR      height above which CZR applies, m
  #   LPC     minimum LAI defining a closed canopy
  #   CS      ratio of projected SAI to canopy height, m-1
  #input and output
  #   Z0GS     roughness parameter of soil surface, m
  #output
  #   Z0C     roughness length for closed canopy, m
  #   DISPC   zero-plane displacement for closed canopy, m
  #   Z0      roughness parameter, m
  #   DISP    zero-plane displacement, m
  #   ZA      reference height for TA, EA, UA, above ground, m
  #local
  RATIO<-0   #(LAI + SAI) / (LAI + SAI for closed canopy)
  XX<-0
  #intrinsic
  #   LOG, EXP
  #
  if (HEIGHT >= HR) {
    Z0C <- CZR * HEIGHT
  }else if (HEIGHT <= HS){
    Z0C <- CZS * HEIGHT
  }else{
    Z0C <- CZS * HS + (CZR * HR - CZS * HS) * (HEIGHT - HS) / (HR - HS)
  }
  DISPC <- HEIGHT - Z0C / 0.3
  if (Z0GS > Z0C)  Z0GS <- Z0C
  RATIO <- (LAI + SAI) / (LPC + CS * HEIGHT)
  if (RATIO >= 1) {
    #  closed canopy
    Z0 <- Z0C
    DISP <- DISPC
  }else{
    #  sparse canopy modified from Shuttleworth and Gurney (1990)
    XX <- RATIO * (-1 + exp(0.909 - 3.03 * Z0C / HEIGHT)) ^ 4
    DISP <- 1.1 * HEIGHT * log(1 + XX ^ 0.25)
    Z0 <- RMINF(0.3 * (HEIGHT - DISP), Z0GS + 0.3 * HEIGHT * XX ^ 0.5)
  }
  ZA <- HEIGHT + ZMINH
  #
  return(list(Z0GS, Z0C, DISPC, Z0, DISP, ZA))
}

#****************************************************************************
SRSC<-function(RAD, TA, VPD, LAI, SAI, GLMIN, GLMAX, R5, CVPD, RM, CR, TL, T1, T2, TH){
  #canopy surface resistance, RSC, after Shuttleworth and Gurney (1990) and
  #   Stewart (1988)
  #input
  #   RAD     solar radiation on canopy, W/m2
  #   TA      mean  temperature for the day at reference height, degC
  #   VPD     vapor pressure deficit, kPa
  #   LAI     projected leaf area index
  #   SAI     projected stem area index
  #   GLMIN   minimum leaf conductance, closed stomates, all sides, s/m
  #   GLMAX   maximum leaf conductance, open stomates, all sides, s/m
  #   R5      solar radiation at which conductance is halved, W/m2
  #   CVPD    vpd at which leaf conductance is halved, kPa
  #   RM      maximum solar radiation, at which FR = 1, W/m2
  #   CR      light extinction coefficient for LAI, projected area
  #   TL      temperature below which stomates are closed, degC
  #   T1      lowest temp. at which stomates not temp. limited, degC
  #   T2      highest temp. at which stomates not temp. limited,degC
  #   TH      temperature above which stomates are closed, degC
  #output
  #   RSC     canopy surface resistance, s/m
  #local
  FS<-0     # correction for stem area
  R0 <-0    # a light response parameter
  FRINT<-0  # integral of fR dL over Lp
  FD <-0    # dependence of leaf conductance on vpd, 0 to 1
  FT <-0    # dependence of leaf conductance on temperature, 0 to 1
  GSC <-0   # canopy conductance, m/s
  #intrinsic
  #   LOG, EXP
  #solar radiation limitation integrated down through canopy
  #Stewart (1988) and Saugier and Katerji (1991)
  FS <- (2 * LAI + SAI) / (2 * LAI)
  if (RAD <= 0.0000000001){
    FRINT <- 0
  }else{
    R0 <- RM * R5 / (RM - 2 * R5)
    FRINT <- ((RM + R0) / (RM * CR * FS)) * log((R0 + CR * RAD) / (R0 + CR * RAD * exp(-CR * FS * LAI)))
  }
  #vapor deficit limitation
  #Lohammar et al. (1980) and Stannard (1993)
  FD <- 1 / (1 + VPD / CVPD)
  #temperature limitation
  if (TA <= TL) {
    FT <- 0
  }else if (TA > TL && TA < T1) {
    FT <- 1 - ((T1 - TA) / (T1 - TL)) ^ 2
  }else if (TA >= T1 && TA <= T2) {
    FT <- 1
  }else if (TA > T2 && TA < TH) {
    FT <- 1 - ((TA - T2) / (TH - T2)) ^ 2
  }else{
    FT <- 0
  }
  GSC <- FD * FT * FRINT * (GLMAX - GLMIN) + LAI * GLMIN
  RSC <- 1 / GSC
  #
  return(RSC)
}

#**************************************************************************
SWGE<-function(AA, ASUBS, VPD, RAA, RAS, RSS, DELTA, ARATE, ERATE){
  #Shuttleworth and Wallace (1985) ground evaporation when transpiration known
  #input
  #   AA      net radiation at canopy top minus ground flux, W/m2
  #   ASUBS   net radiation minus ground flux at ground, W/m2
  #   VPD     vapor pressure deficit, kPa
  #   RAA     boundary layer resistance, s/m
  #   RAS     ground-air resitance, s/m
  #   RSS     ground evaporation resistance, s/m
  #   DELTA   dEsat/dTair, kPa/K
  #   ARATE   actual transpiration rate, mm/d
  #output
  #   ERATE   ground evaporation rate, mm/d
  #local
  RS<-0
  RA<-0 #as in Shuttleworth and Wallace (1985)
  LE<-0   #total latent heat flux density, W/m2
  LEC<-0    #actual transpiration latent heat flux density, W/m2
  #
  LEC <- ARATE / (ETOM * WTOMJ)
  RS <- (DELTA + GAMMA) * RAS + GAMMA * RSS
  RA <- (DELTA + GAMMA) * RAA
  LE <- (RS / (RS + RA)) * LEC + (CPRHO * VPD + DELTA * RAS * ASUBS + DELTA * RAA * AA) / (RS + RA)
  ERATE <- ETOM * WTOMJ * (LE - LEC)
  #
  return(ERATE)
}

#***************************************************************************
SWGRA<-function(UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS){
  #atmospheric resistances RAA, RAC, and RAS
  #from Shuttleworth and Gurney (1990)
  #input
  #   UA      wind speed at reference height, m/s
  #   ZA      reference height, m
  #   HEIGHT  canopy height, m
  #   Z0      roughness parameter, m
  #   DISP    zero-plane displacement, m
  #   Z0C     roughness length for closed canopy, m
  #   DISPC   zero-plane displacement for closed canopy, m
  #   Z0GS    roughness parameter of soil surface, m
  #   LWIDTH  characteristic leaf width, m
  #   RHOTP   ratio of total leaf area to projected leaf area
  #   NN      wind/diffusivity extinction coefficient
  #   LAI     projected leaf area index
  #   SAI     projected stem area index
  #output
  #   RAA     boundary layer resistance, s/m
  #   RAC     leaf-air resistance, s/m
  #   RAS     ground-air resitance, s/m
  #local
  USTAR<-0
  KH<-0
  UH<-0
  RB<-0
  #intrinsic
  #   LOG, EXP
  #
  USTAR <- K * UA / (log((ZA - DISP) / Z0))
  KH <- K * USTAR * (HEIGHT - DISP)
  RAS <- (HEIGHT * exp(NN) / (NN * KH)) * (exp(-NN * Z0GS / HEIGHT) - exp(-NN * (Z0C + DISPC) / HEIGHT))
  if (RAS < 1) RAS <- 1
  RAA <- log((ZA - DISP) / (HEIGHT - DISP)) / (K * USTAR) + (HEIGHT / (NN * KH)) * (-1 + exp(NN * (HEIGHT - DISPC - Z0C) / HEIGHT))

  UH <- (USTAR / K) * log((HEIGHT - DISP) / Z0)
  #the Shuttleworth-Gurney RB equation is strictly for one side of flat leaves
  #when RHOTP > 2, LWIDTH is small (needles) so RAC is small
  #their equation should have NN in numerator, see Choudhury and Monteith(1988)
  RB <- (100 * NN) * (LWIDTH / UH) ^ 0.5 / (1 - exp(-NN / 2))
  RAC <- RB / (RHOTP * LAI + PI * SAI)
  #note LAI is prevented from being less than 1E-5
  return(list(RAA, RAC, RAS))
}

#*************************************************************************
SWPE<-function(AA, ASUBS, VPD, RAA, RAC, RAS, RSC, RSS, DELTA){
  #Shuttleworth and Wallace (1985) transpiration and ground evaporation
  #input
  #   AA      net radiation at canopy top minus ground flux, W/m2
  #   ASUBS   net radiation minus ground flux at ground, W/m2
  #   VPD     vapor pressure deficit, kPa
  #   RAA     boundary layer resistance, s/m
  #   RAC     leaf-air resistance, s/m
  #   RAS     ground-air resistance, s/m
  #   RSC     canopy surface resistance, s/m
  #   RSS     ground evaporation resistance, s/m
  #   DELTA   dEsat/dTair, kPa/K
  #output
  #   PRATE   potential transpiration rate, mm/d
  #   ERATE   ground evaporation rate, mm/d
  #local
  RS<-0
  RC<-0
  RA<-0
  PMS<-0
  PMC<-0
  D0<-0 #as in Shuttleworth and Wallace (1985)
  CCS<-0
  CCC<-0 #as CC and CS in Shuttleworth and Wallace (1985)
  LE<-0      #total latent heat flux density, W/m2
  #external function needed
  #   PM
  #
  RS <- (DELTA + GAMMA) * RAS + GAMMA * RSS
  RC <- (DELTA + GAMMA) * RAC + GAMMA * RSC
  RA <- (DELTA + GAMMA) * RAA
  CCS <- 1 / (1 + RS * RA / (RC * (RS + RA)))
  CCC <- 1 / (1 + RC * RA / (RS * (RC + RA)))
  PMS <- PM(AA, VPD - DELTA * RAS * (AA - ASUBS) / CPRHO, DELTA, RAA + RAS, RSS)
  PMC <- PM(AA, VPD - DELTA * RAC * ASUBS / CPRHO, DELTA, RAA + RAC, RSC)
  LE <- (CCC * PMC + CCS * PMS)
  D0 <- VPD + RAA * (DELTA * AA - (DELTA + GAMMA) * LE) / CPRHO
  PRATE <- ETOM * WTOMJ * PM(AA - ASUBS, D0, DELTA, RAC, RSC)
  ERATE <- ETOM * WTOMJ * PM(ASUBS, D0, DELTA, RAS, RSS)
  return(list(PRATE, ERATE))
}
#****************************************************************************
WEATHER<-function(TMAX, TMIN, DAYLEN, I0HDAY, EA, UW, ZA, DISP, Z0, WNDRAT, FETCH, Z0W, ZW, SOLRAD, SOLRADC, TA, TADTM, TANTM, UA, UADTM, UANTM){
  #input
  #   TMAX       maximum temperature for the day, øC
  #   TMIN       minimum temperature for the day, øC
  #   DAYLEN     daylength in fraction of day
  #   I0HDAY     potential insolation on horizontal, MJ m-2 d-1
  #   EA         vapor pressure for the day, kPa
  #   UW         average wind speed for day at weather station, m/s
  #   ZA         reference height for TA, EA, UA, above ground, m
  #   DISP       zero-plane displacement, m
  #   Z0         roughness parameter, m
  #   WNDRAT     ratio of nighttime to daytime wind speed
  #   FETCH      weather station fetch, m
  #   Z0W        weather station roughness parameter, m
  #   ZW         weather station measurement height for wind, m
  #   SOLRAD     solar radiation for the day, horizontal surface, MJ/m2
  #output
  #   SOLRADC    corrected solar radiation for the day, horizontal surface, MJ/m2
  #   TA         mean temperature for the day, øC
  #   TADTM      average daytime temperature, øC
  #   TANTM      average nighttime temperature, øC
  #   UA         average wind speed for the day at reference height, m/s
  #   UADTM      average wind speed for daytime at ZA, m/s
  #   UANTM      average wind speed for nighttime at ZA, m/s
  #local
  dummy<-0
  #intrinsic
  #  SIN
  #external function needed
  #  WNDADJ
  #
  #estimate SOLRAD if missing or limit if too high
  if (SOLRAD < 0.001) {
    SOLRADC <<- RRD * I0HDAY
  }else if (SOLRAD > I0HDAY) {
    SOLRADC <<- 0.99 * I0HDAY
  }else{
    SOLRADC <<- SOLRAD
  }
  #average temperature for day
  TA <<- (TMAX + TMIN) / 2
  #daytime and nighttime average air temperature
  TADTM <<- TA + ((TMAX - TMIN) / (2 * PI * DAYLEN)) * sin(PI * DAYLEN)
  TANTM <<- TA - ((TMAX - TMIN) / (2 * PI * (1 - DAYLEN))) * sin(PI * DAYLEN)
  #if no vapor pressure data, use saturated vapor pressure at minimum temp.
  if (EA == 0) {esat<-ESAT(TMIN, EA, dummy)
  EA<<-unlist(esat[1])
  }
  #if no wind data, use value from frmmainb90 - Measured wind of zero must be input as 0.1, a problem
  if (UW == 0) assign("UW", .1) #UW <<-0.1
  #if wind < 0.2 m/s, set to 0.2 to prevent zero divide
  if (UW < 0.2) assign("UW", .2) #UW <<- 0.2
  #adjust wind speed from weather station to ZA
  if (Z0W < 0.000001) {
    assign("UA", UW) #UA <<- UW
  }else{
    assign("UA", UW * WNDADJ(ZA, DISP, Z0, FETCH, ZW, Z0W)) #UA <<- UW * WNDADJ(ZA, DISP, Z0, FETCH, ZW, Z0W)
  }
  #daytime and nighttime average wind speed
  UADTM <<- UA / (DAYLEN + (1 - DAYLEN) * WNDRAT)
  UANTM <<- WNDRAT * UADTM
  #
  return(list(SOLRADC, TA, TADTM, TANTM, UA, UADTM, UANTM))
}

#**************************************************************************
WNDADJ<-function(ZA, DISP, Z0, FETCH, ZW, Z0W){
  #ratio of wind speed at reference height (above canopy) to
  #   wind speed at weather station
  #input
  #  ZA      reference height, m
  #  DISP    height of zero-plane, m
  #  Z0      roughness parameter, m
  #  FETCH   weather station fetch, m
  #  ZW      weather station measurement height for wind,above any zero plane, m
  #  Z0W     weather station roughness parameter, m
  #output
  #  WNDADJ  ratio
  #local
  HIBL<-0    #height of internal boundary layer, m
  #intrinsic
  #  LOG
  #
  #Brutsaert (1982) equation 7-39
  HIBL <- 0.334 * FETCH ^ 0.875 * Z0W ^ 0.125
  #Brutsaert equations 7-41 and 4-3
  WNDADj <- log(HIBL / Z0W) * log((ZA - DISP) / Z0) / (log(HIBL / Z0) * log(ZW / Z0W))
  #
  return(WNDADj)
}


#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden
#   Institut für Hydrologie und Meteorologie
#   Professur für Meteorologie
#   2017
#
#
#
#
#
#

#***************************************************************************
AVAILEN<-function (SLRAD, ALBEDO, C1, C2, C3, TA, EA, RATIO, SHEAT, CR, LAI, SAI){
  #available energy at canopy and ground
  #longwave equations and parameters from Brutsaert (1982)
  #net radiation extinction from Shuttleworth and Wallace(1985)
  #input
  #   SLRAD   solar radiation on slope, W/m2
  #   ALBEDO  albedo
  #   C1      intercept of relation of solar radiation to sunshine duration
  #   C2      slope of relation of solar radiation to sunshine duration
  #   C3      longwave correction factor for overcast sky
  #   TA      air temperature, degC
  #   RATIO   ratio of solar radiation on horizontal to potential insolation for day
  #   EA      vapor pressure, kPa
  #   SHEAT   average soil heat flux for the day, W/m2, usually 0
  #   CR      light extinction coefficient for projected LAI + SAI"
  #   LAI     leaf area index, m2/m2
  #   SAI     stem area index, m2/m2
  #output
  #   AA      available energy, W/m2
  #   ASUBS   availble energy at ground, W/m2
  #local
  SOLNET <-0  #net solar radiation, W/m2
  EFFEM<-0   #effective emissivity from clear sky
  NOVERN<-0  #sunshine duration fraction of daylength
  CLDCOR<-0  #cloud cover correction to net longwave under clear sky
  LNGNET<-0   #net longwave radiation, W/m2
  RN  <-0    # net radiation, W/m2
  #intrinsic
  #   EXP
  #constant
  #   SIGMA
  #
  SOLNET <- (1 - ALBEDO) * SLRAD
  #Brutsaert equation for effective clear sky emissivity
  EFFEM <- 1.24 * (EA * 10 / (TA + 273.15)) ^ (1 / 7)
  NOVERN <- (RATIO - C1) / C2
  if (NOVERN > 1)  NOVERN <- 1
  if (NOVERN < 0)  NOVERN <- 0
  CLDCOR <- C3 + (1 - C3) * NOVERN
  #emissivity of the surface taken as 1.0 to also account for reflected
  LNGNET <- (EFFEM - 1) * CLDCOR * SIGMA * (TA + 273.15) ^ 4
  RN <- SOLNET + LNGNET
  AA <- RN - SHEAT
  ASUBS <- RN * exp(-CR * (LAI + SAI)) - SHEAT

  #if(AA>0) points(IDAY,AA,col="black")

  return(list(RN, AA, ASUBS))
}

#*************************************************************************
EQUIVSLP<-function (LAT, SLOPE, ASPECT){
  #latitude and time shift of "equivalent slope", the point on globe where a
  #  horizontal surface is parallel to the slope
  #needed only once for each non-horizontal slope
  #Swift#s L1 and L2, Lee (3.31, 3.32)
  #inputs
  #  LAT     latitude, radians (S neg)
  #  SLOPE   slope, radians
  #  ASPECT  aspect, radians from N thru E
  #outputs
  #  L1      latitude of equivalent slope, radians
  #  L2      time shift of equivalent slope, radians
  #local
  D1<-0
  #external function needed
  #  ASIN
  #intrinsic
  #  SIN, COS, ATN
  #
  L1 <- ASINF(cos(SLOPE) * sin(LAT) + sin(SLOPE) * cos(LAT) * cos(ASPECT))
  D1 <- cos(SLOPE) * cos(LAT) - sin(SLOPE) * sin(LAT) * cos(ASPECT)
  if (D1 == 0)  D1 <- .0000000001
  L2 <- atan(sin(SLOPE) * sin(ASPECT) / D1)
  if (D1 < 0) L2 <- L2 + PI
  return(list(L1, L2))
}

#*****************************************************************************
FUNC3<-function(DEC, L2, L1, T3, T2){
  #daily integration for slope after Swift (1976), d
  #input
  #  DEC     declination of the sun, radians
  #  L2      time shift of equivalent slope, radians
  #  L1      latitude of equivalent slope, radians
  #  T3      hour angle of sunset on slope
  #  T2      hour angle of sunrise on slope
  #intrinsic
  #  SIN, COS
  FUnC3 <- (1 / (2 * 3.14159)) * (sin(DEC) * sin(L1) * (T3 - T2) + cos(DEC) * cos(L1) * (sin(T3 + L2) - sin(T2 + L2)))
  return(FUnC3)
}

#******************************************************************************
HAFDAY<-function (LAT, DEC){
  #half day length in radians
  #inputs
  #  LAT      latitude, radians (S neg)
  #  DEC      declination of the sun, radians
  #output
  #  HAFDAY   half daylength, radians
  #local
  ARG<-0
  #external function needed
  #  ACOS
  #intrinsic
  #  ABS, SGN, TAN
  #
  if (abs(LAT) >= PI / 2)  LAT <- sign(LAT) * (PI / 2 - 0.01)
  ARG <- -tan(DEC) * tan(LAT)
  if (ARG >= 1) {
    #  sun stays below horizon
    HAFDAy <- 0
  }else if (ARG <= -1) {
    #  sun stays above horizon
    HAFDAy <- PI
  }else{
    HAFDAy <- ACOSF(ARG)
  }
  return(HAFDAy)
}
#******************************************************************************
SUNDS<-function (LAT, SLOPE, DOY, L1, L2, DAYLEN, I0HDAY, SLFDAY){
  #daylength, potential daily solar radiation on horizontal,
  #  and ratio of potential on slope (map area) to horizontal
  #from Swift (1976)
  #input
  #  LAT     latitude, radians
  #  SLOPE   slope, radians
  #  DOY%    day of the year
  #  L1      latitude of equivalent slope, radians, from EQUIVSLP
  #  L2      time shift of equivalent slope, radians, from EQUIVSLP
  #outputs
  #  DAYLEN  daylength (sun above horizontal) in fraction of day, d
  #  I0HDAY  potential insolation on horizontal surface, MJ/m2
  #  SLFDAY  ratio of potential insolation on slope to horizontal, map area
  #local
  I0SDAY <-0 #potential insolation on slope, map area basis, MJ/m2
  SCD <-0   # solar constant for day, W/m2
  DEC <-0   # declination of the sun, radians
  TWORIS <-0 # if two sunrises on slope
  Temp  <-0     #temporary variable
  T0 <-0     #hour angle of sunrise on horizontal, radians
  T1 <-0     #hour angle of sunset on horizontal
  T2 <-0    # hour angle of sunrise on slope
  T3 <-0    # hour angle of sunset on slope
  T6 <-0     #hour angle of sunrise on equivalent slope
  T7 <-0     #hour angle of sunset on equivalent slope
  T8 <-0    # hour angle of second sunrise on slope
  T9 <-0     #hour angle of second sunset on slope
  #constants
  #  WTOMJ   (MJ m-2 d-1) / (W/m2)
  #  PI
  #  SC       solar constant, W/m2
  #external functions needed
  #  HAFDAY, FUNC3, RMIN, RMAX, ASIN
  #intrinsic
  #  COS, SIN
  #
  SCD <- SC / (1 - .0167 * cos(.0172 * (DOY - 3))) ^ 2
  DEC <- ASINF(.39785 * sin(4.868961 + .017203 * DOY + .033446 * sin(6.224111 + .017202 * DOY)))
  Temp <- HAFDAY(LAT, DEC)
  DAYLEN <- RMAXF(.0001, RMINF(.9999, Temp / PI))
  #  to avoid zero divides for 0 and 1
  T1 <- Temp
  T0 <- -Temp
  Temp <- HAFDAY(L1, DEC)
  T7 <- Temp - L2
  T6 <- -Temp - L2
  T3 <- RMINF(T1, T7)
  T2 <- RMAXF(T0, T6)
  if (T3 < T2) {
    T2 <- 0
    T3 <- 0
  }
  T6 <- T6 + 2 * PI
  if (T6 < T1) {
    T8 <- T6
    T9 <- T1
    TWORIS <- 1
  }
  T7 <- T7 - 2 * PI
  if (T7 > T0) {
    T8 <- T0
    T9 <- T7
    TWORIS <- 1
  }else{
    TWORIS <- 0
  }
  if (TWORIS == 1) {   # two sunrises
    I0SDAY <- WTOMJ * SCD * (FUNC3(DEC, L2, L1, T3, T2) + FUNC3(DEC, L2, L1, T9, T8)) / cos(SLOPE)
    #  "daylength" on the slope = ((T3 - T2) + (T9 - T8)) / (2. * PI)
  }else{    #  one sunrise
    I0SDAY <- WTOMJ * SCD * FUNC3(DEC, L2, L1, T3, T2) / cos(SLOPE)
    #  COS(SLOPE) adjusts from slope area to map area
    #  "daylength" on the slope = (T3 - T2) / (2. * PI)
  }
  I0HDAY <- WTOMJ * SCD * FUNC3(DEC, 0, LAT, T1, T0)
  if (I0HDAY <= 0){
    SLFDAY <- 0
  }else{
    SLFDAY <- I0SDAY / I0HDAY
  }

  return(list(DAYLEN, I0HDAY, SLFDAY))
}


#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden
#   Institut für Hydrologie und Meteorologie
#   Professur für Meteorologie
#   2017
#
#
#
#
#
#
#
#************************************************************************
BYFLFR<-function(){
  #bypass flow fraction of infiltration to layer
  #input
  #  BYPAR%    1 to allow BYFL, or 0 to prevent BYFL
  #  NLAYER%   number of soil layers to be used in model, <= ML%
  #  WETNES()  wetness, fraction of saturation
  #  WETF()    wetness at field capacity, dimensionless
  #  QFFC      BYFL fraction at field capacity
  #  QFPAR     quick flow parameter
  #output
  #  BYFRAC()  fraction of layer infiltration to bypass flow
  #
  for(i in 1:NLAYER){
    if (BYPAR == 1) {
      if (QFPAR > 0.01){
        BYFRAC[i] <- QFFC ^ (1 - (1 / QFPAR) * (WETNES[i] - WETF[i]) / (1 - WETF[i]))
        if (BYFRAC[i] > 1)  BYFRAC[i] <- 1
      }else{ # bucket for the layer
        if(WETNES[i] >= WETF[i]){
          BYFRAC[i] <- 1
        }else{
          BYFRAC[i] <- 0
        }
      }
    }else{
      BYFRAC[i] <- 0
    }
  }



  #
  return(BYFRAC)
}

#************************************************************************
DSLOP<-function(i){
  #downslope flow rate from layer
  #input
  #  DSLOPE     slope for soil water flow, radians
  #             no DSFLI if DSLOPE = 0
  #  LENGTH     slope length (for DSFLI), m
  #  THICK[]      layer thicknesses, mm
  #  STONEF[]     stone volume fraction, unitless
  #  PSIM[]       matric soil water potential, kPa
  #  RHOWG      density of water times acceleration of gravity, kPa/mm
  #  KK[]         hydraulic conductivity, mm/d
  #output
  #  DSFLI[]      downslope flow rate from layer, mm/d
  #local
  LL<-0        # LENGTH in mm
  GRAD <-0      #downslope potential gradient, kPa/mm
  ARATIO  <-0   #outflow area / map area
  #intrinsic
  #  SIN, COS
  #
  LL <- 1000 * LENGTH
  GRAD <- RHOWG * sin(DSLOPE) + (2 * PSIM[i] / LL) * cos(DSLOPE)
  ARATIO <- THICK[i] * (1 - STONEF[i]) * cos(DSLOPE) / LL
  DSFLi <- KK[i] * ARATIO * GRAD / RHOWG
  #no water uptake into dry soil because no free water at outflow face
  if (DSFLi < 0)  DSFLi <- 0
  return(DSFLi)
}

#******************************************************************************
GWATER<-function(GWAT, GSC, GSP, DT, VRFLN){
  #calculates groundwater flow and seepage loss
  #input
  #  GWAT     groundwater storage below soil layers, mm
  #  GSC      discharge from GWAT, fraction per day, d-1
  #  GSP      fraction of discharge to seepage
  #  DT       time step for interval, d
  #  VRFLN    vertical drainage rate from lowest layer, mm/d
  #output
  #  GWFL      streamflow from groundwater discharge, mm/d
  #  SEEP      deep seepage loss from groundwater, mm/d
  #
  if (GSC < 0.00000001){
    #  no groundwater
    SEEP <- GSP * VRFLN
    GWFL <- VRFLN - SEEP
  }else{
    SEEP <- GWAT * GSC * GSP
    GWFL <- GWAT * GSC * (1 - GSP)
    #  prevent negative GWAT
    if (GWAT / DT - (GWFL + SEEP) < 0){
      SEEP <- GSP * GWAT / DT
      GWFL <- (1 - GSP) * GWAT / DT
    }
  }
  return(list(GWFL, SEEP))
}

#***************************************************************************
INFLOW<-function(){
  #inflow and byflow for each layer, and net inflow including E and T withdrawal
  #input
  #  NLAYER%    number of soil layers being used, max of 20
  #  DTI        time step for iteration interval, d
  #  INFRAC()   fraction of infiltration to each layer
  #  BYFRAC()   fraction of layer infiltration to bypass flow
  #  SLFL       input rate to soil surface, mm/d
  #  DSFLI()    downslope flow rate from layer, mm/d
  #  TRANI()    transpiration rate from layer, mm/d
  #  SLVP       evaporation rate from soil, mm/d
  #  SWATMX()   maximum water storage for layer, mm
  #  SWATI()    water volume in layer, mm
  #  VRFLI()    vertical drainage rate from layer, mm/d
  #output
  #  VV()       modified VRFLI, mm/d
  #  BYFLI()    bypass flow rate from layer, mm/d
  #  INFLI()    infiltration rate into layer, mm/d
  #  NTFLI()    net flow rate into layer, mm/d
  #local
  #Dim i%       #index variable for layer number
  INFIL<-0    # water reaching layer, SLFL * INFRAC(i%), mm/d
  MAXIN<-0    # maximum allowed rate of input of water to layer, mm/d
  INFLi<-INFLI
  #
  for(i in seq(NLAYER,1,-1)){
    #  need to do from bottom up
    INFIL <- SLFL * INFRAC[i]
    BYFLI[i] <- BYFRAC[i] * INFIL
    INFLi[i] <- INFIL - BYFLI[i]
    if (i == NLAYER)
    { VV[i] <- VRFLI[i]}

    if (i > 1) {
      MAXIN <- (SWATMX[i] - SWATI[i]) / DTI + VV[i] + DSFLI[i] + TRANI[i]
      if (VRFLI[i - 1] + INFLi[i] > MAXIN) {
        #        adjust to prevent oversaturation
        if (BYFRAC[i] > 0) {
          if (VRFLI[i - 1] < MAXIN) {
            #              reduce INFLI, increase BYFLI
            BYFLI[i] <- BYFLI[i] + INFLi[i] - (MAXIN - VRFLI[i - 1])
            INFLi[i] <- MAXIN - VRFLI[i - 1]
            VV[i-1] <- VRFLI[i-1]
          }else{
            #              shift all INFLI to BYFLI and reduce VRFLI(i% - 1)
            BYFLI[i] <- BYFLI[i] + INFLi[i]
            INFLi[i] <- 0
            VV[i-1] <- MAXIN
          }
        }else{
          #           no bypass flow allowed, reduce VRFLI(i%-1), INFLI(i%) unchanged
          VV[i-1] <- MAXIN - INFLi[i]
        }
      }else{
        #        BYFLI and INFLI unchanged
        VV[i-1] <- VRFLI[i-1]
      }
      NTFLI[i] <- VV[i-1] + INFLi[i] - VV[i] - DSFLI[i] - TRANI[i]
    }else{
      #     i% = 1
      MAXIN <- (SWATMX[1] - SWATI[1]) / DTI + VV[1] + DSFLI[1] + TRANI[1] + SLVP
      if (INFLi[1] > MAXIN) {
        #        increase BYFLI(1) to prevent oversaturation
        BYFLI[1] <- BYFLI[1] + INFLi[1] - MAXIN
        INFLi[1] <- MAXIN
        #        may be negative
      }
      NTFLI[1] <- INFLi[1] - VV[1] - DSFLI[1] - TRANI[1] - SLVP
    }
  }

  INFLI<- INFLi
  return(list(VV, INFLI, BYFLI, NTFLI))
}

#******************************************************************************
INFPAR<-function(INFEXP, IDEPTH, NLAYER, THICK){
  #modified for Version 4, June 2, 1999
  #input
  # INFEXP     infiltration exponent, 0 all to top, 1 uniform with depth
  #                    >1.0=more at bottom than at top
  # IDEPTH     depth over which infiltration is distributed
  # NLAYER%    number of soil layers being used
  # THICK      layer thicknesses, mm
  #output
  # ILAYER%    number of layers over which infiltration is distributed
  # INFRAC()   fraction of infiltration to each layer
  #local
  THICKT<-0   #total thickness of ILAYERs, mm
  THICKA<-rep(0,ML)   #accumulated thickness downward, mm
  #
  if (INFEXP <= 0 || IDEPTH == 0) {
    ILAYER <- 1  # probably not used
    INFRAC[1] <- 1
    for (i in seq(2,NLAYER,1)){
      INFRAC[i] <- 0
    }
  }else{
    #must have at least one layer
    THICKT <- THICK[1]
    ILAYER <- 1
    for (i in 2:NLAYER){
      if (THICKT + 0.5 * THICK[i] <= IDEPTH){
        #include layer
        ILAYER <- ILAYER + 1
        THICKT <- THICKT + THICK[i]
      }else{
        i<-NLAYER
      }
    }
    THICKA[1] <- 0
    for(i in 1:NLAYER){  #  oder doch ab 1 ???
      if (i <= ILAYER) {


        if(i==1){
          THICKA[i] <- THICK[i]
          INFRAC[i] <- (THICKA[i] / THICKT) ^ INFEXP - (0 / THICKT) ^ INFEXP
        }else{
          THICKA[i] <- THICKA[i - 1] + THICK[i]
          INFRAC[i] <- (THICKA[i] / THICKT) ^ INFEXP - (THICKA[i - 1] / THICKT) ^ INFEXP
        }

      }else{
        INFRAC[i] <- 0
      }
    }

  }
  return(list(ILAYER, INFRAC))
}

#*************************************************************************
ITER<-function(NLAYER, DTI, DPSIDW, NTFLI, SWATMX, PSITI, DSWMAX, DPSIMX){
  #input
  #  NLAYER%    number of soil layers to be used in model
  #  DTI        time step for iteration interval, d
  #  DPSIDW()   rate of change of total potential with water content, kPa/mm
  #  NTFLI()    net flow rate into layer, mm/d
  #  SWATMX()   maximum water storage for layer, mm
  #  PSITI()    total potential, kPa
  #  DSWMAX     maximum change allowed in SWATI, percent of SWATMX(i)
  #  DPSIMX     maximum potential difference considered "equal", kPa
  #output
  #  DTINEW     second estimate of DTI
  #local
  A<- rep(0,50)
  temp<-rep(0,50)
  TT  <-0      #new potential difference between layers
  PP<-0        #original potential difference between layers
  #intrinsic
  #  ABS, SGN
  #external functions needed
  #  RMIN, RMAX
  #
  #first approximation to new total potential
  for ( i in 1:NLAYER){
    A[i] <- NTFLI[i] * DPSIDW[i] / SWATMX[i]
    temp[i] <- PSITI[i] + A[i] * DTI
  }
  #test to see if DTI should be reduced
  DTINEW <- DTI
  for( i in 1:NLAYER){
    #  prevent too large a change in water content
    DTINEW <- RMINF(DTINEW, 0.01 * DSWMAX * SWATMX[i] / RMAXF(0.000001, abs(NTFLI[i])))
    #  prevent oscillation of potential gradient
    if (i < NLAYER) {
      #     total potential difference at beginning of iteration
      PP <- PSITI[i] - PSITI[i + 1]
      #     first approx to total potential difference at end of iteration
      TT <- temp[i] - temp[i + 1]
      if ((abs(TT) > DPSIMX) && (abs(PP) > DPSIMX) && (sign(TT) != sign(PP))){
        DTINEW <- RMINF(DTINEW, -PP / (A[i] - A[i + 1]))
      }
    }
  }
  return(DTINEW)
}

#*************************************************************************
RTDEN<-function(ROOTDEN, NLAYER, THICK){
  #for Version 4, June 2, 1999
  #25 and 50 here refer to number of values in ROOTDEN parameter array
  #input
  #  ROOTDEN()  array (1-50) of root layer thickness and root density per unit stonefree volume
  #  NLAYER%  number of soil layers being used
  #  THICK    soil layer thicknesses, mm
  #output
  #  RELDEN   relative root density per unit stonefree volume for soil layer
  #local
  #Dim i%    # soil layer
  #Dim J%    # root layer
  #Dim DONE As Integer
  RTHICK<-rep(0,ML) #root layer thickness
  RDEN<-rep(0,ML) #relative root density in layer
  RREMAIN<-0 # remaining thickness of root layer
  TREMAIN<-0 # remaining thickness of soil layer
  #
  for( J in 1:ML){
    RTHICK[J] <- ROOTDEN[2 * J - 1]
    RDEN[J] <- ROOTDEN[2 * J]
  }
  DONE <- FALSE
  j <- 1
  RREMAIN <- RTHICK[j]
  for( i in 1:NLAYER){ # new soil layer
    #accumulate RELDEN as total root length in soil layer
    RELDEN[i] <- 0
    if (!DONE){
      TREMAIN <- THICK[i]
      while(RREMAIN < TREMAIN && j < ML-1){
        #remaining root layer thickness < remaining soil layer thickness
        RELDEN[i] <- RELDEN[i] + RDEN[j] * RREMAIN
        TREMAIN <- TREMAIN - RREMAIN
        j <- j + 1
        if( j == ML){
          DONE <- TRUE
        }
        RREMAIN <- RTHICK[j]
      }
      #remaining root layer thickness >= remaining soil layer thickness
      if(!DONE){
        RELDEN[i] <- RELDEN[i] + RDEN[j] * TREMAIN
        RREMAIN <- RREMAIN - TREMAIN
      }
    }
    #convert back to unit volume basis
    RELDEN[i] <- RELDEN[i] / THICK[i]
  }
  return(RELDEN)
}

#*************************************************************************
SRFLFR<-function(){
  #input
  #  QLAYER%  number of soil layers for SRFL
  #  SWATI()  water volume by layer, mm
  #  SWATQX   maximum water storage for layers 1 through QLAYER%
  #  QFPAR    quickflow parameter, 0 for bucket
  #  SWATQF   water storage at field capacity for layers 1 through QLAYER%, mm
  #  QFFC     SRFL fraction at field capacity
  #output
  #  SAFRAC   source area fraction
  #local
  SUM<-0      #soil water in layers 1 through QLAYER%
  safra<-0
  for( i in  1:QLAYER){
    SUM <- SUM + SWATI[i]
  }
  if( QFPAR > 0.01){
    safra <- QFFC ^ (1 - (1 / QFPAR) * (SUM - SWATQF) / (SWATQX - SWATQF))
    if (safra > 1) {safra <- 1}
  }else{ # bucket over QLAYERs
    if (SUM >= SWATQF){
      safra <- 1
    }else{
      safra <- 0
    }
  }

  #
  return(safra)
}

#***********************************************************************
SRFPAR<-function(QDEPTH, NLAYER, THETAF, THICK, STONEF, SWATMX){
  #Modified for Version 4, June 2, 1999
  #source area parameters
  #input
  #  QDEPTH    soil depth for SRFL calculation, 0 to prevent SRFL
  #  NLAYER%   number of soil layers to be used
  #  THETAF()  volumetric water content of layer at field capacity
  #  THICK()   layer thickness, mm
  #  STONEF()  stone volume fraction of layer
  #  SWATMX()  maximum water storage for layer, mm
  #output
  #  QLAYER%   number of soil layers for SRFL
  #  SWATQX    maximum water storage for layers 1 through QLAYER%, mm
  #  SWATQF    water storage at field capacity for layers 1 through QLAYER%, mm
  #local
  THICKT<-0
  #
  if( QDEPTH == 0 ){
    #no SRFL with QLAYER% = 0, SWATQX and SWATQF not used
    QLAYER <- 0
    SWATQX <- 0
    SWATQF <- 0
    return(list( QLAYER, SWATQX, SWATQF))
  }
  QLAYER <- 1
  THICKT <- THICK[1]
  for( i in  2:NLAYER){
    if( THICKT + 0.5 * THICK[i] <= QDEPTH){
      THICKT <- THICKT + THICK[i]
      QLAYER <- QLAYER+ 1
    }else{
      i<-NLAYER
    }
  }
  SWATQX <- 0
  SWATQF <- 0
  for( i in  1:QLAYER){
    SWATQX <- SWATQX + SWATMX[i]
    SWATQF <- SWATQF + THETAF[i] * THICK[i] * (1 - STONEF[i])
  }
  #
  return(list( QLAYER, SWATQX, SWATQF))
}

#**************************************************************************
VERT<-function(i){
  #modified March 17, 2001 to change KKMEAN
  #vertical flow rate
  #VERT(KK[i], KK[i+1], KSAT[i], KSAT[i+1], THICK[i], THICK[i+1], PSITI[i], PSITI[i+1], STONEF[i], STONEF[i+1], RHOWG, VRFLI[i],i)
  ###
  #  flow rate = gradient * cond    / rhog
  #   mm/day   = kPa/mm   * mm/day  / kPa/mm
  #input
  #  KK         hydraulic conductivity for upper layer, mm/d
  #  KK1        hydraulic conductivity for lower layer, mm/d
  #  KSAT       saturated hydraulic conductivity of upper layer, mm/d
  #  KSAT1      saturated hydraulic conductivity of lower layer, mm/d
  #  THICK      thickness of upper layer, mm
  #  THICK1     thickness of lower layer, mm
  #  PSIT       total potential of upper layer, kPa
  #  PSIT1      total potential of lower layer, kPa
  #  STONEF      stone volume fraction of upper layer, unitless
  #  STONE1     stone volume fraction of lower layer, unitless
  #  RHOWG      density of water times gravity acceleration, kPa/mm
  #  i          Layer
  #output
  #  VRFLI      vertical drainage rate from layer i, mm/d
  #local
  GRAD <-0     # potential gradient, positive downward, kPa/mm
  KKMEAN<-0     #geometric mean conductivity
  #
  KKMEAN <- exp((log(KK[i]) + log(KK[i+1])) / 2)
  #for Version 4.2 and 4.3 was
  #KKMEAN = Exp((THICK * Log(KK) + THICK1 * Log(KK1)) / (THICK + THICK1))
  #for Version 4.1 and 3.25a and earlier was
  #  KKMEAN = Exp((THICK1 * Log(KK) + THICK * Log(KK1)) / (THICK + THICK1))
  #limit KKMEAN to lesser saturated conductivity
  if (KKMEAN > KSAT[i])  KKMEAN <- KSAT[i]
  if (KKMEAN > KSAT[i+1])  KKMEAN <- KSAT[i+1]
  #through Version 4.3a was GRAD = (PSIT - PSIT1) / ((THICK + THICK1) / 2!)
  GRAD <- (PSITI[i] - PSITI[i+1]) / RMINF(THICK[i], THICK[i+1])
  VRFLi <- (GRAD * KKMEAN / RHOWG) * (1 - (STONEF[i] + STONEF[i+1]) / 2)


  return(VRFLi)
}


#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden
#   Institut für Hydrologie und Meteorologie
#   Professur für Meteorologie
#   2017
#
#
#
#
#
#
#
#
#***************************************************************************
SNOENRGY<-function(TSNOW, TA, DAYLEN, CCFAC, MELFAC, SLFDAY, LAI, SAI, LAIMLT, SAIMLT){
  #  snow surface energy balance
  #input
  #   TSNOW      snowpack temperature (isothermal assumed), degC
  #   TA         "mean" temperature for the day, øC
  #   DAYLEN     daylength in fraction of day
  #   CCFAC      cold content factor, MJ m-2 d-1 K-1
  #   MELFAC     degree day melt factor for open, MJ m-2 d-1 K-1
  #   SLFDAY     ratio of potential insolation on slope to on horizontal for day
  #   LAI        leaf area index, m2/m2
  #   SAI        stem area index, m2/m2
  #   LAIMLT     parameter for snowmelt dependence on LAI, dimensionless
  #   SAIMLT     parameter for snowmelt dependence on SAI, dimensionless
  #output
  #   SNOEN      energy flux density to snow surface, MJ m-2 d-1
  #intrinsic
  #   EXP
  #
  if (TA <= 0) {
    #  snow warms or cools proportional to snow-air temperature difference
    SNOEN <- CCFAC * 2 * DAYLEN * (TA - TSNOW)
  }else{
    #  energy input proportional to TA, modified by cover, slope-aspect, and daylength
    SNOEN <- MELFAC * 2 * DAYLEN * TA * exp(-SAIMLT * SAI) * exp(-LAIMLT * LAI) * SLFDAY
  }
  return(SNOEN)
}

#****************************************************************************
SNOFRAC<-function (TMAX, TMIN, RSTEMP){
  #separates RFAL from SFAL
  #input
  #   TMAX       maximum temperature for the day, øC
  #   TMIN       minimum temperature for the day, øC
  #   RSTEMP     base temperature for snow-rain transition, øC
  #output
  #   SNOFRC     fraction of precipitation for the day as SFAL, unitless
  #
  if (TMIN >= RSTEMP) {
    SNOFRC <- 0
  }else if (TMAX < RSTEMP){
    SNOFRC <- 1
  }else{
    SNOFRC <- 1 - (TMAX - RSTEMP) / (TMAX - TMIN)
  }
  return(SNOFRC)
}

#*************************************************************************
SNOVAP<-function (TSNOW, TA, EA, UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, KSNVP){
  #snow evaporation and condensation
  #input
  #   DISP              zero-plane displacement, m
  #   DISPC             zero-plane displacement for closed canopy of HEIGHT, m
  #   EA                vapor pressure for the day, kPa
  #   HEIGHT            canopy height, m
  #   KSNVP             multiplier to fix snow evaporation problem
  #   LAI               leaf area index, m2/m2
  #   LWIDTH            leaf width, m
  #   NN                wind/diffusivity extinction coefficient
  #   RHOTP             ratio of total leaf area to projected area
  #   SAI               stem area index, m2/m2
  #   TA                mean  temperature for the day at reference height, øC
  #   TSNOW             snowpack temperature (isothermal assumed), øC
  #   UA                average wind speed for the day at reference height, m/s
  #   Z0                roughness parameter, m
  #   Z0C               roughness parameter for closed canopy of HEIGHT, m
  #   Z0GS              snow surface roughness, m
  #   ZA                reference height for TA, EA, UA, above ground, m
  #output
  #   PSNVP             potential snow evaporation, mm/d
  #local
  ESNOW <-0         #  vapor pressure at snow surface, kPa
  RAA   <-0         #  Shuttleworth-Wallace atmosphere aerodynamic resistance,s/m
  RAS   <-0         #  Shuttleworth-Wallace ground aerodynamic resistance, s/m
  #external functions needed
  #   ESAT, SWGRA, RMIN
  #
  #  ignores effect of interception on PSNVP or of PSNVP on PTRAN
  if (TSNOW > -0.1) {
    ESNOW <- 0.61
  }else{
    #     snow surface vapor pressure saturated at lower of TA and TSNOW
    esatt<-ESAT(RMINF(TA, TSNOW), ESNOW, dummy)
    ESNOW<-unlist(esatt[1])
  }
  swgra<-SWGRA(UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS)
  RAA<-unlist(swgra[1])
  RAC<-unlist(swgra[2])
  RAS<-unlist(swgra[3])

  ###                                                                                    ^^^       ^^^
  PSNVP <- (WTOMJ / LS) * (CPRHO / GAMMA) * (ESNOW - EA) / (RAA + RAS)
  #  fix for PSNVP overestimate
  PSNVP <- KSNVP * PSNVP

  return(PSNVP)
}

#***************************************************************************
SNOWPACK<-function(RTHR, STHR, PSNVP, SNOEN, CC, SNOW, SNOWLq, DTP, TA, MAXLQF, GRDMLT){
  #   adds net snowfall or rainfall to snowpack, subtracts groundmelt, evaporation, and melt

  #input
  #   RTHR    rain throughfall rate, mm/d
  #   STHR    SNOW throughfall rate, mm/d
  #   PSNVP   potential evaporation rate from snowpack, mm/d
  #   SNOEN   energy flux density to SNOW surface, MJ m-2 d-1
  #   DTP     time step for precipitation interval, may be <= 1 d
  #   TA      "mean" temperature for the day, øC
  #   MAXLQF  maximum liquid water fraction of SNOW, dimensionless
  #   GRDMLT  rate of groundmelt of snowpack, mm/d
  #input and output
  #   CC      cold content of snowpack (positive), MJ/m2
  #   SNOW    water equivalent of SNOW on the ground, mm
  #   SNOWLQ  liquid water content of SNOW on the ground, mm
  #output
  #   RSNO    rain added to snowpack, mm/d
  #   SNVP    evaporation rate from snowpack, mm/d
  #   SMLT    melt drainage rate from snowpack, mm/d
  #local
  SNOWLQ<-SNOWLq
  FRAC <-0 # groundmelt and evaporation fraction of SNOW, dimensionless
  EQEN <-0 # meltwater equivalent of energy input, including warm rain, mm
  NMLT<-0 # -EQEN when EQEN is negative, "negative melt", mm
  ALQ <-0 # MAXLQF*SNOW - SNOWLQ, available space for liquid water, mm
  RIN <-0 # RTHR*DTP, rain input to SNOW, mm
  #external functions needed
  #   RMIN, RMAX
  #
  #SNOW throughfall and its cold content, SNOWLQ unchanged
  SNOW <- SNOW + STHR * DTP
  CC <- CC + CVICE * RMAXF(-TA, 0) * STHR * DTP

  if (CC > 0 && SNOWLQ > 0) {
    if (CC > SNOWLQ * LF) {
      #     refreeze all liquid
      CC <- CC - SNOWLQ * LF
      SNOWLQ <- 0
    }else{
      #     refreeze part
      SNOWLQ <- SNOWLQ - CC / LF
      CC <- 0
    }
  }

  #if (SNOW > 0) {#
  #groundmelt and evaporation loss as fraction of SNOW
  FRAC <- (GRDMLT + PSNVP) * DTP / SNOW
  #FRAC can be negative if condensation exceeds groundmelt
  if (FRAC < 1) {
    SMLT <- GRDMLT
    SNVP <- PSNVP
    #  reduce CC, SNOWLQ, and SNOW proportionally for groundmelt and evaporation
    #  increase them proportionally if condensation exceeds groundmelt
    CC <- CC * (1 - FRAC)
    SNOWLQ <- SNOWLQ * (1 - FRAC)
    SNOW <- SNOW * (1 - FRAC)
  }else{
    #  all SNOW disappears from groundmelt and/or evaporation
    SMLT <- GRDMLT / FRAC
    SNVP <- PSNVP / FRAC
    RSNO <- 0
    CC <- 0
    SNOWLQ <- 0
    SNOW <- 0
  }
  #}

  #snowpack cooling or warming
  if (SNOW > 0) {
    #  equivalent ice melted by energy input including warm rain, mm
    EQEN <- DTP * (SNOEN + RTHR * RMAXF(TA, 0) * CVLQ) / LF
    if (EQEN <= 0) {
      #     snowpack cooling
      NMLT <- -EQEN
      if (NMLT < SNOWLQ) {
        #        only part of SNOWLQ refreezes
        CC <- 0
        #        should be 0 already because SNOWLQ is positive
        SNOWLQ <- SNOWLQ - NMLT
      }else{
        #        all SNOWLQ (if any) refreezes, remaining NMLT increases CC
        NMLT <- NMLT - SNOWLQ
        SNOWLQ <- 0
        CC <- CC + NMLT * LF
        #        do not allow TSNOW to cool below TA
        CC <- RMINF(CC, -TA * SNOW * CVICE)
      }
    }else{
      #     snowpack warming  (can#t have both CC and SNOWLQ)
      if (EQEN * LF < CC || TA < 0) {
        #        reduce but don#t eliminate CC
        if (TA < 0) {
          #           do not allow TSNOW to warm above TA when TA < 0
          CC <- RMAXF(CC - EQEN * LF, -TA * SNOW * CVICE)
        }else{
          CC <- CC - EQEN * LF
        }
        SNOWLQ <- 0
      }else{
        #        CC eliminated
        EQEN <- EQEN - CC / LF
        CC <- 0
        if (EQEN <= MAXLQF * SNOW - SNOWLQ){
          #           remaining energy increases liquid water
          SNOWLQ <- SNOWLQ + EQEN
          #           SMLT and SNOW unchanged
        }else{
          #           liquid water capacity reached, SNOW melt produced
          EQEN <- EQEN - (MAXLQF * SNOW - SNOWLQ)
          if (SNOW * (1 - MAXLQF) > EQEN) {
            #              melt is ice plus the liquid included in it
            SMLT <- SMLT + (EQEN / DTP) / (1 - MAXLQF)
            SNOW <- SNOW - EQEN / (1 - MAXLQF)
            SNOWLQ <- MAXLQF * SNOW
          }else{
            #              all SNOW melts
            SMLT <- SMLT + SNOW / DTP
            SNOW <- 0
            SNOWLQ <- 0
          }
        }
      }
    }

    #  add rain to snowpack,
    if (RTHR == 0 || SNOW == 0) {
      RSNO <- 0
    }else{
      #     rain on SNOW
      RIN <- RTHR * DTP
      if (CC > 0) {
        #        use CC to refreeze rain
        if (CC > RIN * LF) {
          #           refreezes all rain
          CC <- CC - RIN * LF
          RSNO <- RTHR
          SNOW <- SNOW + RIN
        }else{
          #           CC refreezes part of rain
          SNOW <- SNOW + CC / LF
          RSNO <- (CC / LF) / DTP
          CC <- 0
          #           remaining rain
          RIN <- RIN - RSNO * DTP
          #           increase liquid water, SNOWLQ initially zero
          if (RIN < MAXLQF * SNOW / (1 - MAXLQF)) {
            #              remaining RIN all to SNOWLQ
            SNOWLQ <- RIN
            RSNO <- RSNO + RIN / DTP
            SNOW <- SNOW + RIN
          }else{
            SNOWLQ <- MAXLQF * SNOW / (1 - MAXLQF)
            RSNO <- RSNO + SNOWLQ / DTP
            SNOW <- SNOW + SNOWLQ
          }
        }
      }else{
        #        CC = 0.
        if (SNOWLQ >= MAXLQF * SNOW) {
          #           SNOW already holding maximum liquid
          RSNO <- 0
        }else{
          ALQ <- MAXLQF * SNOW - SNOWLQ
          if (RIN < ALQ) {
            #              all RIN to SNOW
            RSNO <- RTHR
            SNOWLQ <- SNOWLQ + RIN
            SNOW <- SNOW + RIN
          }else{
            #              maximum liquid reached
            RSNO <- (ALQ / (1 - MAXLQF)) / DTP
            SNOW <- SNOW + RSNO * DTP
            SNOWLQ <- MAXLQF * SNOW
          }
        }
      }
    }
  }
  return (list(CC,SNOW,SNOWLQ,RSNO, SNVP, SMLT))
}


#   Code by R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#  modified RK [16112017]
#
#   TU Dresden
#   Institut für Hydrologie und Meteorologie
#   Professur für Meteorologie
#   2017
#
#
#
#
#
#
#
MSBSETVARS<-function(){
  #solar parameters depending on DOY%
  sundss<-SUNDS(LAT, ESLOPE, DOY, L1, L2)

  DAYLEN<<-unlist(sundss[1])
  I0HDAY<<-unlist(sundss[2])
  SLFDAY<<-unlist(sundss[3])
  #                                     ^^^^^^  ^^^^^^  ^^^^^^
  #canopy parameters depending on DOY%
  cano<-CANOPY(DOY, MAXHT, RELHT, MAXLAI, RELLAI, SNOW, SNODEN, MXRTLN, MXKPL, CS, DENSEF)
  HEIGHT<<-unlist(cano[1])
  LAI<<-unlist(cano[2])
  SAI<<-unlist(cano[3])
  RTLEN<<-unlist(cano[4])
  RPLANT<<-unlist(cano[5])
  ###                                                                                          ^^^^^^  ^^^  ^^^  ^^^^^  ^^^^^^
  #roughness parameters
  if (SNOW > 0) {
    Z0GS <<- Z0S
  }else{
    Z0GS <<- Z0G
  }
  rough<-ROUGH(HEIGHT, ZMINH, LAI, SAI, CZS, CZR, HS, HR, LPC, CS, Z0GS)
  Z0GS<<-unlist(rough[1])
  Z0C<<-unlist(rough[2])
  DISPC<<-unlist(rough[3])
  Z0<<-unlist(rough[4])
  DISP<<-unlist(rough[5])
  ZA<<-unlist(rough[6])
  #                                                                    ^^^  ^^^^^  ^^  ^^^^  ^^
  #plant resistance components
  plnt<-PLNTRES(NLAYER, THICK, STONEF, RTLEN, RELDEN, RTRAD, RPLANT, FXYLEM)
  RXYLEM<<-plnt[1]
  RROOTI<<-plnt[2:(ML+1)]
  ALPHA<<-plnt[(ML+2):(ML*2+1)]
  ###                                                                             ^^^^^^  ^^^^^^^^  ^^^^^^^
  #calculated weather data
  SHEAT <<- 0
  WEATHER(TMAX, TMIN, DAYLEN, I0HDAY, EA, UW, ZA, DISP, Z0, WNDRAT, FETCH, Z0W, ZW, SOLRAD, SOLRADC, TA, TADTM, TANTM, UA, UADTM, UANTM)
  ###                                                                                            ^^^^^^^  ^^  ^^^^^  ^^^^^  ^^  ^^^^^  ^^^^^
  #fraction of precipitation as SFAL
  SNOFRC<<- SNOFRAC(TMAX, TMIN, RSTEMP)
  #                                ^^^^^^
  if (SNOW > 0) {
    #  snowpack temperature at beginning of day
    TSNOW <<- -CC / (CVICE * SNOW)
    #  potential snow evaporation
    PSNVP<<-SNOVAP(TSNOW, TA, EA, UA, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, KSNVP)
    ###                                                                                                           ^^^^^
    ALBEDO <<- ALBSN
    RSS <<- 0
  }else{
    TSNOW <<- 0
    PSNVP <<- 0
    ALBEDO <<- ALB
    #  soil evaporation resistance
    RSS <<- FRSS(RSSA, RSSB, PSIF[1], PSIM[1])
    # check for zero or negative RSS
    if (RSS < 0.000001) {
      #MsgBox ("RSS is very small or negative. Run ends. Check RSSA and RSSB values.")
      rstop <<- 3
    }
  }
  #snow surface energy balance (even if SNOW = 0 in case of snow during day)
  SNOEN<<-SNOENRGY(TSNOW, TA, DAYLEN, CCFAC, MELFAC, SLFDAY, LAI, SAI, LAIMLT, SAIMLT)
  ###                                                                               ^^^^^

}

subdatafileline<-function(row){
  YY <<- MData[[1]][row]
  #change two-digit year to four-digit
  if( YY < 100){
    if (YY > 20){
      YY <<- YY + 1900
    }else{
      YY <<- YY + 2000
    }
  }
  MM<<- MData[[2]][row]
  DD<<- MData[[3]][row]
  SOLRAD<<- MData[[4]][row]
  TMAX <<- MData[[5]][row]
  TMIN <<- MData[[6]][row]
  EA <<- MData[[7]][row]
  UW <<- MData[[8]][row]
  PRECIN <<- MData[[9]][row]
  MESFL <<- MData[[10]][row]
}

subprfileline<-function(row){
  YY <<- MhhData[[1]][row]
  #change two-digit year to four-digit
  if( YY < 100){
    if( YY > 20 ){
      YY <<- YY + 1900
    }else{
      YY <<- YY + 2000
    }
  }
  MM <<- MhhData[[2]][row]
  DD <<- MhhData[[3]][row]
  II <<- MhhData[[4]][row]
  PREINT <<- MhhData[[5]][row]
  MESFLP <<- MhhData[[6]][row]
}

msum<-function(){
  PRECM <<- RFALM + SFALM
  STHRM <<- SFALM - SINTM
  RTHRM <<- RFALM - RINTM
  RNETM <<- RTHRM - RSNOM
  EVAPM <<- IRVPM + ISVPM + SNVPM + SLVPM + TRANM
  FLOWM <<- SRFLM + BYFLM + DSFLM + GWFLM
}

paccum<-function(){
  #     accumulate flows over precip interval (below ground only)
  #     zeroed by ZPINT.INC
  #for(i in 1:NLAYER){
  VRFLPI <<- VRFLPI + VRFLI * DTI
  SLFLPI <<- SLFLPI + SLFLI * DTI
  INFLPI <<- INFLPI + INFLI * DTI
  BYFLPI <<- BYFLPI + BYFLI * DTI
  DSFLPI <<- DSFLPI + DSFLI * DTI
  NTFLPI <<- NTFLPI + NTFLI * DTI
  TRANPI <<- TRANPI + TRANI * DTI
  #        note TRANI() are constant over precipitation interval
  #}
  SRFLP <<- SRFLP + SRFL * DTI
  SLFLP <<- SLFLP + SLFL * DTI
  GWFLP <<- GWFLP + GWFL * DTI
  SEEPP <<- SEEPP + SEEP * DTI
  #  sum flows for precip interval from components
  sumii<-SUMI(NLAYER, BYFLPI, INFLPI, DSFLPI, TRANPI, DUMM, DUMM, BYFLP, INFLP, DSFLP, TRANP, dummy, dummy)
  BYFLP<<-unlist(sumii[1])
  INFLP<<-unlist(sumii[2])
  DSFLP<<-unlist(sumii[3])
  TRANP<<-unlist(sumii[4])
}

yaccum<-function(){
  #  accumulate flows over year
  #  zeroed by ZYEAR.INC
  ACCUMI(NLAYER, VRFLMI, INFLMI, BYFLMI, DSFLMI, NTFLMI, VRFLYI, INFLYI, BYFLYI, DSFLYI, NTFLYI)
  #                                                                         ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^
  ACCUMI(NLAYER, TRANMI, SLFLMI, DUMM, DUMM, DUMM, TRANYI, SLFLYI, DUMM, DUMM, DUMM)
  #                                                                   ^^^^^^^^
  ACCUM(SRFLM, SLFLM, GWFLM, SEEPM, dummy, SRFLY, SLFLY, GWFLY, SEEPY, dummy)
  #                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^
  ACCUM(ISVPM, IRVPM, SNVPM, SLVPM, SFALM, ISVPY, IRVPY, SNVPY, SLVPY, SFALY)
  #                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
  ACCUM(RFALM, SINTM, RINTM, RSNOM, SMLTM, RFALY, SINTY, RINTY, RSNOY, SMLTY)
  #                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
  ACCUM(MESFLM, PTRANM, PINTM, dummy, dummy, MESFLY, PTRANY, PINTY, dummy, dummy)
  #                                                  ^^^^^^  ^^^^^^  ^^^^^
  #  sum flows for year from components
  SUMI(NLAYER, BYFLYI, INFLYI, DSFLYI, TRANYI, DUMM, DUMM, BYFLY, INFLY, DSFLY, TRANY, dummy, dummy)
}

#Sub youtput()
#Dim strng$
##flows as amounts in mm for year
#If runflag% = 1 Then   # Rerun
#Line Input #31, strng$
#Print #21, strng$;
#Else
#Print #21, Format$(YEARN%, "0000"); "   "; "   "; "   ";
#End If
#Call msbvaroutput(1, yvars%, yvar%(), 21)
#Print #21,   # to end output line
#End Sub

ysum<-function(){
  PRECY <<- RFALY + SFALY
  STHRY <<- SFALY - SINTY
  RTHRY <<- RFALY - RINTY
  RNETY <<- RTHRY - RSNOY
  EVAPY <<- IRVPY + ISVPY + SNVPY + SLVPY + TRANY
  FLOWY <<- SRFLY + BYFLY + DSFLY + GWFLY
}

zday<-function(){
  #zero daily accumulators
  VRFLDI<<-rep(0,ML)
  INFLDI<<-rep(0,ML)
  BYFLDI<<-rep(0,ML)
  DSFLDI<<-rep(0,ML)
  NTFLDI<<-rep(0,ML)
  TRANDI<<-rep(0,ML)
  SLFLDI<<-rep(0,ML)
  SRFLD<<-0
  GWFLD<<-0
  SEEPD<<-0
  SLFLD<<-0

  IRVPD<<-0
  ISVPD<<-0
  SLVPD<<-0
  SNVPD<<-0
  SFALD<<-0
  RFALD<<-0
  SINTD<<-0
  RINTD<<-0
  RSNOD<<-0
  SMLTD<<-0
  MESFLD<<-0
  PTRAND<<-0
  PINTD<<-0
}

zmonth<-function(){
  #zero monthly accumulators

  VRFLMI<<-rep(0,ML)
  INFLMI<<-rep(0,ML)
  BYFLMI<<-rep(0,ML)
  DSFLMI<<-rep(0,ML)
  NTFLMI<<-rep(0,ML)
  TRANMI<<-rep(0,ML)
  SLFLMI<<-rep(0,ML)

  SRFLM<<-0
  GWFLM<<-0
  SEEPM<<-0
  SLFLM<<-0
  IRVPM<<-0
  ISVPM<<-0
  SLVPM<<-0
  SNVPM<<-0
  SFALM<<-0
  RFALM<<-0
  SINTM<<-0
  RINTM<<-0
  RSNOM<<-0
  SMLTM<<-0
  MESFLM<<-0
  PTRANM<<-0
  PINTM<<-0
}

zpint<-function(){
  #zero precip interval accumulators
  VRFLPI<<-rep(0,ML)
  INFLPI<<-rep(0,ML)
  BYFLPI<<-rep(0,ML)
  DSFLPI<<-rep(0,ML)
  NTFLPI<<-rep(0,ML)
  TRANPI<<-rep(0,ML)
  SLFLPI<<-rep(0,ML)
  SRFLP<<-0
  SLFLP<<-0
  GWFLP<<-0
  SEEPP<<-0

}

zyear<-function(){
  #zero annual accumulators
  VRFLYI<<-rep(0,ML)
  INFLYI<<-rep(0,ML)
  BYFLYI<<-rep(0,ML)
  DSFLYI<<-rep(0,ML)
  NTFLYI<<-rep(0,ML)
  TRANYI<<-rep(0,ML)
  SLFLYI<<-rep(0,ML)
  SRFLY<<-0
  GWFLY<<-0
  SEEPY<<-0
  SLFLY<<-0
  IRVPY<<-0
  ISVPY<<-0
  SLVPY<<-0
  SNVPY<<-0
  SFALY<<-0
  RFALY<<-0
  SINTY<<-0
  RINTY<<-0
  RSNOY<<-0
  SMLTY<<-0
  MESFLY<<-0
  PTRANY<<-0
  PINTY<<-0
}


psum<-function(){
  EVAPP <<- (ISVP + IRVP + SNVP + SLVP) * DTP + TRANP
  FLOWP <<- SRFLP + BYFLP + DSFLP + GWFLP
}


MSBPREINT<-function(){
  #
  #  convert precipitation interval mm to rate in mm/d
  PREC <<- PREINT / DTP
  SFAL <<- SNOFRC * PREC
  RFAL <<- PREC - SFAL
  if (NPINT > 1) {
    #     more than one precip interval in day
    #     snow interception
    if (PINT < 0 && TA > 0) {
      #        prevent frost when too warm, carry negative PINT to rain
      temppp<-INTER(SFAL, 0, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DTP, INTS, SINT, ISVP)
      SINT<<-unlist(temppp[1])
      ISVP<<-unlist(temppp[2])
      #                                                                                 ^^^^  ^^^^
    }else{
      temppp<-INTER(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DTP, INTS, SINT, ISVP)
      SINT<<-unlist(temppp[1])
      ISVP<<-unlist(temppp[2])
      #                                                                                      ^^^^  ^^^^
    }
    #     rain interception,  note potential interception rate is PID/DT-ISVP
    temppp<-INTER(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DTP, INTR, RINT, IRVP)
    RINT<<-unlist(temppp[1])
    IRVP<<-unlist(temppp[2])
    #                                                                                        ^^^^  ^^^^
  }else{
    #     one precip interval in day, use storm DURATN and INTER24
    #     snow interception
    if (PINT < 0 && TA > 0) {
      #        prevent frost when too warm, carry negative PINT to rain
      temm<-INTER24(SFAL, 0, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DURATN, INTS, SINT, ISVP, MONTHN)
      SINT<<-unlist(temm[1])
      ISVP<<-unlist(temm[2])
      #                                                                                               ^^^^  ^^^^
    }else{
      temm<-INTER24(SFAL, PINT, LAI, SAI, FSINTL, FSINTS, CINTSL, CINTSS, DURATN, INTS, SINT, ISVP, MONTHN)
      SINT<<-unlist(temm[1])
      ISVP<<-unlist(temm[2])
      #                                                                                                  ^^^^  ^^^^
    }
    #     rain interception,  note potential interception rate is PID/DT-ISVP
    temm<-INTER24(RFAL, PINT - ISVP, LAI, SAI, FRINTL, FRINTS, CINTRL, CINTRS, DURATN, INTR, RINT, IRVP, MONTHN)
    RINT<<-unlist(temm[1])
    IRVP<<-unlist(temm[2])
    #                                                                                                      ^^^^  ^^^^
  }
  #  throughfall
  RTHR <<- RFAL - RINT
  STHR <<- SFAL - SINT
  #
  #  reduce transpiration for fraction of precip interval that canopy is wet
  WETFR <<- RMINF(1, (IRVP + ISVP) / PINT)
  PTRAN <<- (1 - WETFR) * PTRAN
  for( i in 1:NLAYER){
    TRANI[i] <<- (1 - WETFR) * TRANI[i]
  }
  #
  if (SNOW <= 0 && STHR <= 0) {
    #     no snow, soil evaporation weighted for WETFR
    SLVP <<- WETFR * GIVP + (1 - WETFR) * GEVP
    RNET <<- RTHR
    RSNO <<- 0
    SNVP <<- 0
    SMLT <<- 0
  }else{
    if (SNOW <= 0 && STHR > 0){
      #        new snow only, zero CC and SNOWLQ assumed
      CC <<- 0
      SNOWLQ <<- 0
    }
    #     snow accumulation and melt
    spa<-SNOWPACK(RTHR, STHR, PSNVP, SNOEN, CC, SNOW, SNOWLQ, DTP, TA, MAXLQF, GRDMLT)
    CC      <<-unlist(spa[1])
    SNOW    <<-unlist(spa[2])
    SNOWLQ  <<-unlist(spa[3])
    RSNO    <<-unlist(spa[4])
    SNVP    <<-unlist(spa[5])
    SMLT    <<-unlist(spa[6])
    #                                             ^^  ^^^^  ^^^^^^                           ^^^^  ^^^^  ^^^^
    RNET <<- RTHR - RSNO
    SLVP <<- 0
  }

}

MSBITERATE<-function(){
  #
  #     source area flow rate
  #  print("Currently here")
  if (QLAYER > 0) {
    SAFRAC<<-SRFLFR()
  }else{
    SAFRAC <<- 0
  }
  SRFL <<- RMINF(1, (IMPERV + SAFRAC)) * (RNET + SMLT)
  #
  #     water supply rate to soil surface
  SLFL <<- RNET + SMLT - SRFL
  #
  #     bypass fraction of infiltration to each layer
  BYFRAC<<-BYFLFR()

  #                                                                 ^^^^^^^^
  #     begin layer loop
  for( iiii in  seq(NLAYER,1,-1)){
    #        downslope flow rates
    if( LENGTH == 0 || DSLOPE == 0){        # added in Version 4
      DSFLI[iiii]<<- 0
    }else{
      DSFLI[iiii]<<-DSLOP(iiii)
      ###                                                                                    ^^^^^^^^^
    }
    #        vertical flow rates
    if (iiii < NLAYER) {
      if (abs(PSITI[iiii] - PSITI[iiii+1]) < DPSIMX) {
        VRFLI[iiii] <<- 0

      }else{
        VRFLI[iiii]<<-VERT(iiii)
        ###                                                                                                                                                         ^^^^^^^^^
      }
    }else{
      #           bottom layer
      if( DRAIN > 0.0001){
        #              gravity drainage only
        VRFLI[NLAYER] <<- DRAIN * KK[NLAYER] * (1 - STONEF[NLAYER])
        #            ElseIf DRAIN < -.0001 Then
        #              fixed water table at bottom of profile - not implemented, oscillation problems
        #               VRFLI(NLAYER%) = -DRAIN * (1 - STONEF(NLAYER%)) * KK(NLAYER%) * (PSIM(NLAYER%) / RHOWG + THICK(NLAYER%) / 2)
      }else{
        #              bottom of profile sealed
        VRFLI[NLAYER] <<- 0
      }
    }
  }

  #     end of layer loop
  #
  #     first approximation for iteration time step, time remaining or DTIMAX
  DTI <<- RMINF(DTRI, DTIMAX)
  #
  #     net inflow to each layer including E and T withdrawal adjusted for interception
  inflo<-INFLOW()
  VV<<-unlist(inflo[1])
  INFLI<<-unlist(inflo[2])
  BYFLI<<-unlist(inflo[3])
  NTFLI<<-unlist(inflo[4])
  ###                                                                                                           ^^    ^^^^^    ^^^^^    ^^^^^
  #
  #     second approximation to iteration time step
  for( iiii in 1:NLAYER){
    DPSIDW[iiii] <<- FDPSIDWF(iiii)
  }
  DTINEW<<-ITER(NLAYER, DTI, DPSIDW, NTFLI, SWATMX, PSITI, DSWMAX, DPSIMX)
  ###                                                                                 ^^^^^^
  if (DTINEW < DTI) {
    #        recalculate flow rates with new DTI
    if (mnuhalfiter == FALSE) {
      DTI <<- DTINEW
    }else{
      DTI <<- 0.5 * DTINEW
    }
    inflo<-INFLOW()
    VV<<-unlist(inflo[1])
    INFLI<<-unlist(inflo[2])
    BYFLI<<-unlist(inflo[3])
    NTFLI<<-unlist(inflo[4])
    ###                                                                                                              ^^    ^^^^^    ^^^^^    ^^^^^
  }
  #     VV is the new VRFLI
  for( iiii in 1:NLAYER){
    VRFLI[iiii] <<- VV[iiii]
  }
  #
  #     groundwater flow and seepage loss
  gwa<-GWATER(GWAT, GSC, GSP, DT, VRFLI[NLAYER])
  GWFL<<-unlist(gwa[1])
  SEEP<<-unlist(gwa[2])
  #                                                     ^^^^  ^^^^
  #     end of rate calculations

}

daccum<-function(){
  #  accumulate above ground flows over day
  #  zeroed by ZDAY.INC
  ISVPD <<- ISVPD + ISVP * DTP
  IRVPD <<- IRVPD + IRVP * DTP
  SNVPD <<- SNVPD + SNVP * DTP
  SLVPD <<- SLVPD + SLVP * DTP
  SFALD <<- SFALD + SFAL * DTP
  RFALD <<- RFALD + RFAL * DTP
  SINTD <<- SINTD + SINT * DTP
  RINTD <<- RINTD + RINT * DTP
  RSNOD <<- RSNOD + RSNO * DTP
  SMLTD <<- SMLTD + SMLT * DTP
  MESFLD <<- MESFLD + MESFLP * DTP
  PTRAND <<- PTRAND + PTRAN * DTP
  PINTD <<- PINTD + PINT * DTP
  #  accumulate below ground flows over day
  accumi<-ACCUMI(NLAYER, VRFLPI, INFLPI, BYFLPI, DSFLPI, NTFLPI, VRFLDI, INFLDI, BYFLDI, DSFLDI, NTFLDI)
  #                                                                         ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^
  VRFLDI<<-unlist(accumi[1])
  INFLDI<<-unlist(accumi[2])
  BYFLDI<<-unlist(accumi[3])
  DSFLDI<<-unlist(accumi[4])
  NTFLDI<<-unlist(accumi[5])

  accumi<-ACCUMI(NLAYER, TRANPI, SLFLPI, DUMM, DUMM, DUMM, TRANDI, SLFLDI, DUMM, DUMM, DUMM)
  #                                                                   ^^^^^^^^  ^^^^^^
  TRANDI<<-unlist(accumi[1])
  SLFLDI<<-unlist(accumi[2])

  accum<-ACCUM(SRFLP, SLFLP, GWFLP, SEEPP, dummy, SRFLD, SLFLD, GWFLD, SEEPD, dummy)
  SRFLD<<-unlist(accum[1])
  SLFLD<<-unlist(accum[2])
  GWFLD<<-unlist(accum[3])
  SEEPD<<-unlist(accum[4])
  #                                                ^^^^^  ^^^^^  ^^^^^  ^^^^^

  #sum flows for day from components
  summii<-SUMI(NLAYER, BYFLDI, INFLDI, DSFLDI, TRANDI, DUMM, DUMM, BYFLD, INFLD, DSFLD, TRAND, dummy, dummy)
  BYFLD<<-unlist(summii[1])
  INFLD<<-unlist(summii[2])
  DSFLD<<-unlist(summii[3])
  TRAND<<-unlist(summii[4])
}



maccum<-function(){
  #accumulate flows over month
  #zeroed by ZMONTH.INC
  accumi <- ACCUMI(NLAYER, VRFLDI, INFLDI, BYFLDI, DSFLDI, NTFLDI, VRFLMI, INFLMI, BYFLMI, DSFLMI, NTFLMI)
  VRFLMI<<-unlist(accumi[1])
  INFLMI<<-unlist(accumi[2])
  BYFLMI<<-unlist(accumi[3])
  DSFLMI<<-unlist(accumi[4])
  NTFLMI<<-unlist(accumi[5])
  #                                                                      ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^  ^^^^^^^^
  accumi<-ACCUMI(NLAYER, TRANDI, SLFLDI, DUMM, DUMM, DUMM, TRANMI, SLFLMI, DUMM, DUMM, DUMM)
  TRANMI<<-unlist(accumi[1])
  SLFLMI<<-unlist(accumi[2])
  #                                                                ^^^^^^^^  ^^^^^^
  accumii<-ACCUM(SRFLD, SLFLD, GWFLD, SEEPD, dummy, SRFLM, SLFLM, GWFLM, SEEPM, dummy)
  SRFLM<<-unlist(accumii[1])
  SLFLM<<-unlist(accumii[2])
  GWFLM<<-unlist(accumii[3])
  SEEPM<<-unlist(accumii[4])
  #                                             ^^^^^  ^^^^^  ^^^^^  ^^^^^
  accumii<-ACCUM(ISVPD, IRVPD, SNVPD, SLVPD, SFALD, ISVPM, IRVPM, SNVPM, SLVPM, SFALM)
  ISVPM<<-unlist(accumii[1])
  IRVPM<<-unlist(accumii[2])
  SNVPM<<-unlist(accumii[3] )
  SLVPM<<-unlist(accumii[4])
  SFALM<<-unlist(accumii[5])
  #                                             ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
  acumii<-ACCUM(RFALD, SINTD, RINTD, RSNOD, SMLTD, RFALM, SINTM, RINTM, RSNOM, SMLTM)
  RFALM<<-unlist(acumii[1])
  SINTM<<-unlist(acumii[2])
  RINTM<<-unlist(acumii[3])
  RSNOM<<-unlist(acumii[4])
  SMLTM<<-unlist(acumii[5])
  #                                             ^^^^^  ^^^^^  ^^^^^  ^^^^^  ^^^^^
  acumii<-ACCUM(MESFLD, PTRAND, PINTD, dummy, dummy, MESFLM, PTRANM, PINTM, dummy, dummy)
  MESFLM<<-unlist(acumii[1] )
  PTRANM<<-unlist(acumii[2])
  PINTM<<-unlist(acumii[3])
  #                                               ^^^^^^  ^^^^^^  ^^^^^
  #  sum flows for month from components
  summi<-SUMI(NLAYER, BYFLMI, INFLMI, DSFLMI, TRANMI, DUMM, DUMM, BYFLM, INFLM, DSFLM, TRANM, dummy, dummy)
  BYFLM<<-unlist(summi[1])
  INFLM<<-unlist(summi[2])
  DSFLM<<-unlist(summi[3])
  TRANM<<-unlist(summi[4])
}


dsum<-function(){
  PRECD <<- RFALD + SFALD
  STHRD <<- SFALD - SINTD
  RTHRD <<- RFALD - RINTD
  RNETD <<- RTHRD - RSNOD
  EVAPD <<- IRVPD + ISVPD + SNVPD + SLVPD + TRAND
  FLOWD <<- SRFLD + BYFLD + DSFLD + GWFLD
}

fnleap<-function(){
  if ((YEARN %% 4 == 0)  && ((YEARN %% 100 != 0) || (YEARN %% 400 == 0))) {
    return(TRUE)
  }else{
    return(FALSE)
  }
}


MSBDAYNIGHT<-function(){
  SOVERI <- 0
  for( JJJ in  1:2){
    #  1 for daytime, 2 for nighttime
    #  net radiation
    if (JJJ ==1){
      SLRAD <<- SLFDAY * SOLRADC / (WTOMJ * DAYLEN)
      SLRADd<<-SLRAD
      TAJ <<- TADTM
      UAJ <<- UADTM
    }else{
      SLRAD <<- 0
      TAJ <<- TANTM
      UAJ <<- UANTM
    }
    if (I0HDAY <= 0.01){
      #     no sunrise, assume 50% clouds for longwave
      SOVERI <- 0.5
    }else{
      SOVERI <- SOLRADC / I0HDAY
    }
    avai<-AVAILEN(SLRAD, ALBEDO, C1, C2, C3, TAJ, EA, SOVERI, SHEAT, CR, LAI, SAI)
    AA<<-unlist(avai[2])
    ASUBS<<-unlist(avai[3])

    ###                                                                                       ^^  ^^^^^
    #  vapor pressure deficit
    esat <- ESAT(TAJ, ES, DELTA)
    ES <<- unlist(esat[1])
    DELTA <<- unlist(esat[2])

    #                 ^^  ^^^^^
    VPD <<- ES - EA
    #  Shuttleworth-Wallace resistances
    swgra <- SWGRA(UAJ, ZA, HEIGHT, Z0, DISP, Z0C, DISPC, Z0GS, LWIDTH, RHOTP, NN, LAI, SAI, RAA, RAC, RAS)
    RAA <<- unlist(swgra[1])
    RAC <<- unlist(swgra[2])
    RAS <<- unlist(swgra[3])
    ###                                                                                     ^^^  ^^^  ^^^
    if (JJJ == 1) {
      RSC <<- SRSC(SLRAD, TA, VPD, LAI, SAI, GLMIN, GLMAX, R5, CVPD, RM, CR, TL, T1, T2, TH)
      ###                                                                                       ^^^
    }else{
      RSC <<- 1 / (GLMIN * LAI)
    }
    #  Shuttleworth-Wallace potential transpiration and ground evaporation rates
    swpe <- SWPE(AA, ASUBS, VPD, RAA, RAC, RAS, RSC, RSS, DELTA)
    PTR[JJJ] <<- unlist(swpe[1])
    GER[JJJ] <<- unlist(swpe[2])
    #                                                            ^^^^^^^  ^^^^^^^
    #  Shuttleworth-Wallace potential interception and ground evap. rates
    #  RSC = 0, RSS not changed
    swpe <- SWPE(AA, ASUBS, VPD, RAA, RAC, RAS, 0, RSS, DELTA)
    PIR[JJJ] <<- unlist(swpe[1])
    GIR[JJJ] <<- unlist(swpe[2])
    #                                                           ^^^^^^^  ^^^^^^^
    #  actual transpiration and ground evaporation rates
    if (PTR[JJJ] > 0.001) {
      rbl <- TBYLAYER(JJJ, PTR[JJJ], DISPC, ALPHA, KK, RROOTI, RXYLEM, PSITI, NLAYER, PSICR, NOOUTF)
      #ATR[JJJ] <<- rbl[1]
      #ATRANI <<- rbl[1:ML+1]
      #PSIT<<-unlist(tbl[1])
      ATR[JJJ]<<-unlist(rbl[1])
      ATRANI<<-unlist(rbl[2])###                                                                                                        ^^^^^^^  ^^^^^^^^
      for (iiii in 1:NLAYER){
        ATRI[JJJ,iiii] <<- ATRANI[iiii]
      }
      if (ATR[JJJ] < PTR[JJJ]){
        #        soil water limitation, new GER
        GER[JJJ] <<- SWGE(AA, ASUBS, VPD, RAA, RAS, RSS, DELTA, ATR[JJJ], GER[JJJ])

        #                                                                 ^^^^^^^
      }
    }else{
      #     no transpiration, condensation ignored, new GER
      PTR[JJJ] <<- 0
      ATR[JJJ] <<- 0
      for( iiii in 1:NLAYER){
        ATRI[JJJ,iiii] <<- 0
      }
      GER[JJJ] <<- SWGE(AA, ASUBS, VPD, RAA, RAS, RSS, DELTA, 0, GER[JJJ])
      #                                                         ^^^^^^^
    }
    #
  }

}

swchek<-function(i){
  #test for SWATI(I%) < 0 or > SWATMX(I%)
  if(SWATI[i] <= 0){
    #stop(paste0("Serious problem! Run stopped. Negative soil water of ", SWATI[i], " for layer ", i, ", iteration ", NITS, ", year ", YEARN, ", month ", MONTHN, ", day ", DOM, ", preint ", N, ". Error in layer 1 may be caused by too high SLVP for THICK(1). Examine output and parameters to try to determine other causes."))
    SWCHECKED<<-TRUE
  }
  if(SWATI[i] > SWATMX[i]) {
    if(SWATI[i] > SWATMX[i] + 0.00001){
      #stop(paste0("Serious problem. Run stopped. Supersaturated soil water of ", SWATI[i], " for layer ", i, ", iteration ", NITS, ", year ", YEARN, ", month ", MONTHN, ", day ", DOM, ", preint ", N, ". Examine output and parameters to try to determine the cause."))
      SWCHECKED<<-TRUE
    }
    SWATI[i] <<- SWATMX[i]
  }

  #if (SWATI[i] <= 0) {
  #
  #  if(swatproblem >0){}
  #}else if (SWATI[i] > SWATMX[i]){
  #
  #  if (SWATI[i] > SWATMX[i] + 0.00001) {
  #    if(swatproblem >0){}
  #  }else{
  #    ##     rounding error only
  #    SWATI[i] <<- SWATMX[i]
  #  }
  #}
}

#********************************************************************************
DOYF<-function(day,month, daymo){

  doyy<-0
  if(fnleap()){
    daymo[2]<-29
  }else{
    daymo[2]<-28
  }

  if(month>1)
    doyy<-daymo[1]+doyy
  if(month>2)
    doyy<-daymo[2]+doyy
  if(month>3)
    doyy<-daymo[3]+doyy
  if(month>4)
    doyy<-daymo[4]+doyy
  if(month>5)
    doyy<-daymo[5]+doyy
  if(month>6)
    doyy<-daymo[6]+doyy
  if(month>7)
    doyy<-daymo[7]+doyy
  if(month>8)
    doyy<-daymo[8]+doyy
  if(month>9)
    doyy<-daymo[9]+doyy
  if(month>10)
    doyy<-daymo[10]+doyy
  if(month>11)
    doyy<-daymo[11]+doyy
  if(month>12)
    doyy<-daymo[12]+doyy

  doyy<-doyy+day

  return(doyy)
}


#environment variables defined during model run; must be initialized to prevent setting of global variables
IInterValDay <- NULL
daymax <- NULL
precc <- NULL
evpp <- NULL
floww <- NULL
rnett <- NULL
ptrann <- NULL
irvpp <- NULL
isvpp <- NULL
snoww <- NULL
swatt <- NULL
pintt <- NULL
snvpp <- NULL
slvpp <- NULL
trandd <- NULL
mesfld <- NULL
smltd <- NULL
slfld <- NULL
rfald <- NULL
sfald <- NULL
awatt <- NULL
adeff <- NULL
sintdd <- NULL
rintdd <- NULL
rthrd <- NULL
sthrd <- NULL
rsnod <- NULL
SWCHECKED<<-FALSE
#theta <- NULL
#' Execute BROOK90 model in this environment
#'
#' @author R.Kronenberg as RK [06112017]  based on Brook90 by Federer et.al
#' @author TU Dresden, Institut für Hydrologie und Meteorologie, Professur für Meteorologie, 2017
execute <- function(){


  #B90<-function(){   ANMACHEN WIEDER
  #called and returned only from one location in msbrunB990

  #modified for Version 4, June 3, 1999
  #
  #  ^^^^ shows variables that are returned from or altered by subroutines (requires a fixed width font)
  #
  #intrinsic functions needed
  #  CSNG, INT, INKEY$, CHR$
  #
  #
  #program initializations if New Run or Rerun
  if ((runflag == 0) || (runflag == 1)){
    #
    DAYMO[1] <<- 31
    DAYMO[2] <<- 28
    DAYMO[3] <<- 31
    DAYMO[4] <<- 30
    DAYMO[5] <<- 31
    DAYMO[6] <<- 30
    DAYMO[7] <<- 31
    DAYMO[8] <<- 31
    DAYMO[9] <<- 30
    DAYMO[10] <<- 31
    DAYMO[11] <<- 30
    DAYMO[12] <<- 31
    IDAY <<-1 #11689#12054# 1
    IInterValDay <<- 1
    NDAYS <<- length(MData[[1]])#12418#length(MData[[1]])#12418#length(MData[[1]])
    NITSR <<- 0
    NITSY <<- 0
    NITSM <<- 0
    YEARN <<- as.numeric(MData[[3]][1])

    daymax <<- NDAYS-IDAY+1
    maxF <- 0
    precc <<- rep(0,daymax)
    evpp <<- rep(0,daymax)
    floww <<- rep(0,daymax)
    rnett <<- rep(0,daymax)
    ptrann <<- rep(0,daymax)
    irvpp <<- rep(0,daymax)
    isvpp <<- rep(0,daymax)
    snoww <<- rep(0,daymax)
    swatt <<- rep(0,daymax)
    pintt <<- rep(0,daymax)
    snvpp <<- rep(0,daymax)
    slvpp <<- rep(0,daymax)
    trandd <<- rep(0,daymax)
    mesfld <<- rep(0,daymax)
    smltd <<- rep(0,daymax)
    slfld <<- rep(0,daymax)
    rfald <<- rep(0,daymax)
    sfald <<- rep(0,daymax)
    awatt <<- rep(0,daymax)
    adeff <<- rep(0,daymax)
    sintdd <<- rep(0,daymax)
    rintdd <<- rep(0,daymax)
    rthrd <<- rep(0,daymax)
    sthrd <<- rep(0,daymax)
    rsnod <<- rep(0,daymax)
    #theta <<- rep(0,daymax)
    # change two-digit year to four-digit
    if( YEARN < 100){
      if(YEARN > 20){
        YEARN <<- YEARN + 1900
      }else{
        YEARN <<- YEARN + 2000
      }
    }
    #[rk] geändert 01022018
    #MONTHN <<- as.numeric(MData[[2]][1])
    #DOM <<- as.numeric(MData[[3]][1])
    #NPINT =1
    #DOY <<- as.POSIXlt(paste(sprintf("%02d", DOM),sprintf("%02d", MONTHN),YEARN,sep=""), format = "%d%m%y")$yday+1


    MONTHN <<- as.numeric(MData[[2]][IDAY])
    DOM <<- as.numeric(MData[[3]][IDAY])
    #NPINT =1
    DOY <<- DOYF(DOM,MONTHN,DAYMO)
    if (fnleap()) {
      DAYMO[2] <<- 29
    } else{
      DAYMO[2] <<- 28
    }
    #open dfile
    ##Close #1 # in case still open from previous run
    #Open frmmainB90.txtdfilename.Text For Input As #1
    #open prfile if needed
    if (SUBDAYDATA) {
      DTP <<- DT / NPINT
      #	Close #2 # in case still open from previous run
      #	Open frmmainB90.txtprfilename.Text For Input As #2
    }else{
      DTP <<- DT
    }
    #
    #zero accumulators
    zyear()
    zmonth()
    #
    #initial values
    SNOW <<- SNOWIN
    GWAT <<- GWATIN
    INTR <<- INTRIN
    INTS <<- INTSIN
    for( i in 1:NLAYER){
      PSIM[i] <<- PSIMIN[i]
    }
    #
    #soil water parameters and initial variables
    soilp <- SOILPAR()
    PSIG <<- unlist(soilp[2])
    SWATMX <<- unlist(soilp[3])
    WETF <<- unlist(soilp[4])
    WETC <<- unlist(soilp[5])
    CHM <<- unlist(soilp[6])
    CHN <<- unlist(soilp[7])
    WETNES <<- unlist(soilp[8])
    SWATI <<- unlist(soilp[9])
    KSAT <<- unlist(soilp[10])
    ###                                                                                                        ^^^^^^  ^^^^^^^^  ^^^^^^  ^^^^^^  ^^^^^  ^^^^^  ^^^^^^^^  ^^^^^^^  ^^^^^^
    #more initial soil water variables
    soil <- SOILVAR()
    PSITI <<- soil[1:ML]
    THETA <<- soil[(ML+1):(2*ML)]
    KK <<- soil[(2*ML+1):(3*ML)]
    SWAT <<- soil[(3*ML+1)]
    ###                                                                                     ^^^^^    ^^^^^    ^^    ^^^^
    #initial total water in system
    STORD <<- INTR + INTS + SNOW + SWAT + GWAT
    STORM <<- STORD
    STORY <<- STORD
    #any initial snow has zero liquid water and cold content
    CC <<- 0
    SNOWLQ <<- 0
  }
  #
  #parameter initializations for New Run, Rerun, and Continue Run
  #parameter conversions
  GLMAX <<- GLMAXC / 100#
  GLMIN <<- GLMINC / 100#
  LAT <<- LATD / 57.296
  ESLOPE <<- ESLOPED / 57.296
  DSLOPE <<- DSLOPED / 57.296
  ASPECT <<- ASPECTD / 57.296
  #equivalent slope for radiation calculations
  equi <- EQUIVSLP(LAT, ESLOPE, ASPECT)
  L1 <<- unlist(equi[1])
  L2 <<- unlist(equi[2])
  #                                  ^^  ^^
  #infiltration parameters
  infpa <- INFPAR(INFEXP, IDEPTH, NLAYER, THICK)
  ILAYER <<- unlist(infpa[1])
  INFRAC <<- unlist(infpa[2])
  #                                             ^^^^^^   ^^^^^^
  #source area parameters
  srfp <- SRFPAR(QDEPTH, NLAYER, THETAF, THICK, STONEF, SWATMX)
  QLAYER <<- unlist(srfp[1])
  SWATQX <<- unlist(srfp[2])
  SWATQF <<- unlist(srfp[3])
  #                                                                   ^^^^^^   ^^^^^^  ^^^^^^
  #root density parameterS
  RELDEN <<- RTDEN(ROOTDEN, NLAYER, THICK)
  #                                       ^^^^^^
  #
  #***************  B E G I N    D A Y   L O O P  ***************************
  #
  while( IDAY <= NDAYS){  # ANMACHEN WIEDER
    #

    #yield time to Windows - may not be needed
    #DoEvents
    #
    #initiate graphics window if necessary
    #	if Not graphon% And frmmainB90!chkgraph Then
    #If gnumber% = maxgraphs% Then
    #MsgBox "You have reached the maximum allowed number of graphs." & Chr$(10) & "Only Run - New Run is #allowed; this will clear all graphs."
    #rstop% = 4 # and run will stop
    #Else
    #gnumber% = gnumber% + 1
    #Call subgraph(FG(gnumber%), 1)
    #graphon% = True
    #End If
    #End If
    #test if run should stop here (end of day)
    #	if (rstop > 0) break
    #
    NITSD <- 0
    #
    #	if( IDAY == 1) {
    #read first data line from dfile, later lines are read at end of day loop
    subdatafileline(IDAY)
    #points(IDAY,TMIN,col="blue")
    #points(IDAY,TMAX,col="brown")
    #points(IDAY,TA,col="red")
    #points(IDAY,UW,col="yellow")
    #if( frmmainB90!chkdates) {  subcheckdata()
    #if (rstop = 5 ) break
    #		}
    #
    #visible display of progress
    #frmmainB90!lbldaynvalue = DOM%
    #frmmainB90!lblmonthnvalue = MONTHN%
    #frmmainB90!lblyearnvalue = YEARN%
    #frmmainB90.lbldorvalue = IDAY&

    if( IDAY == INIDAYS + 1){
      #end of initialization, reinitialize year and month accumulators
      STORD <<- INTR + INTS + SNOW + SWAT + GWAT
      STORM <<- STORD
      STORY <<- STORD
      NITSY <<- 0
      NITSM <<- 0
      zyear()
      zmonth()
    }
    #
    #calculate derived variables
    MSBSETVARS()
    #if( rstop = 3) break
    #
    #* * * * *  B E G I N   D A Y - N I G H T   E T   L O O P  * * * * * * * * *
    #
    #potential and actual interception, evaporation, and transpiration
    MSBDAYNIGHT()
    #
    #* * * * * * * * *  E N D   D A Y - N I G H T   L O O P  * * * * * * * * * *
    #
    #average rates over day
    PTRAN <<- (PTR[1] * DAYLEN + PTR[2] * (1 - DAYLEN)) / DT
    GEVP <<- (GER[1] * DAYLEN + GER[2] * (1 - DAYLEN)) / DT
    PINT <<- (PIR[1] * DAYLEN + PIR[2] * (1 - DAYLEN)) / DT
    GIVP <<- (GIR[1] * DAYLEN + GIR[2] * (1 - DAYLEN)) / DT
    for(i in 1:NLAYER){
      TRANI[i] <<- (ATRI[1, i] * DAYLEN + ATRI[2, i] * (1 - DAYLEN)) / DT
    }
    #TRAN from ATR(J%) is not needed
    #
    #zero daily integrators
    zday()
    #
    #* * * * * * * * B E G I N   P R E C I P   I N T E R V A L * * * * * * * * *
    #
    for( N in 1:NPINT){


      #ANMACHEN WIEDER
      #If (frmmainB90.txtprfilename.Text <> "None") Then
      #	#     precip data from prfile
      #	If EOF(2) Then
      #	If frmmainB90!chkdates Then
      ##	MsgBox "End of precip interval file before end of dfile. Run ends."
      #		GoTo endrun
      #	Else
      #	#restart prfile
      #	Close #2
      #	Open frmmainB90.txtprfilename.Text For Input As #2
      #	End If
      #End If

      #If frmmainB90!chkdates Then
      #     check PRFILE order
      #If (YEARN% <> YY% Or MONTHN% <> MM% Or DOM% <> DD% Or N% <> II%) Then
      #Call msbprferrortext
      #frmprferror.Show 1
      #GoTo endrun
      #End If
      #End If

      if (SUBDAYDATA){
        subprfileline(IInterValDay)
        #if MESFLP = -1 use MESFL/DT for all precip intervals in day
        #CHECK if there is als hourly data of precip
        if (MESFLP <= -0.01) {MESFLP <<- MESFL / DT}
        #MESFLP = MESFL / DT


      }else{
        #     precip data from data file
        PREINT <<- PRECIN / DT
        MESFLP <<- MESFL / DT
      }
      #
      #  interception and snow accumulation/melt
      MSBPREINT()

      #
      #  initialize for iterations
      #  initial time remaining in iteration time step = precip time step
      DTRI <<- DTP
      #  initialize iteration counter
      NITS <<- 0
      #
      #  zero precip interval integrators
      zpint()
      #
      #  *  *  *  *  *  *  B E G I N   I T E R A T I O N   *  *  *  *  *  *  *  *
      #


      while( DTRI > 0){  # 	print("Currently here")


        NITS <<- NITS + 1
        #     check for events
        if (NITS %% 100 == 0) {}
        #     test for Stop Iterations
        #if (rstop == 3) break
        #
        #     water movement through soil
        MSBITERATE() # iteration calculations
        #
        #     calculate SLFLI vertical macropore infiltration out of layer
        SLFLI[1] <<- SLFL - INFLI[1] - BYFLI[1]
        for (i in 2:ILAYER){ # does not execute if ILAYER% = 1 or 0
          if(i > 1) SLFLI[i] <<- SLFLI[i - 1] - INFLI[i] - BYFLI[i]
        }
        for( i in (ILAYER + 1):NLAYER){ # does not execute if NLAYER% < ILAYER% + 1
          if(NLAYER >= (ILAYER + 1)) SLFLI[i] <<- 0
        }
        #
        #     integrate below ground storages over iteration interval
        for( i in 1:NLAYER){
          SWATI[i] <<- SWATI[i] + NTFLI[i] * DTI
        }
        GWAT <<- GWAT + (VRFLI[NLAYER] - GWFL - SEEP) * DTI
        #
        #     new soil water variables and test for errors
        for (i in 1:NLAYER){


          swchek(i)
          if(SWCHECKED)break()
          #if(rstop== 3) break
          WETNES[i] <<- SWATI[i] / SWATMX[i]
          PSIM[i] <<- FPSIMF(WETNES[i], PSIF[i], BEXP[i], WETINF[i], WETF[i], CHM[i], CHN[i])
        }

        if(SWCHECKED)  break()

        soil <- SOILVAR()
        PSITI <<- soil[1:ML]
        THETA <<- soil[(ML+1):(2*ML)]
        KK <<- soil[(2*ML+1):(3*ML)]
        SWAT <<- soil[(3*ML+1)]
        ###                                                                                           ^^^^^    ^^^^^    ^^    ^^^^
        #     iteration output
        #	if (outselect[5] && IDAY > INIDAYS) ioutput()
        #     flows accumulated over precip interval
        paccum()
        #
        #     time remaining in precipitation time-step
        DTRI <<- DTRI - DTI
        #
        NITSR <<- NITSR + 1  # for visible display of iterations
        #	if( NITS Mod 200 == 0 )Then frmmainB90!lblitsrunvalue = NITSR&
        #DoEvents # to allow iteration update every 200 iterations
        #
      }
      #
      #  *  *  *  *   E N D   i T E R A T I O N    L O O P  *  *  *  *  *  *  *  *
      #
      #  display iterations
      #frmmainB90!lblitsrunvalue = NITSR&
      #DoEvents # to allow iterations update every precip interval
      #  integrate interception storages over precip interval
      INTS <<- INTS + (SINT - ISVP) * DTP
      INTR <<- INTR + (RINT - IRVP) * DTP
      #
      #  flows for precip interval summed from components
      psum()
      #  precipitation interval output
      #If outselect(4) And IDAY& > INIDAYS& Then poutput
      #  flows accumulated over day
      daccum()

      #  accumulate iterations
      NITSD <<- NITSD + NITS
      NITSM <<- NITSM + NITS
      NITSY <<- NITSY + NITS
      #
      IInterValDay <<- IInterValDay+1

      if(SWCHECKED) break()

    }
    #
    #* * * * *  E N D   P R E C I P   I N T E R V A L   L O O P  * * * * * * * *
    #
    #flows for day summed from components
    dsum()
    #
    #check for water balance error
    BALERD <<- STORD - (INTR + INTS + SNOW + SWAT + GWAT) + PRECD - EVAPD - FLOWD - SEEPD
    #If (Abs(BALERD) > 0.003) Then
    #   MsgBox "Run Stopped with water balance error >0.003. BALERD = " & BALERD
    #   GoTo endrun
    #End If
    STORD <<- INTR + INTS + SNOW + SWAT + GWAT

    #daily output, graphs only when requested
    #If outselect(3) And IDAY& > INIDAYS& Then Call doutput
    #If frmmainB90!chkgraph Then Call subgraph(FG(gnumber%), 2)
    #
    #flows accumulated over month
    maccum()
    #
    #If frmmainB90.chkdates Then
    #date checking on
    if(DOM == DAYMO[MONTHN]){
      #  end of month
      #  flows for month summed from components
      #msum()
      #  monthly output
      #If outselect(2) And IDAY& > INIDAYS& Then Call moutput
      #If frmmainB90!chkgraph Then Call subgraph(FG(gnumber%), 2)
      #  flows accumulated over year
      #yaccum()
      #  set up for next month
      zmonth()

      #MONTHN <<- MONTHN + 1
      assign("MONTHN", MONTHN + 1) #assign to current environment

      DOM <<- 0
      NITSM <<- 0
    }  #for end of month
    if (MONTHN == 13) {
      #  end of year
      #  flows for year summed from components
      # ysum()
      #  annual output
      #If outselect(1) And IDAY& > INIDAYS& Then Call youtput
      #If frmmainB90!chkgraph Then
      #graphon% = False
      #End If
      #  set up for next year
      MONTHN <<- 1
      DOM <<- 1
      DOY <<- 1
      YEARN <<- YEARN + 1
      zyear()
      if (fnleap() ){
        DAYMO[2] <<- 29
      }else{
        DAYMO[2] <<- 28
      }
      NITSY <<- 0
      NITSM <<- 0
    }

    #for end of year
    #If EOF(1) Then # End of data file with IDAY <= NDAYS
    #rstop% = 2  # will disable runcontinue
    #GoTo endrun
    #End If
    #set up for next day
    #[€rk] geändert seit 01022018
    #DOM <<- DOM + 1
    #DOY <<- DOY + 1
    IDAY <<- IDAY + 1

    MONTHN = as.numeric(MData[[2]][IDAY])
    DOM = as.numeric(MData[[3]][IDAY])
    YEARN = as.numeric(MData[[1]][IDAY])
    #NPINT =1
    if(IDAY <= NDAYS)
      DOY<<-DOYF(DOM,MONTHN,DAYMO)

    #* * * I N P U T   W E A T H E R   L I N E   F R O M   D F I L E * * *
    #subdatafileline()
    #Call subcheckdata

    #
    # ***************   E N D    D A Y   L O O P    **************************
    #

    #

    precc[daymax-NDAYS+IDAY-1] <<- PRECD
    evpp[daymax-NDAYS+IDAY-1] <<- EVAPD
    floww[daymax-NDAYS+IDAY-1] <<- FLOWD
    rnett[daymax-NDAYS+IDAY-1] <<- RNET
    irvpp[daymax-NDAYS+IDAY-1] <<- IRVPD
    isvpp[daymax-NDAYS+IDAY-1] <<- ISVPD
    ptrann[daymax-NDAYS+IDAY-1] <<- PTRAND
    snoww[daymax-NDAYS+IDAY-1] <<- SNOW
    swatt[daymax-NDAYS+IDAY-1] <<- SWAT
    pintt[daymax-NDAYS+IDAY-1] <<- PINTD
    snvpp[daymax-NDAYS+IDAY-1] <<- SNVPD
    slvpp[daymax-NDAYS+IDAY-1] <<- SLVPD
    trandd[daymax-NDAYS+IDAY-1] <<- TRAND
    mesfld[daymax-NDAYS+IDAY-1] <<- MESFLD
    smltd[daymax-NDAYS+IDAY-1] <<- SMLTD
    slfld[daymax-NDAYS+IDAY-1] <<- SLFLD
    rfald[daymax-NDAYS+IDAY-1] <<- RFALD
    awatt[daymax-NDAYS+IDAY-1] <<- AWAT
    adeff[daymax-NDAYS+IDAY-1] <<- ADEF
    sintdd[daymax-NDAYS+IDAY-1] <<- SINTD
    rintdd[daymax-NDAYS+IDAY-1] <<- RINTD
    sfald[daymax-NDAYS+IDAY-1] <<- SFALD
    rthrd[daymax-NDAYS+IDAY-1] <<- RTHRD
    sthrd[daymax-NDAYS+IDAY-1] <<- STHRD
    rsnod[daymax-NDAYS+IDAY-1] <<- RSNOD
    #theta[daymax-NDAYS+IDAY-1] <<- THETA

    if(SWCHECKED) break()


  } #End B90

}
