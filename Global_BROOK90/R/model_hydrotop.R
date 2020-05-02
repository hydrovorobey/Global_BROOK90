##### Call default BROOK90 envirinment, replace necessary pars (hydrotop specific), execute model #####
### 25/03/2020 update
### Ivan Vorobevskii
### Germany, TU Dresden, Institute of Hydrology and Meteorology, Chair of Meteorology

brook90.run.subcatchment = function(params, df.meteoFile, df.precFile, ts.results, package_dir) {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  package_dir = getwd()
  load(paste0(package_dir,'/Brook_utility/brook90.environment.RData'))
  source(paste0(package_dir,'/Brook_utility/brook90.utility.R'), local=brook90.environment)
  for(param in names(params)){
    brook90.environment[[param]] <- params[[param]]
  }
  brook90.environment$MData <- matrix(df.meteoFile)
  brook90.environment$MhhData <- matrix(df.precFile)
  brook90.environment$execute()
  return(mget(ts.results, brook90.environment))
}
