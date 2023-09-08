# Global_BROOK90: **_R-package for automatic modelling of water balance all over the world._** 

[![Github All Releases](https://img.shields.io/github/downloads/hydrovorobey/Global_BROOK90/total.svg)]()

**Licence** Attribution-NonCommercial-NoDerivatives 4.0 International (CC BY-NC-ND 4.0)

**Main reference** - https://doi.org/10.3390/w12072037

**Reanalysis+Forecast version** - https://github.com/hydrovorobey/Global_BROOK90/releases/tag/v2.1


**Principal scheme of v2.1**
![fig 3](https://user-images.githubusercontent.com/25793656/202659423-caae90de-67bc-420e-8ad6-adf454a8f549.jpg)


*The package incorporates:*
- BROOK90 physical lumped water balance model with a special focus on a detailed representation of vertical water fluxes within the atmosphere-plant-soil system at a single site
- Connection to multiply global datasets: land cover (Land Cover 100 m), soil characteristics (SoilGrids250), elevation data (Amazon Web Service Terrain Tiles), meteorological forcing (ERA5, MERRA-2, ECMWF seasonal forecast), vegetation characteristics (MODIS LAI, Global Forest Canopy Height)
- Automatic framework to download and process initial data, parameterize and run the model, process and save output results


*Technical remarks*:
- package is running from *Run_framework.R* file and does not require installation (but require build-up procedure in R-studio), only *Pacman* package should be pre-installed (rest libraries will be installed automatically)
- keep third-party R libraries up-to-date (check_packages_updates parameter of main function)
- framework is currently working in R-studio only, a web-based application is planned
- before running the framework User needs to register in [Copernicus CDS](https://cds.climate.copernicus.eu/user/register?destination=%2F%23!%2Fhome), copy UID and API Key from the profile data to the framework input, acceps "Terms of use" in the bottom of [ERA5 download page](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form) (needs to be done only once)
- some issues with third-party R libraries were detected (not solved) when working on MAC and Linux systems


*Publications*:
- Vorobevskii, I., Luong, T.T., Kronenberg, R.: Seasonal forecasting of local-scale soil moisture droughts with Global BROOK90, https://doi.org/10.5194/nhess-2023-9, 2023
- Vorobevskii, I., Luong, T.T., Kronenberg, R., Grünwald, T., Bernhofer, C.: Modelling evaporation with local, regional and global BROOK90 frameworks: importance of parameterization and forcing. Hydrology and Earth System Sciences, https://doi.org/10.5194/hess-26-3177-2022, 2022
- Vorobevskii, I. and Kronenberg, R.: ‘Drop a catchment and receive model output’: introduction to an open-source R-Package to model the water balance wherever you want, EGU General Assembly, Vienna, Austria (online), https://doi.org/10.5194/egusphere-egu2020-2767, 2020
- Vorobevskii, I., Kronenberg, R., Bernhofer, C.: Global BROOK90 R Package: An Automatic Framework to Simulate the Water Balance at Any Location, Water, 12, https://doi.org/10.3390/w12072037, 2020
- Vorobevskii, I., Kronenberg, R., Bernhofer, C.: On the runoff validation of ‘Global BROOK90’ automatic modeling framework. Hydrology Research, 52, 1083–1099, https://doi.org/10.2166/nh.2021.150, 2021
- Vorobevskii, I. and Kronenberg, R.: Global BROOK90: validation, uncertainties, current progress and future outline., EGU General Assembly, Vienna, Austria (online), https://doi.org/10.5194/egusphereegu21-742, 2021
