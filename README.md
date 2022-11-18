# Global_BROOK90
**_R-package for automatic modelling of water balance all over the world._** 

**Project page and up-to-date news** https://www.researchgate.net/project/Development-of-R-package-Global-BROOK90

**Main reference** - https://doi.org/10.3390/w12072037

**Stable version** - XXX

**Last version** (not stable, permanent updates) - XXX


The package is running from **Run_framework.R** file. *Pacman* package should be pre-installed. Important remark - keep your libraries up-to-date.

**The package incorporates:**
- *BROOK90*: physical lumped hydrological model with a special focus on a detailed representation of vertical water fluxes within the soil-water-plant system at a single site
- used global datasets: land cover (Land Cover 100 m), soil characteristics (SoilGrids250), elevation data (Amazon Web Service Terrain Tiles), meteorological forcing (ERA5, MERRA-2, ECMWF seasonal forecast), vegetation characteristics (MODIS LAI, Global Forest Canopy Height)
- modelling framework for auto download and process initial data, parameterize and run the model, process and save output results


Additional technical comments:
- framework is currently working in R-studio only
- before running the framework User needs to register in [Copernicus CDS](https://cds.climate.copernicus.eu/user/register?destination=%2F%23!%2Fhome), copy UID and API Key from the profile data to the framework input, acceps "Terms of use" in the bottom of [ERA5 download page](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form) (needs to be done only once)
- some issues with third-party R packages were detected (not solved) when working on MAC and Linux systems

Publications:

Vorobevskii, I. and Kronenberg, R.: ‘Drop a catchment and receive model output’: introduction to an open-source R-Package to model the water balance wherever you want, EGU General Assembly, Vienna, Austria (online), https://doi.org/10.5194/egusphere-egu2020-2767, 2020

Vorobevskii, I., Kronenberg, R., Bernhofer, C.: Global BROOK90 R Package: An Automatic Framework to Simulate the Water Balance at Any Location, Water, 12, https://doi.org/10.3390/w12072037, 2020

Vorobevskii, I., Kronenberg, R., Bernhofer, C.: On the runoff validation of ‘Global BROOK90’ automatic modeling framework. Hydrology Research, 52, 1083–1099, https://doi.org/10.2166/nh.2021.150, 2021

Vorobevskii, I. and Kronenberg, R.: Global BROOK90: validation, uncertainties, current progress and future outline., EGU General Assembly, Vienna, Austria (online), https://doi.org/10.5194/egusphereegu21-742, 2021

Vorobevskii, I., Luong, T.T., Kronenberg, R., Grünwald, T., Bernhofer, C.: Modelling evaporation with local, regional and global BROOK90 frameworks: importance of parameterization and forcing. Hydrology and Earth System Sciences, https://doi.org/10.5194/hess‐2021‐602, 2022
