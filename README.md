# Global_BROOK90
**R-package for automatic modelling of water balance all over the world.** 

Project page https://www.researchgate.net/project/Development-of-R-package-Global-BROOK90

Main reference - https://doi.org/10.3390/w12072037

The package is running from **Run_framework.R** file. *Pacman* package should be pre-installed. Important remark - keep your libraries up-to-date.

**The package incorporates:**
- *BROOK90*: physical lumped hydrological model with a special focus on a detailed representation of vertical water fluxes within the soil-water-plant system at a single site
- global datasets of land cover (*Land Cover 100 m*), soil characteristics (*SoilGrids250*), elevation data (*Amazon Web Service Terrain Tiles*) and meteorological forcing (*ERA5*)
- modelling framework for auto download and process initial data, parameterize and run the model, process and save output results

Methodology and examples are presented in this paper [missing] 

Additional technical comments:
- framework is currently working in R-studio only
- before running the framework User needs to register in [Copernicus CDS](https://cds.climate.copernicus.eu/user/register?destination=%2F%23!%2Fhome), copy UID and API Key from the profile data to the framework input, acceps "Terms of use" in the bottom of [ERA5 download page](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form) (needs to be done only once)
- some issues with third-party R packages were detected (not solved) when working on MAC system
