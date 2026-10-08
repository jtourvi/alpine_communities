# alpine_communities
Vegetation and environmental data from long-term transects in Northeastern US alpine (New Hampshire and New York)

# File and Data Description 
# Code (.R)

Alpine_comm.R: Code for running data entry, cleaning, analysis, and data visualization

# Data (.csv) - with colum-level metadata (column name, data type, description, ancillary information). Note that all missing data represented with NAs.

MWO_raw: Full time series temperature data from summit of Mt. Washington collected by Mt. Washington Observatory.
  
  -NAME (text, categorical): name of site.
  -DATE (date): month/day/year of observation.
  -TMAX (numeric): daily max temperature in Celcius.
  -TMIN (numeric): daily min temperature in Celcius.
  -TMEAN (numeric): daily mean temperature in Celcius.
  -YEAR (numeric): year of observation.
  -MONTH (numeric): month of observation.
  -DAY (numeric): day of month of observation.

MWO_temp: Cleaned time series temperature data from summit of Mt. Washington collected by Mt. Washington Observatory.

  -Year (numeric): year of observation.
  -Annual_Avg (numeric): annual mean temperature in Celcius.
  -Spring (numeric): annual spring mean temperature in Celcius.
  -Summer (numeric): annual summer mean temperature in Celcius.
  -Fall (numeric): annual fall mean temperature in Celcius.
  -Winter (numeric): annual winter mean temperature in Celcius.

NADP_sum: Chemistry data from four northeastern US NADP sites from late 1970's to present (total nitrogen).

  -Year (numeric): year of observation.
  -Bennington (numeric): measured annual mean total nitrogen deposition in kg/ha at Bennington NADP site.
  -Hubbard Brook (numeric): measured annual mean total nitrogen deposition in kg/ha at Hubbard Brook NADP site.
  -Underhill (numeric): measured annual mean total nitrogen deposition in kg/ha at Underhill NADP site.
  -Whiteface Mountain (numeric): measured annual mean total nitrogen deposition in kg/ha at Whiteface Mountain NADP site.
  
whf_chem: Chemistry data from Whiteface Mountain (NY) from the 1980's to present (nitrate, sulfate, ammonium, total organic carbon).

  -Year (numeric): year of observation.
  -SO4 (numeric): measured annual mean SO4 deposition in mg/L.
  -NO3 (numeric): measured annual mean NO3 deposition in mg/L.
  -NH4 (numeric): measured annual mean NH4 deposition in mg/L.
  -TOC (numeric): measured annual mean TOC deposition in mg/L.

chem_raw: Chemistry data from Lakes of the Clouds site (NH) from the 1980's to present (nitrate, sulfate, ammonium, total organic carbon).

  -Year (numeric): year of observation.
  -Type (text, categorical): type of measurement source, CLOUD vs. RAIN.
  -SO4 (numeric): measured annual mean SO4 deposition in mg/L.
  -NO3 (numeric): measured annual mean NO3 deposition in mg/L.
  -NH4 (numeric): measured annual mean NH4 deposition in mg/L.
  -TOC (numeric): measured annual mean TOC deposition in mg/L.

clound_ph: Cloud based pH data collected near Lakes of the Clouds Hut on Mt. Washington (NH) starting from 1980's to present.

  -Year (numeric): year of observation.
  -v (text, categorical): type of measurement, PH.
  -CLOUD (numeric): measured annual mean pH in cloudwater.
  -RAIN (numeric): measured annual mean pH in rainwater.

Wright_2: Subset of transition states for the Adirondacks - just for visualization.

  -ID (text, categorical): name of sampling point along each transect for each ADK mountain.
  -1994 (text, categorical): measured species ID.
  -2002 (text, categorical): measured species ID.
  -2007 (text, categorical): measured species ID.
  -2017 (text, categorical): measured species ID.

a_spp: Species NMDS ordination information for just the Adirondack sites - just for visualization.

  -NMDS1 (numeric): NMDS axis 1 coordinate.
  -NMDS2 (numeric): NMDS axis 2 coordinate.
  -species (text, categorical): measured species ID.

w_spp: Species NMDS ordination information for just the White Mountain sites - just for visualization.

  -NMDS1 (numeric): NMDS axis 1 coordinate.
  -NMDS2 (numeric): NMDS axis 2 coordinate.
  -species (text, categorical): measured species ID.

adk_tile: Subset of transition values for the Adirondacks - just for visualization.

  -range (text, categorical): name of mountain range.
  -mountain (text, categorical): name of mountain, Algonquin, Wright, Iroquois.
  -group (text, categorical): name of comparison type, Stature (plant height), Form (plant growth form), Group (plant biogeographic type).
  -variable (text, categorical): name of plant trait corresponding to group column. 
  -year (numeric): year of survey.
  -value (numeric): relative frequency of variable (range between 0-1).

wm_tile: Subset of transition values for the White Mountains - just for visualization.

  -range (text, categorical): name of mountain range.
  -mountain (text, categorical): name of mountain, Washington, Lafayette.
  -group (text, categorical): name of comparison type, Stature (plant height), Form (plant growth form), Group (plant biogeographic type).
  -variable (text, categorical): name of plant trait corresponding to group column. 
  -year (numeric): year of survey.
  -value (numeric): relative frequency of variable (range between 0-1).

alp_cov_matrix1: Matrix of species releative cover values for the Adirondacks.

  -site (text, categorical): transect name.
  -year (numeric): year of survey.
  -Column C to Column BH (numeric): relative frequency of species within transect (range between 0-1). Each column reflects a unique plant species within community matrix.

alp_cov_matrix2: Matrix of species releative cover values for the White Mountains.

  -site (text, categorical): transect name.
  -year (numeric): year of survey.
  -Column C to Column BH (numeric): relative frequency of species within transect (range between 0-1). Each column reflects a unique plant species within community matrix.

alpine_site_env: Site-level environmental data subset for all sites.

  -id (text, categorical): unique identifier for transect.
  -mountain (text, categorical): name of mountain.
  -site (text, categorical): name of site name or transect name within mountain.
  -state (text, categorical): US state of measurement using 2-letter abbreviation.
  -measure (text, categorical): survey type used, line vs. point intercept.
  -Year (numeric): year of survey.
  -site_year (text, categorical): unique identifier for transect and year.
  -name (text, categorical): name of mountain for visualization.
  -period (text, categorical): survey period, 1983-2003 vs. 2004-2024.
  -SWE (numeric): annual total snow water equivalent in cm. 
  -Tmax (numeric): annual max temperature in Celcius.
  -Tmin (numeric): annual min temperature in Celcius.
  -Tmean (numeric): annual mean temperature in Celcius.
  -TotalN (numeric): measured annual mean total nitrogen deposition in kg/ha near site.
  -2D (numeric): relative frequency of 2D plants for unique ID transect/survey.
  -3D (numeric): relative frequency of 3D plants for unique ID transect/survey.
  -Arctic (numeric): relative frequency of Arctic plants for unique ID transect/survey.
  -Non-Arctic (numeric): relative frequency of Non-Arctic plants for unique ID transect/survey.
  -Transitional (numeric): relative frequency of Transitional plants for unique ID transect/survey. 
  -Non-Plant (numeric): relative frequency of Non-Plant observations for unique ID transect/survey.
  -Shrub (numeric): relative frequency of Shrub plants for unique ID transect/survey.

alpine_site_env_a: Site-level environmental data subset for just the Adirondack Mountains (same structure as alpine_site_env).

alpine_site_env_w: Site-level environmental data subset for just the White Mountains (same structure as alpine_site_env).

alpine_spp_trait: Species traits (stature, life form, and biogeographic group) for all species recorded in all alpine surveys.

  -species (text, categorical): species ID.
  -group (text, categorical): name of comparison type, plant biogeographic type.
  -stature (numeric, categorical): name of comparison type, plant height.
  -life_form (text, categorical): name of comparison type, plant growth form. 

master_alpine_comm_data: Matrix of species presence/absence values for all sites across all years.

  -mountain (text, categorical): name of mountain.
  -site (text, categorical): name of site name or transect name within mountain.
  -state (text, categorical): US state of measurement using 2-letter abbreviation.
  -measure (text, categorical): survey type used, line vs. point intercept.
  -year (numeric): year of survey.
  -id (text, categorical): unique identifier for mountain, transect, and year.
  -species1 (text, categorical): species ID.
  -count (numeric): number of points species intersects along transect.
  -total_count (numeric): total number of possible intersects along transect. 
  -rel_freq (numeric): relativized occurrence frequency of species within transect (range 0-1).
  -presence (binary): presence/absence of species within transect (0 = absent, 1 = present).
  -rel_point_count (numeric, integer): relativized number of point intersects for species along transect.
  -species (text, categorical): species ID for visualization.
  -mountain1 (text, categorical): name of mountain for visualization.

source_data1: Raw line-interecpt data from historical NH-based surveys. Also used for transition values for visualization of White Mountain data.

  -Year (numeric): year of survey.
  -Site (text, categorical): name of site name or transect name within mountain.
  -Transect (text, categorical): name of unique transect within mountain.
  -round (numeric, integer): point intercept distance along transect in gradations of 10cm.
  -group (text, categorical): name of comparison type, plant biogeographic type.
  -stature (numeric, categorical): name of comparison type, plant height.
  -life_form (text, categorical): name of comparison type, plant growth form. 
  -id (text, categorical): unique identifier for mountain, year, and transect distance.
  -uid (text, categorical): unique identifier for mountain, transect, and transect distance.
  -year (text, categorical): year of survey for data pasting purposes. 

# Data - Shapefile (.cpg, .dbf, .prj, .shp, .shx)

cb_2018_us_state_20m: Boundary shapefile for all US states and Canadian territories. Used for map in Figure 1 of associated publication. CRS = WGS84. Source = USGS.

# Data - Raster (.tif)

na_clip: Digital elevation model (DEM) of North America clipped to Northeastern region. Used for map in Figure 1 of associated publication. CRS = WGS84. Source = USGS. Spatial resolution = 30m, pixels = elevation value in meters.
