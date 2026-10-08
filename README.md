# alpine_communities
Vegetation and environmental data from long-term transects in Northeastern US alpine (New Hampshire and New York)

# File and Data Description 
# Code (.R)

Alpine_comm.R: Code for running data entry, cleaning, analysis, and data visualization

# Data (.csv) - with colum-level metadata (column name, data type, description, ancillary information)

MWO_raw: Full time series temperature data from summit of Mt. Washington collected by Mt. Washington Observatory.
  
  NAME (text) - name of site
  DATE (date) - month/day/year of observation
  TMAX (numeric) - daily max temperature in Celcius
  TMIN (numeric) - daily min temperature in Celcius
  TMEAN (numeric) - daily mean temperature in Celcius
  YEAR (numeric) - year of observation
  MONTH (numeric) - month of observation
  DAY (numeric) - day of month of observation

MWO_temp: Cleaned time series temperature data from summit of Mt. Washington collected by Mt. Washington Observatory.

NADP_sum: Chemistry data from four northeastern US NADP sites from late 1970's to present (nitrate, sulfate, ammonium, total organic carbon).

whf_chem: Chemistry data from Whiteface Mountain (NY) from the 1980's to present (nitrate, sulfate, ammonium, total organic carbon).

chem_raw: Chemistry data from Lakes of the Clouds site (NH) from the 1980's to present (nitrate, sulfate, ammonium, total organic carbon).

clound_ph: Cloud based pH data collected near Lakes of the Clouds Hut on Mt. Washington (NH) starting from 1980's to present.

Wright_2: Subset of transition states for the Adirondacks - just for visualization.

a_spp: Species trait information for just the Adirondack sites.

w_spp: Species trait information for just the White Mountain sites.

adk_tile: Subset of transition values for the Adirondacks - just for visualization.

wm_tile: Subset of transition values for the White Mountains - just for visualization.

alp_cov_matrix1: Matrix of species releative cover values for the Adirondacks.

alp_cov_matrix2: Matrix of species releative cover values for the White Mountains.

alpine_site_env: Site-level environmental data subset for all sites.

alpine_site_env_a: Site-level environmental data subset for just the Adirondack Mountains.

alpine_site_env_w: Site-level environmental data subset for just the White Mountains.

alpine_spp_trait: Species traits (stature, life form, and biogeographic group) for all species recorded in all alpine surveys.

master_alpine_comm_data: Matrix of species presence/absence values for all sites across all years.

source_data1: Raw line-interecpt data from historical NH-based surveys. Also used for transition values for visualization of White Mountain data.

# Data - Shapefile (.cpg, .dbf, .prj, .shp, .shx)

cb_2018_us_state_20m: Boundary shapefile for all US states and Canadian territories. Used for map in Figure 1 of associated publication. CRS = WGS84. Source = USGS.

# Data - Raster (.tif)

na_clip: Digital elevation model (DEM) of North America clipped to Northeastern region. Used for map in Figure 1 of associated publication. CRS = WGS84. Source = USGS. Spatial resolution = 30m, pixels = elevation value in meters.
