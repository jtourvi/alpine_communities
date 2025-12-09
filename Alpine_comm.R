###AMC Alpine Community Study Stats and Figures - Ecosphere
#updated 12/9/2025

#"Alpine Vegetation Shifts in the Northeastern US: Evidence for Combined Climate and Nitrogen Forcing"

#setwd here

#required packages
require(ggplot2)
require(reshape)
require(vegan)
require(viridis)
require(plyr)
require(tidyr)
require(dplyr)
require(MASS)
library(indicspecies)
require(betapart)
require(alluvial)
require(ggalluvial)
require(ggpp)

##Cloud and Rain Chemistry
#NADP Data - read data and plotting

nadp = read.csv("NADP_sum.csv", header = TRUE)
nadp1 = melt(nadp, id = "Year")

n_plot = ggplot(nadp1, aes(x = Year, y = value, group = variable)) +
  geom_line(aes(color = variable, lty = variable), lwd = 1) +
  guides(lty = FALSE) +
  geom_point(aes(color = variable), size = 2) +
  geom_smooth(aes(color = variable), method = "lm", se = FALSE, lwd = 1.5) +
  scale_colour_viridis_d(name = "Station", labels = c("Bennington", "Hubbard Brook",
                                                      "Underhill", "Whiteface")) +
  ylab("Inorganic N (kg/ha)") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.8))
n_plot

tiff(file = "nadp_plot.tiff", width = 6, height = 5, units = 'in', res = 600, pointsize = 11)
n_plot
dev.off()

#Temperature - Mt. Washington
#Read data, plotting, basic linear trends

mwo = read.csv("MWO_temp.csv", header = TRUE)

lm1 = lm(Annual_Avg ~ Year, data = mwo)
summary(lm1)

lm2 = lm(Spring ~ Year, data = mwo)
summary(lm2)

lm3 = lm(Summer ~ Year, data = mwo)
summary(lm3)

lm4 = lm(Fall ~ Year, data = mwo)
summary(lm4)

lm5 = lm(Winter ~ Year, data = mwo)
summary(lm5)

mwo$ann_mean = mean(mwo$Annual_Avg)
mwo$ann_anom = (mwo$Annual_Avg - mwo$ann_mean)

mean_mwo = ggplot(mwo, aes(x = Year, y = Annual_Avg, color = ann_anom)) +
  geom_point(size = 3) +
  scale_color_gradient(low="blue", high="red", name = "Anomaly (C\u00b0)") +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Year (1935-2024)") +
  ylab("Mean Temperature (C\u00b0)") +
  ggtitle("Annual") +
  theme_bw() +
  xlim(1925, 2025) +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black", linewidth = 1)) +
  annotate("text", x=1942, y=-0, label= "Slope = 0.014***")
mean_mwo

mwo1 = melt(mwo, id = "Year")
mwo2 = subset(mwo1, variable == "Spring" | variable == "Summer" |
                variable == "Fall" | variable == "Winter")

mwo3 = ddply(mwo2, ~variable, summarise, mean1 = mean(value, na.rm = TRUE))
mwo4 = merge(mwo2, mwo3, by = "variable")

mwo4$anom = (mwo4$value - mwo4$mean1)

mwo4$variable <- factor(mwo4$variable, levels = c("Spring", "Summer", "Fall", "Winter"))

dat_text <- data.frame(
  label = c("Slope = 0.014**", "Slope = 0.008*", "Slope = 0.015**", "Slope = 0.013*"),
  variable   = c("Spring", "Summer", "Fall", "Winter"),
  x     = c(2.5, 11, 3, -9),
  y     = c(1945, 1945, 1945, 1945))

season_mwo = ggplot(mwo4, aes(x = Year, y = value, color = anom)) +
  geom_point() +
  scale_color_gradient(low="blue", high="red", name = "Anomaly (C\u00b0)") +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Year (1935-2024)") +
  ylab("Mean Temperature (C\u00b0)") +
  ggtitle("Season") +
  xlim(1925, 2025) +
  theme_bw() +
  facet_wrap(~variable, ncol=2, scales = "free") +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black", linewidth = 1))
season_mwo

mwo_all = (mean_mwo / season_mwo) +
  plot_annotation(tag_levels = "A") 
mwo_all

tiff(file = "mwo_all.tiff", width = 6, height = 8, units = 'in', res = 600, pointsize = 11)
mwo_all
dev.off()

#pH + Local Clound Chemistry, Rain
#Read data, plotting

chem = read.csv("chem_raw.csv", header = TRUE)

chem1 = melt(chem, id = c("Year", "Type"))

chem2 = ddply(chem1, ~Year + Type + variable, summarise, var = mean(value, na.rm = TRUE),
              ses = sd(value, na.rm = TRUE) / sqrt(length(value)))

chem3 = ddply(chem2, ~Type + variable, summarise, mean1 = mean(var, na.rm = TRUE))
chem4 = merge(chem2, chem3, by = c("Type", "variable"))

chem4$anom = (chem4$var - chem4$mean1)

ph = read.csv("clound_ph.csv", header = TRUE)
ph2 = melt(ph, id = c("Year", "v"))

#chem3 = rbind(chem2, ph2)

chem_plot = ggplot(chem4, aes(x = Year, y = var, color = anom)) +
  geom_point(size = 2) +
  scale_color_gradient(low="blue", high="red", name = "Anomaly (mg/L)") +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Year (1995-2024)") +
  ylab("mg/L") +
  ggtitle("Washington") +
  xlim(1990,2025) +
  theme_bw() +
  facet_grid(vars(Type), vars(variable), scales = "free") +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black", linewidth = 1)) +
  theme(legend.position="bottom", legend.box = "horizontal")
chem_plot

ph3 = ddply(ph2, ~Type, summarise, mean1 = mean(var, na.rm = TRUE))
ph4 = merge(ph2, ph3, by = "Type")
ph4$anom = (ph4$var - ph4$mean1)
ph4$variable = "pH"

ph_plot = ggplot(ph4, aes(x = Year, y = var, color = anom)) +
  geom_point(size = 2) +
  scale_color_gradient(low="blue", high="red", name = "Anomaly") +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Year (1995-2024)") +
  ylab("pH") +
  ggtitle("") +
  #ylim(0,10) +
  theme_bw() +
  facet_grid(vars(Type), vars(variable), scales = "free") +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black", linewidth = 1)) +
  theme(legend.position="bottom", legend.box = "horizontal")
ph_plot

chem_all = (chem_plot | ph_plot) + plot_layout(widths = c(3, 1))
chem_all

tiff(file = "chem_all.tiff", width = 9, height = 5, units = 'in', res = 600, pointsize = 11)
chem_all
dev.off()

#####WHF Chemisrtry Data - plotting

whf = read.csv("whf_chem.csv", header = TRUE)
whf1 = melt(whf, id = "Year")

whf2 = ddply(whf1, ~variable, summarise, mean1 = mean(value, na.rm = TRUE))
whf3 = merge(whf1, whf2, by = "variable")
whf3$anom = (whf3$value - whf3$mean1)

w_plot = ggplot(whf3, aes(x = Year, y = value, color = anom)) +
  geom_point(size = 2) +
  scale_color_gradient(low="blue", high="red", name = "Anomaly (mg/L)") +
  geom_line(lwd = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  xlab("Year (1994-2022)") +
  ylab("mg/L") +
  #ylab("mg/L") +
  ggtitle("Whiteface") +
  xlim(1990,2025) +
  theme_bw() +
  facet_wrap(vars(variable), scales = "free", ncol = 4) +
  theme(panel.border = element_rect(fill = "transparent",
                                    color = "black", linewidth = 1)) +
  theme(legend.position="bottom", legend.box = "horizontal")
w_plot

chem_all = (chem_plot / w_plot) + plot_layout(heights = c(2.5, 1))
chem_all

c_new = (chem_all | ph_plot) + plot_annotation(tag_levels = "A") + plot_layout(widths = c(2, 1))
c_new

tiff(file = "chem_n.tiff", width = 11, height = 8, units = 'in', res = 600, pointsize = 12)
c_new
dev.off()

###Map of sites - Figure 1
#required packages
require(sf)
require(tidyverse)
require(rgdal)
require(maps)
require(raster)
require(cowplot)

# The input file geodatabase - change
fgdb1 <- "C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Documents/ArcGIS/Projects/MyProject1.gdb/"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb1)
print(fc_list)

# Read the feature class
states1 <- readOGR(dsn=fgdb1,layer="cb_2018_us_sta_ExportF_Union")
a_ras <- readOGR(dsn=fgdb1,layer="RasterT_Alp_ras1")

# Determine the FC extent, projection, and attribute information
summary(states)

# View the feature class
plot(states1)
plot(a_ras)

dma.df1 <- fortify(states1)
al <- fortify(a_ras)

#rasters

elev <- raster("C:/Users/jtourville/OneDrive - Appalachian Mountain Club/Desktop/elev_clip.tif")
test_spdf1 <- as(elev, "SpatialPixelsDataFrame")
test_df1 <- as.data.frame(test_spdf1)
colnames(test_df1) <- c("value", "x", "y")

big_map = ggplot() + 
  geom_polygon(data=dma.df1, aes(x=long, y=lat, group=group), color = "black", linewidth = 0.6, fill = "white") +
  geom_polygon(data=al, aes(x=long, y=lat, group=group), color = "black", linewidth = 0.5, fill = "red") +
  geom_tile(data=test_df1, aes(x=x, y=y, fill=value), alpha=0.7) +
  #geom_line(data = al, aes(x=long, y=lat, group=group), color = "red", linewidth = 0.5, lty = "dashed") +
  #coord_map(clip = "off") +
  #geom_point(data = a_point, aes(x = long, y = lat), color = "red", size = 5, pch = 17) +
  coord_cartesian(xlim = c(-78, -67), ylim = c(42.5, 47.5)) +
  scale_fill_viridis(name = "Elevation \n(m a.s.l.)", option = "C", breaks = c(0,400,800,1200,1600,2000),
                     limits = c(0, 2000)) +
  #xlim(-92,-66)+
  #ylim(30,47.5) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
big_map

tiff(file = "big_map.tiff", width = 7.5, height = 4.5, units = 'in', res = 600, pointsize = 11)
big_map
dev.off()

##################################Alpine communities

###community data - read data

alp = read.csv("master_alpine_comm_data.csv", header = TRUE)
env = read.csv("alpine_site_env.csv", header = TRUE)
spp = read.csv("alpine_spp_trait.csv", header = TRUE)
alp_spp = merge(alp, spp, by = "species")
alp_g = ddply(alp_spp, ~ id + group, summarise, sum1 = sum(rel_freq))#stature grouping
alp_s = ddply(alp_spp, ~ id + stature, summarise, sum2 = sum(rel_freq))#group grouping
alp_l = ddply(alp_spp, ~ id + life_form, summarise, sum3 = sum(rel_freq))#life form grouping

a_ny = subset(alp, state == "NY")#break up by range
a_nh = subset(alp, state == "NH")

alp_s_matrix1 = cast(a_ny, id ~ species, max, value = "presence")#presence/absence comm matrix
is.na(alp_s_matrix1)<-sapply(alp_s_matrix1, is.infinite)
alp_s_matrix1[is.na(alp_s_matrix1)]<-0

alp_s_matrix2 = cast(a_nh, id ~ species, max, value = "presence")
is.na(alp_s_matrix2)<-sapply(alp_s_matrix2, is.infinite)
alp_s_matrix2[is.na(alp_s_matrix2)]<-0

alp_cov_matrix = cast(alp, id ~ species, sum, value = "rel_freq")#percent cover comm matrix
alp_cov_matrix1 = cast(a_ny, id ~ species, sum, value = "rel_freq")
alp_cov_matrix2 = cast(a_nh, id ~ species, sum, value = "rel_freq")

###Transitions between functional types: 2D vs 3D, distribution and group.

#mean group proportions visualization - tables and plots - by state

#NY
alp_group = ddply(alp_ny, ~mountain1 + site + year + group, summarise, rel_f = sum(rel_freq))
alp_stature = ddply(alp_ny, ~mountain1 + site + year + stature, summarise, rel_f = sum(rel_freq))
alp_form = ddply(alp_ny, ~mountain1 + site + year + life_form, summarise, rel_f = sum(rel_freq))

alp_group1 = ddply(alp_group, ~ mountain1 + year + group, summarise, rel_freq = mean(rel_f))
alp_stat1 = ddply(alp_stature, ~ mountain1 + year + stature, summarise, rel_freq = mean(rel_f, na.rm = TRUE))
alp_form1 = ddply(alp_form, ~ mountain1 + year + life_form, summarise, rel_freq = mean(rel_f))

# format data - stature
orderedclasses <- c("2", "3")
alp_stat1$stature <- factor(alp_stat1$stature, levels = orderedclasses)

#plot - ADK stature

gg1 <- ggplot(alp_stat1, aes(x=year, y=stature, fill=rel_freq))
gg1 <- gg1 + geom_tile(color="white", size=0.1)
gg1 <- gg1 + geom_text(aes(label = round(rel_freq, 2)), color = "white", size = 5)
gg1 <- gg1 + scale_fill_viridis(name="Relative Frequency")
gg1 <- gg1 + xlab("Year")
gg1 <- gg1 + ylab("")
gg1 <- gg1 + ggtitle("Adirondacks")
gg1 <- gg1 + coord_equal()
gg1 <- gg1 + facet_wrap(~mountain1, ncol=3)
gg1 <- gg1 + theme(axis.ticks=element_blank())
gg1 <- gg1 + theme(axis.text=element_text(size=12))
gg1 <- gg1 + theme(axis.title =element_text(size=14))
gg1 <- gg1 + theme(panel.border=element_blank())
gg1 <- gg1 + theme(plot.title=element_text(hjust=0))
gg1 <- gg1 + theme(strip.text=element_text(hjust=0))
gg1 <- gg1 + theme(strip.text=element_text(size = 14))
gg1 <- gg1 + theme(legend.title=element_text(size=12))
gg1 <- gg1 + theme(legend.title.align=1)
gg1 <- gg1 + theme(legend.text=element_text(size=10))
gg1 <- gg1 + theme(legend.position="none")
gg1 <- gg1 + theme(legend.key.size=unit(0.2, "cm"))
gg1 <- gg1 + theme(legend.key.width=unit(1, "cm"))
gg1

# format data - group
#orderedclasses <- c("2", "3")
#alp_group1$group <- factor(alp_group1$group, levels = orderedclasses)
#alp_stat1$id = paste(alp_stature$mountain, alp_stature$site ,sep="-")
#str(alp_group1)

#plot - ADK group

alp_group1$group[alp_group1$group == 'transitional'] <- 'Transitional'
alp_group1$group[alp_group1$group == 'non-plant'] <- 'Non-Plant'
alp_group1$group[alp_group1$group == 'non-arctic'] <- 'Non-Arctic'
alp_group1$group[alp_group1$group == 'arctic'] <- 'Arctic'

gg2 <- ggplot(alp_group1, aes(x=year, y=group, fill=rel_freq))
gg2 <- gg2 + geom_tile(color="white", size=0.1)
gg2 <- gg2 + geom_text(aes(label = round(rel_freq, 2)), color = "white", size = 4)
gg2 <- gg2 + scale_fill_viridis(name="Relative Frequency")
gg2 <- gg2 + xlab("Year")
gg2 <- gg2 + ylab("")
gg2 <- gg2 + ggtitle("")
gg2 <- gg2 + coord_equal()
gg2 <- gg2 + facet_wrap(~mountain1, ncol=3)
gg2 <- gg2 + theme(axis.ticks=element_blank())
gg2 <- gg2 + theme(axis.text=element_blank())
gg2 <- gg2 + theme(axis.title =element_blank())
gg2 <- gg2 + theme(panel.border=element_blank())
gg2 <- gg2 + theme(plot.title=element_text(hjust=0))
gg2 <- gg2 + theme(strip.text=element_text(hjust=0))
gg2 <- gg2 + theme(strip.text=element_text(size=14))
gg2 <- gg2 + theme(legend.title=element_text(size=12))
gg2 <- gg2 + theme(legend.title.align=1)
gg2 <- gg2 + theme(legend.text=element_text(size=10))
gg2 <- gg2 + theme(legend.position="none")
gg2 <- gg2 + theme(legend.key.size=unit(0.2, "cm"))
gg2 <- gg2 + theme(legend.key.width=unit(1, "cm"))
gg2

# format data - form
#orderedclasses <- c("2", "3")
#alp_form1$life_form <- factor(alp_form1$life_form, levels = orderedclasses)
#alp_stat1$id = paste(alp_stature$mountain, alp_stature$site ,sep="-")

#plot - ADK form

alp_form1$life_form[alp_form1$life_form == 'tree'] <- 'Tree'
alp_form1$life_form[alp_form1$life_form == 'shrub'] <- 'Shrub'
alp_form1$life_form[alp_form1$life_form == 'non_plant'] <- 'Non-Plant'
alp_form1$life_form[alp_form1$life_form == 'lyco'] <- 'Lycophyte'
alp_form1$life_form[alp_form1$life_form == 'lichen'] <- 'Lichen'
alp_form1$life_form[alp_form1$life_form == 'gram'] <- 'Graminoid'
alp_form1$life_form[alp_form1$life_form == 'forb'] <- 'Forb'
alp_form1$life_form[alp_form1$life_form == 'bryo'] <- 'Moss'

gg4 <- ggplot(alp_form1, aes(x=year, y=life_form, fill=rel_freq))
gg4 <- gg4 + geom_tile(color="white", size=0.1)
gg4 <- gg4 + geom_text(aes(label = round(rel_freq, 2)), color = "white", size = 4)
gg4 <- gg4 + scale_fill_viridis(name="Relative Frequency")
gg4 <- gg4 + xlab("Year")
gg4 <- gg4 + ylab("")
gg4 <- gg4 + ggtitle("")
gg4 <- gg4 + coord_equal()
gg4 <- gg4 + facet_wrap(~mountain1, ncol=3)
gg4 <- gg4 + theme(axis.ticks=element_blank())
gg4 <- gg4 + theme(axis.text=element_text(size=12))
gg4 <- gg4 + theme(axis.title=element_blank())
gg4 <- gg4 + theme(panel.border=element_blank())
gg4 <- gg4 + theme(plot.title=element_text(hjust=0))
gg4 <- gg4 + theme(strip.text=element_text(hjust=0))
gg4 <- gg4 + theme(strip.text=element_text(size=14))
gg4 <- gg4 + theme(legend.title=element_text(size=12))
gg4 <- gg4 + theme(legend.title.align=1)
gg4 <- gg4 + theme(legend.text=element_text(size=10))
gg4 <- gg4 + theme(legend.position="none")
gg4 <- gg4 + theme(legend.key.size=unit(0.2, "cm"))
gg4 <- gg4 + theme(legend.key.width=unit(1, "cm"))
gg4

#NH
alp_group3 = ddply(alp_nh, ~mountain1 + site + year + group, summarise, rel_f = sum(rel_freq))
alp_stature3 = ddply(alp_nh, ~mountain1 + site + year + stature, summarise, rel_f = sum(rel_freq))
alp_form3 = ddply(alp_nh, ~mountain1 + site + year + life_form, summarise, rel_f = sum(rel_freq))

alp_group4 = ddply(alp_group3, ~ mountain1 + year + group, summarise, rel_freq = mean(rel_f))
alp_stat4 = ddply(alp_stature3, ~ mountain1 + year + stature, summarise, rel_freq = mean(rel_f))
alp_form4 = ddply(alp_form3, ~ mountain1 + year + life_form, summarise, rel_freq = mean(rel_f))

# format data - stature
orderedclasses <- c("2", "3")
alp_stat4$stature <- factor(alp_stat4$stature, levels = orderedclasses)

#plot - NH stature

gg <- ggplot(alp_stat4, aes(x=year, y=stature, fill=rel_freq))
gg <- gg + geom_tile(color="white", size=0.1)
gg <- gg + geom_text(aes(label = round(rel_freq, 2)), color = "white", size = 5)
gg <- gg + scale_fill_viridis(name="Relative Frequency")
gg <- gg + xlab("Year")
gg <- gg + ylab("Stature")
gg <- gg + ggtitle("White Mountains")
gg <- gg + coord_equal()
gg <- gg + facet_wrap(~mountain1, ncol=2)
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text=element_text(size=12))
gg <- gg + theme(axis.title=element_text(size=14))
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(plot.title=element_text(hjust=0))
gg <- gg + theme(strip.text=element_text(hjust=0))
gg <- gg + theme(strip.text=element_text(size = 14))
gg <- gg + theme(legend.title=element_text(size=12))
gg <- gg + theme(legend.title.align=1)
gg <- gg + theme(legend.text=element_text(size=10))
gg <- gg + theme(legend.position="none")
gg <- gg + theme(legend.key.size=unit(0.2, "cm"))
gg <- gg + theme(legend.key.width=unit(1, "cm"))
gg

# format data - group
#orderedclasses <- c("2", "3")
#alp_group1$group <- factor(alp_group1$group, levels = orderedclasses)
#alp_stat1$id = paste(alp_stature$mountain, alp_stature$site ,sep="-")
#str(alp_group1)

alp_group4$group[alp_group4$group == 'transitional'] <- 'Transitional'
alp_group4$group[alp_group4$group == 'non-plant'] <- 'Non-Plant'
alp_group4$group[alp_group4$group == 'non-arctic'] <- 'Non-Arctic'
alp_group4$group[alp_group4$group == 'arctic'] <- 'Arctic'

#plot - ADK

gg3 <- ggplot(alp_group4, aes(x=year, y=group, fill=rel_freq))
gg3 <- gg3 + geom_tile(color="white", size=0.1)
gg3 <- gg3 + geom_text(aes(label = round(rel_freq, 2)), color = "white", size = 4)
gg3 <- gg3 + scale_fill_viridis(name="Relative Frequency")
gg3 <- gg3 + xlab("Year")
gg3 <- gg3 + ylab("Group")
gg3 <- gg3 + ggtitle("")
gg3 <- gg3 + coord_equal()
gg3 <- gg3 + facet_wrap(~mountain1, ncol=2)
gg3 <- gg3 + theme(axis.ticks=element_blank())
gg3 <- gg3 + theme(axis.text=element_text(size = 12))
gg3 <- gg3 + theme(axis.title=element_blank())
gg3 <- gg3 + theme(panel.border=element_blank())
gg3 <- gg3 + theme(plot.title=element_text(hjust=0))
gg3 <- gg3 + theme(strip.text=element_text(hjust=0))
gg3 <- gg3 + theme(strip.text=element_text(size=14))
gg3 <- gg3 + theme(legend.title=element_text(size=12))
gg3 <- gg3 + theme(legend.title.align=1)
gg3 <- gg3 + theme(legend.text=element_text(size=10))
gg3 <- gg3 + theme(legend.position="none")
gg3 <- gg3 + theme(legend.key.size=unit(0.2, "cm"))
gg3 <- gg3 + theme(legend.key.width=unit(1, "cm"))
gg3

# format data - form
#orderedclasses <- c("2", "3")
#alp_form1$life_form <- factor(alp_form1$life_form, levels = orderedclasses)
#alp_stat1$id = paste(alp_stature$mountain, alp_stature$site ,sep="-")

#plot NH form

alp_form4$life_form[alp_form4$life_form == 'tree'] <- 'Tree'
alp_form4$life_form[alp_form4$life_form == 'shrub'] <- 'Shrub'
alp_form4$life_form[alp_form4$life_form == 'non_plant'] <- 'Non-Plant'
alp_form4$life_form[alp_form4$life_form == 'lyco'] <- 'Lycophyte'
alp_form4$life_form[alp_form4$life_form == 'lichen'] <- 'Lichen'
alp_form4$life_form[alp_form4$life_form == 'gram'] <- 'Graminoid'
alp_form4$life_form[alp_form4$life_form == 'forb'] <- 'Forb'
alp_form4$life_form[alp_form4$life_form == 'bryo'] <- 'Moss'

#plot

gg5 <- ggplot(alp_form4, aes(x=year, y=life_form, fill=rel_freq))
gg5 <- gg5 + geom_tile(color="white", size=0.1)
gg5 <- gg5 + geom_text(aes(label = round(rel_freq, 2)), color = "white", size = 4)
gg5 <- gg5 + scale_fill_viridis(name="Relative Frequency")
gg5 <- gg5 + xlab("Year")
gg5 <- gg5 + ylab("Life Form")
gg5 <- gg5 + ggtitle("")
gg5 <- gg5 + coord_equal()
gg5 <- gg5 + facet_wrap(~mountain1, ncol=2)
gg5 <- gg5 + theme(axis.ticks=element_blank())
gg5 <- gg5 + theme(axis.text=element_text(size=12))
gg5 <- gg5 + theme(axis.title=element_blank())
gg5 <- gg5 + theme(panel.border=element_blank())
gg5 <- gg5 + theme(plot.title=element_text(hjust=0))
gg5 <- gg5 + theme(strip.text=element_text(hjust=0))
gg5 <- gg5 + theme(strip.text=element_text(size=14))
gg5 <- gg5 + theme(legend.title=element_text(size=12))
gg5 <- gg5 + theme(legend.title.align=1)
gg5 <- gg5 + theme(legend.text=element_text(size=10))
gg5 <- gg5 + theme(legend.position="none")
gg5 <- gg5 + theme(legend.key.size=unit(0.2, "cm"))
gg5 <- gg5 + theme(legend.key.width=unit(1, "cm"))
gg5

#putting it all together 

wm_tile = read.csv("wm_tile.csv", header = TRUE)
wm_tile$year = as.factor(wm_tile$year)
wm_tile$variable <- factor(wm_tile$variable, levels = c("Forb", "Graminoid", "Lichen", "Lycophyte", "Moss", "Non-Plant", "Shrub", "Tree",
                                                        "Arctic", "Non-Arctic", "Substrate", "Transitional", "3D", "2D"))
wm_tile$group <- factor(wm_tile$group, levels = c("Stature", "Group", "Form"))

gg6 <- ggplot(wm_tile, aes(x=year, y=variable, fill=value))
gg6 <- gg6 + geom_tile(color="white", size=0.1)
gg6 <- gg6 + geom_text(aes(label = round(value, 2)), color = "white", size = 4)
gg6 <- gg6 + scale_fill_viridis(name="Relative \nFrequency")
gg6 <- gg6 + ylab("")
gg6 <- gg6 + ggtitle("White Mountains")
gg6 <- gg6 + coord_equal()
gg6 = gg6 + facet_grid(group ~ mountain, space="free_x", scales="free_y", switch="y") +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA,colour=NA),
        panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
  #annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -4, ymax = 0) +
  coord_cartesian(clip="off") 
gg6
gg6 <- gg6 + theme(axis.text=element_text(size=14))
gg6 <- gg6 + theme(axis.title=element_blank())
gg6 <- gg6 + theme(panel.border=element_blank())
gg6 <- gg6 + theme(plot.title=element_text(hjust=0))
gg6 <- gg6 + theme(plot.title=element_text(face="bold"))
gg6 <- gg6 + theme(plot.title=element_text(size = 14))
gg6 <- gg6 + theme(strip.text=element_text(size=14))
gg6 <- gg6 + theme(legend.title=element_text(size=12))
gg6 <- gg6 + theme(legend.text=element_text(size=10))
gg6

adk_tile = read.csv("adk_tile.csv", header = TRUE)
adk_tile$year = as.factor(adk_tile$year)
adk_tile$variable <- factor(adk_tile$variable, levels = c("Forb", "Graminoid", "Lichen", "Lycophyte", "Moss", "Non-Plant", "Shrub", "Tree",
                                                          "Arctic", "Non-Arctic", "Substrate", "Transitional", "3D", "2D"))
adk_tile$group <- factor(adk_tile$group, levels = c("Stature", "Group", "Form"))

gg7 <- ggplot(adk_tile, aes(x=year, y=variable, fill=value))
gg7 <- gg7 + geom_tile(color="white", size=0.1)
gg7 <- gg7 + geom_text(aes(label = round(value, 2)), color = "white", size = 4)
gg7 <- gg7 + scale_fill_viridis(name="Relative \nFrequency")
gg7 <- gg7 + ylab("")
gg7 <- gg7 + ggtitle("Adirondacks")
gg7 <- gg7 + coord_equal()
gg7 = gg7 + facet_grid(group ~ mountain, space="free_x", scales="free_y", switch="y") +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill=NA,colour=NA),
        panel.spacing=unit(0,"cm"), axis.title.y = element_blank()) +
  #annotation_custom(grob = linesGrob(), xmin = -0.75, xmax = -0.75, ymin = -4, ymax = 0) +
  coord_cartesian(clip="off") 
gg7
gg7 <- gg7 + theme(axis.text=element_text(size=14))
gg7 <- gg7 + theme(axis.title=element_text(size=14))
gg7 <- gg7 + theme(panel.border=element_blank())
gg7 <- gg7 + theme(plot.title=element_text(hjust=0))
gg7 <- gg7 + theme(plot.title=element_text(face="bold"))
gg7 <- gg7 + theme(plot.title=element_text(size = 14))
gg7 <- gg7 + theme(strip.text=element_text(size=14))
gg7 <- gg7 + theme(legend.title=element_text(size=12))
gg7 <- gg7 + theme(legend.text=element_text(size=10))
gg7 <- gg7 + xlab("Year")
gg7

tile_all = (gg6 / gg7) + plot_annotation(tag_levels = "A")
tile_all

tiff(file = "tile_all_plot.tiff", width = 10, height = 11, units = 'in', res = 600, pointsize = 11)
tile_all
dev.off()

###Diversity measures
#site-level tables for species richness through time and shannon-weiner index values
#ANOVAs and post-hoc tests used to test significance of diversity measures

sppr <- specnumber(alp_cov_matrix)
#write.csv(sppr, "sppr.csv")
sppr_aov <- aov(sppr ~ Year, data = env)
summary(sppr_aov)
TukeyHSD(sppr_aov)

shannondiv <- diversity(alp_cov_matrix)
write.csv(shannondiv, "div.csv")
sppdiv_aov <- aov(shannondiv ~ Year + mountain, data = env)
summary(sppdiv_aov)
TukeyHSD(sppr_aov)

shandiv_df <- shannondiv %>% 
  # put all those calculations into a data frame
  enframe() %>% 
  # rename columns for ease of joining
  rename(site = name,
         shan_div = value)

# ======================================================
# Temporal Beta Diversity
# ======================================================

# Load required libraries
library(vegan)     # for dissimilarity and ordination
library(betapart)  # for beta diversity partitioning

# ======================================================
# Multi-Year Temporal Beta Diversity (Turnover + Nestedness)
# ======================================================

nh_mat = read.csv("alp_cov_matrix2.csv", header = TRUE)#read again if needed
ny_mat = read.csv("alp_cov_matrix1.csv", header = TRUE)

nh_mat1 = melt(nh_mat, id = c("site", "year"))
colnames(nh_mat1)[4] <- "abundance"
colnames(nh_mat1)[3] <- "species"

ny_mat1 = melt(ny_mat, id = c("site", "year"))
colnames(ny_mat1)[4] <- "abundance"
colnames(ny_mat1)[3] <- "species"

# ------------------------------------------------------
# 3. Compute temporal beta diversity (vs. baseline year) - NH
# ------------------------------------------------------
# Compare each later year to 1983 baseline for each site

beta_abund_results <- nh_mat1 %>%
  group_by(site) %>%
  group_modify(~{
    # Reshape to site-specific abundance matrix: rows = years, cols = species
    comm_matrix <- pivot_wider(.x, names_from = species, values_from = abundance, values_fill = 0) %>%
      column_to_rownames("year")
    
    # Compute Bray-Curtis components
    beta <- betapart::beta.pair.abund(comm_matrix, index.family = "bray")
    
    tibble(
      Component = c("Total", "Turnover (balanced variation)", "Nestedness (abundance gradients)"),
      Matrix = list(
        as.data.frame(as.table(as.matrix(beta$beta.bray)),
                      stringsAsFactors = FALSE),
        as.data.frame(as.table(as.matrix(beta$beta.bray.bal))),
        as.data.frame(as.table(as.matrix(beta$beta.bray.gra)))
      )
    ) %>%
      unnest(Matrix) %>%
      rename(Year1 = Var1, Year2 = Var2, Beta = Freq) %>%
      mutate(Year1 = as.numeric(Year1),
             Year2 = as.numeric(Year2),
             Lag = Year2 - Year1)
  })

beta_abund_results <- beta_abund_results %>%
  filter(Year1 < Year2)

beta_abund_summary <- beta_abund_results %>%
  group_by(Lag, Component) %>%
  summarize(mean_beta = mean(Beta, na.rm = TRUE), .groups = "drop")

nh_b = ggplot(beta_abund_summary, aes(x = Lag, y = mean_beta, color = Component)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, method = "loess") +
  theme_minimal(base_size = 13) +
  scale_color_viridis_d() +
  theme(legend.position = "none",
        axis.title.y = element_text(size = 16)) +
  labs(
    title = "Temporal Beta Diversity (Abundance-Based) - White Mountains",
    #subtitle = "Partitioned into balanced variation (turnover) and abundance gradients (nestedness)",
    x = "",
    y = "Mean Bray–Curtis dissimilarity"
  )

nh_b

nh_b_sup = ggplot(beta_abund_results, aes(x = Year1, y = Year2, fill = Beta)) +
  geom_tile(color = "white") +
  facet_grid(site ~ Component) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Temporal Beta Diversity per Site (Abundance-Based) - White Mountains",
    x = "Year 1", y = "Year 2", fill = "Bray–Curtis dissimilarity"
  )

nh_b_sup

baseline_year <- 1983

beta_baseline <- nh_mat1 %>%
  group_by(site) %>%
  group_modify(~{
    # Make site matrix (years × species)
    comm_matrix <- pivot_wider(.x, names_from = species, values_from = abundance, values_fill = 0) %>%
      column_to_rownames("year")
    
    # Compute abundance-based beta diversity
    beta <- betapart::beta.pair.abund(comm_matrix, index.family = "bray")
    
    # Extract the total dissimilarity matrix (Bray–Curtis)
    mat <- as.matrix(beta$beta.bray)
    
    # Subset comparisons to the baseline year
    base_row <- as.character(baseline_year)
    df <- tibble(
      site = unique(.x$site),
      Year2 = as.numeric(rownames(mat)),
      Beta_total = mat[base_row, ],
      Beta_turnover = as.matrix(beta$beta.bray.bal)[base_row, ],
      Beta_nestedness = as.matrix(beta$beta.bray.gra)[base_row, ]
    ) %>%
      filter(Year2 > baseline_year) %>%
      mutate(Lag = Year2 - baseline_year)
  }) %>%
  ungroup()

# Compute slopes per site
beta_trends <- beta_baseline %>%
  pivot_longer(cols = starts_with("Beta_"), names_to = "Component", values_to = "Beta") %>%
  group_by(site, Component) %>%
  summarize(
    slope = coef(lm(Beta ~ Lag))[2],   # slope of beta vs. time lag
    intercept = coef(lm(Beta ~ Lag))[1],
    r2 = summary(lm(Beta ~ Lag))$r.squared,
    .groups = "drop"
  )

beta_trends

beta_baseline %>%
  pivot_longer(cols = starts_with("Beta_"), names_to = "Component", values_to = "Beta") %>%
  ggplot(aes(x = Year2, y = Beta, color = Component)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~site) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Temporal Beta Diversity Trends per Site (vs. Baseline Year)",
    subtitle = "Partitioned into Total, Turnover, and Nestedness components",
    x = "Year", y = "Beta diversity (Bray–Curtis dissimilarity)"
  )

nh_b2_sup = ggplot(beta_trends, aes(x = site, y = slope, fill = Component)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Rates of Temporal Change in Beta Diversity - White Mountains",
    x = "Site", y = "Slope (Δβ per decade)"
  )

nh_b2_sup

# ------------------------------------------------------
# 3. Compute temporal beta diversity (vs. baseline year) - NY
# ------------------------------------------------------
# Compare each later year to 1984 baseline for each site

beta_abund_results <- ny_mat1 %>%
  group_by(site) %>%
  group_modify(~{
    # Reshape to site-specific abundance matrix: rows = years, cols = species
    comm_matrix <- pivot_wider(.x, names_from = species, values_from = abundance, values_fill = 0) %>%
      column_to_rownames("year")
    
    # Compute Bray-Curtis components
    beta <- betapart::beta.pair.abund(comm_matrix, index.family = "bray")
    
    tibble(
      Component = c("Total", "Turnover (balanced variation)", "Nestedness (abundance gradients)"),
      Matrix = list(
        as.data.frame(as.table(as.matrix(beta$beta.bray)),
                      stringsAsFactors = FALSE),
        as.data.frame(as.table(as.matrix(beta$beta.bray.bal))),
        as.data.frame(as.table(as.matrix(beta$beta.bray.gra)))
      )
    ) %>%
      unnest(Matrix) %>%
      rename(Year1 = Var1, Year2 = Var2, Beta = Freq) %>%
      mutate(Year1 = as.numeric(Year1),
             Year2 = as.numeric(Year2),
             Lag = Year2 - Year1)
  })

beta_abund_results <- beta_abund_results %>%
  filter(Year1 < Year2)

beta_abund_summary <- beta_abund_results %>%
  group_by(Lag, Component) %>%
  summarize(mean_beta = mean(Beta, na.rm = TRUE), .groups = "drop")

ny_b = ggplot(beta_abund_summary, aes(x = Lag, y = mean_beta, color = Component)) +
  geom_point(size = 3) +
  geom_smooth(se = FALSE, method = "loess") +
  theme_minimal(base_size = 13) +
  scale_color_viridis_d() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 16)) +
  labs(
    title = "Temporal Beta Diversity (Abundance-Based) - Adirondacks",
    #subtitle = "Partitioned into balanced variation (turnover) and abundance gradients (nestedness)",
    x = "Time lag (years)",
    y = "Mean Bray–Curtis dissimilarity"
  )

ny_b

ny_b_sup = ggplot(beta_abund_results, aes(x = Year1, y = Year2, fill = Beta)) +
  geom_tile(color = "white") +
  facet_grid(site ~ Component) +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Temporal Beta Diversity per Site (Abundance-Based) - Adirondacks",
    x = "Year 1", y = "Year 2", fill = "Bray–Curtis dissimilarity"
  )

ny_b_sup

baseline_year <- 1984

beta_baseline <- ny_mat1 %>%
  group_by(site) %>%
  group_modify(~{
    # Make site matrix (years × species)
    comm_matrix <- pivot_wider(.x, names_from = species, values_from = abundance, values_fill = 0) %>%
      column_to_rownames("year")
    
    # Compute abundance-based beta diversity
    beta <- betapart::beta.pair.abund(comm_matrix, index.family = "bray")
    
    # Extract the total dissimilarity matrix (Bray–Curtis)
    mat <- as.matrix(beta$beta.bray)
    
    # Subset comparisons to the baseline year
    base_row <- as.character(baseline_year)
    df <- tibble(
      site = unique(.x$site),
      Year2 = as.numeric(rownames(mat)),
      Beta_total = mat[base_row, ],
      Beta_turnover = as.matrix(beta$beta.bray.bal)[base_row, ],
      Beta_nestedness = as.matrix(beta$beta.bray.gra)[base_row, ]
    ) %>%
      filter(Year2 > baseline_year) %>%
      mutate(Lag = Year2 - baseline_year)
  }) %>%
  ungroup()

# Compute slopes per site
beta_trends <- beta_baseline %>%
  pivot_longer(cols = starts_with("Beta_"), names_to = "Component", values_to = "Beta") %>%
  group_by(site, Component) %>%
  summarize(
    slope = coef(lm(Beta ~ Lag))[2],   # slope of beta vs. time lag
    intercept = coef(lm(Beta ~ Lag))[1],
    r2 = summary(lm(Beta ~ Lag))$r.squared,
    .groups = "drop"
  )

beta_trends

beta_baseline %>%
  pivot_longer(cols = starts_with("Beta_"), names_to = "Component", values_to = "Beta") %>%
  ggplot(aes(x = Year2, y = Beta, color = Component)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~site) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Temporal Beta Diversity Trends per Site (vs. Baseline Year)",
    subtitle = "Partitioned into Total, Turnover, and Nestedness components",
    x = "Year", y = "Beta diversity (Bray–Curtis dissimilarity)"
  )

nyb_sup = ggplot(beta_trends, aes(x = site, y = slope, fill = Component)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Rates of Temporal Change in Beta Diversity - Adirondacks",
    x = "Site", y = "Slope (Δβ per decade)"
  )

nyb_sup

b_main = (nh_b/ny_b) + plot_layout(axis_titles = "collect")
b_main

tiff(file = "b_div.tiff", width = 8, height = 6, units = 'in', res = 600, pointsize = 12)
b_main
dev.off()

b_sup1 = nh_b_sup/ny_b_sup
b_sup1

tiff(file = "b_div1.tiff", width = 9.5, height = 16, units = 'in', res = 600, pointsize = 12)
b_sup1
dev.off()

b_sup2 = nh_b2_sup/nyb_sup
b_sup2

tiff(file = "b_div2.tiff", width = 12, height = 7.5, units = 'in', res = 600, pointsize = 12)
b_sup2
dev.off()

###Ordinations
#NMDS for sites (mountain ranges separate) and years (cluster by both)
#PERMANOVA and betadispersion for significant differences between sites and years
#Overlay species and environmental vectors on NMDS
#Indicator species analysis

# ======================================================
# NMDS Ordination + PERMANOVA + Ellipses + Environmental Vectors - NH
# ======================================================

# ------------------------------------------------------
# 1. Load data
# ------------------------------------------------------

# ------------------------------------------------------
# 2. NMDS ordination
# ------------------------------------------------------
set.seed(99)
row.names(alp_s_matrix2) = alp_s_matrix2[,1]
alp_s_matrix2 = alp_s_matrix2[,-1]
nmds1 <- metaMDS(alp_s_matrix2, distance = "bray", k = 2, trymax = 100)
nmds1

# Extract site/spp scores
nmds_df1 <- as.data.frame(scores(nmds1, display = "sites"))
nmds_df1 <- cbind(nmds_df1, alp.env.w)

# ------------------------------------------------------
# 3. PERMANOVA
# ------------------------------------------------------
adonis_res <- adonis2(alp_s_matrix2 ~ mountain + Year,
                      data = alp.env.w, method = "bray")
adonis_res

# ------------------------------------------------------
# 4. Check assumption of equal dispersion
# ------------------------------------------------------
disp <- betadisper(vegdist(alp_s_matrix2, "bray"), alp.env.w$mountain)
anova(disp)   # p > 0.05 → assumption met

disp <- betadisper(vegdist(alp_s_matrix2, "bray"), alp.env.w$Year)
anova(disp)

# ------------------------------------------------------
# 5. Environmental fitting (envfit)
# ------------------------------------------------------
envfit_res1 <- envfit(nmds1, alp.env.w[, c("mountain", "Year", "name", "period", "Tmean", "TotalN",
                                           "X2D", "X3D", "Arctic", "Non.Arctic", "Transitional",
                                           "Non.Plant", "Shrub")], permutations = 999)
envfit_res1

alp.spp.fit <- envfit(nmds1, alp_s_matrix2, permutations = 999)

sppscores(nmds1) = alp_s_matrix2

species.scores <- as.data.frame(scores(nmds1, display = "species"))
species.scores$Species <- rownames(species.scores)

spp_w = read.csv("w_spp.csv", header = TRUE)

species.scores = merge(species.scores, spp_w, by = "Species")

# Filter for significant species (e.g., p < 0.05)
significant_species_vectors <- species.scores[alp.spp.fit$vectors$pvals < 0.05,]

# Extract arrows for significant environmental variables
arrow_df1 <- as.data.frame(scores(envfit_res1, "vectors"))
arrow_df1$Variable <- rownames(arrow_df1)

# Rescale arrows for visualization
arrow_mult <- 1.5
arrow_df1$NMDS1 <- arrow_df1$NMDS1 * arrow_mult
arrow_df1$NMDS2 <- arrow_df1$NMDS2 * arrow_mult

# ------------------------------------------------------
# 7. Publication-quality ggplot
# ------------------------------------------------------
nhnmds1 = ggplot(nmds_df1, aes(x = NMDS1, y = NMDS2)) +
  # Site points
  geom_point(size = 3, alpha = 0.9, aes(color = period, shape = name)) +
  # Group ellipses
  #geom_path(data = ellipse_df, aes(x = x, y = y, color = group),
  #linewidth = 1, linetype = 2, show.legend = FALSE) +
  stat_ellipse(data=nmds_df1, level = 0.9, aes(x=NMDS1,y=NMDS2,colour=period),alpha=0.50, lwd = 1, linetype = "dashed") +
  # Group centroids
  # geom_point(data = centroids, aes(x = NMDS1, y = NMDS2),
  #           color = "black", size = 4, shape = 4, stroke = 1.2) +
  #geom_text(data = centroids, aes(label = period),
  #          color = "black", vjust = -1, fontface = "bold") +
  # Environmental arrows
  annotate("text", label = "Stress: 0.14", x = -0.9, y = 1.35, size = 4) +
  geom_segment(data = arrow_df1,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey25", linewidth = 1, alpha = 0.6) +
  #geom_text(data = arrow_df1,
  #aes(x = NMDS1, y = NMDS2, label = Variable),
  # color = "grey10", size = 4, hjust = 0.2, vjust = 0, fontface = "italic") +
  ggrepel::geom_text_repel(data = arrow_df1, aes(x = NMDS1, y = NMDS2, label = Variable), position = position_nudge_center(0.2, 0.1, 0, 0), direction = "both", segment.size = 0.25,
                           size = 4, fontface = "italic", color = "grey10") +
  # Aesthetics
  xlim(-1.25, 1.25) +
  scale_color_viridis(discrete = TRUE) +
  coord_equal() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white")
  ) +
  labs(
    title = "White Mountains",
    color = "Period",
    shape = "Mountain"
  )

nhnmds1

tiff(file = "nhnmds1.tiff", width = 7, height = 6, units = 'in', res = 600, pointsize = 12)
nhnmds1
dev.off()

spp_score = read.csv("w_spp.csv")#fir species vectors

nhnmds2 = ggplot(nmds_df1, aes(x = NMDS1, y = NMDS2)) +
  # Site points
  geom_point(size = 3, alpha = 0.3, aes(color = period, shape = mountain)) +
  # Group ellipses
  #geom_path(data = ellipse_df, aes(x = x, y = y, color = group),
  #linewidth = 1, linetype = 2, show.legend = FALSE) +
  stat_ellipse(data=nmds_df1, level = 0.9, aes(x=NMDS1,y=NMDS2,colour=period),alpha=0.3, lwd = 1, linetype = "dashed") +
  # Group centroids
  # geom_point(data = centroids, aes(x = NMDS1, y = NMDS2),
  #           color = "black", size = 4, shape = 4, stroke = 1.2) +
  #geom_text(data = centroids, aes(label = period),
  #          color = "black", vjust = -1, fontface = "bold") +
  # Environmental arrows
  annotate("text", label = "Stress: 0.14", x = -0.9, y = 1.35, size = 4) +
  geom_segment(data = arrow_df1,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey25", linewidth = 1, alpha = 0.4) +
  #geom_text(data = arrow_df1,
  #aes(x = NMDS1, y = NMDS2, label = Variable),
  #color = "grey10", size = 4, hjust = 0.2, vjust = 0, fontface = "italic", alpha = 0.6) +
  ggrepel::geom_text_repel(data = arrow_df1, aes(x = NMDS1, y = NMDS2, label = Variable), position = position_nudge_center(0.2, 0.1, 0, 0), direction = "both", segment.size = 0.25,
                           size = 4, fontface = "italic", color = "grey10", alpha = 0.4) +
  geom_point(data = spp_score, color = "blue", pch = 3, size = 3) +
  ggrepel::geom_text_repel(
    data = spp_score,
    aes(x = NMDS1, y = NMDS2, label = species),
    color = "blue",
    size = 3.3,
    box.padding = 0.25
  ) +
  # Aesthetics
  xlim(-1.25, 1.25) +
  scale_color_viridis(discrete = TRUE) +
  coord_equal() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white")
  ) +
  labs(
    title = "",
    color = "Period",
    shape = "Mountain"
  )

nhnmds2

tiff(file = "nhnmds2.tiff", width = 7, height = 6, units = 'in', res = 600, pointsize = 12)
nhnmds2
dev.off()

# ======================================================
# NMDS Ordination + PERMANOVA + Ellipses + Environmental Vectors - NY
# ======================================================

# ------------------------------------------------------
# 1. Load data
# ------------------------------------------------------

# ------------------------------------------------------
# 2. NMDS ordination
# ------------------------------------------------------
set.seed(99)
nmds <- metaMDS(alp_s_matrix1, distance = "bray", k = 2, trymax = 100)
nmds

# Extract site scores
nmds_df <- as.data.frame(scores(nmds, display = "sites"))
nmds_df <- cbind(nmds_df, alp.env.a)

# ------------------------------------------------------
# 3. PERMANOVA
# ------------------------------------------------------
adonis_res <- adonis2(alp_s_matrix1 ~ mountain + period,
                      data = alp.env.a, method = "bray")
adonis_res

# ------------------------------------------------------
# 4. Check assumption of equal dispersion
# ------------------------------------------------------
disp <- betadisper(vegdist(alp_s_matrix1, "bray"), alp.env.a$mountain)
anova(disp)   # p > 0.05 → assumption met

disp <- betadisper(vegdist(alp_s_matrix1, "bray"), alp.env.a$Year)
anova(disp)

# ------------------------------------------------------
# 5. Environmental fitting (envfit)
# ------------------------------------------------------
alp.env.a = read.csv("alpine_site_env_a.csv", header = TRUE)
alp.env.w = read.csv("alpine_site_env_w.csv", header = TRUE)

envfit_res <- envfit(nmds, alp.env.a[, c("mountain", "Year", "name", "period", "Tmean", "TotalN",
                                         "X2D", "X3D", "Arctic", "Non.Arctic", "Transitional",
                                         "Non.Plant", "Shrub")], permutations = 999)
envfit_res

#species_scores1 <- as.data.frame(scores(nmds_df, display = "species"))
#species_scores1$species <- rownames(species_scores1) #

# Extract arrows for significant environmental variables
arrow_df <- as.data.frame(scores(envfit_res, "vectors"))
arrow_df$Variable <- rownames(arrow_df)

# Rescale arrows for visualization
arrow_mult <- 1.5
arrow_df$NMDS1 <- arrow_df$NMDS1 * arrow_mult
arrow_df$NMDS2 <- arrow_df$NMDS2 * arrow_mult

# ------------------------------------------------------
# 7. Publication-quality ggplot
# ------------------------------------------------------
nynmds1 = ggplot(nmds_df, aes(x = NMDS1, y = NMDS2)) +
  # Site points
  # Group ellipses
  #geom_path(data = ellipse_df, aes(x = x, y = y, color = group),
  #linewidth = 1, linetype = 2, show.legend = FALSE) +
  stat_ellipse(data=nmds_df, level = 0.9, aes(x=NMDS1,y=NMDS2,colour=period),alpha=0.50, lwd = 1, linetype = "dashed") +
  geom_point(size = 3, alpha = 0.9, aes(NMDS1, NMDS2, color = period, shape = name)) +
  xlim(-1, 1) +
  labs(title = "Adirondacks",
       color = "Period",
       shape = "Mountain") +
  # Group centroids
  # geom_point(data = centroids, aes(x = NMDS1, y = NMDS2),
  #           color = "black", size = 4, shape = 4, stroke = 1.2) +
  #geom_text(data = centroids, aes(label = period),
  #          color = "black", vjust = -1, fontface = "bold") +
  # Environmental arrows
  annotate("text", label = "Stress: 0.18", x = -0.8, y = 0.9, size = 4) +
  geom_segment(data = arrow_df,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey50", linewidth = 1) +
  #geom_text(data = arrow_df,
  #aes(x = NMDS1, y = NMDS2, label = Variable),
  #color = "grey10", size = 4, hjust = 1.2, vjust = -0.3, fontface = "italic") +
  ggrepel::geom_text_repel(data = arrow_df, aes(x = NMDS1, y = NMDS2, label = Variable), position = position_nudge_center(0.2, 0.1, 0, 0), direction = "both", segment.size = 0.25,
                           size = 4, fontface = "italic", color = "grey10", alpha = 0.9) +
  # Aesthetics
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  scale_color_viridis(discrete = TRUE) +
  coord_equal() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white")
  )

nynmds1

tiff(file = "nynmds1.tiff", width = 7, height = 6, units = 'in', res = 600, pointsize = 12)
nynmds1
dev.off()

a_spp = read.csv("a_spp.csv")#fir species vectors

nynmds2 = ggplot(nmds_df, aes(x = NMDS1, y = NMDS2)) +
  # Site points
  # Group ellipses
  #geom_path(data = ellipse_df, aes(x = x, y = y, color = group),
  #linewidth = 1, linetype = 2, show.legend = FALSE) +
  geom_point(size = 3, alpha = 0.25, aes(NMDS1, NMDS2, color = period, shape = mountain)) +
  xlim(-1, 1) +
  labs(title = "",
       color = "Period",
       shape = "Mountain") +
  stat_ellipse(data=nmds_df, level = 0.9, aes(x=NMDS1,y=NMDS2,colour=period),alpha=0.25, lwd = 1, linetype = "dashed") +
  # Group centroids
  # geom_point(data = centroids, aes(x = NMDS1, y = NMDS2),
  #           color = "black", size = 4, shape = 4, stroke = 1.2) +
  #geom_text(data = centroids, aes(label = period),
  #          color = "black", vjust = -1, fontface = "bold") +
  # Environmental arrows
  annotate("text", label = "Stress: 0.18", x = -0.8, y = 0.9, size = 4) +
  geom_segment(data = arrow_df,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               color = "grey50", linewidth = 1, alpha = 0.3) +
  #geom_text(data = arrow_df,
  #aes(x = NMDS1, y = NMDS2, label = Variable),
  #color = "grey10", size = 4, hjust = 1.2, vjust = -0.3, fontface = "italic", alpha = 0.6) +
  ggrepel::geom_text_repel(data = arrow_df, aes(x = NMDS1, y = NMDS2, label = Variable), position = position_nudge_center(0.2, 0.1, 0, 0), direction = "both", segment.size = 0.25,
                           size = 4, fontface = "italic", color = "grey10", alpha = 0.3) +
  geom_point(data = a_spp, color = "blue", pch = 3, size = 3) +
  ggrepel::geom_text_repel(
    data = a_spp,
    aes(x = NMDS1, y = NMDS2, label = species),
    color = "blue",
    size = 3.3,
    box.padding = 0.25
  ) +
  # Aesthetics
  guides(fill = "none") +
  scale_color_viridis(discrete = TRUE) +
  coord_equal() +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "white")
  )

nynmds2

nmds_1 = ((nhnmds1 + nynmds1) / (nhnmds2 + nynmds2)) + plot_annotation(tag_levels = 'A')
nmds_1

tiff(file = "nmds1.tiff", width = 15, height = 10, units = 'in', res = 600, pointsize = 12)
nmds_1
dev.off()

nmds_2 = (nhnmds2 + nynmds2)
nmds_2

tiff(file = "nmds2.tiff", width = 14, height = 5, units = 'in', res = 600, pointsize = 12)
nmds_2
dev.off()

###########alluvial plots

##################transition probabilities - NH

nh = read.csv("source_data1.csv", header = TRUE)

nh_stat = cast(nh, uid ~ year, value = "stature")
nh_stat = na.omit(nh_stat)
nh_group = cast(nh, uid ~ year, value = "group")
nh_group = na.omit(nh_group)
nh_form = cast(nh, uid ~ year, value = "life_form")
nh_form = na.omit(nh_form)

t_s <- nh_stat %>%
  group_by(`1983_X`, `1989_X`, `2000_X`, `2009_X`, `2017_X`) %>%
  dplyr::summarise(n = n(), .groups = "drop")

t_g <- nh_group %>%
  group_by(`1983_X`, `1989_X`, `2000_X`, `2009_X`, `2017_X`) %>%
  dplyr::summarise(n = n(), .groups = "drop")

t_l <- nh_form %>%
  group_by(`1983_X`, `1989_X`, `2000_X`, `2009_X`, `2017_X`) %>%
  dplyr::summarise(n = n(), .groups = "drop")

#####NY

wright = read.csv("Wright_2.csv", header = TRUE)

wright1 = melt(wright, id = "ID")
colnames(wright1)[3] <- "species"

wright2 = merge(wright1, spp, by = "species")

wright_stat = cast(wright2, ID ~ variable, value = "stature")
wright_stat = na.omit(wright_stat)
wright_group = cast(wright2, ID ~ variable, value = "group")
wright_group = na.omit(wright_group)
wright_form = cast(wright2, ID ~ variable, value = "life_form")
wright_form = na.omit(wright_form)

transitions_s <- wright_stat %>%
  group_by(X1994, X2002, X2007, X2017) %>%
  dplyr::summarise(n = n(), .groups = "drop")

transitions_g <- wright_group %>%
  group_by(X1994, X2002, X2007, X2017) %>%
  dplyr::summarise(n = n(), .groups = "drop")

transitions_l <- wright_form %>%
  group_by(X1994, X2002, X2007, X2017) %>%
  dplyr::summarise(n = n(), .groups = "drop")

#############NY - plotting

data_long <- to_lodes_form(transitions_s,
                           key = "time",
                           value = "state",
                           axes = 1:4)
ny1 = data_long %>%
  ggplot(aes(
    x = time,
    stratum = state,
    alluvium = alluvium,
    y = n,
    fill = state
  )) +
  geom_flow(color = "darkgrey") +
  geom_stratum(alpha = .9) +
  #theme_tufte(base_size = 18) +
  theme_bw() +
  ggtitle("Adirondacks") +
  labs(x = "Year",
       y = "Frequency",
       fill = "Stature") +
  scale_fill_viridis_d(direction = -1, option = "D", labels = c("2D", "3D")) +
  scale_x_discrete(labels = c("1994", "2002", "2007", "2017")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "right", hjust = 1, 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

ny1

data_long_g <- to_lodes_form(transitions_g,
                             key = "time",
                             value = "state",
                             axes = 1:4)
ny2 = data_long_g %>%
  ggplot(aes(
    x = time,
    stratum = state,
    alluvium = alluvium,
    y = n,
    fill = state
  )) +
  geom_flow(color = "darkgrey") +
  geom_stratum(alpha = .9) +
  #theme_tufte(base_size = 18) +
  theme_bw() +
  labs(x = "Year",
       y = "Frequency",
       fill = "Group") +
  scale_fill_viridis_d(direction = -1, option = "D", labels = c("Arctic", "Non-Arctic",
                                                                "Non-Plant", "Transitional")) +
  scale_x_discrete(labels = c("1994", "2002", "2007", "2017")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

data_long_f <- to_lodes_form(transitions_l,
                             key = "time",
                             value = "state",
                             axes = 1:4)
ny3 = data_long_f %>%
  ggplot(aes(
    x = time,
    stratum = state,
    alluvium = alluvium,
    y = n,
    fill = state
  )) +
  geom_flow(color = "darkgrey") +
  geom_stratum(alpha = .9) +
  #theme_tufte(base_size = 18) +
  theme_bw() +
  labs(x = "Year",
       y = "Frequency",
       fill = "Form") +
  scale_fill_viridis_d(direction = -1, option = "D", labels = c("Bryophyte", "Forb",
                                                                "Graminoid", "Lichen",
                                                                "Non-Plant",
                                                                "Shrub", "Tree")) +
  scale_x_discrete(labels = c("1994", "2002", "2007", "2017")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

ny3

#############NH - plotting

data_long_ns <- to_lodes_form(t_s,
                              key = "time",
                              value = "state",
                              axes = 1:5)
nh1 = data_long_ns %>%
  ggplot(aes(
    x = time,
    stratum = state,
    alluvium = alluvium,
    y = n,
    fill = state
  )) +
  geom_flow(color = "darkgrey") +
  geom_stratum(alpha = .9) +
  #theme_tufte(base_size = 18) +
  theme_bw() +
  ggtitle("White Mountains") +
  labs(x = "Year",
       y = "Frequency",
       fill = "Stature") +
  scale_fill_viridis_d(direction = -1, option = "D", labels = c("2D", "3D")) +
  scale_x_discrete(labels = c("1983", "1989", "2000", "2009", "2017")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "none"
  )

data_long_ng <- to_lodes_form(t_g,
                              key = "time",
                              value = "state",
                              axes = 1:5)
nh2 = data_long_ng %>%
  ggplot(aes(
    x = time,
    stratum = state,
    alluvium = alluvium,
    y = n,
    fill = state
  )) +
  geom_flow(color = "darkgrey") +
  geom_stratum(alpha = .9) +
  #theme_tufte(base_size = 18) +
  theme_bw() +
  labs(x = "Year",
       y = "Frequency",
       fill = "Group") +
  scale_fill_viridis_d(direction = -1, option = "D", labels = c("Arctic", "Non-Arctic",
                                                                "Non-Plant", "Transitional")) +
  scale_x_discrete(labels = c("1983", "1989", "2000", "2009", "2017")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "none"
  )

data_long_nf <- to_lodes_form(t_l,
                              key = "time",
                              value = "state",
                              axes = 1:5)
nh3 = data_long_nf %>%
  ggplot(aes(
    x = time,
    stratum = state,
    alluvium = alluvium,
    y = n,
    fill = state
  )) +
  geom_flow(color = "darkgrey") +
  geom_stratum(alpha = .9) +
  #theme_tufte(base_size = 18) +
  theme_bw() +
  labs(x = "Year",
       y = "Frequency",
       fill = "Form") +
  scale_fill_viridis_d(direction = -1, option = "D", labels = c("Bryophyte", "Forb",
                                                                "Graminoid", "Lichen",
                                                                "Lycophyte", "Non-Plant",
                                                                "Shrub", "Tree")) +
  scale_x_discrete(labels = c("1983", "1989", "2000", "2009", "2017")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "none"
  )

trans_plot = ((nh1 + ny1) + plot_layout(guides = 'collect')) / ((nh2 + ny2) + plot_layout(guides = 'collect')) / ((nh3 + ny3) + plot_layout(guides = 'collect')) + plot_annotation(tag_levels = "A")
trans_plot

tiff(file = "trans_plot.tiff", width = 8, height = 7.5, units = 'in', res = 600, pointsize = 12)
trans_plot
dev.off()

#####################################END Script