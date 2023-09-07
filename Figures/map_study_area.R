#' Map of area of interest
#' 
#' EMC 9/21/22

library(dplyr)
library(terra)
library(ggmap)

# read in study area raster
studyarea = terra::rast('Data/Spatial/study-area-mask.tif')
plot(studyarea)

# reproject to lon/lat
studyarea_lonlat = project(studyarea, "+proj=longlat +datum=WGS84")

# change to larger resolution so things go faster
studyarea_coarse = aggregate(studyarea_lonlat, fact=10, fun='max', na.rm=T)
plot(studyarea_coarse)

# convert studyarea to data frame
test = as.data.frame(studyarea_coarse, xy=T)
test$value = round(test$`bps-mlra42-new-mexico-soil-elev-shrub-grass`)
test$label = rep('Study Area')


# get outline of US states
states <- map_data("state")
westernstates <- subset(states, region %in% c('new mexico','arizona','colorado','utah','texas','oklahoma','kansas'))



# state labels
library(data.table)
dt2 <- data.table::as.data.table(copy(state.x77))
dt2$state <- tolower(rownames(state.x77))
dt2 <- dt2[,.(state, Population)]
setkey(dt2, state)
states <- data.table::setDT(ggplot2::map_data("state"))
data.table::setkey(states, region)
# join data to map: left join states to dt2
dt2 <- dt2[states]
# create states location and abbreviations for label
# incl `Population` (the value to plot) in the label dataset, if want to fill with color. 
state_label_dt <- unique(dt2[, .(Population, x = mean(range(long)), y = mean(range(lat))), by = state]) %>% 
  dplyr::filter(state %in% c('new mexico','arizona','utah','colorado','texas','oklahoma','kansas'))
snames <- data.table(state = tolower(state.name), abb = state.abb) # these are dataset within R
setkey(state_label_dt, state)
setkey(snames, state)
state_label_dt <- snames[state_label_dt]
# All labels for states to the right of lon = -77 will be on the right of lon = -50.
x_boundary = -77
x_limits <- c(-50, NA) # optional, for label repelling




# colors for map
colors <- c("Study Area" = "darkred")

map = ggplot() + 
  #coord_fixed(1.3) + 
  geom_polygon(data = westernstates, mapping = aes(x = long, y = lat, group=group), color = "black", fill = "gray") +
  geom_raster(test, mapping=aes(x=x, y=y, fill=label)) +
  ggsn::scalebar(data= westernstates, location='bottomleft', transform=T, dist=200, dist_unit='km', st.size=3) +
  scale_fill_manual(values=colors) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = c(.2,.2)) +
  geom_text(data=state_label_dt[x<x_boundary,], aes(x=x,y=y, label=abb), 
            size=3, inherit.aes=F) 
map

ggsave('Figures/study_area_map.png', plot=map, width=4, height=4)
