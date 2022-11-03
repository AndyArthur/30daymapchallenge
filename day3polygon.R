library(sf)
library(tidyverse)

blow1950 <- arcpullr::get_spatial_layer('https://services2.arcgis.com/8krRUWgifzA4cgL3/ArcGIS/rest/services/Damage1950Blowdown/FeatureServer/0')
stland <- arcpullr::get_spatial_layer('https://services2.arcgis.com/8krRUWgifzA4cgL3/ArcGIS/rest/services/StateLandMap/FeatureServer/0')
bl <- arcpullr::get_spatial_layer('https://services2.arcgis.com/8krRUWgifzA4cgL3/ArcGIS/rest/services/BluelinePolygon/FeatureServer/0')

adkcty <- tigris::counties('ny') %>% st_transform(st_crs(bl)) %>% st_intersection(bl) %>% .$NAME
water <- tigris::area_water('ny', county = adkcty) %>% st_transform(st_crs(bl)) %>% st_intersection(bl) %>% rmapshaper::ms_simplify()

stlands <- stland %>% st_make_valid() %>% rmapshaper::ms_simplify()

blow1950 <- blow1950 %>% filter(Note != 'NONE (INTERNAL POLYGON)') %>% mutate(Note = str_to_lower(Note) %>% str_replace( 'blowdown','')) %>%
  st_intersection(bl) %>% rmapshaper::ms_simplify()

ggplot() +
  geom_sf(data=stlands, fill='darkgreen', size=0, alpha=0.3) +
  geom_sf(data=blow1950, aes(fill=Note), size=0, alpha=0.7) +
  geom_sf(data=water, fill='cyan', size=0) +
  geom_sf(data=bl, fill=NA, color='blue', size=1) +
  scale_fill_manual(values=c('orange','red'), name='Blowdown') +
  theme_minimal() +
  coord_sf(expand=F) +
  labs(
    title = '1950 Adirondack Park Blowdown',
    tag=paste('<b>#30daymapchallenge</b> - Day 3, Polygons - ',
              'Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y")),
   )  +
  theme(
    legend.key.height = unit(1,'cm'),
    legend.key.width = unit(1,'cm'),
    legend.position = c(0,0.08),
    legend.spacing.y = unit(0.5, 'cm'),
    text= element_text(family='National Park',size=14, color='burlywood4'),
    panel.grid = element_line(color='bisque2'),
    plot.title=element_textbox(halign = 0.5, hjust=0, face='bold',size=33, margin=unit(c(5,0,0,0),'pt'), maxheight=0, width=0.5),
    plot.background = element_rect(fill = "floralwhite", color="floralwhite"),
    plot.subtitle=element_textbox(hjust=0.5, halign=0.5, margin=unit(c(5,0,5,0),'pt')),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag = element_textbox(halign = 1, size=10,hjust=0),
    plot.tag.position = c(0.0,-0.01),
  ) +
  guides(fill = guide_legend(byrow = TRUE) )

fn <- str_c('1950-adk-park-blowdown')
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1200, height=1400, units='px', dpi=120, device = grDevices::jpeg)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1200, height=1400, units='px', dpi=120, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


