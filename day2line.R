library(tidyverse)
library(sf)

zf <- tempfile()
download.file('https://gis.ny.gov/gisdata/fileserver/?DSID=427&file=NYSsnowmobile2018-19.zip',zf)

unzip(zf, exdir='/tmp' )
sno <- read_sf('/tmp/NYSsnowmobile2018-19.shp') %>% rmapshaper::ms_simplify()

snow_bbox <- sno %>% st_transform(3857) %>% st_bbox()

ny <- states(cb=T) %>% filter(STUSPS == 'NY')
cty <- counties(cb=T, 'ny')

ggplot() +
  geom_sf(data=ny, fill='gray30', color='black', size=0.9) +
  geom_sf(data=cty, fill=NA, color='black', size=0.3) +
  geom_sf(data=sno, color='orange') +
  coord_sf(crs=3857, xlim = c(snow_bbox[1], snow_bbox[3]), ylim = c(snow_bbox[2], snow_bbox[4]), expand=F) +
  theme_void() +
  labs(
    title='<span style="color: orange; font-size: 90px">Snowmobile Trails</span><br /><span style="font-size: 40px">2018-19 State Funded<br />Snowmobile Trails',
    tag='<span style="color: orange">#30DayMapChallenge</span> - Day 2 Lines',
  ) +
  theme(
    text=element_text(family='Ubuntu',size=14, color='gray90'),
    plot.background = element_rect(fill='gray20', color='gray20'),
    plot.title=ggtext::element_textbox(family='Ubuntu Mono', halign = 0.5, hjust=0, width=0.45, face='bold',size=36, margin=unit(c(45,0,5,0),'pt'), lineheight = 0.5, height=0),
    plot.tag=ggtext::element_textbox(size=10,hjust=0,lineheight = 0.9, margin=unit(c(40,0,0,0),'pt')),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0.02),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.key = element_rect(size=0, fill=NA),
    legend.position = 'top',
  ) +
  guides(color = guide_legend(nrow=1, override.aes = list(size = 3, fill=NA, linetype=0 ) ))

fn <- 'snowmobile-trails'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=110)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1080, height=1920, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))


           
