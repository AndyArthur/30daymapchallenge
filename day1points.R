library(RPostgreSQL)
library(tidyverse)
library(sf)
library(tigris)

con <- DBI::dbConnect(RPostgres::Postgres(), dbname='gis', host='localhost', 
                      port=5432, user='postgres', password='xxxxxx')

# get columns list
sql <- 'select 
 nbr_bedrooms, "Shape"
from  nytax_ctr_pt 
WHERE 
 citytown_name = \'Bethlehem\' AND
  prop_class = \'210\' AND nbr_bedrooms > 0
 
;'

beds <- read_sf(con, query=sql)
beds <- mutate(beds, 
                 bedrooms = ifelse(nbr_bedrooms > 3, '4+', nbr_bedrooms),  
                 bedrooms = factor(bedrooms)) %>% st_transform(3857)

bbox <- beds %>% st_bbox()

qry <- paste("SELECT NAME, ref, ST_Simplify(\"way\",10) AS way FROM new_york_osm_line WHERE way && ST_MakeEnvelope(",
             bbox[1],',',
             bbox[2],',',
             bbox[3],',',
             bbox[4],
             ", 3857) AND highway IS NOT NULL",sep="")

roads <- st_read(con, query=qry, geom='way') %>% st_intersection(cs %>% st_transform(3857))



cs <- county_subdivisions('ny') %>% filter(NAME == 'Bethlehem')

ggplot() +
  geom_sf(data=cs, fill=NA, size=0.5, color='gray90') +
  geom_sf(data=roads, size=0.1, color='white') +
  geom_sf(data=beds, size=0.3, aes(color=bedrooms)) +
  scale_color_brewer(palette = 'RdYlBu', direction=-1, name='') + 
  theme_void() +
  labs(
    title='Bethlehem, NY:<br />A <span style="color: orange">Bedroom</span> Community<br /><span style="font-size: 20px">Number of Bedrooms',
    tag='Created in R using NYS Tax Parcel Centroids. <br />Street Map &copy; OpenstreetMap.org contributors. Map by Andy Arthur, <br />11/1/2022 as part of <span style="color: orange">#30DayMapChallenge',
  ) +
  coord_sf(expand=F) +
  theme(
    text=element_text(family='Ubuntu Mono',size=14, color='gray90'),
    plot.background = element_rect(fill='gray20', color='gray20'),
    plot.title=ggtext::element_textbox(family='Ubuntu Mono', halign = 0.5, hjust=0.5, face='bold',size=28, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.tag=ggtext::element_textbox(size=10,hjust=0,lineheight = 0.9, margin=unit(c(30,0,0,0),'pt')),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(0,0.02),
    legend.key.height = unit(0.2,'cm'),
    legend.key.width = unit(0.5,'cm'),
    legend.key = element_rect(size=0, fill=NA),
    legend.position = 'top',
  ) +
  guides(color = guide_legend(nrow=1, override.aes = list(size = 3, fill=NA, linetype=0 ) ))

fn <- 'bedroomcommunity'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=110)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1080, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

