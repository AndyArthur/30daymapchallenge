library(tidyverse)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(sf)
rm(list=ls())

ne_states('Ukraine') %>% st_as_sf() -> ukraineprov

ne_countries(country = 'Ukraine', scale = 10) %>% st_as_sf() -> ukr

ukrbbox = ukr %>% st_bbox()

ne_download(category = 'physical', type = 'rivers_lake_centerlines', scale=10) %>%
  st_as_sf() %>% st_intersection(ukr) -> ukrw

ne_download(category = 'physical', type = 'lakes', scale=10) %>%
  st_as_sf() %>% st_make_valid() %>% st_intersection(ukr) -> ukrl

ne_download(category = 'cultural', type = 'urban_areas', scale=10) %>%
  st_as_sf() %>% st_make_valid() %>% st_intersection(ukr) -> ukrua

ukraineprov %>% 
  mutate(color = factor(ggredist::map_coloring(.))) %>%
  ggplot() +
  geom_sf(aes(fill=color), alpha=0.7, size=0.5, linetype='dashed') +
  geom_sf(data=ukrua, fill='gray50', alpha=0.5, size=0) +
  geom_sf(data=ukrw, color='cyan')+
  geom_sf(data=ukrl, color='cyan', fill='cyan')+
  geom_sf(data=ukr, fill=NA) +
  ggsflabel::geom_sf_text_repel(aes(label=name), bg.r=0.1, bg.color='#ffffffcc', size=4, point.size=NA) +
  ggredist::scale_fill_natgeo() +
  coord_sf(expand=F, xlim=c(ukrbbox[1],ukrbbox[3]),  ylim=c(ukrbbox[2],ukrbbox[4])) +
  theme_minimal() +
  labs(
    title='Providences of Ukraine',
    tag='Natural Earth Dataset, R/ggplot<br />Map by Andy Arthur, 11/4/2022 as part of <b>#30DayMapChallenge',
    x='',y=''
  ) +
  theme(
    text=element_text(family='Roboto Condensed',size=14, color='gray10'),
    plot.background = element_rect(fill='gray90', color='gray90'),
    plot.title=ggtext::element_textbox(family='Roboto', halign = 0.5, hjust=0.5, face='bold',size=28, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.tag=ggtext::element_textbox(size=10, hjust=1, halign=1, lineheight = 0.9, margin=unit(c(30,0,0,0),'pt'), width=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    panel.grid = element_line(color='gray80'),
    plot.tag.position = c(1,0.02),
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.key = element_rect(size=0, fill=NA),
    legend.position = 'none',
  ) 

fn <- 'ukraine'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=110)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1920, height=1080, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))

