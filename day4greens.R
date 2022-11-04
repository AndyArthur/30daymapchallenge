library(tigris)
library(tidyverse)
library(classInt)
library(tidycensus)
library(sf)
library(wacolors)

rm(list=ls())

vars <- load_variables(2020, 'acs5', cache=T)


acs <- get_acs("tract",
               state='ny',
               survey='acs5',
               county=c('Albany','Schenectady','Saratoga','Rensselaer'),
               variables = c(
                 'Walk to Work'='B08006_015',
                  'Total Commuters'='B08006_001'
               ), 
               output='wide',
               geometry = T,
               year = 2020
)

nyco <- counties('ny',cb=T) %>% filter(NAME %in% c('Albany','Schenectady','Saratoga','Rensselaer'))
nycos <- county_subdivisions('ny', county = c('Albany','Schenectady','Saratoga','Rensselaer'), cb=T)

acs %>% janitor::clean_names() %>% 
  ggplot() + geom_sf(aes(fill=walk_to_work_e/total_commuters_e), size=0) +
  geom_sf(data=nyco, fill=NA, color='darkgreen') +
  geom_sf(data=nycos, fill=NA, color='darkgreen', size=0.2) +
  scale_fill_fermenter(palette='Greens', direction=1, name='', labels=scales::label_percent()) +
  theme_void() +
  theme_void() +
  labs(
    title='Persons who Walk to Work',
    tag='2020 American Community Survey, 5-yr<br />Map by Andy Arthur, 11/4/2022 as part of <b>#30DayMapChallenge',
  ) +
  coord_sf(expand=F) +
  theme(
    text=element_text(family='Roboto Condensed',size=14, color='darkgreen'),
    plot.background = element_rect(fill='MintCream', color='MintCream'),
    plot.title=ggtext::element_textbox(family='Pharmacy', halign = 0.5, hjust=0.5, face='bold',size=28, margin=unit(c(5,0,5,0),'pt'), lineheight = 0.5),
    plot.tag=ggtext::element_textbox(size=10, hjust=1, halign=1, lineheight = 0.9, margin=unit(c(30,0,0,0),'pt'), width=1),
    plot.margin = unit(c(1,1,1,1), 'lines'),
    plot.tag.position = c(1,0.02),
    legend.key.height = unit(0.5,'cm'),
    legend.key.width = unit(2,'cm'),
    legend.key = element_rect(size=0, fill=NA),
    legend.position = 'top',
  ) +
  guides(color = guide_legend(nrow=1, override.aes = list(size = 3, fill=NA, linetype=0 ) ))

fn <- 'persons_who_walk_to_work'
ggsave(paste('/tmp/',fn,'.jpg',sep=''), width=1920, height=1080, units='px', dpi=110)
ggsave(paste('/tmp/',fn,'.svg',sep=''), width=1080, height=1920, units='px', dpi=110, device = grDevices::svg)
system(paste('scour /tmp/',fn,'.svg /tmp/',fn,'.svgz',sep=''))
