

library(tidyverse)
library(ggraph)
library(tidygraph)
library(geosphere)
library(igraph)

air <- read_csv("https://raw.githubusercontent.com/datasets/airport-codes/master/data/airport-codes.csv", na = character())

air %>% 
  group_by(type) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))


cc <- strsplit(air$coordinates, "[,]")

air <- air %>%  
  mutate(lon = unlist(cc)[2*(1:length(air$coordinates))-1],
         lat = unlist(cc)[2*(1:length(air$coordinates))  ])



# Subset large ------------------------------------------------------------

air_large <- air %>% filter(type=="large_airport") %>% group_by(name) %>% summarise_all(.funs=first) %>% slice(1:200)

m <- air_large %>% select(lat, lon) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>% 
  as.matrix()
  
dista <- distm(m, fun = distHaversine)

dista <- as.data.frame(dista)
names(dista) <- air_large$name
dista$name <- air_large$name

mm <- dista %>% gather(variable, value,-name)






tg2 <- as_tbl_graph(mm) %E>% 
  filter(value < 	600000) %N>%
  filter(!node_is_isolated())

#summary(tg2)

p <- tg2 %>% 
  ggraph(layout = "stress") + 
  geom_edge_link() + 
  geom_node_point() +
  #geom_node_text(aes(label = name), size=2)
  labs(title="map")

p

tg2 %N>%  
  filter(!node_is_isolated()) %>% 
  ggraph('circlepack', weight = 'value') + 
  geom_edge_link() + 
  geom_node_point() +
  coord_fixed()



ggsave(p, file="hello.jpg", width = 5, height = 5, units = "in")



















tg2 %>% ggraph() + 
  geom_edge_link(aes(width=value)) + 
  geom_node_point(size = 1, colour = 'steelblue')






dista$iso <- air_large$iso_country














df <- data.frame(id = air_large$name, region = air_large$continent)

names(mm) <- c("from", "to", "value")

tbl_graph(edges=mm, nodes = df) %E>% 
  filter(value < 700000) %N>%
  filter(!node_is_isolated()) %>% 
  ggraph(layout = "kk") + 
  geom_edge_link() + 
  geom_node_point(aes(color=region),size = 5) +
  geom_node_text(aes(label = id), size=2) + facet_nodes(~region)













