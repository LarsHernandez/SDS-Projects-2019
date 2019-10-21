



g <- fri_n2
V(g)$name <- letters[1:vcount(g)]

layouts <- c("kk", "mds", "randomly", "circle","sphere")

long2 <- lapply(layouts, create_layout, graph = g) %>%
  enframe(name = "frame") %>%
  unnest()
long2 <- rbind(create_layout(graph=g, layout="kk") %>% mutate(frame=1),
               create_layout(graph=g, layout="mds") %>% mutate(frame=2),
               create_layout(graph=g, layout="kk") %>% mutate(frame=3),
               create_layout(graph=g, layout="mds") %>% mutate(frame=4),
               create_layout(graph=g, layout="kk") %>% mutate(frame=5))

set.seed(3)
create_layout(graph=g, layout="sphere") %>% mutate(frame=1)

long2 <- lapply(c(1:5), function(x) {create_layout(graph = g, layout="sphere")}) %>%
  enframe(name = "frame") %>%
  unnest()

edges_df <- igraph::as_data_frame(g, "edges") %>% 
  tibble::rowid_to_column() %>%
  gather(end, name, -rowid) %>%
  left_join(long2 %>% select(frame, name, x, y)) %>%
  gather(coord, val, x:y) %>%
  mutate(col = paste0(coord, if_else(end == "to", "end", ""))) %>%
  select(frame, rowid, col, val) %>%
  arrange(frame, rowid) %>%
  spread(col, val) %>%
  left_join(long2 %>% select(frame, x, y, start_name = name)) %>%
  left_join(long2 %>% select(frame, xend = x, yend = y, end_name = name)) %>%
  unite(edge_name, c("start_name", "end_name"))

a <- ggplot() +
  geom_segment(data = edges_df, aes(x = x, xend = xend, y = y, yend = yend), color="white") +
  ease_aes("quadratic-in-out") +
  transition_states(frame, state_length = 0.5) + 
  scale_x_continuous(limits=c(-5,5)) + 
  scale_y_continuous(limits=c(-5,5)) + 
  theme_void() + theme(plot.background = element_rect(fill="black"))

animate(a, nframes = 200, fps = 20, width = 300, height = 300)





















layouts <- c(1:5)

long2 <- lapply(c(1:5), function(x) {create_layout(graph = g, layout="kk")}) %>%
  enframe(name = "frame") %>%
  unnest()





























