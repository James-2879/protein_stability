library(tidyverse)
library(roxygen2)

add_metadata <- function(data, tank, time) {
  data %>% 
    mutate("dosage" = substr(dose, 1, 1)) %>% 
    mutate("repeat" = substr(dose, 2, 2)) %>% 
    rename("wine_hot" = "wine hot",
           "wine_cold" = "wine cold") %>% 
    mutate("tank" = tank) %>% 
    mutate("time" = time) %>% 
    select(-dose) %>% 
    select(tank, time, dosage, `repeat`, water, wine_hot, wine_cold) %>% 
    mutate(wine_hot = if_else(wine_hot == "-", water, as.double(wine_hot))) %>% 
    mutate(wine_cold = if_else(wine_cold == "-", water, as.double(wine_cold)))
}

pre_2305 <- read_tsv(file = "~/Documents/bento/data/input/2305_pre.tsv") %>% 
  add_metadata(tank = 2305, time = 0)
pre_2309 <- read_tsv(file = "~/Documents/bento/data/input/2309_pre.tsv") %>% 
  add_metadata(tank = 2309, time = 0)
pre_1807 <- read_tsv(file = "~/Documents/bento/data/input/1807_pre.tsv") %>% 
  add_metadata(tank = 1807, time = 0)
pre_422 <- read_tsv(file = "~/Documents/bento/data/input/422_pre.tsv") %>% 
  add_metadata(tank = 422, time = 0)

post_2305 <- read_tsv(file = "~/Documents/bento/data/input/2305_post.tsv") %>% 
  add_metadata(tank = 2305, time = 1)
post_2309 <- read_tsv(file = "~/Documents/bento/data/input/2309_post.tsv") %>% 
  add_metadata(tank = 2309, time = 1)
post_1807 <- read_tsv(file = "~/Documents/bento/data/input/1807_post.tsv") %>% 
  add_metadata(tank = 1807, time = 1)
post_422 <- read_tsv(file = "~/Documents/bento/data/input/422_post.tsv") %>% 
  add_metadata(tank = 422, time = 1)

all_data <- bind_rows(pre_2305, pre_2309, pre_1807, pre_422,
                      post_2305, post_2309, post_1807, post_422) %>% 
  pivot_longer(cols = (5:7), names_to = "solvent", values_to = "turbidity") %>% 
  mutate("dosage" = as.double(dosage))

remove(pre_2305, pre_2309, pre_1807, pre_422,
       post_2305, post_2309, post_1807, post_422) 



# maybe find the percent different between the media as a basic metric o efficiency

# ---- Graphing

plot_scatter <- all_data %>%
  filter(time == 1) %>%
  # filter(dosage > 2) %>%
  ggplot(mapping = aes(
    x = dosage,
    y = log10(turbidity),
    group = solvent
  )) +
  geom_point() +
  geom_hline(yintercept = log10(2), color = "#5cd65c") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "darkgrey") +
  geom_vline(xintercept = 5.15, linetype = "dashed", color = "darkgrey") +
  geom_smooth(aes(group=1), se = FALSE) +
  geom_smooth(method = "lm", aes(group=1), se = FALSE, color = "red", linetype = "dashed") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )
png(filename = "~/Downloads/scatter.png", width = 3000, height = 1500, res = 300)
plot_scatter
dev.off()

plot_scatter_minimal <- all_data %>%
  filter(time == 1) %>%
  ggplot(mapping = aes(
    x = dosage,
    y = log10(turbidity),
    group = solvent
  )) +
  # geom_point() +
  geom_hline(yintercept = log10(2), color = "#5cd65c") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )
plot_scatter_minimal

plot_scatter_trends <- plot_scatter_minimal + 
  geom_smooth(data = all_data %>% filter(solvent == "wine_hot", time == 1), 
              aes(x = dosage,
                  y = log10(turbidity),
                  color = "hot wine"),
              se = FALSE,
              # color = "red"
  ) +
  geom_smooth(data = all_data %>% filter(solvent == "wine_cold", time == 1), 
              aes(x = dosage,
                  y = log10(turbidity),
                  color = "cold wine"),
              se = FALSE,
              # color = "blue"
  ) + 
  geom_smooth(data = all_data %>% filter(solvent == "water", time == 1), 
              aes(x = dosage,
                  y = log10(turbidity),
                  color = "water"),
              se = FALSE,
              # color = "orange"
  ) +
  scale_color_manual(name = "Solvent", values =c("blue", "red", "orange"))

png(filename = "~/Downloads/trends.png", width = 3000, height = 1500, res = 300)
plot_scatter_trends
dev.off()
  
heatmap <- all_data %>% 
  filter(time == 1) %>% 
  # unite(col = repeat_solvent, `repeat`, solvent) %>% 
  ggplot(mapping = aes(x = factor(solvent, level = c("water", "wine_hot", "wine_cold")), y = dosage, fill = log10(turbidity))) +
  geom_tile() +
  scale_fill_gradient2(low = "green",
                       mid = "white",
                       high = "orange",
                       midpoint = log10(2)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  ) +
  xlab("solvent") +
  labs(title = "Heatmap of solvent impact on stability across dosages",
       subtitle = "Orange indicates instability, white indicates 'just stable', green indicates stable")
 
png(filename = "~/Downloads/heatmap.png", width = 3000, height = 1500, res = 300) 
heatmap
dev.off()


# ---- extra


NTU <- 40.5
required_addition <- (log10(2)-log10(NTU))


# Percent differences


data <- all_data %>% 
  filter(time == 1) %>% 
  select(-time) %>% 
  group_by(dosage, solvent) %>% 
  summarise("mean" = mean(turbidity))

library(plotly)
temp_plotly <- ggplotly(plot_scatter_trends)
temp_plotly
