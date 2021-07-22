# Load libraries ----
library(tidyverse)
library(shadia)

# Shad variable with no by-catch ----
# . Read in data if not pre-compiled ----
files <- dir("results")[grep('_variable', dir("results"))]

res <- vector(mode='list', length=length(files))

for(i in 1:length(files)){
  # Load file
  load(paste0("results/", files[i]))
  
  # Extract results list from output list
  out <- plotter
      
  # Add to res list
  res[[i]] <- out
}

pdata <- do.call(rbind, res)

# . Figure 2 ----
# Line graphs of population size by upstream/downstream passage
# jpeg(
#   filename = paste0('results/Figure2.jpeg'),
#   height = 1200,
#   width = 2400,
#   res = 300,
#   pointsize = 6
#   )

# Summary data for plotting
plotter <- pdata %>%
  group_by(river, downstream, downstream_juv, upstream) %>%
  summarize(
    pop=mean(pop),
    lci=CI(pop)[1],
    uci=CI(pop)[2],
    samp = sum(samp),
    .groups = "keep"
    )

# Convert grouping vars to character
plotter <- plotter %>%
  mutate(
    downstream = as.character(downstream),
    downstream_juv = paste0("Juvenile downstream = ", downstream_juv))

# Annotation text
ann_text <- data.frame(
  label = c("Juvenile downstream survival = 0.90",
            "Juvenile downstream survival = 0.95", 
            "Juvenile downstream survival = 1.00"),
  downstream_juv = c("Juvenile downstream = 0.9",
            "Juvenile downstream = 0.95", 
            "Juvenile downstream = 1"),
  downstream = as.character(1),
  y = rep(3.9e6, 3),
  x = rep(0, 3)
)

# Scale enforcement by row
scalar <- data.frame(
  riv = sort(rep(unique(plotter$river), 3)),
  post = sort(rep(c(2.5e5, 2.5e6, 3.5e6, 2.5e6, 1e6, 2e6, 2e5, 3e6), 3)),
  upstream = 0.05
)

# Plotting code
ggplot(plotter,
       aes(x = upstream, y = pop, 
           color = downstream, fill = downstream)) +
  geom_line() +
  geom_ribbon(
    aes(x = upstream, ymin = lci, ymax = uci, color = NULL),
    alpha = 0.1) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  # geom_line(aes(y = 2e6), color = "gray40", lty = 2, lwd = .25) +
  facet_wrap(~ river + downstream_juv, ncol = 3, scales = "free") +
  xlab("Upstream passage per day") +
  ylab("Millions of spawners") +
  labs(color = "Adult downstream survival",
       fill = "Adult downstream survival") +
  # scale_y_continuous(breaks = seq(0,10e7,.5e6),
  #                    labels = format(seq(0, 100, 0.5), digits=2)) + 
  theme_bw() +
  theme(
    panel.spacing = unit(.02, units = "npc"),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(unit(.5, units = "npc")),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    strip.background = element_blank(),
    strip.text.x = element_blank()
    ) #+
  # geom_text(
  #   mapping = aes(x = x, y = y, label = label),
  #   data = ann_text,
  #   hjust = 0,
  #   color = "black",
  #   size = 1.5
  # )
    
# dev.off()