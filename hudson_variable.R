# Load R packages
library(snowfall)
library(rlecuyer)
library(shadia)

# Initialize snowfall
sfInit(parallel=TRUE, cpus=7, type="SOCK")

# Define a function that can be called in parallel
# to run dam passage performance standard model
wrapper <- function(x) {
        
  # Randomly sampling passage efficiencies
  # Upstream passage through dams
  upstream_p <- seq(0, 1, .1)   
  upstreamx <- sample(upstream_p, 1, replace = TRUE)
  
  # Adult downstream survival through dams
  downstream_p <- seq(0.5, 1, .1)
  downstreamx <- sample(downstream_p, 1, replace = TRUE)
  
  # Juvenile downstream survival through dams
  downstream_juvp <- c(0.90, 0.95, 1.00)
  downstream_juvx <- sample(downstream_juvp, 1, replace = TRUE)
  
  # Run the model with desired settings or
  # a random set of conditions
  res1 <- mohawkHudsonRiverModel(
    species = 'shad',
    pMohawk = 0,
    nRuns = 1,
    nYears = 20,
    n_adults = rnorm(1, 1e4, 100),
    timing = rep(1, 26),
    upstream = list(
      federal = upstreamx,
      C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1,
      E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1,
      E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11 = 1, E12 = 1,
      E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1,
      E19 = 1, E20 = 1
    ),
    downstream = list(
      federal = downstreamx,
      C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1,
      E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1,
      E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11 = 1, E12 = 1,
      E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1,
      E19 = 1, E20 = 1
    ),
    downstream_juv = list(
      federal = downstream_juvx,
      C01 = 1, C02 = 1, C03 = 1, C04 = 1, C05 = 1, C06 = 1,
      E02 = 1, E03 = 1, E04 = 1, E05 = 1, E06 = 1,
      E07 = 1, E08 = 1, E09 = 1, E10 = 1, E11 = 1, E12 = 1,
      E13 = 1, E14 = 1, E15 = 1, E16 = 1, E17 = 1, E18 = 1,
      E19 = 1, E20 = 1
    ),  
    lockMortality = 0,
    inRiverF = 0,
    commercialF = 0,
    bycatchF = 0,
    indirect = 1,
    latent = 1,
    watershed = TRUE,
    k_method = 'cumulative',
    sensitivity = FALSE,
    spatially_explicit_output = FALSE,
    output_years = "last",
    output_p_repeat = FALSE  
    )

  # Define the output list
  retlist <- list(
    sim = res1)       
  return(retlist)
}

# Load packages on workers
sfLibrary(shadia)
sfLibrary(rlecuyer)

# Number of iterations
niterations <- 100

# Store and print start time
start <- Sys.time()
start

# Distribute pre-defined wrapper function
# to workers using sfLapply()
result <- sfLapply(1:niterations, wrapper) 

# Calculate run time and print
Sys.time() - start

# Stop snowfall
sfStop()


# Extract results list from output list
out <- lapply(result, function(x) x[[c('sim')]])

# Extract user inputs and population metrics
# res <- lapply(out, function(x) x[[c('res')]])
resdf <- do.call(rbind, out)

# . Abundance at mouth ----
library(tidyverse)
plotter <- resdf %>%
  group_by(downstream_juv, downstream, upstream) %>%
  summarize(
    pop=mean(populationSize),
    lci=CI(populationSize)[1],
    uci=CI(populationSize)[2],
    samp = n(),
    river = "Hudson",
    .groups = "keep"
    )


# Save result to .rda file
save(plotter, file = "results/hudson_variable.rda")

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

# Line graphs of population size by upstream/downstream passage
# Plotting code
ggplot(plotter, 
       aes(x = upstream, 
           y = pop, 
           color = factor(downstream), 
           fill = factor(downstream))) +
  geom_ribbon(
    aes(x = upstream, ymin = lci, ymax = uci, color = NULL), alpha = 0.10) +
  geom_line() +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
  facet_wrap(~ downstream_juv) +
  xlab("Upstream passage (72 hours)") +
  ylab("Millions of spawners") +
  labs(color = "Adult downstream survival",
       fill = "Adult downstream survival") +
  scale_y_continuous(breaks = seq(0,10e7,.5e6),
                     labels = format(seq(0, 100, 0.5), digits=2)) + 
  theme_bw() +
  theme(
    panel.spacing = unit(.02, units = "npc"),
    # panel.grid = element_blank(),
    legend.position = "top",
    legend.box = "horizontal",
    legend.margin = margin(unit(.5, units = "npc")),
    axis.text = element_text(color = "black"),
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    strip.background = element_blank(),
    strip.text.x = element_blank()
    ) +
  geom_text(
    mapping = aes(x = x, y = y, label = label),
    data = ann_text,
    hjust = 0,
    color = "black",
    size = 2.5
  ) 
