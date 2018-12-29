
setwd("/Users/kim.larsen/Documents/Code/Pretend_Company_Growth")

source("./src/functions.R")

### Baseline scenario: Create a company
baseline <- run_scenario(marketing_elasticity=0.30, engagement=1, price=60, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, base=1000, survival_rate=0.9, gm=0.4, initial_dropoff=0.15, 
                          maxlim_revenue=1400, maxlim_cac=500)

setwd("/Users/kim.larsen/Documents/Code/Pretend_Company_Growth/charts_optimization")

ggsave(baseline[[3]], file="baseline_revenue.png", device = "png", dpi=72, width=9, height=6)
ggsave(baseline[[4]], file="baseline_cac.png", device = "png", dpi=72, width=9, height=6)
ggsave(baseline[[6]], file="baseline_churn_acq_ratio.png", device = "png", dpi=72, width=9, height=6)

### Optimize

View(Scenarios)