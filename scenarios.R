
setwd("/Users/thirdlovechangethisname/Documents/Code/Pretend_Company_Growth/")

source("functions.R")

### Baseline scenario: Create a company
growth1 <- run_scenario(marketing_elasticity=0.30, engagement=1, price=60, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, base=1000, survival_rate=0.9, gm=0.4, initial_dropoff=0.15, 
                          maxlim_revenue=1400, maxlim_cac=500)

ggsave(growth1[[3]], file="baseline_revenue.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth1[[4]], file="baseline_cac.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth1[[6]], file="baseline_churn_acq_ratio.png", device = "png", dpi=72, width=9, height=6)

### Scenario 2: Add more marketing in year 3+
growth2 <- run_scenario(marketing_elasticity=0.30, engagement=1, price=60, n=60, initial_marketing=500000/12,
                           marketing_allocation=0.2, base=1000, survival_rate=0.9, gm=0.4, initial_dropoff=0.15, 
                           boost_year=3, marketing_boost=1.5, maxlim_revenue=1400, maxlim_cac=500)

ggsave(growth2[[3]], file="mktgboost_revenue.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth2[[4]], file="mktgboost_cac.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth2[[6]], file="mktgboost_churn_acq_ratio.png", device = "png", dpi=72, width=9, height=6)

### Scenario 3: Instead of adding more marketing, improve retention by 300 bps starting year 3
growth3 <- run_scenario(marketing_elasticity=0.30, engagement=1, price=60, n=60, initial_marketing=500000/12,
                           marketing_allocation=0.2, base=1000, survival_rate=0.9, gm=0.4, initial_dropoff=0.15, 
                           boost_year=3, retention_boost=0.02, fixed_marketing_plan=growth1[[1]]$Marketing_Spend, 
                           maxlim_revenue=1400, maxlim_cac=500)

ggsave(growth3[[3]], file="retentionboost_revenue.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth3[[4]], file="retentionboost_cac.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth3[[6]], file="retentionboost_churn_acq_ratio.png", device = "png", dpi=72, width=9, height=6)

### Scenario 4: Instead of adding more marketing, improve retention by 300 bps starting year 3
growth4 <- run_scenario(marketing_elasticity=0.30, engagement=1, price=60, n=60, initial_marketing=500000/12,
                           marketing_allocation=0.2, base=1000, survival_rate=0.9, gm=0.4, initial_dropoff=0.15, 
                           boost_year=3, retention_boost=0.02, fixed_marketing_plan=growth2[[1]]$Marketing_Spend, 
                           maxlim_revenue=1400, maxlim_cac=500)

ggsave(growth4[[3]], file="doubleboost_revenue.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth4[[4]], file="doubleboost_cac.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth4[[6]], file="doubleboost_churn_acq_ratio.png", device = "png", dpi=72, width=9, height=6)

