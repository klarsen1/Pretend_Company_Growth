
setwd("/Users/thirdlovechangethisname/Documents/Code/Pretend_Company_Growth/")

source("functions.R")

### Create a company
growth1 <- combine_cohorts(marketing_elasticity=0.30, engagement=1, price=60, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, base=1000, survival_rate=0.9, gm=1, initial_dropoff=0.80, maxlim_money=1000)

ggsave(growth1[[3]], file="baseline_revenue.png", device = "png", dpi=72, width=9, height=6)
ggsave(growth1[[4]], file="baseline_cac.png", device = "png", dpi=72, width=9, height=6)

growth1[[6]]

### Add more marketing in year 3+
growth2 <- combine_cohorts(marketing_elasticity=0.25, engagement=2, price=35, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, marketing_boost=1.5, survival_rate=0.9, gm=0.4, initial_dropoff=0.80)

growth2[[3]]
growth2[[4]]

### Instead of adding more marketing, improve retention by 300 bps starting year 3
growth3 <- combine_cohorts(marketing_elasticity=0.25, engagement=2, price=35, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, retention_boost=0.03, survival_rate=0.9, gm=0.4,
                          fixed_marketing_plan=growth1[[1]]$Marketing_Spend, initial_dropoff=0.80)

growth3[[3]]
growth3[[4]]

