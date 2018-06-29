source("/Users/thirdlovechangethisname/Documents/Code/Pretend_Company_Growth/functions.R")

### Create a company
growth1 <- combine_cohorts(marketing_elasticity=0.25, engagement=2, price=35, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, base=1000, survival_rate=0.9)

growth1[[3]]
growth1[[4]]

d <- filter(growth1[[1]], EOY==1) %>%
  select(Year, Customers_EOY, Annual_Revenue, Annual_Acquisition, Annual_Marketing_Spend, Annual_CAC, Annual_LTV, Annual_Marginal_CAC, Marketing_Percent_of_Revenue)


### Add more marketing in year 3+
growth2 <- combine_cohorts(marketing_elasticity=0.25, engagement=2, price=35, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, marketing_boost=1.5, survival_rate=0.9)

growth2[[3]]
growth2[[4]]

### Instead of adding more marketing, improve retention by 300 bps starting year 3
growth3 <- combine_cohorts(marketing_elasticity=0.25, engagement=2, price=35, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, retention_boost=0.03, survival_rate=0.9,
                          fixed_marketing_plan=growth1[[1]]$Marketing_Spend)

growth3[[3]]
growth3[[4]]

