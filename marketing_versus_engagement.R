
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

Results <- list()
s <- 1
for (p in 1:3){
   pe <- 3 - p/2
   for (m in 2:4){
   me <- m/10
      for (i in 20:40){
         j <- 40 - i
         print(paste0("Marketing Elasticity: ", me))
         print(paste0("Price Elasticity: ", pe))
         marketing_percent <- i/100
         discount_percent <- j / 100
         price_scalar <-  1 - discount_percent
         engagement_scalar <- exp(pe*discount_percent)
         marketing_scalar <- marketing_percent/0.2
         print(paste0("Marketing %: ", marketing_percent))
         print(paste0("Discount %: ", discount_percent))
         print(paste0("Price Scalar: ", price_scalar))
         print(paste0("Engagement Scalar: ", engagement_scalar))
         print(paste0("Marketing Scalar: ", marketing_scalar))
         cat("\n")
         scenario <- run_scenario(marketing_elasticity=me, engagement=1, price=60, n=60, initial_marketing=500000/12,
                                   marketing_allocation=0.2, base=1000, survival_rate=0.9, gm=0.4, initial_dropoff=0.15, 
                                   maxlim_revenue=1400, maxlim_cac=500, 
                                   price_boost=price_scalar, marketing_boost=marketing_scalar, engagement_boost=engagement_scalar,
                                   boost_year=4)
         print(paste0("S = ", s))
         Results[[s]] <- filter(scenario[[2]], Year==5) %>% 
           mutate(Marketing_Elasticity=me, 
                  Price_Elasticity=pe,
                  Marketing_Allocation=marketing_percent/(discount_percent+marketing_percent),
                  Marketing_Percent_of_Revenue = marketing_percent,
                  Engagement_Boost=engagement_scalar, 
                  Discount = discount_percent, 
                  Year5_Revenue=Annual_Revenue) %>%
           select(Marketing_Elasticity, Price_Elasticity, Engagement_Boost, Marketing_Allocation, Discount, Marketing_Percent_of_Revenue, Year5_Revenue)
         s <- s+1
      }
   }
}

Scenarios <- data.frame(rbindlist(Results)) %>% 
  arrange(Marketing_Elasticity, Price_Elasticity, -Year5_Revenue) %>% 
  group_by(Marketing_Elasticity, Price_Elasticity) %>%
  filter(as.numeric(row_number())==1)
  
