
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
for (a in seq(.15, .35, by=.1)){
  for (pe in seq(2, 3.5, by=0.5)){
     for (me in seq(0.2, 0.4, by=0.1)){
        for (i in seq(a-.1, a, by=0.01)){
           print(paste0("Scenario: ", s))
           print(paste0("Marketing Elasticity: ", me))
           print(paste0("Price Elasticity: ", pe))
           print(paste0("Current Allocation: ", a))
           marketing_percent <- i
           discount_percent <- a - i
           price_scalar <-  1 - discount_percent
           engagement_scalar <- (1-discount_percent)^(-pe)
           marketing_scalar <- marketing_percent/a
           print(paste0("Marketing %: ", marketing_percent))
           print(paste0("Discount %: ", discount_percent))
           print(paste0("Price Scalar: ", price_scalar))
           print(paste0("Engagement Scalar: ", engagement_scalar))
           print(paste0("Marketing Scalar: ", marketing_scalar))
           cat("\n")
           scenario <- run_scenario(marketing_elasticity=me, engagement=1, price=60, n=60, initial_marketing=500000/12,
                                     marketing_allocation=a, base=1000, survival_rate=0.9, gm=0.4, initial_dropoff=0.15, 
                                     maxlim_revenue=1400, maxlim_cac=500, 
                                     price_boost=price_scalar, marketing_boost=marketing_scalar, engagement_boost=engagement_scalar,
                                     boost_year=4)
           Results[[s]] <- filter(scenario[[2]], Year==5) %>% 
             mutate(Marketing_Elasticity=me, 
                    Price_Elasticity=pe,
                    Current_Allocation=a,
                    Marketing_Percent_of_Revenue = marketing_percent,
                    Engagement_Boost=engagement_scalar, 
                    Discount = discount_percent, 
                    Year5_Revenue=Annual_Revenue) %>%
             select(Current_Allocation, Marketing_Elasticity, Price_Elasticity, Engagement_Boost, Discount, Marketing_Percent_of_Revenue, Year5_Revenue)
           s <- s+1
        }
     }
  }
}

Scenarios <- data.frame(rbindlist(Results)) %>% 
  arrange(Current_Allocation, Marketing_Elasticity, Price_Elasticity, -Year5_Revenue) %>% 
  group_by(Current_Allocation, Marketing_Elasticity, Price_Elasticity) %>%
  filter(as.numeric(row_number())==1) %>%
  mutate(Scenario=paste0("MA: ", round(100*Current_Allocation,2), "% ME: ", round(100*Marketing_Elasticity,2), "%"))
         
graph <- ggplot(Scenarios, aes(x=Price_Elasticity, y=Discount)) +
  geom_bar(stat="identity") +
  facet_wrap(Scenario ~ ., scales="fixed") +
  xlab("Price Elasticity") +
  ylab("% Incentives") +
  scale_y_continuous(limits = c(0, 0.1), breaks=seq(0, .10, by=.05), labels = scales::percent) + 
  scale_x_continuous(breaks=seq(1.5, 3, by=.5))
graph  


d <- data.frame(matrix(nrow=5, ncol=2))

d[1,1] <- 100
d[1,2] <- 4

d[2,1] <- 90
d[2,2] <- 6

d[3,1] <- 80
d[3,2] <- 12

d[4,1] <- 70
d[4,2] <- 18

d[5,1] <- 60
d[5,2] <- 29


