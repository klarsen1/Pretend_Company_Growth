

growth <- combine_cohorts(marketing_elasticity=0.2, engagement=2, price=40, n=60, initial_marketing=500000/12,
                          marketing_allocation=0.2, marketing_boost=0.2, base=1000, survival_rate=0.9) %>% 
  filter(EOY==1) %>%
  select(Year, Customers_EOY, Annual_Revenue, Annual_Acquisition, Annual_Marketing_Spend, Annual_CAC, Annual_LTV, Annual_Marginal_CAC, Marketing_Percent_of_Revenue)

ggplot(data=growth, aes(x=Year, y=Annual_Revenue)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Revenue") + 
  scale_y_continuous(labels = scales::dollar)

ggplot(data=growth, aes(x=Year, y=Annual_CAC)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("CAC") + 
  scale_y_continuous(labels = scales::dollar)

ggplot(data=growth, aes(x=Year, y=Marketing_Percent_of_Revenue)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Marketing / Revenue") + 
  scale_y_continuous(labels = scales::percent)

