library(data.table)
library(ggplot2)
library(dplyr)

### function to create a survival curve
get_survival_curve <- function(p0, p, length){
  r <- rep(p0, length)
  for (i in 2:length){
    r[i] <- p0 * p^(i-1)
  }
  return(r)
}

### function to create cohorts
create_cohort <- function(id, acquisition, p_survival, engagement, price){ 
  zeros <- rep(0, id-1)
  window <- length(p_survival)
  row <- data.frame(id, t(c(zeros, t(p_survival[1:(window-(id-1))])*acquisition)))
  names(row) <- c("Cohort", paste0("s", seq_along(1:window)))
  return(row)
}

### combine cohorts
combine_cohorts <- function(marketing_elasticity=0.3, engagement=1, price=NA, n=60, marketing_allocation=0.2, survival_rate=0.9,
                            initial_dropoff=0.8, retention_boost=0, engagement_boost=1, marketing_boost=0, 
                            initial_marketing=500000/12, base=1000, boost_year=3){
  cohorts <- list()
  df <- list()
  s <- get_survival_curve(initial_dropoff, survival_rate, n)
  marketing <- initial_marketing
  for (i in 1:n){
    acquisition <- base * (marketing+1)**marketing_elasticity
    marginal_cac <- 1 / (base*( (marketing+2)**marketing_elasticity - (marketing+1)**marketing_elasticity))
    cohorts[[i]] <- create_cohort(i, acquisition, s, engagement, price)
    customers <- sum(data.frame(rbindlist(cohorts))[,i+1])
    revenue <- customers*price*engagement
    year <- ceiling(i/12)
    df[[i]] <- data.frame(Month=i, Year=year, Marketing_Spend=marketing, Acquisition=acquisition, Revenue=revenue, CAC=marketing/acquisition, Customers=customers, LTV=sum(s[1:24]*price*engagement), Marginal_CAC=marginal_cac)
    marketing <- max(marketing_allocation*revenue, initial_marketing)
    if (year >= boost_year){
      marketing <- max((marketing_allocation+marketing_boost)*revenue, initial_marketing)
      s <- get_survival_curve(initial_dropoff, survival_rate+retention_boost, n)
    }
  }
  d <- data.frame(rbindlist(df)) %>% 
    group_by(Year) %>% 
    mutate(EOY=as.numeric(row_number()==n()),
           Customers_EOY=max(Customers*EOY), 
           Annual_Revenue=sum(Revenue), 
           Annual_Acquisition=sum(Acquisition), 
           Annual_Marketing_Spend=sum(Marketing_Spend),
           Annual_CAC=Annual_Marketing_Spend/Annual_Acquisition,
           Annual_LTV=mean(LTV),
           Annual_Marginal_CAC=sum(Marginal_CAC*Marketing_Spend)/sum(Marketing_Spend), 
           Marketing_Percent_of_Revenue=Annual_Marketing_Spend/Annual_Revenue) %>%
    ungroup()
  return(d)
}

