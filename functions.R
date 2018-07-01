library(data.table)
library(ggplot2)
library(dplyr)
library(grid)
library(cowplot)

### function to create a survival curve
get_survival_curve <- function(p0, p, length){
  r <- rep(1, length)
  r[2] <- p0
  for (i in 3:length){
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
combine_cohorts <- function(marketing_elasticity=0.3, engagement=1, price=NA, n=60, marketing_allocation=NA, survival_rate=NA,
                            initial_dropoff=NA, retention_boost=0, engagement_boost=1, marketing_boost=1, gm=NA,
                            initial_marketing=500000/12, base=1000, boost_year=3, fixed_marketing_plan=NULL, maxlim_money=NULL, maxlim_units=NULL){
  cohorts <- list()
  df <- list()
  marketing <- initial_marketing
  for (i in 1:n){
    acquisition <- base * (marketing+1)**marketing_elasticity
    marginal_cac <- 1 / (base*( (marketing+2)**marketing_elasticity - (marketing+1)**marketing_elasticity))
    year <- ceiling(i/12)
    if (year >= boost_year){
       s <- get_survival_curve(initial_dropoff, survival_rate+retention_boost, n)
    } else{
       s <- get_survival_curve(initial_dropoff, survival_rate, n)
    }
    cohorts[[i]] <- create_cohort(i, acquisition, s, engagement, price)
    customers <- sum(data.frame(rbindlist(cohorts))[,i+1])
    revenue <- customers*price*engagement
    df[[i]] <- data.frame(Month=i, Year=year, Marketing_Spend=marketing, Acquisition=acquisition, Revenue=revenue, CAC=marketing/acquisition, Customers=customers, LTV=sum(s[1:24]*price*engagement*gm), Marginal_CAC=marginal_cac)
    if (is.null(fixed_marketing_plan)){
       marketing <- max(marketing_allocation*revenue, initial_marketing)
    } else{
      if (length(fixed_marketing_plan) != n){
        print("Error: length of marketing plan is different than n")
      }
      marketing <- fixed_marketing_plan[i]
    }
    if (year >= boost_year){
      if (is.null(fixed_marketing_plan)){
         marketing <- max(marketing_allocation*marketing_boost*revenue, initial_marketing)
      } else{
         marketing <- fixed_marketing_plan[i]*marketing_boost
      }
    }
  }
  d <- data.frame(rbindlist(df))
  dd <- d %>% 
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
    filter(EOY==1) %>%
    ungroup() %>%
    mutate(YOY_Revenue_Growth=(Annual_Revenue-lag(Annual_Revenue))/lag(Annual_Revenue),
           YOY_Cust_Delta=Customers_EOY-lag(Customers_EOY),
           Annual_Churn=Annual_Acquisition-YOY_Cust_Delta)
  
  ## create the key charts
  ### Revenue growth
  
  max <- max(max(dd$Annual_Revenue), max(dd$Annual_Marketing_Spend))/1000000
  if (is.null(maxlim_money)==FALSE){
    max <- max(max, maxlim_money)
  }

  p1 <- ggplot(data=dd, aes(x=Year, y=Annual_Revenue/1000000)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Revenue ($MM)") + 
    scale_y_continuous(labels = scales::dollar,limits = c(0, max))

  ### Marketing spend
  p2 <- ggplot(data=filter(dd, Year>1), aes(x=Year, y=YOY_Revenue_Growth)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("YoY Growth") + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 2))

  max <- max(max(dd$Annual_CAC), max(dd$Annual_Marginal_CAC), max(dd$Annual_LTV))
  if (is.null(maxlim_money)==FALSE){
    max <- max(max, maxlim_money)
  }
  
  ### CAC by year
  p3 <- ggplot(data=dd, aes(x=Year, y=Annual_CAC)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("CAC") + 
    scale_y_continuous(labels = scales::dollar, limits = c(0, max))
  
  ### Marginal CAC
  p4 <- ggplot(data=dd, aes(x=Year, y=Annual_Marginal_CAC)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Marginal CAC") + 
    scale_y_continuous(labels = scales::dollar, limits = c(0, max))

  max <- max(max(dd$Annual_Acquisition), max(dd$Annual_Churn))
  if (is.null(maxlim_units)==FALSE){
    max <- max(max, maxlim_units)
  }
  
  ### Churn
  p5 <- ggplot(data=filter(dd, Year>1), aes(x=Year, y=Annual_Churn)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Churned Customers") + 
    scale_y_continuous(labels = scales::comma, limits = c(0, max))

  ### Acquisition
  p6 <- ggplot(data=dd, aes(x=Year, y=Annual_Acquisition)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Acquired Customers") + 
    scale_y_continuous(labels = scales::comma, limits = c(0, max))
  
  ### Churn divided by acquisition
  p7 <- ggplot(data=filter(dd, Year>1), aes(x=Year, y=Annual_Churn/Annual_Acquisition)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Churn/Acquisiton") + 
    geom_hline(yintercept=1, linetype=3)
  
  return(list(d, dd, plot_grid(p1, p2), plot_grid(p3, p4), plot_grid(p5, p6), p7))
}

