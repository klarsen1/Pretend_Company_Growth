library(data.table)
library(ggplot2)
library(dplyr)
library(grid)
library(cowplot)

### Helper function to create a survival curve
# p0: the month 1 survival rate
# p: the monthly survival rate after month 1
# length: the length of the outout vector (number of months)
get_survival_curve <- function(p0, p, length){
  r <- rep(1, length)
  r[2] <- p0
  for (i in 3:length){
    r[i] <- p0 * p^(i-1)
  }
  return(r)
}

### Helper Function to create cohorts
# acquisition: size of the cohort
# p_survival: monthly survival rate
# engagement: the average number of monthly transactions made by active customers
# price: the average price associated with a transaction
create_cohort <- function(id, acquisition, p_survival, engagement, price){ 
  zeros <- rep(0, id-1)
  window <- length(p_survival)
  row <- data.frame(id, t(c(zeros, t(p_survival[1:(window-(id-1))])*acquisition)))
  names(row) <- c("Cohort", paste0("s", seq_along(1:window)))
  return(row)
}

### Function to combine cohorts and compute metrics
# marketing_elascity: e.g., 0.3 means that a 10% increase in marketing drives a 3% increase in acquisition.
# engagement: (see the create_cohort function)
# price: (see the create_cohort function)
# marketing_allocation: e.g., 0.2 means that 20% of last month's revenue will be allocated to this month's marketing spend
# survival rate: 1 - churn rate after month 1
# initial_dropoff: churn rate for the first month
# retention_boost: factor to boost retention after a given year (specified y boost_year). 
#                  This is an additive way boost -- e.g., if you put 0.02 and the survival rate is 0.9, you get a boosted survival rate of 0.92
# engagement_boost: factor to boost engagement after a given year (specified y boost_year). This is a multiplier
# marketing_boost: factor to boost engagement after a given year (specified y boost_year). This is a multiplier
# boost_year (optional): see above.
# initial_marketing: marketing spend for the first month
# base: base for the acquisition equation: base * spend^elasticity
# fixed_marketing_plan (optional): a vector of monthly marketing spend values
# n: number of months
# maxlim_revenue (optional): y-axis limit for revenue charts
# maxlim_units (optional): y-axis limit for customer charts
# maxlim_cac (optional): y-axis limit for CAC charts
run_scenario <- function(marketing_elasticity=NULL, engagement=NULL, price=NA, n=60, 
                        marketing_allocation=NA, survival_rate=NA,
                        initial_dropoff=NA, retention_boost=0, engagement_boost=1, marketing_boost=1, gm=NA,
                        initial_marketing=500000/12, base=1000, boost_year=NULL, fixed_marketing_plan=NULL, 
                        maxlim_revenue=NULL, maxlim_units=NULL, maxlim_cac=NULL){
  cohorts <- list()
  df <- list()
  marketing <- initial_marketing
  for (i in 1:n){
    ## Calculate acquisition and retention
    acquisition <- base * (marketing+1)**marketing_elasticity
    marginal_cac <- 1 / (base*( (marketing+2)**marketing_elasticity - (marketing+1)**marketing_elasticity))
    year <- ceiling(i/12)
    if (is.null(boost_year)==FALSE){
       if (year >= boost_year){
          s <- get_survival_curve(1-initial_dropoff, survival_rate+retention_boost, n)
       } else{
         s <- get_survival_curve(1-initial_dropoff, survival_rate, n)
       }
    } else{ 
       s <- get_survival_curve(1-initial_dropoff, survival_rate, n)
    }
    
    ## Create a cohort for the given level of acquisition and retention
    cohorts[[i]] <- create_cohort(i, acquisition, s, engagement, price)
    customers <- sum(data.frame(rbindlist(cohorts))[,i+1])
    revenue <- customers*price*engagement
    df[[i]] <- data.frame(Month=i, Year=year, Marketing_Spend=marketing, Acquisition=acquisition, Revenue=revenue, CAC=marketing/acquisition, Customers=customers, LTV=sum(s[1:24]*price*engagement*gm), Marginal_CAC=marginal_cac)
    
    ## Calculate marketing spend for the next month
    if (is.null(fixed_marketing_plan)){
       marketing <- max(marketing_allocation*revenue, initial_marketing)
    } else{
      if (length(fixed_marketing_plan) != n){
        print("Error: length of marketing plan is different than n")
      }
      marketing <- fixed_marketing_plan[i]
    }
    if (is.null(boost_year)==FALSE){
      if (year >= boost_year){
        marketing <- marketing*marketing_boost  
      }

    }
  }
  
  ## Create the monthly dataframe by collapsing the rows
  d <- data.frame(rbindlist(df))
  
  ## Create the annual dataframe
  dd <- d %>% 
    group_by(Year) %>% 
    mutate(EOY=as.numeric(row_number()==n()),
           Customers_EOY=Customers, 
           Annual_Revenue=sum(Revenue), 
           Annual_Acquisition=sum(Acquisition), 
           Annual_Marketing_Spend=sum(Marketing_Spend),
           Annual_CAC=Annual_Marketing_Spend/Annual_Acquisition,
           Annual_LTV=mean(LTV),
           Annual_Marginal_CAC=sum(Marginal_CAC*Marketing_Spend)/sum(Marketing_Spend), 
           Marketing_Percent_of_Revenue=Annual_Marketing_Spend/Annual_Revenue) %>%
    filter(EOY==1) %>%
    select(Year, Customers_EOY, Annual_Revenue, Annual_Acquisition, Annual_Marketing_Spend, Annual_CAC, Annual_LTV, Annual_Marginal_CAC, Marketing_Percent_of_Revenue) %>%
    ungroup() %>%
    mutate(YOY_Revenue_Growth=(Annual_Revenue-lag(Annual_Revenue))/lag(Annual_Revenue),
           YOY_Cust_Delta=Customers_EOY-lag(Customers_EOY),
           Annual_Churn=Annual_Acquisition-YOY_Cust_Delta)
  
  ## Create some plots fo export

  # Revenue
  max <- max(dd$Annual_Revenue/1000000)
  if (is.null(maxlim_revenue)==FALSE){
    max <- max(max, maxlim_revenue)
  }
  b <- seq(0, max, by=200)
  
  p1 <- ggplot(data=dd, aes(x=Year, y=Annual_Revenue/1000000)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Revenue ($MM)") + 
    scale_y_continuous(labels = scales::dollar,limits = c(0, max), breaks=b)

  # Growth
  p2 <- ggplot(data=filter(dd, Year>1), aes(x=Year, y=YOY_Revenue_Growth)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("YoY Growth") + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 2), breaks=seq(0, 2, by=.20))

  max <- max(max(dd$Annual_CAC), max(dd$Annual_Marginal_CAC))
  if (is.null(maxlim_cac)==FALSE){
    max <- max(max, maxlim_cac)
  } 
  b <- seq(0, max, by=100)
  
  # CAC
  p3 <- ggplot(data=dd, aes(x=Year, y=Annual_CAC)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("CAC") + 
    scale_y_continuous(labels = scales::dollar, limits = c(0, max), breaks=b)
  
  # Marginal CAC
  p4 <- ggplot(data=dd, aes(x=Year, y=Annual_Marginal_CAC)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Marginal CAC") + 
    scale_y_continuous(labels = scales::dollar, limits = c(0, max), breaks=b)

  max <- max(max(dd$Annual_Acquisition/1000000), max(dd$Annual_Churn/1000000))
  if (is.null(maxlim_units)==FALSE){
    max <- max(max, maxlim_units)
  }
  
  # Churn
  p5 <- ggplot(data=filter(dd, Year>1), aes(x=Year, y=Annual_Churn/1000000)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Churned Customers") + 
    scale_y_continuous(labels = scales::comma, limits = c(0, max))

  # Acquisition
  p6 <- ggplot(data=dd, aes(x=Year, y=Annual_Acquisition/1000000)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Acquired Customers (MM)") + 
    scale_y_continuous(labels = scales::comma, limits = c(0, max))
  
  # Churn divided by acquisition
  p7 <- ggplot(data=filter(dd, Year>1), aes(x=Year, y=Annual_Churn/Annual_Acquisition)) +
    geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Churn/Acquisiton") + 
    geom_hline(yintercept=1, linetype=3) + scale_y_continuous(limits = c(0, 1), breaks=seq(0, 2, by=.20))

  # Net LTV
  p8 <- ggplot(data=filter(dd, Year>1), aes(x=Year, y=Annual_LTV)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Net LTV") + 
    geom_hline(yintercept=1, linetype=3)
  
  ### Return the dataframes and the plots
  
  return(list(d, dd, plot_grid(p1, p2), plot_grid(p3, p4), plot_grid(p5, p6), p7, p8))
}

