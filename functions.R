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
combine_cohorts <- function(marketing_elasticity=0.3, engagement=1, price=NA, n=60, marketing_allocation=0.2, survival_rate=0.9,
                            initial_dropoff=0.8, retention_boost=0, engagement_boost=1, marketing_boost=1, 
                            initial_marketing=500000/12, base=1000, boost_year=3, fixed_marketing_plan=NULL, maxlim=NULL){
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
    df[[i]] <- data.frame(Month=i, Year=year, Marketing_Spend=marketing, Acquisition=acquisition, Revenue=revenue, CAC=marketing/acquisition, Customers=customers, LTV=sum(s[1:12]*price*engagement), Marginal_CAC=marginal_cac)
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
      s <- get_survival_curve(initial_dropoff, survival_rate+retention_boost, n)
    }
  }
  dd <- data.frame(rbindlist(df)) %>% 
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
    ungroup()
  
  ## create the key charts
  ### Revenue growth
  
  max <- max(max(dd$Annual_Revenue), max(dd$Annual_Marketing_Spend))/1000000
  if (is.null(maxlim)==FALSE){
    max <- max(max, maxlim)
  }

  p1 <- ggplot(data=dd, aes(x=Year, y=Annual_Revenue/1000000)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Revenue ($MM)") + 
    scale_y_continuous(labels = scales::dollar,limits = c(0, max))

  ### Marketing spend
  p2 <- ggplot(data=dd, aes(x=Year, y=Annual_Marketing_Spend/1000000)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Marketing Spend ($MM)") + 
    scale_y_continuous(labels = scales::dollar, limits = c(0, max))

  max <- max(max(dd$Annual_CAC), max(dd$Annual_Marginal_CAC))
  if (is.null(maxlim)==FALSE){
    max <- max(max, maxlim)
  }
  
  ### CAC by year
  p3 <- ggplot(data=dd, aes(x=Year, y=Annual_CAC)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("CAC") + 
    scale_y_continuous(labels = scales::dollar, limits = c(0, max))
  
  ### Marginal CAC
  p4 <- ggplot(data=dd, aes(x=Year, y=Annual_Marginal_CAC)) + geom_bar(stat="identity", position = "identity") + xlab("Year") + ylab("Marginal CAC") + 
    scale_y_continuous(labels = scales::dollar, limits = c(0, max)) + geom_hline(yintercept=max(growth$Annual_LTV))
  
  return(list(d, plot_grid(p1, p2), plot_grid(p3, p4)))
}

