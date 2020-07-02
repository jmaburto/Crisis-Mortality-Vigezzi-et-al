#-----------------------------------------------------
# Topic: Functions for decomposition EDSD thesis
# Author: Serena Vigezzi (code from Allison and José Manuel)
# Date: 7/04/2020
#-----------------------------------------------------

# Absolute variability
#------------------------------

# Standard deviation
sd.dec <- function(mx){
  
  # Other quantities for lifetables
  Age=0:110
  
  ax=c(0.31,rep(0.5,110))
  
  # Calculating lifetables
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  mx_na <- which(is.na(mx==T))
  # if so, we can close the lifetable at the last age before the NaN
  last <- mx_na[1]-1
  if(is.na(last)==F){
    mx <- mx[1:last]
    n <- n[1:last]
    ax <- ax[1:last]
    n[last] <- ax[last] <- 1/mx[last]
    Age <- Age[1:last]
  }
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- n*mx / (1+(n-ax)*mx)
  px <- 1 - qx
  # number of survivors using a radix of 100 000
  lx <- rep(NA,length(qx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*px[i]
  }
  lx <- lx[1:length(n)]
  # number of deaths (difference between the number of survivors at each age)
  # for open-aged interval it is the number of survivors at the open aged interval
  dx <- c(abs(diff(lx)),lx[length(n)])
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  1
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  # 
  sd <-  sqrt(sum(dx/lx[1]*(Age + ax - ex[1])^2))
  
  return(sd)
}

# Absolute Gini
aG.dec <- function(mx){
  
  # Other quantities for lifetables
  Age=0:110
  ax=c(0.31,rep(0.5,110)) # The last year will probably be off
  
  # Calculating lifetables
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  mx_na <- which(is.na(mx==T))
  # if so, we can close the lifetable at the last age before the NaN
  last <- mx_na[1]-1
  if(is.na(last)==F){
    mx <- mx[1:last]
    n <- n[1:last]
    ax <- ax[1:last]
    n[last] <- ax[last] <- 1/mx[last]
    Age <- Age[1:last]
  }
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- n*mx / (1+(n-ax)*mx)
  px <- 1 - qx
  # number of survivors using a radix of 100 000
  lx <- rep(NA,length(qx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*px[i]
  }
  lx <- lx[1:length(n)]
  # number of deaths (difference between the number of survivors at each age)
  # for open-aged interval it is the number of survivors at the open aged interval
  dx <- c(abs(diff(lx)),lx[length(n)])
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  1
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  # 
  aG <- ex[1]*Gini.fun(x = Age,nax = ax,ndx = dx/100000,ex = ex)
  return(aG)
}

# Lifespan disparity

edag.dec <- function(mx){
  
  # Other quantities for lifetables
  Age=0:110
  ax=c(0.31,rep(0.5,110)) # The last year will probably be off
  
  # Calculating lifetables
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  mx_na <- which(is.na(mx==T))
  # if so, we can close the lifetable at the last age before the NaN
  last <- mx_na[1]-1
  if(is.na(last)==F){
    mx <- mx[1:last]
    n <- n[1:last]
    ax <- ax[1:last]
    n[last] <- ax[last] <- 1/mx[last]
    Age <- Age[1:last]
  }
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- n*mx / (1+(n-ax)*mx)
  px <- 1 - qx
  # number of survivors using a radix of 100 000
  lx <- rep(NA,length(qx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*px[i]
  }
  lx <- lx[1:length(n)]
  # number of deaths (difference between the number of survivors at each age)
  # for open-aged interval it is the number of survivors at the open aged interval
  dx <- c(abs(diff(lx)),lx[length(n)])
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  1
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  # 
  edag <- sum((ax*c(ex[-1L], 0) + (1-ax)*ex)*dx/lx[1])
  return(edag)
}


# Relative variability
#----------------------

# Coefficient of variation

cv.dec <- function(mx){
  
  # Other quantities for lifetables
  Age=0:110
  
  ax=c(0.31,rep(0.5,110))
  
  # Calculating lifetables
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  mx_na <- which(is.na(mx==T))
  # if so, we can close the lifetable at the last age before the NaN
  last <- mx_na[1]-1
  if(is.na(last)==F){
    mx <- mx[1:last]
    n <- n[1:last]
    ax <- ax[1:last]
    n[last] <- ax[last] <- 1/mx[last]
    Age <- Age[1:last]
  }
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- n*mx / (1+(n-ax)*mx)
  px <- 1 - qx
  # number of survivors using a radix of 100 000
  lx <- rep(NA,length(qx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*px[i]
  }
  lx <- lx[1:length(n)]
  # number of deaths (difference between the number of survivors at each age)
  # for open-aged interval it is the number of survivors at the open aged interval
  dx <- c(abs(diff(lx)),lx[length(n)])
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  1
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  # 
  sd <-  sqrt(sum(dx/lx[1]*(Age + ax - ex[1])^2))
  cv <- sd/ex[1]
  
  return(cv)
}

# Relative Gini

rG.dec <- function(mx){
  
  # Other quantities for lifetables
  Age=0:110
  ax=c(0.31,rep(0.5,110)) # The last year will probably be off
  
  # Calculating lifetables
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  mx_na <- which(is.na(mx==T))
  # if so, we can close the lifetable at the last age before the NaN
  last <- mx_na[1]-1
  if(is.na(last)==F){
    mx <- mx[1:last]
    n <- n[1:last]
    ax <- ax[1:last]
    n[last] <- ax[last] <- 1/mx[last]
    Age <- Age[1:last]
  }
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- n*mx / (1+(n-ax)*mx)
  px <- 1 - qx
  # number of survivors using a radix of 100 000
  lx <- rep(NA,length(qx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*px[i]
  }
  lx <- lx[1:length(n)]
  # number of deaths (difference between the number of survivors at each age)
  # for open-aged interval it is the number of survivors at the open aged interval
  dx <- c(abs(diff(lx)),lx[length(n)])
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  1
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  # 
  rG <- Gini.fun(x = Age,nax = ax,ndx = dx/100000,ex = ex)
  return(rG)
}

# Entropy

h.dec <- function(mx){
  
  # Other quantities for lifetables
  Age=0:110
  ax=c(0.31,rep(0.5,110)) # The last year will probably be off
  
  # Calculating lifetables
  n <- c(diff(Age),ax[length(ax)]) # this is a vector of the length of the age categories
  # we should first check that we don't have any NaN in the mx vector
  # These are typically caused by 0 person-years in the denominator
  mx_na <- which(is.na(mx==T))
  # if so, we can close the lifetable at the last age before the NaN
  last <- mx_na[1]-1
  if(is.na(last)==F){
    mx <- mx[1:last]
    n <- n[1:last]
    ax <- ax[1:last]
    n[last] <- ax[last] <- 1/mx[last]
    Age <- Age[1:last]
  }
  # now running through all of the lifetable functions
  # probability of dying and surviving
  qx <- n*mx / (1+(n-ax)*mx)
  px <- 1 - qx
  # number of survivors using a radix of 100 000
  lx <- rep(NA,length(qx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*px[i]
  }
  lx <- lx[1:length(n)]
  # number of deaths (difference between the number of survivors at each age)
  # for open-aged interval it is the number of survivors at the open aged interval
  dx <- c(abs(diff(lx)),lx[length(n)])
  # person-years lived (contribution of the living plus the contribution of the dead)
  Lx <- rep(NA,length(n))
  for(i in 1:(length(n)-1)){
    Lx[i] <- lx[i+1]*n[i] + dx[i]*ax[i]
  }
  Lx[length(n)] <- lx[length(n)] * ax[length(n)]
  
  1
  
  # person-years lived above age x
  Tx = rev(cumsum(rev(Lx)))
  # remaining life expectancy
  ex <- Tx/lx
  
  # 
  edag <- sum((ax*c(ex[-1L], 0) + (1-ax)*ex)*dx/lx[1])
  h <- edag/ex[1]
  
  return(h)
}
