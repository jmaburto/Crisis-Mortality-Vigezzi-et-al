library(data.table)
load("HMD_Data.RData")

HMDL <- data.table(HMDL)

#Gini function from PASH
Gini.fun <- function (x, nax, ndx, ex) {
  e = rep(1, length(x))
  D = outer(ndx, ndx)
  x_ = x+nax
  X_ = abs(e%*%t(x_) - x_%*%t(e))
  G = sum(D*X_)/(2*ex[1L])
  return(g=G)
}

#Allison's lifetable function
lifetable <- function(mx,Age,ax){
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
  # lifetable
  lifetable <- data.frame(Age=Age,n=round(n,0),mx=round(mx,5),qx=round(qx,5),
                          px=round(px,5),ax=round(ax,2),
                          dx=round(dx,0),lx=round(lx,0),Lx=round(Lx,0),
                          Tx=round(Tx,0),ex=round(ex,2))
  
  return(lifetable)
}

#Sweden
#-------
Swe <- subset(HMDL, subset=PopName=="SWE")
#1773
Swe1773t <- subset(Swe,subset=Year==1773 & Sex=="f")
# Average of 5 years prior
Swe6771t <- subset(Swe,subset=Year<1772 & Year>1766 & Sex=="f")

mean.mx <- Swe6771t[,list(mean.mx = mean(mx), mean.ax = mean(ax)),  by = list(Age)]

# 
# mean.mx <- 0
# for(i in 0:110){
#   mean.mx[i+1] <- mean(Swe6771t$mx[Swe6771t$Age==i])
# }
# 
# 
# mean.ax <- 0
# for(i in 0:110){
#   mean.ax[i+1] <- mean(Swe6771t$ax[Swe6771t$Age==i])
# }

Swe73t.mean <- data.table(mean.mx[,lifetable(mx=mean.mx,Age=Swe1773t$Age,ax=mean.ax)])

#Standard deviation in 1773
e0   <- Swe1773t$ex[Swe1773t$Age==0]
sd   <- Swe1773t[,list(sd = sqrt(sum(dx/lx[1]*(Age + ax - ex[1])^2)))]
edag <- Swe1773t[,list(edag = sum((ax*c(ex[-1L], 0) + (1-ax)*ex)*dx/lx[1]))]
Gini <- Swe1773t[,list(Gini = Gini.fun(x = Age,nax = ax,ndx = dx/100000,ex = ex))]
entropy <- edag$edag/e0

# age.d <- Swe1773t$Age+Swe1773t$ax # Average age at death for each age category
# distance <- 0
# for (i in 1:111){
# distance[i] <- ((age.d[i]-e0)^2)*Swe1773t$dx[i] # here there was missing an indicator in age.d
# }
# warnings()
# sd <- sqrt(sum(distance)/(Swe1773t$lx[Swe1773t$Age==0]))
# round(sd,1)

#Standard deviation for prior years
e0.mean <- Swe73t.mean$ex[Swe73t.mean$Age==0]
sd.mean <- Swe73t.mean[,list(sd = sqrt(sum(dx/lx[1]*(Age + ax - ex[1])^2)))]
edag.mean    <- Swe73t.mean[,list(edag = sum((ax*c(ex[-1L], 0) + (1-ax)*ex)*dx/lx[1]))]
Gini.mean    <- Swe73t.mean[,list(Gini = Gini.fun(x = Age,nax = ax,ndx = dx/100000,ex = ex))]
entropy.mean <- edag.mean$edag/e0.mean

# 
# e0.mean <- Swe73t.mean$ex[Swe73t.mean$Age==0]
# age.d.mean <- Swe73t.mean$Age+Swe73t.mean$ax # Average age at death for each age category
# distance.mean <- 0
# for (i in 1:(dim(Swe73t.mean)[1])){
#   distance.mean[i] <- ((age.d.mean[i]-e0.mean)^2)*Swe73t.mean$dx[i] # here there was missing an indicator in age.d
# }
# sd.mean <- sqrt(sum(distance.mean)/(Swe73t.mean$lx[Swe73t.mean$Age==0]))
# round(sd.mean,1)
# 
# #Life disparity in 1773
# #Allison's code
# Swe1773t[,"n"] <- rep(1,dim(Swe1773t)[1])
# step1 <- rep(NA,dim(Swe1773t)[1])
# for (i in 1:(dim(Swe1773t)[1]-1)){
#   step1[i] <- Swe1773t$dx[i]*(Swe1773t$ex[i]+(Swe1773t$ax[i]/Swe1773t$n[i]*(Swe1773t$ex[i+1]-Swe1773t$ex[i])))
# }
# step1[dim(Swe1773t)[1]] <- Swe1773t$dx[dim(Swe1773t)[1]]*Swe1773t$ex[dim(Swe1773t)[1]]
# edag <- sum(step1)/Swe1773t$lx[1]
# round(edag,1)
# 
# #Life disparity for prior years
# step1 <- rep(NA,dim(Swe73t.mean)[1])
# for (i in 1:(dim(Swe73t.mean)[1]-1)){
#   step1[i] <- Swe73t.mean$dx[i]*(Swe73t.mean$ex[i]+(Swe73t.mean$ax[i]/Swe73t.mean$n[i]*(Swe73t.mean$ex[i+1]-Swe73t.mean$ex[i])))
# }
# step1[dim(Swe73t.mean)[1]] <- Swe73t.mean$dx[dim(Swe73t.mean)[1]]*Swe73t.mean$ex[dim(Swe73t.mean)[1]]
# edag.mean <- sum(step1)/Swe73t.mean$lx[1]
# round(edag.mean,1)
# 
# #Gini coefficient for 1773
# G <- 0
# for (i in 1:(dim(Swe1773t)[1]-1)){
#   G[i] <- (Swe1773t$lx[i+1]/Swe1773t$lx[Swe1773t$Age==0])^2+Swe1773t$ax[i]*((Swe1773t$lx[i]/Swe1773t$lx[Swe1773t$Age==0])^2-(Swe1773t$lx[i+1]/Swe1773t$lx[Swe1773t$Age==0])^2)
# }
# G <- 1-(1/e0)*sum(G)
# round(G,2)
# 
# #Gini coefficient for prior years
# G.mean <- 0
# for (i in 1:(dim(Swe73t.mean)[1]-1)){
#   G.mean[i] <- (Swe73t.mean$lx[i+1]/Swe73t.mean$lx[Swe73t.mean$Age==0])^2+Swe73t.mean$ax[i]*((Swe73t.mean$lx[i]/Swe73t.mean$lx[Swe73t.mean$Age==0])^2-(Swe73t.mean$lx[i+1]/Swe73t.mean$lx[Swe73t.mean$Age==0])^2)
# }
# G.mean <- 1-(1/e0)*sum(G)
# round(G.mean,2)
# 
# 
# 
