#----------------------------------------------------
# Topic: Decomposition of lifetable entropy Sweden 1773, EDSD thesis
# Author: Serena Vigezzi (code from Allison)
# Date: 14/04/2020
#-----------------------------------------------------

# Packages and scripts
#----------------------

library(DemoDecomp)
library(tidyverse)

source("C:/Users/Toi/Documents/Scuola/EDSD/EPC/Mortality crises/Scripts/Decomposition functions.R")

source("C:/Users/Toi/Documents/Scuola/EDSD/EPC/Mortality crises/Scripts/Sweden, 1773/Handling data, Sweden 1773 - T.R")
source("C:/Users/Toi/Documents/Scuola/EDSD/EPC/Mortality crises/Scripts/Sweden, 1773/Handling data, Sweden 1773 - F.R")
source("C:/Users/Toi/Documents/Scuola/EDSD/EPC/Mortality crises/Scripts/Sweden, 1773/Handling data, Sweden 1773 - M.R")

# Calculations
#--------------

# 1771 to 1772 

# Total
h_dect <- horiuchi(h.dec,Swe6778t$mx[Swe6778t$Year==1771],
                    Swe6778t$mx[Swe6778t$Year==1772],200)

Swe7172th <- matrix(h_dect,nrow=length(h_dect),
                     ncol=1,byrow=F)
Swe7172th <- mutate(as.data.frame(Swe7172th),age=0:110)
Swe7172th <- reshape2::melt(Swe7172th,id.vars="age")
sum(Swe7172th$value)
entropy.years[6] - entropy.years[5]
h.dec(Swe6778t$mx[Swe6778t$Year==1772]) - h.dec(Swe6778t$mx[Swe6778t$Year==1771])

# Females
h_decf <- horiuchi(h.dec,Swe6778f$mx[Swe6778f$Year==1771],
                    Swe6778f$mx[Swe6778f$Year==1772],200)

Swe7172fh <- matrix(h_decf,nrow=length(h_decf),
                     ncol=1,byrow=F)
Swe7172fh <- mutate(as.data.frame(Swe7172fh),age=0:110)
Swe7172fh <- reshape2::melt(Swe7172fh,id.vars="age")
sum(Swe7172fh$value)
entropyf.years[6] - entropyf.years[5]
h.dec(Swe6778f$mx[Swe6778f$Year==1772]) - h.dec(Swe6778f$mx[Swe6778f$Year==1771])

# Males
h_decm <- horiuchi(h.dec,Swe6778m$mx[Swe6778m$Year==1771],
                    Swe6778m$mx[Swe6778m$Year==1772],200)

Swe7172mh <- matrix(h_decm,nrow=length(h_decm),
                     ncol=1,byrow=F)
Swe7172mh <- mutate(as.data.frame(Swe7172mh),age=0:110)
Swe7172mh <- reshape2::melt(Swe7172mh,id.vars="age")
sum(Swe7172mh$value)
entropym.years[6] - entropym.years[5]
h.dec(Swe6778m$mx[Swe6778m$Year==1772]) - h.dec(Swe6778m$mx[Swe6778m$Year==1771])

# 1772 to 1773 

# Total
h_dect <- horiuchi(h.dec,Swe6778t$mx[Swe6778t$Year==1772],
                    Swe6778t$mx[Swe6778t$Year==1773],200)

Swe7273th <- matrix(h_dect,nrow=length(h_dect),
                     ncol=1,byrow=F)
Swe7273th <- mutate(as.data.frame(Swe7273th),age=0:110)
Swe7273th <- reshape2::melt(Swe7273th,id.vars="age")
sum(Swe7273th$value)
entropy.years[7] - entropy.years[6]
h.dec(Swe6778t$mx[Swe6778t$Year==1773]) - h.dec(Swe6778t$mx[Swe6778t$Year==1772])

# Females
h_decf <- horiuchi(h.dec,Swe6778f$mx[Swe6778f$Year==1772],
                    Swe6778f$mx[Swe6778f$Year==1773],200)

Swe7273fh <- matrix(h_decf,nrow=length(h_decf),
                     ncol=1,byrow=F)
Swe7273fh <- mutate(as.data.frame(Swe7273fh),age=0:110)
Swe7273fh <- reshape2::melt(Swe7273fh,id.vars="age")
sum(Swe7273fh$value)
entropyf.years[7] - entropyf.years[6]
h.dec(Swe6778f$mx[Swe6778f$Year==1773]) - h.dec(Swe6778f$mx[Swe6778f$Year==1772])

# Males
h_decm <- horiuchi(h.dec,Swe6778m$mx[Swe6778m$Year==1772],
                    Swe6778m$mx[Swe6778m$Year==1773],200)

Swe7273mh <- matrix(h_decm,nrow=length(h_decm),
                     ncol=1,byrow=F)
Swe7273mh <- mutate(as.data.frame(Swe7273mh),age=0:110)
Swe7273mh <- reshape2::melt(Swe7273mh,id.vars="age")
sum(Swe7273mh$value)
entropym.years[7] - entropym.years[6]
h.dec(Swe6778m$mx[Swe6778m$Year==1773]) - h.dec(Swe6778m$mx[Swe6778m$Year==1772])

# 1773 to 1774 

# Total
h_dect <- horiuchi(h.dec,Swe6778t$mx[Swe6778t$Year==1773],
                    Swe6778t$mx[Swe6778t$Year==1774],200)

Swe7374th <- matrix(h_dect,nrow=length(h_dect),
                     ncol=1,byrow=F)
Swe7374th <- mutate(as.data.frame(Swe7374th),age=0:110)
Swe7374th <- reshape2::melt(Swe7374th,id.vars="age")
sum(Swe7374th$value)
entropy.years[8] - entropy.years[7]
h.dec(Swe6778t$mx[Swe6778t$Year==1774]) - h.dec(Swe6778t$mx[Swe6778t$Year==1773])

# Females
h_decf <- horiuchi(h.dec,Swe6778f$mx[Swe6778f$Year==1773],
                    Swe6778f$mx[Swe6778f$Year==1774],200)

Swe7374fh <- matrix(h_decf,nrow=length(h_decf),
                     ncol=1,byrow=F)
Swe7374fh <- mutate(as.data.frame(Swe7374fh),age=0:110)
Swe7374fh <- reshape2::melt(Swe7374fh,id.vars="age")
sum(Swe7374fh$value)
entropyf.years[8] - entropyf.years[7]
h.dec(Swe6778f$mx[Swe6778f$Year==1774]) - h.dec(Swe6778f$mx[Swe6778f$Year==1773])

# Males
h_decm <- horiuchi(h.dec,Swe6778m$mx[Swe6778m$Year==1773],
                    Swe6778m$mx[Swe6778m$Year==1774],200)

Swe7374mh <- matrix(h_decm,nrow=length(h_decm),
                     ncol=1,byrow=F)
Swe7374mh <- mutate(as.data.frame(Swe7374mh),age=0:110)
Swe7374mh <- reshape2::melt(Swe7374mh,id.vars="age")
sum(Swe7374mh$value)
entropym.years[8] - entropym.years[7]
h.dec(Swe6778m$mx[Swe6778m$Year==1774]) - h.dec(Swe6778m$mx[Swe6778m$Year==1773])

# Merging datasets
merged_h <- rbind(Swe7172th,Swe7172fh,Swe7172mh,
                   Swe7273th,Swe7273fh,Swe7273mh,
                   Swe7374th,Swe7374fh,Swe7374mh)
merged_h$variable <- c(rep(1,333),rep(2,333),rep(3,333))
merged_h$variable2 <- factor(merged_h$variable,levels=c(1,2,3),labels=c("Before","During","After"))
merged_h$Sex <- rep(c(rep("Total",111),rep("Females",111),rep("Males",111)),3)
colnames(merged_h) <- c("Age","Period","Contribution","Period2","Sex")

# Plotting
png("C:/Users/Toi/Documents/Scuola/EDSD/EPC/Mortality crises/H dec, Sweden 1773.png",
    width=1300,height=800)

ggplot(data=merged_h, aes(x=as.factor(Age), y=Contribution))+
  facet_grid(rows=vars(Period2),cols=vars(Sex)) +
  ggtitle(bquote(~'Difference in lifetable entropy, Sweden 1773' ))+
  geom_bar(stat = "identity", position = "stack")+
  scale_x_discrete("Age", seq(0,110,5)) + 
  ylab("Contribution") +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()

