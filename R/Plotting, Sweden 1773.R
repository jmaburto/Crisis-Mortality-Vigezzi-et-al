#Survival curve

#1773
swe73 <- plot(Swe1773t$Age,Swe1773t$lx,type="l",
              col=rgb(red=0, green=0.5, blue=1, alpha=1),
              ylab="Survival",xlab="Age",
              main="Survival curves for the 1773 famine, Sweden",
              ylim=c(0,110000),xlim=c(0,130))
# Adding the mean of the previous years
lines(Swe73t.mean$Age,Swe73t.mean$lx,type="l",lty=2,
      col=rgb(red=1, green=1, blue=0, alpha=1),
      ylab="Survival",xlab="Age",
      main="Survival curve in 1770, Sweden",
      ylim=c(0,120000),xlim=c(0,100))
#Shading under the mean curve
xx.mean <- c(Swe1773t$Age,rev(Swe1773t$Age))
yy.mean <- c(rep(0,nrow(Swe73t.mean)),rev(Swe73t.mean$lx))
polygon(xx.mean,yy.mean,col=rgb(red=1, green=1, blue=0, alpha=0.5),
        border=rgb(red=1, green=1, blue=0, alpha=1))
e0.mean <- Swe73t.mean$ex[Swe73t.mean$Age==0]
abline(v=e0.mean,col=rgb(red=1, green=1, blue=0, alpha=1))
#Shading under the 1773 curve
xx <- c(Swe1773t$Age,rev(Swe1773t$Age))
yy <- c(rep(0,nrow(Swe1773t)),rev(Swe1773t$lx))
polygon(xx,yy,col=rgb(red=0, green=0.5, blue=1, alpha=0.5),
        border=rgb(red=0, green=0.5, blue=1, alpha=1))
e0 <- Swe1773t$ex[Swe1773t$Age==0]
abline(v=e0,col=rgb(red=0, green=0.5, blue=1, alpha=1))
#Legend
legend(x=55, y=100000,lty=1,cex=0.75,
       col=c(rgb(red=0, green=0.5, blue=1, alpha=1), NA, rgb(red=1, green=1, blue=0, alpha=1), NA),
       legend=c("1773", "S=17.7,e=21.4,G=0.64", "1767-1771", "S=35.6,e=26.5,G=0.96"))


#Unfinished plots
#Death count
#plot(Swe1773t$Age,Swe1773t$dx,type="l",
 #    ylab="Number of deaths",xlab="Age",
 #    main="Number of deaths by age in 1773, Sweden")

#Mortality rates
#plot(Swe1773t$Age,Swe1773t$mx,type="l",
#     ylab="Mortality rates",xlab="Age",
 #    main="Mortality rates by age in 1773, Sweden")


