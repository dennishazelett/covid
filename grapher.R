library(readr)
library(ggplot2)
library(mgcv)
library(ggthemes)

dat = read_csv(url("https://covidtracking.com/api/v1/states/daily.csv"))
dat$date = as.Date(as.character(dat$date),format="%Y%m%d");
dat$state = as.factor(dat$state)
dat = dat[order(dat$date),]

system("rm *pdf")

pdf("hospitalizations.pdf")
for(i in levels(dat$state)){
    print(ggplot(dat[dat$state==i,]) + geom_point(aes(date,hospitalized))+labs(title=paste(as.character(i),"Hospitalized")))
}
dev.off()

hospitalizations_state <- ggplot(dat) + 
    geom_point(aes(date, hospitalized, color = state), cex = 0.5) +
    geom_line(aes(date, hospitalized, color = state)) +
    scale_y_log10() +
    scale_x_date(limits = as.Date(c("2020-03-18", "2020-04-24"))) +
    geom_text(
        data = subset(dat, date == "2020-04-24"), 
        aes(date, hospitalized, label = state),
        # nudge_x = as.Date("1d"),
        na.rm = TRUE,
        cex = 2) +
    theme_clean() +
    theme(legend.position = "none")

hospitalizations_state
head(dat)


ggsave(
    filename = "plots/hospitalizations_by_state.pdf", 
    plot = hospitalizations_state,
    height = 8,
    width = 4)


deaths_state <- ggplot(dat) + 
    geom_point(aes(date, death, color = state), cex = 0.5) +
    geom_line(aes(date, death, color = state)) +
    scale_y_log10() +
    scale_x_date(limits = as.Date(c("2020-03-18", "2020-04-24"))) +
    geom_text(
        data = subset(dat, date == "2020-04-24"), 
        aes(date, death, label = state),
        # nudge_x = as.Date("1d"),
        na.rm = TRUE,
        cex = 2) +
    theme_clean() #+
    #theme(legend.position = "none")

deaths_state

ggsave(
    filename = "plots/deaths_by_state.pdf", 
    plot = deaths_state,
    height = 8,
    width = 6)



pdf("testedvsconfirmed.pdf")
for(i in levels(dat$state)){
    print(ggplot(dat[dat$state==i,]) + geom_point(aes(date,totalTestResults,color="tested"))+geom_point(aes(date,positive,color="confirmed"))+labs(title=paste(as.character(i), "tests")))
}
dev.off()

diffconv = rev(diff(c(1,2,4,2,1)/10))


pdf("casesperday.pdf")

for(i in levels(dat$state)){
    tempdat = dat[dat$state==i,]
    tempdat = tempdat[!is.na(tempdat$death),]
    tempdat[order(tempdat$date),]
#    tempdat$perdaytest = c(0,0,0,convolve(tempdat$totalTestResults,diffconv,type="filter"))
    tempdat$perdaycase = c(0,0,0,convolve(tempdat$positive,diffconv,type="filter"))
    
    print(ggplot(tempdat,aes(date,perdaycase)) + #geom_point(aes(date,perdaytest,color="tested")) +
          geom_point()+geom_smooth()+labs(title=paste(as.character(i),"cases")))
}
dev.off()






pdf("deadperday.pdf")

for(i in levels(dat$state)){
    tempdat = dat[dat$state==i,]
    tempdat[order(tempdat$date),]

    deadsmooth = gam(death ~ s(date,k=12),data=data.frame(date=as.numeric(tempdat$date),death=tempdat$death))
    tempdat$deadest = predict(deadsmooth,newdata=data.frame(date=as.numeric(tempdat$date)))

    tempdat$perdaydead = c(0,diff(tempdat$deadest))
    
    print(ggplot(tempdat,aes(date,perdaydead)) + geom_point(aes(date,deathIncrease))+geom_line()+labs(title=paste(as.character(i),"Dead per day")))
}
dev.off()



