#Author: Mallory Barnes
#Date: 3/6/2016 
#Input: Current Master File
#Output: Four Key Figures

#Necessary Pacakges: 
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
#Define Multiplot function :
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}
#Load File
#Figure 1------------------------------------------------------------
Plot_data <- read.csv("all_data_3_6_2017.csv")
str(Plot_data)
#Cleanup data file:
#Format Dates properly: 
Plot_data$Date <- as.Date(Plot_data$Date.x, "%m/%d/%Y")
#Delete extraneous columns: 
Plot_data <- subset(Plot_data, select=-c(X.2, X.3, X.1, X, Plant_ID.x, Date.x, Date.y))
#Add Phase Column Designation
Plot_data$phase<- ifelse(Plot_data$Date<"2016-06-08", 1,
               ifelse(Plot_data$Date>="2016-06-08" & Plot_data$Date<="2016-06-16", 2,
                      ifelse(Plot_data$Date>="2016-06-16", 3,
                                    NA)))
str(Plot_data)
#Figure 1: Multipanel stacked figure; climate/physiology
#Climate data:
#1) Dual-axis with VPD and precip
#2) VPD Timeseires
Climate_data <- read.csv("C:/Users/Mallory/Dropbox/Summer_2016_Drought_Experiment/Cleaned_Table.csv")
head(Climate_data)
str(Climate_data)
#Custom function for pulling x number of characters from the right of a string: from 
#http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
	
substrRight <- function(x, n){
        	        substr(x, nchar(x)-n+1, nchar(x))
        	}
	
Climate_data$TOD <- substrRight(as.character(Climate_data$TIMESTAMP), 5)
Climate_data$Date <- as.Date(substr(as.character(Climate_data$TIMESTAMP),1, 9), "%m/%d/%Y")
#Need sum of precip, and average daytime temp (06:00 to 18:00 hours)
#Sum precip by date; 
str(precip)
precip <- ddply(Climate_data, .(Date), summarise, Precip = sum(Rain_mm_Tot))
#To get daytime temp: 
daytime_temp <-function(x){
        x$hr <-as.numeric(substr(x$TOD, 1,2))
        print(x$hr)
        daytime <- x[ which( x$hr > 06 & x$hr < 18) , ]
        print(daytime)
        return(ddply(daytime, .(Date), summarise, Temp=mean(AirTC_Avg)))
} 
temp <- daytime_temp(Climate_data)
#To get VPD (Vapor pressure deficit) in kPA
#Doing midday VPD to assess atmospheric demand
get_VPD <- function(x){
        x$hr <-as.numeric(substr(x$TOD, 1,2))
        temp <- x$AirTC_Avg
        RH <- x$RH
        SVP <- 610.7*10^(7.5*temp/(237.3+temp))
        x$VPD <- (((100 - RH)/100)*SVP)/1000
        print(x$VPD)
        midday <- x[ which( x$hr > 10 & x$hr < 14) , ]
        return(ddply(midday, .(Date), summarise, VPD=mean(VPD)))
}

VPD <- get_VPD(Climate_data)
plot_climate <- merge(merge(temp, VPD, by="Date"), precip)

#Physiological data
#Water Potential Data 
str(Plot_data)
w <- ggplot(Plot_data, aes(factor(Date), Water_Pot))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
w1 <- w + geom_boxplot(outlier.colour = NA)
w2 <- w1 + geom_point(position = position_jitter(width = 0.2))

#Vcmax
v <- ggplot(Plot_data, aes(factor(Date), Vcmax))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
v1 <- v + geom_boxplot(outlier.colour = NA)
v2 <- v1 + geom_point(position = position_jitter(width = 0.2)) + theme(axis.title.x=element_blank(),
                                                                       axis.text.x=element_blank(),
                                                                       axis.ticks.x=element_blank())
#Jmax
j <- ggplot(Plot_data, aes(factor(Date), Jmax))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
j1 <- j + geom_boxplot(outlier.colour = NA)
j2 <- j1 + geom_point(position = position_jitter(width = 0.2))+theme(axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank())
grid.newpage()
grid.draw(rbind(ggplotGrob(j2), ggplotGrob(v2), ggplotGrob(w2), size = "last"))

#Alternative: Timeseries for all: 
wl <- ggplot(Plot_data, aes(Date, Water_Pot))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
wl1 <- wl + geom_smooth()
wl2 <- wl1 + geom_point()

#Vcmax
vl <- ggplot(Plot_data, aes(Date, Vcmax))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
vl1 <- vl+ geom_smooth()
vl2 <- vl1 + geom_point() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                                                       axis.ticks.x=element_blank())
#Jmax
jl <- ggplot(Plot_data, aes(Date, Jmax))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
jl1 <- jl+ geom_smooth()
jl2 <- jl1 + geom_point() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank())
grid.newpage()
grid.draw(rbind(ggplotGrob(jl2), ggplotGrob(vl2), ggplotGrob(wl2), size = "last"))


#Now for the climate data: Want a free axis graph with precip and temp and then another with VPD (bars)
str(plot_climate)

p0 <- ggplot(plot_climate, aes(Date, Temp)) + geom_line(colour="darkred") + theme_minimal() + 
        theme(axis.title.x = element_blank(),axis.text.x = element_blank())


p1 <- ggplot(plot_climate, aes(Date, Temp)) + geom_line(colour="darkred") + theme_minimal() + 
theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90))
#par(new=TRUE)
p2 <- ggplot(plot_climate,aes(Date, Precip)) + geom_bar(stat="Identity", fill="blue") + theme_minimal() +
theme(axis.title.x = element_blank(), axis.text.x = element_blank())

vpd <- ggplot(plot_climate,aes(Date, VPD)) + geom_line(colour="red") + theme_minimal() +
       theme(axis.title.x = element_blank(), axis.text.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(vpd), ggplotGrob(p2), ggplotGrob(p1), size = "last"))
grid.draw(rbind(ggplotGrob(vpd), ggplotGrob(p2), ggplotGrob(p0), ggplotGrob(j2), ggplotGrob(v2), ggplotGrob(w2), size = "last"))
grid.draw(rbind(ggplotGrob(vpd), ggplotGrob(p2), ggplotGrob(p0), ggplotGrob(jl2), ggplotGrob(vl2), ggplotGrob(wl2), size = "last"))

#Figure 2---------------------------------------------------------------------------------------