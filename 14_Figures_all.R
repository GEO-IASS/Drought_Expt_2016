#Author: Mallory Barnes
#Date: 3/6/2016 
#Input: Current Master File
#Output: Four Key Figures

#Necessary Pacakges: 
library(lubridate)
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(grid)
library(tidyr)
library(reshape2)
library(gtable)
library(signal)
library(plsropt)
library(pls)
library(gridExtra)
library(outliers)
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
Plot_data <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data_3_6_2017.csv")
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
#Get rid of obs 38 (5-20_2016)
#Remove 5-20 Observation as it's the only one that day: 
Plot_data <- Plot_data[-c(38),]
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
str(Climate_data)
#Get values for results section------------------------------------
#Get average maximum and minimum daily temperature
Climate_data_study <- subset(Climate_data, Date >= as.Date("2016-05-24", "%Y-%m-%d") & Date <= as.Date("2016-07-07", "%Y-%m-%d"))
str(Climate_data_study)
tempbydate <- ddply(Climate_data_study, .(Date), summarise, maxtemp=max(AirTC_Avg), mintemp=min(AirTC_Avg), precip=sum(Rain_mm_Tot))
mean(tempbydate$maxtemp)
mean(tempbydate$mintemp)
sum(tempbydate$precip)

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
#Doing midday VPD to assess atmospheric demand: from 10:00am 2:00pm (10:00 to 14:00 hrs)
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

#Get VPD/temp/Vcmax/Jmax/Water_Potential values for results section---------------
str(plot_climate)
summary(plot_climate)
sd(plot_climate$Temp)
sd(plot_climate$VPD)

plot_climate
sum(plot_climate$Precip)

summary(Plot_data)
sd(Plot_data$Vcmax)
sd(Plot_data$Jmax)
sd(Plot_data$Water_Pot)

#Alternative: Timeseries for all: 
#wl <- ggplot(Plot_data, aes(Date, Water_Pot))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#wl1 <- wl + geom_smooth()
#wl2 <- wl1 + geom_point()

#Vcmax
#vl <- ggplot(Plot_data, aes(Date, Vcmax))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#vl1 <- vl+ geom_smooth()
#vl2 <- vl1 + geom_point() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                                                       #axis.ticks.x=element_blank())
#Jmax
#jl <- ggplot(Plot_data, aes(Date, Jmax))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#jl1 <- jl+ geom_smooth()
#jl2 <- jl1 + geom_point() + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                 # axis.ticks.x=element_blank())
#grid.newpage()
#grid.draw(rbind(ggplotGrob(jl2), ggplotGrob(vl2), ggplotGrob(wl2), size = "last"))

#Now for the climate data: Want a free axis graph with precip and temp and then another with VPD (bars)
str(plot_climate)

#p0 <- ggplot(plot_climate, aes(Date, Temp)) + geom_line(colour="darkred") + theme_minimal() + 
        #theme(axis.title.x = element_blank(),axis.text.x = element_blank())
#vpd <- ggplot(plot_climate,aes(Date, VPD)) + geom_line(colour="red") +
        #scale_y_continuous(position="right")+theme_minimal() +
        #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) %+replace% 
        #theme(panel.background = element_rect(fill = NA))+ 
        #theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90))+
        #theme(panel.border = element_blank(), axis.ticks.x = element_line(size=1,color='black'),
              #axis.ticks.y = element_line(size=1, color='black'), axis.ticks.length=unit(0.25, "cm"), 
              #axis.line = element_blank(), panel.background = element_blank(),
              #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              #axis.text.x = element_text(margin=unit(c(0.5,-0.5,0.5,0.5), "cm")))


p1 <- ggplot(plot_climate, aes(Date, Temp)) + geom_line(colour="darkred") + theme_minimal() + 
theme(axis.title.x = element_blank(),axis.text.x = element_text(angle=90))+
        theme(panel.border = element_blank(), axis.ticks.x = element_line(size=1,color='black'),
              axis.ticks.y = element_line(size=1, color='black'), axis.ticks.length=unit(0.25, "cm"), 
              axis.line = element_blank(), panel.background = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.x = element_text(margin=unit(c(0.5,-0.5,0.5,0.5), "cm")))+
        guides(colour=FALSE)

p2 <- ggplot(plot_climate,aes(Date, Precip)) + geom_bar(stat="Identity", fill="blue") + theme_minimal() +
        geom_rect(aes(xmin=as.Date("2016-06-02", "%Y-%m-%d"),xmax=as.Date("2016-06-07", "%Y-%m-%d") ,ymin=-Inf,ymax=Inf),
                  fill="darkslategray1")+
        geom_rect(aes(xmin=as.Date("2016-06-19", "%Y-%m-%d"),xmax=as.Date("2016-06-20", "%Y-%m-%d") ,ymin=-Inf,ymax=Inf),
                  fill="darkslategray1")+
      ylab("Precip (mm)")+
        theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
              axis.title.y=element_text(vjust=6), axis.text.y=element_text(hjust=-1))+ 
        theme(panel.border = element_blank(), panel.background = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              plot.margin=unit(c(-0.25,2,1,1), "cm"))

#Got both on same axis! YES!
p <- p1 +geom_line(aes(y = VPD*5, colour = 'red'))+ ylab("Temp (C)")

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~./5, name = "VPD [KPa]")) + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
        plot.margin=unit(c(1,1,0,1), "cm"))
grid.arrange(p, p2)
#Physiological data
#Water Potential Data 
str(Plot_data)
w <- ggplot(Plot_data, aes(factor(Date), Water_Pot))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Date")
w1 <- w + geom_boxplot(outlier.colour = NA, width=0.5)
w2 <- w1 + geom_point(position = position_jitter(width = 0.2))+theme(axis.title.x=element_blank(),
                                                                     axis.text.x=element_blank(),
                                                                     axis.ticks.x=element_blank(),
                                                                     plot.margin=unit(c(0,1,0,1), "cm"))+
        theme(panel.border = element_blank(), panel.background = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#Vcmax
v <- ggplot(Plot_data, aes(factor(Date), Vcmax))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
v1 <- v + geom_boxplot(outlier.colour = NA, width=0.4)
v2 <- v1 + geom_point(position = position_jitter(width = 0.2)) + theme(axis.title.x=element_blank(),
                                                                       axis.text.x=element_blank(),
                                                                       axis.ticks.x=element_blank(),
                                                                       plot.margin=unit(c(0,1,0.5,1), "cm"))+
        theme(panel.border = element_blank(), panel.background = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#Jmax
str(Plot_data)
str(Plot_data)
as.Date(Plot_data$Date, format="%Y/%m/%d")
j <- ggplot(Plot_data, aes(factor(Date), y=Jmax))+ 
                            #scale_x_date(labels = date_format("%Y-%m-%d"))+
                            theme(axis.text.x = element_text(angle = 90, hjust = 1))
j1 <- j + geom_boxplot(outlier.colour = NA, width=0.4),aes(xmin = as.Date("2004-04-29", "%Y-%m-%d"), 
                                                              xmax = as.Date("2004-12-20",  "%Y-%m-%d")))

j2 <- j1 + geom_point(position = position_jitter(width = 0.2))+ theme(plot.margin=unit(c(0,1,0,1), "cm"))+
        xlab("Date")+
        theme(panel.border = element_blank(), panel.background = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
grid.newpage()
grid.draw(rbind(ggplotGrob(w2), ggplotGrob(v2), ggplotGrob(j2), size = "last"))

grid.draw(rbind(ggplotGrob(p2), ggplotGrob(p),ggplotGrob(w2), ggplotGrob(v2), ggplotGrob(j2), size = "last"))

#draw it!
grid.arrange(p, p2, w2, v2, j2, ncol=1, heights=c(2,2,3,5.5,8))
grid.arrange(w2, v2, j2, ncol=1, heights=c(6,8,10))

#Quantifying change in Vcmax early vs. late
#early = 5-24 through 6-15
#late = 6-15 through 7-06
str(Plot_data)
early <- subset(Plot_data, Date < as.Date("2016-06-08"))
late <- subset(Plot_data, Date > as.Date("2016-06-24"))
str(early)
str(late)
mean(early$Vcmax)
mean(late$Vcmax)

mean(early$Jmax)
mean(late$Jmax)

#Figure 2---------------------------------------------------------------------------------------
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
textfiles = list.files("ASCII_Reflectance/", pattern = "*.txt")
#txtfiles_subset is to test out the lapply
textfiles_subset = textfiles[1:5]
textfiles_subset=textfiles[448:457]
textfiles_subset
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/")
read.table("b2popmlb_E04_leaf_6-30-201600003.asd.txt")
#User-defined function "format_spectra"
#Formats hyperspectral ASD files and parses various information based on filename
#This takes awhile to run
format_spectra <- function(x){
        tmp = read.table(x,  col.names=c("wavelength", "reflectance"))
        tmp$filename <- basename(x)
        tmp = tmp[-1,]
        tmp$wavelength <- as.numeric(levels(tmp$wavelength))[tmp$wavelength]
        tmp$reflectance <-as.numeric(levels(tmp$reflectance))[tmp$reflectance]
        filename <- substr(tmp[1,3], 1,40)
        print(filename)
        ID <-  substr(tmp[1,3], 10,12)
        date <- (substr(tmp[1,3], 19,27))
        observation =(substr(tmp[1,3], 31,32))
        reflectances <- reshape(tmp, idvar="filename", timevar="wavelength", direction="wide")
        indices=as.data.frame(cbind(ID, date, observation, reflectances))
        return(indices)
}
spectra_tmp <- lapply(textfiles, format_spectra)
spectra <- do.call(rbind, spectra_tmp)
#write.csv(spectra, "spectra_for_fig_2_3_7_2017.csv")
spectra <- read.csv("C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/spectra_for_fig_2_3_7_2017.csv")
spectra$date <-as.Date(spectra$date, format="%m-%d-%Y")
#3) create "unique_ID" column
spectra$ID <- as.character(tolower(spectra$ID))
spectra$uniqueID <- paste(spectra$ID, spectra$date, sep='-') 
spectra$uniqueID
#5) Get rid of "reflectance" in column because we know that's what it is
names(spectra) <- gsub("reflectance.", "", names(spectra))
str(spectra)
str(spectra)
spectra[] <- lapply(spectra, function(x) {
        if(is.factor(x)) as.numeric(as.character(x)) else x
})

#Check to make sure we only have numeric columns
sapply(spectra, class)
str(spectra)
spectra <- spectra[ -c(1:4)]
#Get rid of reflectances less than zero (no meaning)
#645 negative reflectance values in total
spectra[spectra < 0 ] <- NA
str(spectra)
#Need to check for outliers here - some observations might be weird which is why we took 9 of them.
#Test using grubbs.test and this stackoverflow solution; http://stackoverflow.com/questions/22837099/how-to-repeat-the-grubbs-test-and-flag-the-outliers
#Function to flag outliers accoriding to grubbs test
#grubbs.flag <- function(x) {
#        outliers <- NULL
#        test <- x
#        grubbs.result <- grubbs.test(test)
#        pv <- grubbs.result$p.value
#        while(pv < 0.05) {
#                outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
#                test <- x[!x %in% outliers]
#                grubbs.result <- grubbs.test(test)
#                pv <- grubbs.result$p.value
#        }
#        return(data.frame(X=x,Outlier=(x %in% outliers)))
#        df$X <-NA if Outliers="TRUE"
#}
# Then remove observations that are considered outliers
# Then move on to averaging all reflectances by unique ID
#now average all reflectances by 'uniqueID'

#Function to run grubbs flag for each, replace "TRUE" outlier values with NA, then cbind all the results together

#grubbies <- grubbs.flag(spectra$`350`)
#mydata <- if(grubbies$Outlier=="TRUE") grubbies[grubbies$grubbies %in% X==NA,] else mydata
#mydata <- if(MyCondition=="high") mydata[mydata$mydata %in% highRandomNumbers==TRUE,] else mydata

#grubbies
#str(grubbies)        
#grubbies$X

#ggplot(grubbies,aes(x=grubbies$X,color=grubbies$Outlier,fill=grubbies$Outlier))+
#geom_histogram(binwidth=diff(range(X))/30)+
#theme_bw()

#outliers <- ddply(spectra, .(uniqueID), grubbs.flag(spectra))
#This ddply takes 30 secs or so 
hyperspectral <- ddply(spectra, .(uniqueID), colwise(mean, na.rm=TRUE))
hyperspectral1 <-ddply(spectra, .(uniqueID), numcolwise(median, na.rm=TRUE))
#Get range of reflectance
str(hyperspectral)
rel_range <- function(x){
min <- min(x, na.rm=TRUE)
max <- max(x, na.rm=TRUE)
rel<- (max-min)/((min+max)/2) * 100
return(rel)}
ksd<-apply(hyperspectral[-(1:2)],2,rel_range)
str(ksd)


vis_ref <- mean(ksd[101:351])
NIR_ref <- mean(ksd[351:951])
SWIR_ref<- mean(ksd[1151:2151])

ksd_subset <- ksd[101:2151]
which.min(ksd_subset)
which.max(ksd_subset)
min(ksd_subset)
max(ksd_subset)






ksd[101]
ksd[351]
ksd[951]
ksd[1151]
ksd[2151]


str(hyperspectral)
hyperspectral_long <- gather(hyperspectral, uniqueID, reflectance, c(`350`:`2500`), factor_key=TRUE)
hyperspectral1_long <- gather(hyperspectral1, uniqueID, reflectance, c(`350`:`2500`), factor_key=TRUE)

str(hyperspectral_long)
str(hyperspectral1_long)
#Rename second column "wavelength"
names(hyperspectral_long)[3] <- "wavelength"
names(hyperspectral1_long)[2] <- "wavelength"
hyperspectral_long$wavelength <- as.numeric(as.character(hyperspectral_long$wavelength))
hyperspectral1_long$wavelength <- as.numeric(as.character(hyperspectral1_long$wavelength))

str(hyperspectral_long)
str(hyperspectral1_long)
#need to compare "uniqueID" here with "uniqueID" in plot_data to ensure we're seeing looking at the proper observations
anti_join(hyperspectral_long, Plot_data)
anti_join(hyperspectral1_long, Plot_data)

#to delete: eo4-2016-05-20
hyperspectral_long<-hyperspectral_long[!(hyperspectral_long$uniqueID=="e04-2016-05-20" | hyperspectral_long$uniqueID=="a01-2016-05-20"| hyperspectral_long$uniqueID=="h06-2016-05-20" | hyperspectral_long$uniqueID=="b12-2016-05-20" | hyperspectral_long$uniqueID=="b12-2016-06-01"| hyperspectral_long$uniqueID=="c14-2016-05-26"| hyperspectral_long$uniqueID=="c14-2016-05-20" | hyperspectral_long$uniqueID=="f08-2016-06-27"| hyperspectral_long$uniqueID=="f05-2016-05-31"| hyperspectral_long$uniqueID=="g11-2016-05-24"| hyperspectral_long$uniqueID=="h06-2016-05-26"| hyperspectral_long$uniqueID=="h06-2016-06-01"| hyperspectral_long$uniqueID=="h09-2016-05-26"|  hyperspectral_long$uniqueID=="h09-2016-06-01" |hyperspectral_long$uniqueID=="g11-2016-05-31"| hyperspectral_long$uniqueID=="i06-2016-05-20"| hyperspectral_long$uniqueID=="c14-2016-06-01"),]
str(hyperspectral_long)
aggregate(data.frame(count = hyperspectral_long$uniqueID), list(value = hyperspectral_long$uniqueID), length)
hyperspectral_long$uniqueID
str(hyperspectral_long)

Refs <- ggplot(hyperspectral_long, aes(y=reflectance, x=wavelength, group=uniqueID))+ geom_line(alpha=0.2)+
      scale_x_continuous(breaks=seq(500, 2400, 200), limits=c(500,2400))+
       xlim(450,2500)+
        geom_line(data=not_stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="blue")  +
        geom_line(data=stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="red")+
        theme(legend.position="none")+ ggtitle("Mean Reflectance")
#And for median-based
hyperspectral1_long<-hyperspectral1_long[!(hyperspectral1_long$uniqueID=="e04-2016-05-20" | hyperspectral1_long$uniqueID=="a01-2016-05-20"| hyperspectral1_long$uniqueID=="h06-2016-05-20" | hyperspectral1_long$uniqueID=="b12-2016-05-20" | hyperspectral1_long$uniqueID=="b12-2016-06-01"| hyperspectral1_long$uniqueID=="c14-2016-05-26"| hyperspectral1_long$uniqueID=="c14-2016-05-20" | hyperspectral1_long$uniqueID=="f08-2016-06-27"| hyperspectral1_long$uniqueID=="f05-2016-05-31"| hyperspectral1_long$uniqueID=="g11-2016-05-24"| hyperspectral1_long$uniqueID=="h06-2016-05-26"| hyperspectral1_long$uniqueID=="h06-2016-06-01"| hyperspectral1_long$uniqueID=="h09-2016-05-26"|  hyperspectral1_long$uniqueID=="h09-2016-06-01" |hyperspectral1_long$uniqueID=="g11-2016-05-31"| hyperspectral1_long$uniqueID=="i06-2016-05-20"| hyperspectral1_long$uniqueID=="c14-2016-06-01"),]
str(hyperspectral1_long)
aggregate(data.frame(count = hyperspectral1_long$uniqueID), list(value = hyperspectral1_long$uniqueID), length)
hyperspectral1_long$uniqueID
ggplot(hyperspectral1_long, aes(y=reflectance, x=wavelength, group=uniqueID))+ geom_line(alpha=0.2)+
        scale_x_continuous(breaks=seq(350, 2500, 200))+
        geom_line(data=not_stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="blue")  +
        geom_line(data=stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="red")+
        theme(legend.position="none")+ ggtitle("Median-based reflectance")



#Add red and blue lines: 
#User Defined Function: Format Hyperspec: 
not_stressed <- read.table("C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/b2popmlb_E03_Leaf_6-01-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(not_stressed)
stressed <- read.table("C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/b2popmlb_E03_leaf_6-24-201600001.asd.txt", col.names=c("wavelength", "reflectance"))

#Create Column with filename and then delete the first row of data frame
Format_hyperspec <- function(test){
        test$filename <- (test$reflectance[1:1])
        test = test[-1,]
        
        #Change factors to numeric
        test$wavelength <- as.numeric(levels(test$wavelength))[test$wavelength]
        test$reflectance <-as.numeric(levels(test$reflectance))[test$reflectance]
        
        test
}

not_stressed <- Format_hyperspec(not_stressed)
stressed <- Format_hyperspec(stressed)
#Simple Plot of Test
str(not_stressed)
str(stressed)


p <- ggplot() +
        # blue plot
        geom_point(data=not_stressed, aes(x=wavelength, y=reflectance), colour="blue")  +
        #geom_smooth(data=not_stressed, aes(x=wavelength, y=reflectance), fill="blue",
        #colour="darkblue", size=1) +
        # red plot
        geom_point(data=stressed, aes(x=wavelength, y=reflectance, colour="red")) 

#geom_smooth(data=stressed, aes(x=wavelength, y=reflectance), fill="red",
#colour="red", size=1)


#Figure 3
PLSR_formatted <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR_formatted.csv")
#now average all reflectances by 'uniqueID' - this averages all 9 observaitons
str(PLSR_formatted)
hyperspectral_mean <- ddply(PLSR_formatted, .(uniqueID), colwise(mean))
hyperspectral_med <- ddply(PLSR_formatted, .(uniqueID), numcolwise(median))


ddply(PLSR_formatted,~uniqueID,summarise, mean=(means.without.obs(x)))

str(hyperspectral)
hyperspectral <- hyperspectral[ -c(2:3, 5:6)]
#colnames(hyperspectral)[1] <- "uniqueID"
#remove columns: x, observation, filename
#Load file with other data
data_aci_etc <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data_3_6_2017.csv")
str(data_aci_etc)
merged_hyperspec <- merge(data_aci_etc, hyperspectral, by="uniqueID")
str(merged_hyperspec)
#wanna keep date and genotype columns for potential easy subsetting I think
merged_hyperspec <- merged_hyperspec[ -c(2:6, 9:14, 16:19, 21:32)]
str(merged_hyperspec)
#write.csv(merged_hyperspec, "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/poplar_allwavelengths.csv")

#Figure 3---------------------------------------------
#Figure 3 -  new PLSR figure
indices_formatted <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR_formatted.csv")
#now average all reflectances by 'uniqueID'
str(indices_formatted)
hyperspectral <- ddply(indices_formatted, .(uniqueID), colwise(mean))
str(hyperspectral)
hyperspectral <- hyperspectral[ -c(2:7)]
#colnames(hyperspectral)[1] <- "uniqueID"
#remove columns: x, observation, filename
#Load file with other data
data_aci_etc <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Merged_data_to_analyze_3_6_2017.csv")
str(data_aci_etc)
merged_hyperspec <- merge(data_aci_etc, hyperspectral, by="uniqueID")
str(merged_hyperspec)
#wanna keep date and genotype columns for potential easy subsetting I think
head(merged_hyperspec)
#Columns to keep: uniqueID, Vcmax, Jmax, Water_Pot, Genotype, Date.x
#Their positions are: column 1,6,13,16,21 
merged_hyperspec <- merged_hyperspec[ -c(2:4, 7:10, 12:15, 17:20, 22:24)]
str(merged_hyperspec)
#write.csv(merged_hyperspec, "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/poplar_allwavelengths_3_11_2017.csv")
#Prepping data for PLSR
poplar_names <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_3_11_2017.csv")
str(poplar_names)
poplar_names<-poplar_names[!(poplar_names$uniqueID=="e04-2016-05-20"),]
#write.csv(poplar_names, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_3_17_2017.csv")
poplar_names <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_3_17_2017.csv")
poplar_names$Date.x <- as.Date(poplar_names$Date.x, format="%m/%d/%Y")
#Figure 3a&3b: Vcmax &Jmax- randomly order samples; train with 80% and test with 20% of data
#get rid of 'x' column, Water_Pot, and Genotype for now 
poplar_names <- poplar_names[ -c(1:2, 6:8)]
#change col names (all say 'x' right now(?))
names(poplar_names) <- gsub("X", "", names(poplar_names))
str(poplar_names)
#rownames of all data
poplar_all <- poplar_names[,-1]
rownames(poplar_all) <- poplar_names[,1]
#Rowname should be 'UniqueID'
rownames(poplar_all)
#check column names
colnames(poplar_all)[1:5]
#last five column names
colnames(poplar_all)[(ncol(poplar_all)-4):ncol(poplar_all)]
#extract x variables (350 nm - 2500 nm)
x <-extdat(poplar_all, start=350,end=2500)
poplar<-data.frame(poplar_all[,1:2], NIR=I(as.matrix(x)))
#extracting range of 700 nm to 1098 nm
poplar$NIR<-extdat(poplar$NIR,start=350,end=2500)
#preprocessing: standard normal variate
poplar$NIR <- snv(poplar$NIR)
#Savitky-Golay second derivative
poplar$NIR <- matsgolay(poplar$NIR, p=2, n=11, m=2)
#Auto-scaling
poplar$NIR <- scale(poplar$NIR, center = TRUE, scale = TRUE)
#Shuffle Rowwise: 
poplar_rand <- poplar[sample(nrow(poplar)),]
#Divide data set into training and test set
datTrain <- poplar_rand[-c(69:86),]
datTest <- poplar_rand[69:86,]
str(datTrain)
str(datTest)
#Now for the PLSR!
result3a <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = FALSE)

result3b <- plsrPlot(Jmax ~ NIR, data = datTrain, testdata = datTest,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = FALSE)

function

#result$validation
#result.all <- plsrauto(Vcmax ~ NIR, data = datTrain, testdata = datTest, xrange = list(c(350, 2500)))
#str(result.all)

#Figure 3c&d: Vcmax & Jmax - train on high (not stressed) water potential and test on stressed (low water potential)
#When ordering by water potential (descending)
poplar_namescd <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths_3_17_2017.csv")
poplar_namescd$Date.x <- as.Date(poplar_namescd$Date.x, format="%m/%d/%Y")
poplar_namescd <- poplar_namescd[order(poplar_namescd$Date.x),] 
str(poplar_namescd)
#Figure 3a&3b: Vcmax &Jmax- randomly order samples; train with 80% and test with 20% of data
#get rid of 'x' column, Water_Pot, and Genotype for now 
poplar_namescd <- poplar_namescd[ -c(1:2, 6:8)]
#change col names (all say 'x' right now(?))
names(poplar_namescd) <- gsub("X", "", names(poplar_namescd))
str(poplar_namescd)
#rownames of all data
poplar_allcd <- poplar_namescd[,-1]
rownames(poplar_allcd) <- poplar_namescd[,1]
rownames(poplar_allcd)
#check column names
colnames(poplar_allcd)[1:5]
#last five column names
colnames(poplar_allcd)[(ncol(poplar_allcd)-4):ncol(poplar_allcd)]
#extract x variables (350 nm - 2500 nm)
xcd <-extdat(poplar_allcd, start=350,end=2500)
poplarcd<-data.frame(poplar_allcd[,1:2], NIR=I(as.matrix(xcd)))
#extracting range of 700 nm to 1098 nm
poplarcd$NIR<-extdat(poplarcd$NIR,start=350,end=2500)
#preprocessing: standard normal variate
poplarcd$NIR <- snv(poplarcd$NIR)
#Savitky-Golay second derivative
poplarcd$NIR <- matsgolay(poplarcd$NIR, p=2, n=11, m=2)
#Auto-scaling
poplarcd$NIR <- scale(poplarcd$NIR, center = TRUE, scale = TRUE)
#Divide data set into training and test set
datTraincd <- poplarcd[-c(39:51),]
datTestcd <- poplarcd[39:51,]

result3c <- plsrPlot(Vcmax ~ NIR, data = datTraincd, testdata = datTestcd,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = FALSE)

result3d <- plsrPlot(Jmax ~ NIR, data = datTraincd, testdata = datTestcd,
                     ncomp = "auto", maxcomp = 10,
                     validation = "CV", segment.type ="interleaved",
                     output = FALSE)

#Shuffle Rowwise: 
#poplar_rand <- poplar[sample(nrow(poplar)),]

str(datTrain)
str(datTest)

#This is for only obs on June 23rd and 24th which are observations 40-52 (out of 87)
#This is for only obs on June 23rd and 24th for GT 52-276
datTrain <- poplar[-c(40:52),]
datTest <- poplar[40:52,]

result <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = FALSE)

#Figure 4----------------------
Corrs <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/Corr_to_plot_4_25_2017.csv")
str(Corrs)
Corrs$Sum_Corrs <- Corrs$Vcmax_R_squared + Corrs$Jmax_R_squared
str(Corrs)

c1 <- ggplot(Corrs, aes(y=Vcmax_R_squared, x=reorder(Index, -Sum_Corrs)))+
        ylim(0,0.8)+
        geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x=element_blank())+
         ylab("Vcmax R-squared")+
         annotate("text", x = 19:20, y = 0.1, label = c("ns", "ns"))+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        

c2 <- ggplot(Corrs, aes(y=Jmax_R_squared, x=reorder(Index, -Sum_Corrs)))+
        ylim(0,0.8)+
        geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Indices")+
        ylab("Jmax R-squared")+
        annotate("text", x = 18:20, y = 0.1, label = c("ns", "ns", "ns"))+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        

multiplot(c1,c2)

#Figure 3 4-18-2017
#values to plot are from fitted value 'test'
#get r-squared from stats - r-test (gotta square it)
RMSE = function(m, o){
        sqrt(mean((m - o)^2))
}
#Leave-one-out
#Vcmax
file1 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_LOO/2comps(n=86)_train/fittedvalue.csv")
tests1 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result//Vcmax_LOO/2comps(n=86)_train/stats.csv")
rsquared1 <- tests1$R.val^2
lb1 <- paste("R^2 == ", round(rsquared1,2))
RMSE1 <- paste("RMSE==", round(RMSE(file1$yhat.cal, file1$yhat.val),2))
str(file1)
plot1 <- ggplot(file1, aes(y=yhat.cal, x=yhat.val))+
        geom_point(shape=19, colour="tomato2", size=4)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Vcmax")+
        ylab("Predicted Vcmax")+
        theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
        annotate("text", label = lb1, parse=TRUE, x = 90, y = 60, size = 7, colour = "Black")+
        annotate("text", label = RMSE1, parse=TRUE, x = 90, y = 50, size = 7, colour = "Black")+
                theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("a) Vcmax - leave-one-out") + 
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))

plot1

#Jmax
file2 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_LOO/4comps(n=86)_train/fittedvalue.csv")
tests2 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_LOO/4comps(n=86)_train/stats.csv")
rsquared2 <- tests2$R.val^2
lb2 <- paste("R^2 == ", round(rsquared2,2))
RMSE2 <- paste("RMSE==", round(RMSE(file2$yhat.cal, file2$yhat.val),2))
str(file2)
plot2 <- ggplot(file2, aes(y=yhat.cal, x=yhat.val))+
        geom_point(shape=19, colour="tomato2", size=4)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Jmax")+
        ylab("Predicted Jmax")+
        theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
        annotate("text", label = lb2, parse=TRUE, x = 200, y = 80, size = 7, colour = "Black")+
        annotate("text", label = RMSE2, parse=TRUE, x = 200, y = 65, size = 7, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("b) Jmax - leave-one-out") + 
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))

plot2

#80-20
#Vcmax
file3 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/fittedvalue_test.csv")
tests3 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/stats.csv")
rsquared3 <- tests3$R.test^2
lb3 <- paste("R^2 == ", round(rsquared3,2))
RMSE3 <- paste("RMSE==", round(RMSE(file3$y.2.comps, file3$y.test),2))
plot3 <- ggplot(file3, aes(y=y.test, x=y.2.comps))+
        geom_point(shape=19, colour="tomato2", size=5)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Vcmax")+
        ylab("Predicted Vcmax")+
        theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
        annotate("text", label = lb3, parse=TRUE, x = 90, y = 60, size = 7, colour = "Black")+
        annotate("text", label = RMSE3, parse=TRUE, x = 90, y = 50, size = 7, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("c) Vcmax - representative 80/20") +
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))

plot3
#Jmax
file4 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_80_20_fig/4comps(n=68)_test/fittedvalue_test.csv")
tests4 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_80_20_fig/4comps(n=68)_test/stats.csv")
rsquared4 <- tests4$R.test^2
lb4 <- paste("R^2 == ", round(rsquared4,2))
RMSE4 <- paste("RMSE==", round(RMSE(file4$y.4.comps, file4$y.test),2))

plot4 <- ggplot(file4, aes(y=y.test, x=y.4.comps))+
        geom_point(shape=19, colour="tomato2", size=5)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Jmax")+
        ylab("Predicted Jmax")+
        theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
        annotate("text", label = lb4, parse=TRUE, x = 170, y = 80, size = 7, colour = "Black")+
        annotate("text", label = RMSE4, parse=TRUE, x = 170, y = 65, size = 7, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("d) Jmax - representative 80/20") + 
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))

plot4

#Hardest
#Vcmax
file5 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/fittedvalue_test.csv")
tests5 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/stats.csv")
rsquared5 <- tests5$R.test^2
lb5 <- paste("R^2 == ", round(rsquared5,2))
RMSE5 <- paste("RMSE==", round(RMSE(file5$y.2.comps, file5$y.test),2))

plot5 <- ggplot(file5, aes(y=y.test, x=y.2.comps))+
        geom_point(shape=19, colour="tomato2", size=5)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Vcmax")+
        ylab("Predicted Vcmax")+
        theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
        annotate("text", label = lb5, parse=TRUE, x = 75, y = 55, size = 7, colour = "Black")+
        annotate("text", label = RMSE5, parse=TRUE, x = 75, y = 50, size = 7, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("d) Vcmax - tested on variable water potential") +
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))

plot5

#Jmax
file6 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_Period/5comps(n=73)_test/fittedvalue_test.csv")
tests6 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_Period/5comps(n=73)_test/stats.csv")
rsquared6 <- tests6$R.test^2
lb6 <- paste("R^2 == ", round(rsquared6,2))
RMSE6 <- paste("RMSE==", round(RMSE(file6$y.5.comps, file6$y.test),2))


plot6 <- ggplot(file6, aes(y=y.test, x=y.5.comps))+
        geom_point(shape=19, colour="tomato2", size=6)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Jmax")+
        ylab("Predicted Jmax")+
        theme(axis.text.x=element_text(size=14), axis.text.y = element_text(size=14), axis.title=element_text(size=18), plot.title = element_text(size = 18, face = "bold"))+
        annotate("text", label = lb6, parse=TRUE, x = 155, y = 100, size = 7, colour = "Black")+
        annotate("text", label = RMSE5, parse=TRUE, x = 155, y = 94, size = 7, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("d) Jmax - tested on variable water potential") +
        theme(plot.title = element_text(margin = margin(c(10, 10, -10, 0))))

plot6

grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6, ncol=2)
grid.arrange(plot1_resize,plot2_resize,plot3_resize,plot4_resize,plot5_resize,plot6_resize, ncol=2)
plot1_resize
plot2_resize
plot3_resize
plot4_resize
plot5_resize
plot6_resize


#Vcmax
file1 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_LOO/2comps(n=86)_train/fittedvalue.csv")
tests1 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result//Vcmax_LOO/2comps(n=86)_train/stats.csv")
rsquared1 <- tests1$R.val^2
lb1 <- paste("R^2 == ", round(rsquared1,2))
RMSE1 <- paste("RMSE==", round(RMSE(file1$yhat.cal, file1$yhat.val),2))
str(file1)
plot1_resize <- ggplot(file1, aes(y=yhat.cal, x=yhat.val))+
        geom_point(aes(fill="tomato2"), shape=21, colour="black", size=2)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Vcmax")+
        ylab("Predicted Vcmax")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        annotate("text", label = lb1, parse=TRUE, x = 98, y = 60, size = 4, colour = "Black")+
        annotate("text", label = RMSE1, parse=TRUE, x = 98, y = 45, size = 4, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("a) Vcmax - leave-one-out") + 
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
        guides(fill=FALSE)

#Jmax
file2 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_LOO/4comps(n=86)_train/fittedvalue.csv")
tests2 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_LOO/4comps(n=86)_train/stats.csv")
rsquared2 <- tests2$R.val^2
lb2 <- paste("R^2 == ", round(rsquared2,2))
RMSE2 <- paste("RMSE==", round(RMSE(file2$yhat.cal, file2$yhat.val),2))
str(file2)
plot2_resize <- ggplot(file2, aes(y=yhat.cal, x=yhat.val))+
        geom_point(aes(fill="tomato2"), shape=21, colour="black", size=2)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Jmax")+
        ylab("Predicted Jmax")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        annotate("text", label = lb2, parse=TRUE, x = 200, y = 100, size = 4, colour = "Black")+
        annotate("text", label = RMSE2, parse=TRUE, x = 200, y = 80, size = 4, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("b) Jmax - leave-one-out") + 
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
        guides(fill=FALSE)


#80-20
#Vcmax
file3 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/fittedvalue_test.csv")
tests3 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_80_20_fig/2comps(n=68)_test/stats.csv")
rsquared3 <- tests3$R.test^2
lb3 <- paste("R^2 == ", round(rsquared3,2))
RMSE3 <- paste("RMSE==", round(RMSE(file3$y.2.comps, file3$y.test),2))
plot3_resize <- ggplot(file3, aes(y=y.test, x=y.2.comps))+
        geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Vcmax")+
        ylab("Predicted Vcmax")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        annotate("text", label = lb3, parse=TRUE, x = 90, y = 60, size = 4, colour = "Black")+
        annotate("text", label = RMSE3, parse=TRUE, x = 90, y = 48, size = 4, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("c) Vcmax - representative 80/20") +
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
        guides(fill=FALSE)


#Jmax
file4 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_80_20_fig/4comps(n=68)_test/fittedvalue_test.csv")
tests4 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_80_20_fig/4comps(n=68)_test/stats.csv")
rsquared4 <- tests4$R.test^2
lb4 <- paste("R^2 == ", round(rsquared4,2))
RMSE4 <- paste("RMSE==", round(RMSE(file4$y.4.comps, file4$y.test),2))

plot4_resize <- ggplot(file4, aes(y=y.test, x=y.4.comps))+
        geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Jmax")+
        ylab("Predicted Jmax")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        annotate("text", label = lb4, parse=TRUE, x = 170, y = 100, size = 4, colour = "Black")+
        annotate("text", label = RMSE4, parse=TRUE, x = 170, y = 80, size = 4, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("d) Jmax - representative 80/20") + 
        theme(plot.title = element_text(margin = margin(c(10, 10, -20, 0))))+
        guides(fill=FALSE)

#Hardest
#Vcmax
file5 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/fittedvalue_test.csv")
tests5 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/stats.csv")
rsquared5 <- tests5$R.test^2
lb5 <- paste("R^2 == ", round(rsquared5,2))
RMSE5 <- paste("RMSE==", round(RMSE(file5$y.2.comps, file5$y.test),2))

plot5_resize <- ggplot(file5, aes(y=y.test, x=y.2.comps))+
        geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Vcmax")+
        ylab("Predicted Vcmax")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        annotate("text", label = lb5, parse=TRUE, x = 75, y = 62, size = 4, colour = "Black")+
        annotate("text", label = RMSE5, parse=TRUE, x = 75, y = 55, size = 4, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("e) Vcmax - tested on variable water potential") +
        theme(plot.title = element_text(margin = margin(c(10, 10, 0, 0))))+
        guides(fill=FALSE)


#Jmax
file6 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_Period/5comps(n=73)_test/fittedvalue_test.csv")
tests6 <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_Period/5comps(n=73)_test/stats.csv")
rsquared6 <- tests6$R.test^2
lb6 <- paste("R^2 == ", round(rsquared6,2))
RMSE6 <- paste("RMSE==", round(RMSE(file6$y.5.comps, file6$y.test),2))


plot6_resize <- ggplot(file6, aes(y=y.test, x=y.5.comps))+
        geom_point(aes(fill="tomato2"), shape=21, colour="black", size=3)+
        geom_abline(intercept = 0, slope = 1)+
        xlab("Measured Jmax")+
        ylab("Predicted Jmax")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        annotate("text", label = lb6, parse=TRUE, x = 155, y = 115, size = 4, colour = "Black")+
        annotate("text", label = RMSE5, parse=TRUE, x = 155, y = 105, size = 4, colour = "Black")+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("f) Jmax - tested on variable water potential") +
        theme(plot.title = element_text(margin = margin(c(10, 10, 0, 0))))+
        guides(fill=FALSE)


plot6

#Figure 3 Redo: 
#Hyperspectral fig + SR and VIP below? Or SR for Vcmax and Jmax perhaps
SR_Jmax[which.max(SR_Jmax$x),]
SR_Vcmax[which.max(SR_Vcmax$x),]


SR_Jmax <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Jmax_test_stressed_period/5comps(n=73)_test/selectivityRatio.csv")
str(SR_Jmax)
SR_Jmax$wavelength <- SR_Jmax$X+500
SR_plotJ <- ggplot(SR_Jmax, aes(y=x, x=wavelength))+
        geom_line(colour="darkslategray3")+
        xlim(500,2400)+
        ylim(0,3)+
    scale_x_continuous(breaks=seq(500, 2400, 200), limits=c(500,2400))+
          geom_smooth(method="loess",span=0.1, color="red")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("c) Jmax - tested on variable water potential") +
        xlab("wavelength (nm)")+
        ylab("Selectivity Ratio")+
        theme(plot.margin=unit(c(-0.2,0.5,0,0.5), "cm"))+
        theme(plot.title = element_text(margin = margin(c(10, 10, -10, 0))))+
        guides(fill=FALSE)

SR_Vcmax <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/PLSR_result/Vcmax_test_stressed_Period/2comps(n=73)_test/selectivityRatio.csv")
str(SR_Vcmax)
SR_Vcmax$wavelength <- SR_Vcmax$X+500
SR_plotV <- ggplot(SR_Vcmax, aes(y=x, x=wavelength))+
        geom_line(colour="darkslategray3")+
        xlim(500,2400)+
        scale_x_continuous(breaks=seq(500, 2400, 200), limits=c(500,2400))+
        ylim(0,7)+
        geom_smooth(method="loess",span=0.1, color="red")+
        theme(axis.text.x=element_blank(), axis.text.y = element_text(size=10), axis.title.x=element_blank(), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        ggtitle("b) Vcmax - tested on variable water potential") +
        xlab("wavelength (nm)")+
        ylab("Selectivity Ratio")+
        theme(plot.title = element_text(margin = margin(c(10, 10, -10, 0))))+
        guides(fill=FALSE)+
        theme(plot.margin=unit(c(-0.2,0.5,-0.1,0.5), "cm"))

Refs <- ggplot(hyperspectral_long, aes(y=reflectance, x=wavelength, group=uniqueID))+ geom_line(alpha=0.2)+
        #scale_x_continuous(breaks=seq(500, 2400, 200))+
        xlim(500,2400)+
        scale_x_continuous(breaks=seq(500, 2400, 200), limits=c(500,2400))+
        #geom_line(data=not_stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="blue")  +
        #geom_line(data=stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="red")+
        theme(legend.position="none")+ ggtitle("Mean Reflectance")+
        theme(axis.text.x=element_text(size=10), axis.text.y = element_text(size=10), axis.title.x=element_blank(), axis.title=element_text(size=10), plot.title = element_text(size = 10, face = "bold"))+
        theme(panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(plot.margin=unit(c(0.2,0.6,0,0.5), "cm"))+
        ggtitle("a) Leaf Spectra") +
        xlab("wavelength (nm)")+
        ylab("reflectance")+
        theme(plot.title = element_text(margin = margin(c(10, 10, -10, 0))))

str(hyperspectral_long)

mean(hyperspectral_long$reflectance, na.rm=TRUE)
names(hyperspectral_long)[3] <- "wavelength"
hyperspect_stats <- ddply(hyperspectral_long, .(wavelength), summarise, mean_ref=mean(reflectance, na.rm=TRUE), max_ref=max(reflectance,na.rm=TRUE), min_ref=min(reflectance,na.rm=TRUE))
rename(hyperspectral_long, )
grid.newpage()
grid.arrange(Refs, SR_plotV, SR_plotJ, heights=c(6,3,3.7))

