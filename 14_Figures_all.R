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
library(tidyr)
library(reshape2)
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
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
textfiles = list.files("ASCII_Reflectance/", pattern = "*.txt")
#txtfiles_subset is to test out the lapply
textfiles_subset = textfiles[1:5]
textfiles_subset=textfiles[448:457]
textfiles_subset
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/")
read.table("b2popmlb_E04_leaf_6-30-201600003.asd.txt")
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
write.csv(spectra, "spectra_for_fig_2_3_7_2017.csv")
spectra <- read.csv("C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/spectra_for_fig_2_3_7_2017.csv")
spectra$date <-as.Date(spectra$date, format="%m-%d-%Y")
#3) create "unique_ID" column
spectra$ID <- as.character(tolower(spectra$ID))
spectra$uniqueID <- paste(spectra$ID, spectra$date, sep='-') 
spectra$uniqueID
#5) Get rid of "reflectance" in column because we know that's what it is
names(spectra) <- gsub("reflectance.", "", names(spectra))
str(spectra)
#now average all reflectances by 'uniqueID'
str(spectra)
spectra[] <- lapply(spectra, function(x) {
        if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(spectra, class)
str(spectra)
spectra <- spectra[ -c(1:4)]
hyperspectral <- ddply(spectra, .(uniqueID), colwise(mean))
str(hyperspectral)
hyperspectral_long <- gather(hyperspectral, uniqueID, reflectance, c(`350`:`2500`), factor_key=TRUE)
str(hyperspectral_long)
#Rename second column "wavelength"
names(hyperspectral_long)[3] <- "wavelength"
hyperspectral_long$wavelength <- as.numeric(as.character(hyperspectral_long$wavelength))
str(hyperspectral_long)
ggplot(hyperspectral_long, aes(y=reflectance, x=wavelength, group=uniqueID))+ geom_line(alpha=0.2)+
        scale_x_continuous(breaks=seq(350, 2500, 200))+
        geom_line(data=not_stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="blue")  +
        geom_line(data=stressed, aes(x=wavelength, y=reflectance, group=1, fill=1), size=1, colour="red")+
        theme(legend.position="none")

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
#now average all reflectances by 'uniqueID'
str(PLSR_formatted)
hyperspectral <- ddply(PLSR_formatted, .(uniqueID), colwise(mean))
str(hyperspectral)
hyperspectral <- hyperspectral[ -c(2:3, 5:6)]
#colnames(hyperspectral)[1] <- "uniqueID"
#remove columns: x, observation, filename
#Load file with other data
data_aci_etc <- read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/All_with_more_licor_vars.csv")
str(data_aci_etc)
merged_hyperspec <- merge(data_aci_etc, hyperspectral, by="uniqueID")
str(merged_hyperspec)
#wanna keep date and genotype columns for potential easy subsetting I think
merged_hyperspec <- merged_hyperspec[ -c(2:6, 9:14, 16:19, 21:32)]
str(merged_hyperspec)
write.csv(merged_hyperspec, "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/poplar_allwavelengths.csv")




#Figure 4----------------------
Corrs <- read.csv("C:/Users/Mallory/Dropbox/Paper_2/Corr_to_plot_3_7_2017.csv")
str(Corrs)

c1 <- ggplot(Corrs, aes(y=Vcmax_R_squared, x=reorder(Index, -Vcmax_R_squared)))+
        ylim(0,0.8)+
        geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x=element_blank())+
         ylab("Vcmax R-squared")

c2 <- ggplot(Corrs, aes(y=Jmax_R_squared, x=reorder(Index, -Jmax_R_squared)))+
        ylim(0,0.8)+
        geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        xlab("Indices")+
        ylab("Jmax R-squared")

multiplot(c1,c2)

