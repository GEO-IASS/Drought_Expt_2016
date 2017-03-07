#Author: Mallory Barnes
#Date: 3/6/2016 
#Input: Current Master File
#Output: Four Key Figures

#Necessary Pacakges: 
library(lubridate)
library(ggplot2)
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
#Custom function for pulling x number of characters from the right of a string: from 
#http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
	
substrRight <- function(x, n){
        	        substr(x, nchar(x)-n+1, nchar(x))
        	}
	
Climate_data$TOD <- substrRight(as.character(Climate_data$TIMESTAMP), 5)
Climate_data$Date <- as.Date(substr(as.character(Climate_data$TIMESTAMP),1, 9), "%m/%d/%Y")


#Need sum of precip, and average daytime temp (06:00 to 18:00 hours)
#Sum precip by date; 
ddply(Climate_data, .(Date), summarise, sum = sum(Precip))
