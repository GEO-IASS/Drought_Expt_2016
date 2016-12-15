library(plyr)
library(psych)
library(Hmisc)
merged <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Merged_data_to_analyze.csv")
hyperspec <-read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files.csv")

head(merged)
merged[25:35,]
head(hyperspec)

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


#hyperspectral data - create "unique id" (same as in 'merged')and then average by unique ID
#Then will be able to merge with A/Ci and Climate files
#'uniqueID' takes form: a18-2016-06-08 

hyperspec$uniqueID <- paste(tolower(hyperspec$ID), hyperspec$date, sep='-')

#want means of VIs by uniqueID - use ddply
hyperspec_avg <- ddply(hyperspec,~uniqueID,summarise, NDVI=mean(NDVI), PRI=mean(PRI), NDWI=mean(NDWI))

#merge hyperspec and a/ci data

all_data <- merge(hyperspec_avg, merged, by="uniqueID")
str(all_data)
write.csv(all_data, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data.csv")
#clean up "all_data"
#delete X.2, X.1, and X columns (what even are these?)

all_data <- subset(all_data, select=-c(X,X.1, X.2))

#giant correlation matrix!
c <-(cor(all_data[,unlist(lapply(all_data, is.numeric))]))
write.table(c, "clipboard", sep="\t", row.names=FALSE)

all_data <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data.csv")
str(all_data)
#1) Are Vcmax and Jmax Sensistive to Drought Stress?---------------------
#Function to return correlations by group

all_data$Date.x <- as.Date(all_data$Date.x)
all_data$phase <- 2
phase_2$phase <-2
phase_3$phase <-3
data_graphs <- rbind(phase_2, phase_3)
data_graphs$phase <- as.factor(data_graphs$phase)

phase_1 <-subset(all_data, Date.x<"2016-06-08")
phase_2 <- subset(all_data, Date.x>="2016-06-08" & Date.x<="2016-06-16")
phase_3 <- subset(all_data, Date.x>="2016-06-16")
require(plyr)
func <- function(xx, a, b)
{
        return(data.frame(COR = cor(xx$Jmax, xx$Water_Pot)))
}

ddply(phase_2, .(Genotype), func)

#subsets by genotype
str(data_graphs)
gt52_276 <-subset(data_graphs, Genotype=="52-276")
gtR270 <- subset(data_graphs, Genotype=="R-270")

str(gt52_276)
str(gtR270)

gtR270
#Figure_1: ----------------------------------------
#water potential, vmax, and jmax by genotype with correlations
#So this will need to be 4 panels, each with 2 lines on it

graph1a <- ggplot(data=gt52_276, (aes(x=Water_Pot, y=Vcmax, colour=phase)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+ 
        annotate("text", x=-1.1, y=95, label="Phase 2: r= 0.359, \n Phase 3: r= 0.126")

graph1b <- ggplot(data=gt52_276, (aes(x=Water_Pot, y=Jmax, colour=phase)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        annotate("text", x=-1.1, y=182, label="Phase 2: r= 0.35, \n Phase 3: r= -0.19")

graph1c <- ggplot(data=gtR270, (aes(x=Water_Pot, y=Vcmax, colour=phase)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+ 
        annotate("text", x=-0.75, y=90, label="Phase 2: r= 0.405, \n Phase 3: r= -0.25")

graph1d <- ggplot(data=gtR270, (aes(x=Water_Pot, y=Jmax, colour=phase)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+ 
        annotate("text", x=-0.75, y=165, label="Phase 2: r= -0.25,\n Phase 3: r= -0.11")


fig_1 <- multiplot(graph1a, graph1b, graph1c, graph1d, cols=2)


ggsave(fig_1, file="C:/Users/Mallory/Dropbox/Drought_Expt_2016/Figure_1.png", dpi=500)


#Figure 2---------------------------------------------
#Are hyperspectral indices correlated with Vcmax and Jmax?

graph2a <- ggplot(data=all_data, (aes(x=Vcmax, y=NDVI, colour=Genotype)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        annotate("text", x=80, y=0.68, label="52-276: r= 0.837, \n R270: r=0.544")


graph2b <- ggplot(data=all_data, (aes(x=Jmax, y=NDVI, colour=Genotype)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        annotate("text", x=175, y=0.68, label="52-276: r= 0.786, \n R270: r=0.451")

graph2c <- ggplot(data=all_data, (aes(x=Vcmax, y=PRI, colour=Genotype)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        annotate("text", x=90, y=-0.04, label="52-276: r= 0.478, \n R270: r=0.396")

graph2d <- ggplot(data=all_data, (aes(x=Jmax, y=PRI, colour=Genotype)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        annotate("text", x=190, y=-0.035, label="52-276: r= 0.09, \n R270: r=-0.12")

graph2e <- ggplot(data=all_data, (aes(x=Vcmax, y=NDWI, colour=Genotype)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        annotate("text", x=100, y=0.035, label="52-276: r= 0.27, \n R270: r=0.08")

graph2f <- ggplot(data=all_data, (aes(x=Jmax, y=NDWI, colour=Genotype)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        annotate("text", x=180, y=0.035, label="52-276: r= 0.554, \n R270: r=0.526")



fig_2 <- multiplot(graph2a, graph2b, graph2c, graph2d, graph2e, graph2f, cols=2)

#saved manually because ggsave not working

ggplot(data=all_data,
       aes(x=Delta_T, y=Vcmax, colour=Genotype)) +
        geom_point()+
        geom_smooth()











#What's this stuff - from before right?
#Finding good comparison observations: Looks like Individual E4, E10, E3 and G11 all got pretty stressed
#Gonna subset and re-plot

merged_stressed <- subset(merged, Plant_ID.x == "e04" | Plant_ID.x == "e03" | Plant_ID.x == "e10" | Plant_ID.x == "g11")

ggplot(merged_stressed, aes(Date.x, Water_Pot, colour=Plant_ID.x)) + geom_line(aes(group=Plant_ID.x))

#Let's look at E03 on 6/24 (stressed) and E03 on 06/09 (not stressed)
#Cool but how to 

#Get and Plot Hyperspec Files-------------------
#for my personal laptop
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

not_stressed <- read.table("ASCII_Reflectance/b2popmlb_E3_Leaf_6-01-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(not_stressed)

stressed <- read.table("ASCII_Reflectance/b2popmlb_E03_leaf_6-24-201600001.asd.txt", col.names=c("wavelength", "reflectance"))

#User Defined Function: Format Hyperspec: 

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
p

#Merge the two datafames together to make graph nicer: 

merged_e3 <- merge(stressed, not_stressed, by="wavelength")
str(merged_e3)
