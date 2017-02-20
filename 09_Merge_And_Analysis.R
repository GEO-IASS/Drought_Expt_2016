#Author: Mallory Barnes
#Date: 12/15/2016 
#Input: 'Merged' data frame containing A/Ci and met input varibles, processed hyperspectral files
#'Unique-ID' column is the key to merging 
#Output: Merged Data frame, figs 1 and 2 

library(plyr)
library(psych)
library(Hmisc)

#Multiplot function from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#Allows multiple graphs on one page
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

#pre-analysis merging/ cleaning ------------------------------------------
#only needs to be altered if the Files of origin have been changed (A/Ci output file or hyperspectral files)
#'merged' file contains A/Ci (Vcmax/Jmax), met variables, and 'unique_ID' 
merged <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Merged_data_to_analyze.csv")
#'hyperspec' file contains processed hyperspectral files including 'unique_ID'
hyperspec <-read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files_Added_Indices.csv")

#Checking out both files
head(merged)
merged[25:35,]
head(hyperspec)

#hyperspectral data - create "unique id" (same as in 'merged')and then average by unique ID
#Then will be able to merge with A/Ci and Climate files
#'uniqueID' takes form: a18-2016-06-08 

hyperspec$uniqueID <- paste(tolower(hyperspec$ID), hyperspec$date, sep='-')

#want means of VIs by uniqueID - use ddply
hyperspec_avg <- ddply(hyperspec,~uniqueID,summarise, NDVI=mean(NDVI), PRI=mean(PRI), NDWI=mean(NDWI), Datt4=mean(Datt4), Vogelmann2=mean(Vogelmann2))

#merge hyperspec and a/ci data
all_data <- merge(hyperspec_avg, merged, by="uniqueID")
str(all_data)
write.csv(all_data, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data_new_indices.csv")

#clean up "all_data"
#delete X.2, X.1, and X columns (what even are these?)
all_data <- subset(all_data, select=-c(X,X.1, X.2))

#giant correlation matrix to clipboard for quick glance
#Saved as 'corr matrix' in the Paper_2 folder in dropbox
c <-(cor(all_data[,unlist(lapply(all_data, is.numeric))]))
write.table(c, "clipboard", sep="\t", row.names=FALSE)

#Reading in merged data
#START HERE if no changes to original data files necessary--------------- 
all_data <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data.csv")
str(all_data)
all_data$ratio <- (all_data$Vcmax/all_data$Jmax)
head(all_data)

#1) Are Vcmax and Jmax Sensistive to Drought Stress?---------------------
#Format date as date
all_data$Date.x <- as.Date(all_data$Date.x)
#Defining by experiment phase
phase_1 <-subset(all_data, Date.x<"2016-06-08")
phase_2 <- subset(all_data, Date.x>="2016-06-08" & Date.x<="2016-06-16")
phase_3 <- subset(all_data, Date.x>="2016-06-16")
#Set phases 
all_data$phase <- 2
phase_2$phase <-2
phase_3$phase <-3
#Data graphs (data frame) - file has only phases 2 and 3 for graphing purposes
data_graphs <- rbind(phase_2, phase_3)
data_graphs$phase <- as.factor(data_graphs$phase)

#User-defined function to return correlations by group
#Also returns the p-value of the correlation
#Have to chagnge the variables based on what you want to look at (xx$____ and xx$y)
require(plyr)
func <- function(xx, a, b)
        
{
        print(cor.test(xx$ratio, xx$Water_Pot))
        return(data.frame(COR = cor(xx$ratio, xx$Water_Pot)))
        
}
#For example: this shows the correlations in phase 2 by genotype
ddply(phase_2, .(Genotype), func)
#This shows the correlations in all the data by genotype
ddply(all_data, .(Genotype), func)
#What about the correlations in all the data (dry end only) by genotype? 
#create subset by water potential
dry_data <- subset(data_graphs, Water_Pot < -0.3)
str(dry_data)
func_Vcmax <- function(xx)
{
        COR=cor(xx$Water_Pot, xx$Vcmax)
        SIG=cor.test(xx$Water_Pot,xx$Vcmax)$p.value
        return(data.frame(COR, SIG))
        
}
func_Vcmax(dry_data)
ddply(dry_data, .(phase,Genotype), func_Vcmax)
ddply(dry_data, .(Genotype), func_Vcmax)
ddply(dry_data, .(Plant_ID.x), func_Vcmax)
func_Jmax <- function(xx)
{
        COR=cor(xx$Water_Pot, xx$Jmax)
        SIG=cor.test(xx$Water_Pot,xx$Jmax)$p.value
        return(data.frame(COR, SIG))
        
}




#Explore correlations between delta t and vcmax/jmax

func_Vcmax_T <- function(xx)
{
        COR=cor(xx$Water_Pot, xx$Vcmax)
        SIG=cor.test(xx$Delta_T,xx$Vcmax)$p.value
        return(data.frame(COR, SIG))
        
}
func_Vcmax_T(data_graphs)
ddply(data_graphs, .(phase,Genotype), func_Vcmax)
ddply(data_graphs, .(Genotype), func_Vcmax)
ddply(data_graphs, .(Plant_ID.x), func_Vcmax)



func_Jmax_T <- function(xx)
{
        COR=cor(xx$Water_Pot, xx$Jmax)
        SIG=cor.test(xx$Delta_T,xx$Jmax)$p.value
        return(data.frame(COR, SIG))
        
}
func_Jmax_T(data_graphs)
ddply(data_graphs, .(phase,Genotype), func_Jmax_T)
ddply(data_graphs, .(Genotype), func_Jmax_T)
ddply(data_graphs, .(Plant_ID.x), func_Jmax_T)



#create subsets by genotype
str(data_graphs)
gt52_276 <-subset(data_graphs, Genotype=="52-276")
gtR270 <- subset(data_graphs, Genotype=="R-270")

gt52_276_dry <-subset(dry_data, Genotype=="52-276")
gtR270_dry <- subset(dry_data, Genotype=="R-270")

str(gt52_276)
str(gtR270)

#Figure_1: ----------------------------------------
#water potential, vmax, and jmax by genotype with correlations
#So this will need to be 4 panels, each with 2 lines on it

graph1a <- ggplot(data=gt52_276, (aes(x=Water_Pot, y=Vcmax, colour=phase)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+ 
        #annotate("text", x=-1.1, y=95, label="Phase 2: r= 0.359, \n Phase 3: r= 0.126")
        annotate("text", x=-1.1, y=95, label="Correlations \n nonsignificant")

graph1b <- ggplot(data=gt52_276, (aes(x=Water_Pot, y=Jmax, colour=phase)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+
        #annotate("text", x=-1.1, y=182, label="Phase 2: r= 0.35, \n Phase 3: r= -0.19")
        annotate("text", x=-1.1, y=182, label="Correlations \n nonsignificant")


graph1c <- ggplot(data=gtR270, (aes(x=Water_Pot, y=Vcmax, colour=phase)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+ 
        #annotate("text", x=-0.75, y=90, label="Phase 2: r= 0.405, \n Phase 3: r= -0.25")
        annotate("text", x=-0.75, y=90, label="Correlations \n nonsignificant")

graph1d <- ggplot(data=gtR270, (aes(x=Water_Pot, y=Jmax, colour=phase)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+ 
        #annotate("text", x=-0.75, y=165, label="Phase 2: r= -0.25,\n Phase 3: r= -0.11")
        annotate("text", x=-0.75, y=165, label="Correlations \n nonsignificant")

#Draw figure 1 using multiplot function
fig_1 <- multiplot(graph1a, graph1b, graph1c, graph1d, cols=2)

#Save figure 1 as .png - wasn't working properly 
ggsave(fig_1, file="C:/Users/Mallory/Dropbox/Drought_Expt_2016/Figure_1.png", dpi=500)

#Figure 1.5 - remade for low water potentials only: 

graph15a <- ggplot(data=gt52_276_dry, (aes(x=Water_Pot, y=Vcmax, colour=phase)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+ 
        #annotate("text", x=-1.1, y=95, label="Phase 2: r= 0.359, \n Phase 3: r= 0.126")
        annotate("text", x=-1.1, y=95, label="Correlations \n nonsignificant")

graph15b <- ggplot(data=gt52_276_dry, (aes(x=Water_Pot, y=Jmax, colour=phase)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+
        #annotate("text", x=-1.1, y=182, label="Phase 2: r= 0.35, \n Phase 3: r= -0.19")
        annotate("text", x=-1.1, y=182, label="Correlations \n nonsignificant")


graph15c <- ggplot(data=gtR270_dry, (aes(x=Water_Pot, y=Vcmax, colour=phase)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+ 
        annotate("text", x=-0.75, y=90, label="Phase 2: r= -0.83, \n Phase 3: r= 0.129")
        #annotate("text", x=-0.75, y=90, label="Correlations \n nonsignificant")

graph15d <- ggplot(data=gtR270_dry, (aes(x=Water_Pot, y=Jmax, colour=phase)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+ 
        #annotate("text", x=-0.75, y=165, label="Phase 2: r= -0.25,\n Phase 3: r= -0.11")
        annotate("text", x=-0.75, y=165, label="Correlations \n nonsignificant")

#Draw figure 1 using multiplot function
fig_15 <- multiplot(graph15a, graph15b, graph15c, graph15d, cols=2)

#Save figure 1 as .png - wasn't working properly 
ggsave(fig_15, file="C:/Users/Mallory/Dropbox/Drought_Expt_2016/Figure_15.png", dpi=500)


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
        #geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        #annotate("text", x=190, y=-0.035, label="52-276: r= 0.09, \n R270: r=-0.12")
        annotate("text", x=190, y=-0.035, label="52-276: Corr NS, \n R270: Corr NS")
        

graph2e <- ggplot(data=all_data, (aes(x=Vcmax, y=NDWI, colour=Genotype)))+
        geom_point()+
        #geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        #annotate("text", x=100, y=0.035, label="52-276: r= 0.27, \n R270: r=0.08")+
        annotate("text", x=100, y=0.035, label="52-276: Corr NS, \n R270: Corr NS")


graph2f <- ggplot(data=all_data, (aes(x=Jmax, y=NDWI, colour=Genotype)))+
        geom_point()+
        geom_smooth(method="lm", se=FALSE)+
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        annotate("text", x=180, y=0.035, label="52-276: r= 0.554, \n R270: r=0.526")



fig_2 <- multiplot(graph2a, graph2c, graph2e, graph2b, graph2d, graph2f, cols=2)

#saved manually because ggsave not working

#Figure_3-------------------
#Still trying to find code from the summer (Alec's stuff)

ggplot(data=all_data,
       aes(x=Delta_T, y=Vcmax, colour=Genotype)) +
        geom_point()+
        geom_smooth()



