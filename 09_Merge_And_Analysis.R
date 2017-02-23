#Author: Mallory Barnes
#Date: 12/15/2016 
#Input: 'Merged' data frame containing A/Ci and met input varibles, processed hyperspectral files
#'Unique-ID' column is the key to merging 
#Output: Merged Data frame, figs 1 and 2 

library(plyr)
library(psych)
library(Hmisc)
library(ggplot2)
library(ggpmisc)
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
hyperspec <-read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files_Yendreck_Indices_2_21.csv")

#Checking out both files
head(merged)
merged[25:35,]
head(hyperspec)

#hyperspectral data - create "unique id" (same as in 'merged')and then average by unique ID
#Then will be able to merge with A/Ci and Climate files
#'uniqueID' takes form: a18-2016-06-08 

hyperspec$uniqueID <- paste(tolower(hyperspec$ID), hyperspec$date, sep='-')

#want means of VIs by uniqueID - use ddply
hyperspec_avg <- ddply(hyperspec,~uniqueID,summarise, NDVI=mean(NDVI), PRI=mean(PRI), NDWI=mean(NDWI), Datt4=mean(Datt4), Vogelmann2=mean(Vogelmann2), Maccioni=mean(Maccioni), Double_Difference=mean(Double_Difference), Vogelmann1=mean(Vogelmann1), mSR705=mean(mSR705), SR3=mean(SR3), SR4=mean(SR4), SR1=mean(SR1), Gitelson=mean(Gitelson), SR2=mean(SR2), SIPI=mean(SIPI), mNDVI=mean(mNDVI), mSRCHL=mean(mSRCHL))

#merge hyperspec and a/ci data
all_data <- merge(hyperspec_avg, merged, by="uniqueID")
str(all_data)
write.csv(all_data, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data_Yendreck_indices.csv")

#clean up "all_data"
#delete X.2, X.1, and X columns (what even are these?)
all_data <- subset(all_data, select=-c(X,X.1, X.2))

#giant correlation matrix to clipboard for quick glance
#Saved as 'corr matrix' in the Paper_2 folder in dropbox
c <-(cor(all_data[,unlist(lapply(all_data, is.numeric))]))
write.table(c, "clipboard", sep="\t", row.names=FALSE)

#Reading in merged data
#START HERE if no changes to original data files necessary--------------- 
all_data <- read.csv("C:/Users/rsstudent/Dropbox/Drought_Expt_2016/all_data_Yendreck_indices.csv")
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
str(data_graphs)
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
#If you want the equation in addition to r-squared on the plot, use the line: 
# aes(label = paste(..eq.label.., ..rr.label..,  sep = "~~~")), 
my.formula <- y ~ x

graph2a <- ggplot(data=all_data, (aes(x=Vcmax, y=NDVI, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(0.6, 0.9)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)



graph2b <- ggplot(data=all_data, (aes(x=Jmax, y=NDVI, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(0.6, 0.9)+
        geom_smooth(method="lm", se=FALSE)+
                stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)



graph2c <- ggplot(data=all_data, (aes(x=Vcmax, y=PRI, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(-0.07, 0.03)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2d <- ggplot(data=all_data, (aes(x=Jmax, y=PRI, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(-0.07, 0.03)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)


graph2e <- ggplot(data=all_data, (aes(x=Vcmax, y=NDWI, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(0.02, 0.07)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)


graph2f <- ggplot(data=all_data, (aes(x=Jmax, y=NDWI, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(0.02, 0.07)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)


graph2g <- ggplot(data=all_data, (aes(x=Vcmax, y=Datt4, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(0,4)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2h <- ggplot(data=all_data, (aes(x=Jmax, y=Datt4, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(0,4)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2i <- ggplot(data=all_data, (aes(x=Vcmax, y=Vogelmann2, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(-0.9,-0.5)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2j <- ggplot(data=all_data, (aes(x=Jmax, y=Vogelmann2, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(-0.9,-0.5)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2k<- ggplot(data=all_data, (aes(x=Vcmax, y=Maccioni, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(0.2,0.8)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2l <- ggplot(data=all_data, (aes(x=Jmax, y=Maccioni, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(0.2,0.8)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2m <-ggplot(data=all_data, (aes(x=Vcmax, y=Double_Difference, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(-0.13,0.15)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2n <-ggplot(data=all_data, (aes(x=Jmax, y=Double_Difference, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(-0.13,0.15)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2o <-ggplot(data=all_data, (aes(x=Vcmax, y=Vogelmann1, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(1.1,1.6)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2p <- ggplot(data=all_data, (aes(x=Jmax, y=Vogelmann1, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(1.1,1.6)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2q <-ggplot(data=all_data, (aes(x=Vcmax, y=mSR705, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(1.8,5.1)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2r <-ggplot(data=all_data, (aes(x=Jmax, y=mSR705, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(1.8,5.1)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)


graph2s <-ggplot(data=all_data, (aes(x=Vcmax, y=SR3, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(2,5.2)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2t <-ggplot(data=all_data, (aes(x=Jmax, y=SR3, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(2,5.2)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2u <-ggplot(data=all_data, (aes(x=Vcmax, y=SR4, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(1.8,4)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2v <-ggplot(data=all_data, (aes(x=Jmax, y=SR4, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(1.8,4)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2w <-ggplot(data=all_data, (aes(x=Vcmax, y=SR1, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(1.9,5.5)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2x <-ggplot(data=all_data, (aes(x=Jmax, y=SR1, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(1.9,5.5)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2y <-ggplot(data=all_data, (aes(x=Vcmax, y=Gitelson, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(4.5,11.5)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2z <-ggplot(data=all_data, (aes(x=Jmax, y=Gitelson, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(4.5,11.5)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2aa <-ggplot(data=all_data, (aes(x=Vcmax, y=SR2, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(4,11)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2ab <-ggplot(data=all_data, (aes(x=Jmax, y=SR2, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(4,11)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2ac <-ggplot(data=all_data, (aes(x=Vcmax, y=SIPI, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(0.997,1.018)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2ad <-ggplot(data=all_data, (aes(x=Jmax, y=SIPI, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(0.997,1.018)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2ae <-ggplot(data=all_data, (aes(x=Vcmax, y=mNDVI, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(0.95,1.005)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2af <-ggplot(data=all_data, (aes(x=Jmax, y=mNDVI, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(0.95,1.005)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2ag <-ggplot(data=all_data, (aes(x=Vcmax, y=mSRCHL, colour=Genotype)))+
        geom_point()+
        xlim(35,115)+
        ylim(-100,1200)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

graph2ah <-ggplot(data=all_data, (aes(x=Jmax, y=mSRCHL, colour=Genotype)))+
        geom_point()+
        xlim(80,225)+
        ylim(-100,1200)+
        geom_smooth(method="lm", se=FALSE)+
        stat_poly_eq(formula = my.formula, rr.digits=3,
                     aes(label = paste(..rr.label..,  sep = "~~~")), 
                     parse = TRUE) +         
        scale_color_manual(values=c("#7b3294", "#7fbf7b"))+
        theme_bw(base_size=12)

fig_2 <- multiplot(graph2a, graph2c, graph2e, graph2b, graph2d, graph2f, cols=2)
fig_2b <- multiplot(graph2g, graph2i, graph2h, graph2j, cols=2)
fig_2c <- multiplot(graph2k,  graph2m,  graph2o, graph2l,graph2n,  graph2p, cols=2)
fig_2d<- multiplot(graph2q, graph2s, graph2u, graph2r, graph2t, graph2v, cols=2)
fig_2e <- multiplot(graph2w, graph2y, graph2aa, graph2x, graph2z, graph2ab, cols=2)
fig_2f <- multiplot(graph2ac, graph2ae, graph2ag, graph2ad, graph2af, graph2ah, cols=2)
#saved manually because ggsave not working

#Figure_3-------------------
#Still trying to find code from the summer (Alec's stuff)

ggplot(data=all_data,
       aes(x=Delta_T, y=Vcmax, colour=Genotype)) +
        geom_point()+
        geom_smooth()



