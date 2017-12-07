library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

myData <- read_csv("data/grad_survey_colleges.csv")
totals <- ddply(myData, "year", numcolwise(sum))
totalVec <- NULL
for (i in 1:6) {
	totalVec <- c(totalVec,rep(totals[i,2],9))
}
myData$percent <- round(myData$responses/totalVec*100,1)
myData <-
	myData %>%
	group_by(year) %>%
	mutate(pos=cumsum(percent)-0.5*percent)

degrees <- read_csv("data/grad_survey_degrees.csv")
totalDegrees <- ddply(degrees, "year", numcolwise(sum))

totalDegreeVec <- NULL
for (i in 1:6) {
	totalDegreeVec <- c(totalDegreeVec,rep(totalDegrees[i,2],4))
}

degrees$percent <- round(degrees$responses/totalDegreeVec*100,1)
degrees <-
	degrees %>%
	group_by(year) %>%
	mutate(pos=cumsum(percent)-0.5*percent)

myColors <- brewer.pal(9, "Set3")
ggplot(data = myData, aes(x=year, y=percent, group=college, fill=college)) +
	geom_bar(position=position_stack(reverse = TRUE), stat="identity", color="black")+
	scale_y_continuous(limits=c(0,100), breaks=c(0,20,40,60,80,100), name="Percent Valid Responses") +
	geom_text(data=subset(myData, percent>5), aes(y=pos, label=paste(percent,"%",sep="")), size=5, fontface = "bold") +
	scale_fill_manual(values=myColors) +
	theme_linedraw() +
	xlab("Academic Year") +
	theme(text = element_text(size=15))

myColors2 <- brewer.pal(4, "Set3")
ggplot(data = degrees, aes(x=year, y=percent, group=degree, fill=degree)) +
	geom_bar(position=position_stack(reverse = TRUE), stat="identity", color="black")+
	scale_y_continuous(limits=c(0,100.5), breaks=c(0,20,40,60,80,100), name="Percent Valid Responses") +
	geom_text(data=subset(degrees, percent>5), aes(y=pos, label=paste(percent,"%",sep="")), size=5, fontface = "bold") +
	scale_fill_manual(values=myColors2) +
	theme_linedraw() +
	xlab("Academic Year") +
	theme(text = element_text(size=15))

ggplot(data = totals, aes(x=year, y=responses, label=responses)) +
	geom_bar(stat="identity", colour="black", fill="firebrick") +
	scale_y_continuous(limits=c(0,1200), breaks=c(0,200,400,600,800,1000,1200), name="Total Responses") +
	geom_text(fontface = "bold", size = 6, position = position_stack(vjust = 0.5), color="white") +
	theme_linedraw() +
	xlab("Academic Year") +
	theme(text = element_text(size=15))


questions <- read_csv("data/grad_survey_questions.csv")
plot1 <- questions %>%
	filter(item==1)

plot1$'1: Strongly Disagree' <- round((plot1$level1)/totals$responses*100,1)
plot1$'2: Somewhat Disagree' <- round((plot1$level2)/totals$responses*100,1)
plot1$'3: Somewhat Agree'    <- round((plot1$level3)/totals$responses*100,1)
plot1$'4: Strongly Agree'    <- round((plot1$level4)/totals$responses*100,1)
plot1$'Total positive (3+4)' <- plot1$`4: Strongly Agree`+plot1$`3: Somewhat Agree`

plot1 %>%
	gather(Attitude,responses, '1: Strongly Disagree':'Total positive (3+4)') %>%
	ggplot(aes(x=year, y=responses, group=Attitude, colour=Attitude)) +                   # Plot percentages by year
	scale_linetype_manual(values = c(1,1,1,1,2)) +                                        # Line styles
	geom_line(aes(linetype=Attitude), size=1) +                                           # Set line thickness
	geom_point(fill="white",shape=21, size=4, stroke=1) +                                 # Style the points
	scale_color_manual(values=c("#e4572e", "#f3a712", "#a8c686", "#669bbc","#000000")) +  # Line colors
	scale_y_continuous(limits=c(0,100), breaks=c(0,20,40,60,80,100)) +                    # Scale y axis
	ylab("Percent Valid Responses") +                                                     # Y axis label
	xlab("Year") +                                                                        # X axis label
	theme_linedraw() +
	xlab("Academic Year") +
	theme(text = element_text(size=15))



