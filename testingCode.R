library(tidyverse)
library(plyr)
library(RColorBrewer)

myData <- read_csv("data/grad_survey_colleges.csv")
totals <- ddply(myData, "year", numcolwise(sum))
totalVec <- NULL
for (i in 1:5) {
	totalVec <- c(totalVec,rep(totals[i,2],9))
}
myData$percent <- round(myData$responses/totalVec*100,1)

degrees <- read_csv("data/grad_survey_degrees.csv")
totalDegrees <- ddply(degrees, "year", numcolwise(sum))
totalDegreeVec <- NULL
for (i in 1:5) {
	totalDegreeVec <- c(totalDegreeVec,rep(totalDegrees[i,2],4))
}
degrees$percent <- round(degrees$responses/totalDegreeVec*100,1)

myColors <- brewer.pal(9, "Set3")
ggplot(data = myData, aes(x=year, y=percent, group=college, fill=college, label=paste(percent,"%",sep=""))) +
	geom_bar(position="stack", stat="identity", colour="black") +
	geom_text(size = 4, position = position_stack(vjust = 0.5)) +
	scale_fill_manual(values=myColors) +
	theme_classic()

myColors2 <- brewer.pal(4, "Set3")
ggplot(data = degrees, aes(x=year, y=percent, group=degree, fill=degree, label=paste(percent,"%",sep=""))) +
	geom_bar(position="stack", stat="identity", colour="black") +
	geom_text(size = 4, position = position_stack(vjust = 0.5)) +
	scale_fill_manual(values=myColors2) +
	theme_classic()

ggplot(data = totals, aes(x=year, y=responses, label=responses)) +
	geom_bar(stat="identity", colour="black", fill="firebrick") +
	geom_text(size = 4, position = position_stack(vjust = 0.5), color="white") +
	theme_classic()

questions <- read_csv("data/grad_survey_questions.csv")
plot1 <- questions %>%
	filter(item==21)
plot1$disagree <- round((plot1$level1+plot1$level2)/totals$responses*100,1)
plot1$agree <- round(plot1$level3+plot1$level4/(totals$responses-plot1$level0)*100,1)



plot1 %>%
	gather(attitude,responses, agree:disagree) %>%
	ggplot(aes(x=year, y=responses, group=attitude, colour=attitude)) +
	geom_line(size=1) +
	geom_point(fill="white", size=4) +
	ylim(0,100) +
	ylab("Percent of Valid Responses") +
	xlab("Year") +
	theme_linedraw()

