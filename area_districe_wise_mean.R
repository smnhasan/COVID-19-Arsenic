require(foreign)
require(dplyr)
require(data.table)
library(ggplot)
library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
.libPaths()
library("ggplot2", lib.loc="E:\\Portable Softwares\\R Portable\\R-Portable\\App\\R-Portable\\library")
library("ggrepel", lib.loc="E:\\Portable Softwares\\R Portable\\R-Portable\\App\\R-Portable\\library")

#rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "data.frame"]) # for clear r environment 
setwd('E:\\Arsenic Covid\\Bangladesh MICS6 SPSS Datasets')

## importing dataset
hh <- as.data.frame(read.spss('hh.sav',use.value.labels=F),stringsAsFactors = FALSE)

f <- c('WQ12A', 'HH6','HH7','HH7A')
fd <- hh[, f]
fd1 <- na.omit(fd)
fd1$WQ12A
fd1 <- read.csv("E:\\Arsenic Covid\\fd1.csv")

#for area average contamination 
# x <- fd1%>%
#   group_by(Division)%>%
#   summarise(mean(ARS))
# x

xx <- read.csv("E:\\Arsenic Covid\\division.csv")


cor (xx$MeanARS ,xx$CC, method = c("pearson"), use = "complete.obs")
cor.test(xx$MeanARS , xx$CC, method = c("pearson"), use = "complete.obs")

lm <- glm(MeanARS ~ CC, data=xx)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)


model.3nb <- glm.nb(MeanARS ~ CC, data = xx)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


a <- ggplot(xx, aes(x = log(CC), y = MeanARS))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 5,
             show.legend = F,colour="blue4") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=xx$Division, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("Division wise total cases (Bangladesh) in log-scale") + ylab("Arsenic in source water (ppb)")  +
  geom_smooth(method = "lm", se = FALSE,colour="blue4") 
a



#for Division(HH7){10 for level name here 10 for Barisha}
#HH6 for (urban & rural) 1 for urban 2 for rural 
# y <- fd1%>%
#   group_by(District)%>%
#   summarise(mean(ARS))
# 
# y

yy <- read.csv("E:\\Arsenic Covid\\district.csv")

cor (yy$MeanARS , log(yy$CC), method = c("pearson"), use = "complete.obs")
cor.test(yy$MeanARS , log(yy$CC), method = c("pearson"), use = "complete.obs")

lm <- glm(MeanARS ~ log(CC), data=yy)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)


model.3nb <- glm.nb(MeanARS ~ CC, data = yy)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


b <- ggplot(yy, aes(x = log(CC), y = MeanARS))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="blue4") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=yy$District, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("District wise total cases (Bangladesh) in log-scale") + ylab("Arsenic in source water (ppb)")  +
  geom_smooth(method = "lm", se = FALSE,colour="blue4") 
b



cor (xx$MeanARS , log(xx$CD), method = c("pearson"), use = "complete.obs")
cor.test(xx$MeanARS , log(xx$CD), method = c("pearson"), use = "complete.obs")


lm <- glm(MeanARS ~ log(CD), data=xx)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)

model.3nb <- glm.nb(MeanARS ~ CD, data = xx)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))



c <- ggplot(xx, aes(x = log(CD), y = MeanARS))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 5,
             show.legend = F,colour="blue4") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=xx$Division, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("Division wise total deaths (Bangladesh) in log-scale") + ylab("Arsenic in source water (ppb)")  +
  geom_smooth(method = "lm", se = FALSE,colour="blue4") 
c



#for Division(HH7){10 for level name here 10 for Barisha}
#HH6 for (urban & rural) 1 for urban 2 for rural 
# y <- fd1%>%
#   group_by(District)%>%
#   summarise(mean(ARS))
# 
# y

yy <- read.csv("E:\\Arsenic Covid\\district.csv")

cor (yy$MeanARS , log(yy$CD), method = c("pearson"), use = "complete.obs")
cor.test(yy$MeanARS , log(yy$CD), method = c("pearson"), use = "complete.obs")

lm <- glm(MeanARS ~ log(CD), data=yy)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)

model.3nb <- glm.nb(MeanARS ~ CD, data = yy)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))


d <- ggplot(yy, aes(x = log(CD), y = MeanARS))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="blue4") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=yy$District, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("District wise total deaths (Bangladesh) in log-scale") + ylab("Arsenic in source water (ppb)")  +
  geom_smooth(method = "lm", se = FALSE,colour="blue4") 
d

yy <- read.csv("E:\\Arsenic Covid\\district.csv")

tiff("Arsenic.tiff", units="in", width=12, height=12, res=300)
gridExtra::grid.arrange(a,b,c,d, ncol = 2, nrow=2)
dev.off()


zz <-  read.csv("E:\\Arsenic Covid\\AsData.csv")


data <- aggregate(cbind(No.of.Arsenic.patient, Population.in.2009, CC, Arsenic.Contamination.ratio.., SWD.coverage.ration.) ~ Upazila, data = zz, FUN = mean)


cor (data$Arsenic.Contamination.ratio.. , log(data$CC), method = c("pearson"), use = "complete.obs")
cor.test(data$Arsenic.Contamination.ratio.. , log(data$CC), method = c("pearson"), use = "complete.obs")

lm <- glm(Arsenic.Contamination.ratio.. ~ log(CC), data=data)

summary(lm)
confint(lm)

with(summary(lm), 1 - deviance/null.deviance)

e <- ggplot(data, aes(x = log(CC), y = Arsenic.Contamination.ratio..))  + 
  theme(plot.title = element_text(hjust = 0.5,size=18,face = "bold"),text = element_text(size = 18))+
  geom_point(size = 3.5,
             show.legend = F,colour="blue4") +
  geom_text_repel(show.legend = F, 
                  size = 3.5, 
                  label=data$Upazila, 
                  hjust = 0.2,
                  vjust= 0.5) +
  xlab("Sub-district wise total cases (Noakhali) in log-scale") + ylab("Arsenic in source water (ppb)")  +
  geom_smooth(method = "lm", se = FALSE,colour="blue4") 
e

tiff("ArsenicN.tiff", units="in", width=12, height=6, res=300)
gridExtra::grid.arrange(e)
dev.off()

