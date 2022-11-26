library(lubridate)
library(tidyverse)

### DCS Data (by minute) ###
GasA <- read.csv("GasA.csv")
GasB <- read.csv("GasB.csv")
GasA21 <- read.csv("GasA21.csv")
GasB21 <- read.csv("GasB21.csv")
GasA[,2:11] <- lapply(GasA[,2:11], as.numeric)
GasB[,2:11] <- lapply(GasB[,2:11], as.numeric)
GasA21[,2:11] <- lapply(GasA21[,2:11], as.numeric)
GasB21[,2:11] <- lapply(GasB21[,2:11], as.numeric)
GasA[,12:16] <- lapply(GasA[,12:16], as.character)
GasB[,12:16] <- lapply(GasB[,12:16], as.character)
GasA21[,12:16] <- lapply(GasA21[,12:16], as.character)
GasB21[,12:16] <- lapply(GasB21[,12:16], as.character)
GasA$Date <- dmy_hms(GasA$Date)
GasB$Date <- dmy_hms(GasB$Date)
GasA21$Date <- mdy_hm(GasA21$Date)
GasB21$Date <- mdy_hm(GasB21$Date)
GasA <- union(GasA, GasA21)
GasB <- union(GasB, GasB21)
GasA$HDR <- rep("A", times = length(GasA[,1]))
GasB$HDR <- rep("B", times = length(GasB[,1]))
Gas <- union(GasA, GasB)
Gas$TANK <- str_replace(Gas$TANK, "_", "-")
Gas$OSTS <- if_else(Gas$OSTS=="ON",1,0)
Gas$AIFAIL <- if_else(Gas$AISTS=="Fail",1,0)

### Analyzer vs Lab (Offsets) ###
diff <- read.csv("diff.csv")
diff$Date <- mdy_hms(diff$Date)
diffR <- diff[,c(1,2,4)]

Gas <- merge(Gas, diffR, by="Date", all.x=TRUE)

###  Crew member on board during blending  ###
shift <- read.csv("OMShift.csv")
shift21 <- read.csv("OMShift21.csv")
shift <- union(shift, shift21)
sd <- shift[,1:2]
sn <- shift[,c(1,3)]
sd$Dt <- paste(sd$Dt, "06:00:00")
sn$Dt <- paste(sn$Dt, "18:00:00")
sd$Dt <- mdy_hms(sd$Dt)
sn$Dt <- mdy_hms(sn$Dt)
names(sd)[2] <- "crew"
names(sn)[2] <- "crew"
names(sn)[1] <- "Date"
names(sd)[1] <- "Date"
s <- union(sd,sn)
Gas <- merge(Gas, s, by="Date", all.x=TRUE)
Gas <- Gas[order(Gas$Date),] # Order by date ascending

OM[1] <- "O"  #Crew member 'O' was on the board on midnight of new year 2021

### For Loop takes a long time to run!! Use gas.csv file if possible ###  
### Fill in crew member for each minute of blend ###

#for (x in 1:length(Gas[,21])) {
# if (is.na(Gas$crew[x])) {
#  Gas$crew[x] <- OM[1]
# } else if (!is.na(Gas$crew[x])) {
#    OM[1] <- Gas$crew[x]
#   }
#} 

#write.csv(Gas, "gas.csv")


Gas$crew <- as.factor(Gas$crew)
Gas$BLDN <- paste(month(Gas$Date),day(Gas$Date),Gas$BL

### Summarize Blends by Blend No. ###
Bld <- Gas |> filter(BLNDST=="STEADY STATE") |> group_by(BLDN) |> 
 	summarize(Date=max(Date), Tank=first(TANK), len=n(), HDR=first(HDR), Crew=names(which.max(table(crew))), #Crew finds most common crew member for each blend
		AIRDO=mean(AIRDO),AIRON=mean(AIRON), AIMON=mean(AIMON),
		MDRDO=mean(MDRDO), MDRON=mean(MDRON), MDMON=mean(MDMON),
		EORON=mean(EOFFRON), EOMON=mean(EOFFMON),
  		LLMEAN=mean(LL), LLMAX=max(LL), LLMIN=min(LL),
		OSTSMEAN=mean(OSTS), OSTSMAX=max(OSTS), OSTSMIN=min(OSTS),
		AIF=mean(AIFAIL), ADIFF=mean(A.Diff.RDO, na.rm = TRUE), BDIFF=mean(B.Diff.RDO, na.rm=TRUE))

Bld$BDIFF <- if_else(Bld$BDIFF < -5 | Bld$BDIFF > 5, 0, Bld$BDIFF)
Bld$ADIFF <- if_else(Bld$ADIFF < -5 | Bld$ADIFF > 5 ,0, Bld$ADIFF)

### Lab Data ###
labv <- read.csv("LabVantage.csv")
fin <- subset(labv, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & grepl("D4814", Test.Method.ID, fixed=TRUE)==TRUE)
fin$Sample.Collection.Date <- ymd_hms(fin$Sample.Collection.Date)
names(fin)[11] <- "finDate"
names(fin)[7] <- "Result"
names(fin)[2] <- "ID"
fin <- subset(fin, select=c("finDate","ID","Result"))
fin$ID <- str_extract(fin$ID, "T-...")
names(fin)[2] <- "Tank"
fin <- as_tibble(fin)

labv21 <- read.csv("LabVantage21.csv") # 2021 Lab data
fin21 <- subset(labv21, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & grepl("D4814", Test.Method.ID, fixed=TRUE)==TRUE)
fin21$Sample.Collection.Date <- mdy_hm(fin21$Sample.Collection.Date)
names(fin21)[11] <- "finDate"
names(fin21)[7] <- "Result"
names(fin21)[2] <- "ID"
fin21 <- subset(fin21, select=c("finDate","ID","Result"))
fin21$ID <- str_extract(fin21$ID, "T-...")
names(fin21)[2] <- "Tank"
fin21 <- as_tibble(fin21)

# Combine 2022 and 2021 lab data
fin <- union(fin, fin21) 


# Match DCS blending to certified tank values #
A <- as_tibble(merge(fin, Bld, by="Tank"))
A$diff <- A$finDate - A$Date
A$diff <- as.duration(A$diff)
A <- subset(A, diff < 172800 & diff > 0)  # Assume tank was certified within 2 days and same tank wasn't used within that timeframe
A$HDR <- as.factor(A$HDR)
A$Crew <- as.factor(A$Crew)

sub <- subset(A, Result<88) #Subgrade
sub$GWY <- if_else(sub$Result>83.7, sub$Result - 83.7, 0) #Giveaway
sub$GWYD <- if_else(sub$GWY>0, 1, 0) #Giveaway Discrete
sub$DIFF <- if_else(is.na(sub$ADIFF), sub$BDIFF, sub$ADIFF)


### Subsets ###

sub21 <- subset(sub, year(Date)==2021)
sub22 <- subset(sub, year(Date)==2022)
subA <- subset(sub, HDR=="A")
subB <- subset(sub, HDR=="B")
subA$ai_plus_diff <- subA$AIRDO + subA$ADIFF
subB$ai_plus_diff <- subB$AIRDO + subB$BDIFF
subA$ll_plus_diff <- subA$LLMEAN + subA$ADIFF
subB$ll_plus_diff <- subB$LLMEAN + subB$BDIFF

sub$my <- paste(as.character(month(sub$Date)), as.character(year(sub$Date)))

### Plots ###

sub |>  
 ggplot(aes(x=HDR, y=GWY)) + 
 geom_boxplot() +
 #labs(x="RDO - Blender NIR", y="RDO - Finished Lab Certification", 
 #title="Online NIR vs. Knock Engine", subtitle="B Blender") + 
 theme_light()

sub |> 
filter(HDR=="B", AIF==0) |> 
 ggplot(aes(x=(AIRDO), y=Result)) + 
 geom_point() +  
 coord_fixed(ratio=1, ylim=c(83,85), xlim=c(83,85)) +
 labs(x="DCS Target + Diff", y="RDO - Finished Lab Certification", 
 title="No Correlation - Target vs. Certified", subtitle="B Blender") + 
 theme_light() +
 geom_smooth(method="lm") +
 stat_regline_equation(label.y=84.75, label.x=83.5)

sub |> filter(Result<85 & Result>83 & Crew!="O") |> ggplot(aes(x=Crew, y=GWY)) +
 geom_boxplot(outlier.shape=NA) +
 coord_cartesian(ylim=c(0,1)) 

sub |> filter(year(Date)==2022) |> 
 ggplot(aes(x=my, y=GWY, group=Crew)) + 
 geom_point(aes(fill=Crew)) +
 stat_summary(fun="mean", geom="line", aes(color=Crew)) + 
 facet_wrap(~Crew)

plot(Result ~ AIF, data=subA)

### Models ###

sub <- sub |> mutate(Crew = relevel(Crew, ref = "J"))
sub <- subset(sub, Crew!="O") #Remove crew O, only 2 blends recorded
l <- lm(Result ~ Crew, data=sub)
summary(l)

AIM <- sub |> filter(HDR=="B", AIF==0) 
AIMm <- lm(Result ~ AIRDO + DIFF, data=AIM) 

plot(Result ~ ai_plus_diff, data=subB)
lm <- lm(Result ~ ai_plus_diff, data=subB)
abline(lm)