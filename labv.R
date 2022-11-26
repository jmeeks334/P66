library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

#2022 Data
labv <- read.csv("LabVantage.csv")
fin <- subset(labv, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & grepl("D4814", Test.Method.ID, fixed=TRUE)==TRUE)
fin$Sample.Collection.Date <- ymd_hms(fin$Sample.Collection.Date)
names(fin)[11] <- "Date"
names(fin)[7] <- "Result"
names(fin)[2] <- "ID"
fin <- subset(fin, select=c("Date","ID","Result"))
shift <- read.csv("OMShift.csv")
shift$Dt <- mdy(shift$Dt)
tnk <- read.csv("BlendTnk.csv")
names(tnk)[1] <- "DateTime"
tnk$DateTime <- mdy_hm(tnk$DateTime)
tnk$Date <- as_date(tnk$DateTime)
tnk$shift <- if_else(hour(tnk$DateTime)==4,"D","N")
fin$ID <- str_extract(fin$ID, "T-...")
tnk$A <- str_replace(tnk$A, "_", "-")
tnk$B <- str_replace(tnk$B, "_", "-")
tnkA <- subset(tnk, select=c(1,2))
tnkB <- subset(tnk, select=c(1,3))
names(fin)[2] <- "Tank"
names(tnkA)[2] <- "Tank"
names(tnkB)[2] <- "Tank"
m1 <- merge(fin, tnkA, by="Tank")
m2 <- merge(fin, tnkB, by="Tank")
m <- union(m1,m2)
names(m)[2] <- "finDt"
m$diff <- m$finDt - m$DateTime
m$diff <- as.duration(m$diff)
msub <- subset(m, abs(diff)<172800)
msub$shift <- if_else(hour(msub$DateTime)==4, "D", "N")
msub$shift <- factor(msub$shift)
shiftD <- subset(shift, select=c(1,2))
shiftN <- subset(shift, select=c(1,3))
msub$Dt <- as_date(msub$DateTime)
msubN <- subset(msub, shift=="N")
msubD <- subset(msub, shift=="D")
m1<-merge(msubN, shiftN, by="Dt")
m2<-merge(msubD, shiftD, by="Dt")
names(m1)[8]<-"Crew"
names(m2)[8]<-"Crew"
m<-union(m1,m2)
subG <- subset(m, Result<90)


#2021 Data
labv21 <- read.csv("LabVantage21.csv")
fin21 <- subset(labv21, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & grepl("D4814", Test.Method.ID, fixed=TRUE)==TRUE)
fin21$Sample.Collection.Date <- mdy_hm(fin21$Sample.Collection.Date)
names(fin21)[11] <- "Date"
names(fin21)[7] <- "Result"
names(fin21)[2] <- "ID"
fin21 <- subset(fin21, select=c("Date","ID","Result"))
shift21 <- read.csv("OMShift21.csv")
names(shift21)[1] <- "Dt"
shift21$Dt <- mdy(shift21$Dt)
tnk21 <- read.csv("BlendTnk21.csv")
names(tnk21)[1] <- "DateTime"
tnk21$DateTime <- mdy_hm(tnk21$DateTime)
tnk21$Date <- as_date(tnk21$DateTime)
tnk21$shift <- if_else(hour(tnk21$DateTime)==4,"D","N")
fin21$ID <- str_extract(fin21$ID, "T-...")
tnk21$A <- str_replace(tnk21$A, "_", "-")
tnk21$B <- str_replace(tnk21$B, "_", "-")
tnkA21 <- subset(tnk21, select=c(1,2))
tnkB21 <- subset(tnk21, select=c(1,3))
names(fin21)[2] <- "Tank"
names(tnkA21)[2] <- "Tank"
names(tnkB21)[2] <- "Tank"
m121 <- merge(fin21, tnkA21, by="Tank")
m221 <- merge(fin21, tnkB21, by="Tank")
m21 <- union(m121,m221)
names(m21)[2] <- "finDt"
m21$diff <- m21$finDt - m21$DateTime
m21$diff <- as.duration(m21$diff)
msub21 <- subset(m21, abs(diff)<172800)
msub21$shift <- if_else(hour(msub21$DateTime)==4, "D", "N")
msub21$shift <- factor(msub21$shift)
shiftD21 <- subset(shift21, select=c(1,2))
shiftN21 <- subset(shift21, select=c(1,3))
msub21$Dt <- as_date(msub21$DateTime)
msubN21 <- subset(msub21, shift=="N")
msubD21 <- subset(msub21, shift=="D")
m121 <- merge(msubN21, shiftN21, by="Dt")
m221 <- merge(msubD21, shiftD21, by="Dt")
names(m121)[8] <- "Crew"
names(m221)[8] <- "Crew"
m21 <- union(m121,m221)
subG21 <- subset(m21, Result<90)

#Combine 2021 + 2022
subG2yr <- union(subG, subG21)
subG2yr <- subset(subG2yr, select=-c(1,5,6))
subG2yr$gwy <- if_else(83.7-subG2yr$Result<0,subG2yr$Result-83.7,0)
subG2yr$gwyD <- if_else(83.7-subG2yr$Result>0,1,0)

subG2yr <- subset(subG2yr, Crew!= "S") #Remove crew 'S' due to low number 
subG2022 <- subset(subG2yr, year(finDt)==2022)
subG2021 <- subset(subG2yr, year(finDt)==2021)
subG2yr$Crew <- factor(subG2yr$Crew)
subG2yr <- subG2yr %>% mutate(Crew = relevel(Crew, ref = "J")) #Make 'J' control group (sets performance benchmark)
subG2022$Crew <- factor(subG2022$Crew)
subG2022 <- subG2022 %>% mutate(Crew = relevel(Crew, ref = "J")) #Make 'J' control group (sets performance benchmark)
mm <- lm(gwy ~ Crew, data=subG2022)
summary(mm)
octMdl <- lm(gwy ~ Crew, data=subG2yr)




###   RVP   ###

#2022 RVP
RVP <- subset(labv, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & 
				grepl("D5191", Test.Method.ID, fixed=TRUE)==TRUE &
				grepl("Product", Sample.Process.Unit, fixed=TRUE)==TRUE)
names(RVP)[11] <- "Date"
names(RVP)[7] <- "Result"
names(RVP)[2] <- "ID"
names(RVP)[3] <- "Desc"
RVP <- subset(RVP, select=c("Date", "Result", "ID", "Desc"))
RVP$Desc <- str_replace(RVP$Desc, "#", " RVP")
RVP$Ext <- str_extract(RVP$Desc, "\\s[([0-9]].*?RVP")
RVP$Ext <- str_replace(RVP$Ext, "\\(", "")
RVP$Ext <- str_replace(RVP$Ext, "RVP", "")
RVP$Ext <- str_replace(RVP$Ext, " ", "")
RVP$Ext <- str_replace(RVP$Ext, " ", "")
RVP$ID <- str_extract(RVP$ID, "T-...")
RVP <- subset(RVP, select=c("Date","Result","Ext", "ID"))
names(RVP)[3] <- "Tgt"
names(RVP)[4] <- "Tank"
RVP$Tgt <- as.numeric(RVP$Tgt)
Rm1 <- merge(RVP, tnkA, by="Tank")
Rm2 <- merge(RVP, tnkB, by="Tank")
Rm <- union(Rm1,Rm2)
names(Rm)[2] <- "finDt"
Rm$finDt <- ymd_hms(Rm$finDt)
Rm$diff <- Rm$finDt - Rm$DateTime
Rm$diff <- as.duration(Rm$diff)
Rmsub <- subset(Rm, abs(diff)<172800)
Rmsub$shift <- if_else(hour(Rmsub$DateTime)==4, "D", "N")
Rmsub$shift <- factor(Rmsub$shift)
Rmsub$Dt <- as_date(Rmsub$DateTime)
RmsubN <- subset(Rmsub, shift=="N")
RmsubD <- subset(Rmsub, shift=="D")
Rm1<-merge(RmsubN, shiftN, by="Dt")
Rm2<-merge(RmsubD, shiftD, by="Dt")
names(Rm1)[9]<-"Crew"
names(Rm2)[9]<-"Crew"
Rm<-union(Rm1,Rm2)


#2021 RVP
RVP21 <- subset(labv21, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & 
				grepl("D5191", Test.Method.ID, fixed=TRUE)==TRUE &
				grepl("Product", Sample.Process.Unit, fixed=TRUE)==TRUE)
names(RVP21)[11] <- "Date"
names(RVP21)[7] <- "Result"
names(RVP21)[2] <- "ID"
names(RVP21)[3] <- "Desc"
RVP21 <- subset(RVP21, select=c("Date", "Result", "ID", "Desc"))
RVP21$Desc <- str_replace(RVP21$Desc, "#", " RVP")
RVP21$Ext <- str_extract(RVP21$Desc, "\\s[([0-9]].*?RVP")
RVP21$Ext <- str_replace(RVP21$Ext, "\\(", "")
RVP21$Ext <- str_replace(RVP21$Ext, "RVP", "")
RVP21$Ext <- str_replace(RVP21$Ext, " ", "")
RVP21$Ext <- str_replace(RVP21$Ext, " ", "")
RVP21$ID <- str_extract(RVP21$ID, "T-...")
RVP21 <- subset(RVP21, select=c("Date","Result","Ext", "ID"))
names(RVP21)[3] <- "Tgt"
names(RVP21)[4] <- "Tank"
RVP21$Tgt <- as.numeric(RVP21$Tgt)
Rm121 <- merge(RVP21, tnkA21, by="Tank")
Rm221 <- merge(RVP21, tnkB21, by="Tank")
Rm21 <- union(Rm121,Rm221)
names(Rm21)[2] <- "finDt"
Rm21$finDt <- ymd_hms(Rm21$finDt)
Rm21$diff <- Rm21$finDt - Rm21$DateTime
Rm21$diff <- as.duration(Rm21$diff)
Rmsub21 <- subset(Rm21, abs(diff)<172800)
Rmsub21$shift <- if_else(hour(Rmsub21$DateTime)==4, "D", "N")
Rmsub21$shift <- factor(Rmsub21$shift)
Rmsub21$Dt <- as_date(Rmsub21$DateTime)
RmsubN21 <- subset(Rmsub21, shift=="N")
RmsubD21 <- subset(Rmsub21, shift=="D")
Rm121<-merge(RmsubN21, shiftN21, by="Dt")
Rm221<-merge(RmsubD21, shiftD21, by="Dt")
names(Rm121)[9]<-"Crew"
names(Rm221)[9]<-"Crew"
Rm21<-union(Rm121,Rm221)

#2021 + 2022 RVP
Rm2yr <- union(Rm, Rm21)
#Rm2yr$Crew <- factor(Rm2yr$Crew)
Rm92yr <- subset(Rm2yr, Tgt==9)
Rm92yr$gwy <- if_else(9 - Rm92yr$Result > 0.15, 8.85 - Rm92yr$Result, 0)
Rm92yr$gwyD <- if_else(Rm92yr$gwy > 0, 1, 0)
Rm92yr <- subset(Rm92yr, Crew!= "S")
Rm92yr$Crew <- factor(Rm92yr$Crew)
Rm92yr <- Rm92yr %>% mutate(Crew = relevel(Crew, ref = "C"))
rvpMdl <- lm(gwy ~ Crew, data=Rm92yr)
summary(rvpMdl)
plot(gwy ~ Crew, data=Rm92yr)
tib <- subG2yr %>% group_by(Crew) %>% summarize(mean = mean(gwy), n=n(), sm=sum(gwy))

###   Analyzer Offsets   ###

#Octane
labv <- read.csv("LabVantage.csv")
labv21 <- read.csv("LabVantage21.csv")
labv$Sample.Collection.Date <- ymd_hms(labv$Sample.Collection.Date)
labv21$Sample.Collection.Date <- mdy_hm(labv21$Sample.Collection.Date)
labvmerge <- union(labv, labv21)

diff <- read.csv("Diff.csv")
diff$A_DEST.TK <- str_replace(diff$A_DEST.TK, "_","-")
diff$B_DEST.TK <- str_replace(diff$B_DEST.TK, "_","-")
finOct <- subset(labvmerge, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & grepl("D4814", Test.Method.ID, fixed=TRUE)==TRUE)
finOct$Sample.Collection.Date <- ymd_hms(finOct$Sample.Collection.Date)
names(finOct)[11] <- "finDt"
names(finOct)[7] <- "Result"
names(finOct)[2] <- "Tank"
finOct <- subset(finOct, select=c("finDt","Tank","Result"))
finOct$Tank <- str_extract(finOct$Tank, "T-...")
diff$Date <- mdy_hms(diff$Date)
diffOctA <- subset(diff, select=c(1,2,6))
names(diffOctA)[2] <- "ODiff"
names(diffOctA)[3] <- "Tank"
diffOctA$Bld <- rep("A", times=length(diffOctA$Tank))
diffOctB <- subset(diff, select=c(1,4,7))
names(diffOctB)[2] <- "ODiff"
names(diffOctB)[3] <- "Tank"
diffOctB$Bld <- rep("B", times=length(diffOctB$Tank))
diffOct <- union(diffOctA, diffOctB)

sub <- merge(diffOct, finOct, by="Tank")
sub$TDiff <- sub$Date - sub$finDt
sub$TDiff <- as.duration(sub$TDiff)
sub <- subset(sub, abs(sub$TDiff)<172800)
sub <- subset(sub, select=-c(2,7))
sub <- subset(sub, ODiff>-2 & ODiff<2 & Result<90)
sub$gwy <- if_else(sub$Result>83.7,sub$Result-83.7,0)
sub22 <- subset(sub, year(finDt)==2022)
sub21 <- subset(sub, year(finDt)==2021)

#Linear model - analyzer offsets only
dm <- lm(Result ~ ODiff, data=sub)
dm2 <- lm(gwy ~ ODiff, data=sub)

summary(dm)
plot(Result ~ ODiff, data=sub)
abline(dm)

sub22$Bld <- factor(sub22$Bld)
sub22 <- sub22 %>% mutate(Bld = relevel(Bld, ref = "A"))
bldmdl <- lm(gwy ~ factor(Bld), data=sub22)
summary(bldmdl)
plot_coefs(bldmdl, omit.coefs=FALSE)


#RVP
finRVP <- subset(labvmerge, grepl("All", Sample.Sample.Point, fixed=TRUE)==TRUE & 
				grepl("D5191", Test.Method.ID, fixed=TRUE)==TRUE &
				grepl("Product", Sample.Process.Unit, fixed=TRUE)==TRUE)
names(finRVP)[11] <- "Date"
names(finRVP)[7] <- "Result"
names(finRVP)[2] <- "ID"
names(finRVP)[3] <- "Desc"
finRVP <- subset(finRVP, select=c("Date", "Result", "ID", "Desc"))
finRVP$Desc <- str_replace(finRVP$Desc, "#", " RVP")
finRVP$Ext <- str_extract(finRVP$Desc, "\\s[([0-9]].*?RVP")
finRVP$Ext <- str_replace(finRVP$Ext, "\\(", "")
finRVP$Ext <- str_replace(finRVP$Ext, "RVP", "")
finRVP$Ext <- str_replace(finRVP$Ext, " ", "")
finRVP$Ext <- str_replace(finRVP$Ext, " ", "")
finRVP$ID <- str_extract(finRVP$ID, "T-...")
finRVP <- subset(finRVP, select=c("Date","Result","Ext", "ID"))
names(finRVP)[3] <- "RVPTgt"
names(finRVP)[4] <- "Tank"
finRVP$RVPTgt <- as.numeric(finRVP$RVPTgt)
names(finRVP)[2] <- "Result"
names(finRVP)[1] <- "finDt"
diffRVPA <- subset(diff, select=c(1,3,6))
names(diffRVPA)[2] <- "RDiff"
names(diffRVPA)[3] <- "Tank"
diffRVPA$Bld <- rep("A", times=length(diffRVPA$Tank))
diffRVPB <- subset(diff, select=c(1,5,7))
names(diffRVPB)[2] <- "RDiff"
names(diffRVPB)[3] <- "Tank"
diffRVPB$Bld <- rep("B", times=length(diffRVPB$Tank))
diffRVP <- union(diffRVPA, diffRVPB)
subR <- merge(diffRVP, finRVP, by=c("Tank"))
subR$TDiff <- subR$Date - subR$finDt
subR$TDiff <- as.duration(subR$TDiff)
subR <- subset(subR, abs(subR$TDiff)<172800)
subR <- subset(subR, select=-c(2,8))
subR9 <- subset(subR, RVPTgt==9 & RDiff>-2 & RDiff<2)

Rdm <- lm(Result ~ RDiff, data=subR9)


tib <- sub %>% group_by(Crew) %>% summarize(mean = mean(OgwyD), n=n())


