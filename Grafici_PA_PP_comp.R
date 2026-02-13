library(readxl)
Misure_di_sostegno_passive_e_attive_comparazioni_totale <- read_excel("E:/Rapporto_INAPP2022/Misure di sostegno passive e attive comparazioni totale.xlsx")
View(Misure_di_sostegno_passive_e_attive_comparazioni_totale)
df<-Misure_di_sostegno_passive_e_attive_comparazioni_totale
rm(Misure_di_sostegno_passive_e_attive_comparazioni_totale)
names(df)
df$cy<-paste0(df$LFS_COUNTRY,df$Time)
head(df)
tail(df)
library(ggplo2)
library(dplyr)
attach(df)
df<-data.frame(df);head(df)
df<-df[,c(1,3:5,10,11,15)];head(df)
#df=subset(df,select=-TIME);names(df)
#df=subset(df,select=-c(Flag.Codes,Flags));names(df)
head(df)
#df<-subset(df,select=-c(LFS_COUNTRY,FREQUENCY:Frequency));names(df)
df2<-subset(df,PROG>=110 & MEAS=="EXPPCT");dim(df2);head(df2);tail(df2)
df3<-subset(df2,PROG==110);dim(df3)
df4<-subset(df2,PROG==120);dim(df4)
head(df3)
head(df4)
df3=subset(df3,select=-c(Programmes));names(df3)
df4=subset(df4,select=-c(Programmes));names(df4)
#df5<-subset(df,PROG>=110 & MEAS=="EXP");dim(df2);head(df5);tail(df5)

#df3


#table(MEAS)
colnames(df3)[colnames(df3)=="PROG"]<-"Poliche attive";names(df3)
colnames(df4)[colnames(df4)=="PROG"]<-"Poliche passive";names(df4)
colnames(df3)[colnames(df3)=="Measure"]<-"Valore attive";names(df3)
colnames(df4)[colnames(df4)=="Measure"]<-"Valore passive";names
colnames(df3)[colnames(df3)=="Valore attive"]<-"Misura attive";names(df3)
colnames(df4)[colnames(df4)=="Valore passive"]<-"Misura passive";names(df4)
colnames(df3)[colnames(df3)=="Value"]<-"Valore attive";names(df3)
colnames(df4)[colnames(df4)=="Value"]<-"Valore passive";names(df4)
df6<-merge(df3,df4)
head(df6)
colnames(df6)
#table(df6$`Misura passive`)
#table(df6$`Misura attive`)
#df7=df6[df5$`Misura passive`=="Public expenditure as a percentage of GDP" |df6$`Misura attive`=="Public expenditure as a percentage of GDP", ]
#df7=subset(df7,select=-c("Politiche attive","Politiche passive"));names(df7)
#df7<-df6[,c(1,3,4,7,10)];head(df7)
#colnames(df7)<-c("Paese","Anno","Paese e Anno","Politiche attive su PIL","Politiche passive su PIL")
#install.packages("babynames")
#install.packages("ggrepel")
library(babynames)
library(ggrepel)
library(tidyr)
#install.packages("rccdates")
#library(rccdates)
#df7$Anno=as.year(Df7$Anno)
# data
# Select a few date to label the chart
#tmp_date <- data %>% sample_frac(0.3)



#ggplot(tib, aes(area, pop, label = lab_cont))+
  #geom_line(aes(group = cont), colour = "red")+
  #geom_point(fill = "red", colour = "red", shape = 22, size = 4)+
  #geom_text_repel(aes(label = year))+
  #geom_text_repel(nudge_x = 1, size = 5)
#rm(tib,pop,cont)
#rm(op)

#names(df8)
#data <- df7
#df8<-subset(df7,Paese=="Italy")
#data<-df8
#data<-as_tibble(data)
df9<-subset(df6,LFS_COUNTRY=="ITA"|LFS_COUNTRY=="FRA"|LFS_COUNTRY=="DEU"|LFS_COUNTRY=="ESP"|LFS_COUNTRY=="GBR" | LFS_COUNTRY=="POL"| LFS_COUNTRY=="OTO")
table(df9$LFS_COUNTRY)
df9
#mydata %>%
  #ggplot(aes(x=bsurf, y=pop, group=city, color=city, dlabel=year)) + geom_point(,size = 2)+
  #geom_line(size=1)+ geom_text(label=mydata$year, vjust = 1.2, nudge_y = 0.5)+
  #ggtitle("Dummy Title", subtitle = "Dummy") + xlab("Population") + ylab("Total Built-up Surface")


data<-df9
colnames(data)<-c("Paese","Misura","Anno","Paese e Anno","PA","PA_PIL","PP","PP_PIL")

#data$Paese<- recode(data$Paese, France = 'Francia', 
                        #Germany = 'Germania',
                        #Italy = 'Italia',
                    #Spain = 'Spagna',
                    #"United Kingdom" = 'Regno Unito')

#arrowsPlot <- function(x, y, lwd = .7, col = z, angle = 20, length = 0.2) {
  #invisible(sapply(1:length(x),
                   #function(i) arrows(x[i], y[i], x[i + 1], y[i + 1], lwd = lwd,
                                      #col = col, angle = angle, length = length)))
}


table(data$Paese)
dim(data)
colnames(data)
data

detach(df)

library(dplyr)


data$Paese<-as.factor(data$Paese)
levels(data$Paese)
data$Paese<-recode(data$Paese, DEU = 'GER', 
                   ESP = 'ESP',
                   FRA = 'FRA',GBR='GBR',ITA='ITA',OTO='OCSE',POL='POL')
attach(data)
#data%>%
colors<-c("blue","black","green","red","brown")
library(dplyr)
###########GIUSTO######
ggplot(data,aes(x=PA_PIL,y=PP_PIL,col=Paese,dlabel=Anno))+
  geom_point()+
  geom_text(aes(label=Anno),vjust=1.2,size=4)+
  geom_path(arrow = arrow(ends = "last", type = "closed", length = unit(0.3,"cm")))+
  labs(x = "Politiche attive (% su PIL)", y = "Politiche passive (% su PIL)")
