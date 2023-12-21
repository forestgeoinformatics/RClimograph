library(xlsx)
library(ggplot2)
library(plyr)
library(dplyr)

data <- read.xlsx("datafile.xls",sheetIndex = 1)

unique(data$Station.Name)

unique(as.character(data$Station.Name[data$No..of.Years==100]))

colnames(data)

data$Month <- factor(as.character(data$Month),c("January","February","March","April","May","June","July","August","September","October","November","December"))

#filter(Station.Name %in% c("Pune","Abu","Agra","Ajmer")) %>%

dev.new(x=1920,y=1080)
data %>% filter(No..of.Years==100)%>% filter(Station.Name=="Pune")  %>%ggplot(aes(x=Month,group=1))+
  geom_bar(aes(y=Mean.Rainfall.in.mm/10,colour="Rainfall"),fill="darkgreen",position = position_dodge(),stat="identity",alpha=0.5)+
  geom_line(aes(y=Mean.Temperature.in.degree.C...Maximum,col="Temp. Max."),size=1)+
  geom_line(aes(y=Mean.Temperature..in.degree.C...Minimum,col="Temp. Min."),size=1)+
  geom_point(aes(y=Mean.Temperature.in.degree.C...Maximum),alpha=0.5)+
  geom_point(aes(y=Mean.Temperature..in.degree.C...Minimum),alpha=0.5)+
  theme_bw()+
  facet_wrap(.~Station.Name,scales = "free_y")+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,colour="BLACK"))+
  scale_y_continuous(sec.axis = sec_axis(~.*10,name="Rainfall (in mm)"))+
  scale_color_manual(values = c("BLACK","RED","BLUE"))+
  labs(x="Month",y="Temperature (Degree celsius)",title="Climograph - 100 Years Average 1901 to 2000",colour="Parameter")
  
dev.new(x=1920,y=1080)
data %>% filter(No..of.Years==100)%>%  ggplot(aes(x=Month,group=1))+
  geom_bar(aes(y=Mean.Rainfall.in.mm/10,colour="Rainfall"),fill="darkgreen",position = position_dodge(),stat="identity",alpha=0.5)+
  geom_line(aes(y=Mean.Temperature.in.degree.C...Maximum,col="Temp. Max."),size=1)+
  geom_line(aes(y=Mean.Temperature..in.degree.C...Minimum,col="Temp. Min."),size=1)+
  geom_point(aes(y=Mean.Temperature.in.degree.C...Maximum),alpha=0.5)+
  geom_point(aes(y=Mean.Temperature..in.degree.C...Minimum),alpha=0.5)+
  theme_bw()+
  facet_wrap(.~Station.Name)+
  theme(axis.text.x = element_text(angle=45,vjust=0.5,colour="BLACK"))+
  scale_y_continuous(sec.axis = sec_axis(~.*10,name="Rainfall (in mm)"))+
  scale_color_manual(values = c("BLACK","RED","BLUE"))+
  labs(x="Month",y="Temperature (Degree celsius)",title="Climograph - 100 Years Average 1901 to 2000",colour="Parameter")


data100 <- data %>% filter(No..of.Years == 100)

240/12

data100
cnames <- colnames(data100)
cnames[1] <- "SN"
cnames[4] <- "Years"
cnames[5] <- "TMax"
cnames[6] <- "TMin"
cnames[7] <- "Rainfall"
colnames(data100) <- cnames

# TMean = (TMax + TMin)/2

data100$TMean <- (data100$TMax + data100$TMin)/2

# D Winter
# J Winter
# F Winter

# M PreMonsoon/Summer
# A PreMonsoon/Summer
# M PreMonsoon/Summer

# J Monsoon
# J Monsoon
# A Monsoon
# S Monsoon

# O PostMonsoon
# N PostMonsoon
data100$Season <- NA
data100$Season[data100$Month %in% c("June","July","August","September")] <- "Monsoon"
data100$Season[data100$Month %in% c("October","November")] <- "PostMonsoon"
data100$Season[data100$Month %in% c("December","January","February")] <- "Winter"
data100$Season[data100$Month %in% c("March","April","May")] <- "Summer"

unique(data100$Month)

city20 <- ddply(data100, .variables = c("SN","Season"),function(x){
  Rainfall <- sum(x$Rainfall, na.rm = TRUE)
  data.frame(Rainfall)
})

city20 %>% ggplot(aes(x=Season))+
  geom_bar(aes(y=Rainfall),stat="identity")+
  facet_wrap(.~SN)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,vjust = 0.5,colour="BLACK",size=12))



library(reshape2)

t <- dcast(city20, formula = SN~Season)

t$Total <- t$Monsoon+t$PostMonsoon+t$Summer+t$Winter

t$SN[t$Total == max(t$Total, na.rm = TRUE)]
t$SN[t$Total == min(t$Total, na.rm = TRUE)]

t$SN[t$Monsoon == max(t$Monsoon, na.rm = TRUE)]
t$SN[t$Monsoon == min(t$Monsoon, na.rm = TRUE)]


library(xlsx)
write.xlsx(t, "Climograph.xlsx",
           sheetName = "Rainfall1",
           append = TRUE,
           row.names = FALSE)

write.xlsx(data100, "Climograph.xlsx",sheetName = "Data",append = TRUE,row.names = FALSE)


temp <- ddply(data100, .variables = c("SN"), function(x){
  TMin <- min(x$TMean, na.rm = TRUE)
  TMax <- max(x$TMean, na.rm = TRUE)
  
  TMinMonth <- x$Month[x$TMean==TMin]
  TMaxMonth <- x$Month[x$TMean==TMax]
  
  data.frame(TMin,TMinMonth,TMax,TMaxMonth)
})

plot(table(temp$TMinMonth))
plot(table(temp$TMaxMonth))

temp$SN[temp$TMin==min(temp$TMin, na.rm = T)]
temp$SN[temp$TMax==max(temp$TMax, na.rm = T)]


write.xlsx(temp, "Climograph.xlsx",sheetName = "Temperature",append = TRUE,row.names = FALSE)

data100$Range <- data100$TMax - data100$TMin

rangeAvg <- ddply(data100, .variables = "SN", function(x){
  ravg <- mean(x$Range, na.rm = TRUE)
  rsd <- sd(x$Range, na.rm = TRUE)
  
  data.frame(ravg, rsd)
})

