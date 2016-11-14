#R Code - Survey Data Analysis
#Joel Prince Varghese

#Libraries
library("memisc")
library("dplyr")
library("ggplot2")
library("reshape2")

#Reading the files
Oct8 = as.data.set(spss.system.file(file.choose()))
Oct8=as.data.frame(Oct8)
Oct12 = as.data.set(spss.system.file(file.choose()))
Oct12=as.data.frame(Oct12)

#Q2a - Party Variable
Oct8[,"Party"]=NA
Oct8[grep("Democrat",Oct8$q05a),"Party"]="Democrat"
Oct8[grep("Republican",Oct8$q05a),"Party"]="Republican"
Oct8[grep("independent",Oct8$q05a),"Party"]="DTS/Independent"
Oct8[grep("another",Oct8$q05a),"Party"]="Another Party Registration"
Oct8[grep("not registered|don't know",Oct8$q05a),"Party"]="Not Registered"
Oct8$Party=as.factor(Oct8$Party)

Oct12[,"Party"]=NA
Oct12[grep("Democrat",Oct12$q7a),"Party"]="Democrat"
Oct12[grep("Republican",Oct12$q7a),"Party"]="Republican"
Oct12[grep("independent",Oct12$q7a),"Party"]="DTS/Independent"
Oct12[grep("another",Oct12$q7a),"Party"]="Another Party Registration"
Oct12[grep("not registered|don't know",Oct12$q7a),"Party"]="Not Registered"
Oct12$Party=as.factor(Oct12$Party)

#Subsetting Registered Voters
Oct8=subset(Oct8,Oct8$Party!="Not Registered")
Oct12=subset(Oct12,Oct12$Party!="Not Registered")

#Q2b - Preference Variable
Oct8[,"Preference"]=NA
Oct8[grep("Obama",Oct8$q07),"Preference"]="Democratic ticket"
Oct8[grep("McCain",Oct8$q07),"Preference"]="Republican ticket"
Oct8[grep("someone else",Oct8$q07),"Preference"]="Another ticket"
Oct8$Preference=as.factor(Oct8$Preference)

Oct12[,"Preference"]=NA
Oct12[grep("Obama",Oct12$q9),"Preference"]="Democratic ticket"
Oct12[grep("Romney",Oct12$q9),"Preference"]="Republican ticket"
Oct12[grep("someone else",Oct12$q9),"Preference"]="Another ticket"
Oct12$Preference=as.factor(Oct12$Preference)

#Q2c - Race/Ethnicity (Asian, Black or African American, Hispanic or Latino, White, Other or Multi-Race)
Oct8[,"Ethnicity"]=NA
Oct8[grep("Asian",Oct8$d8),"Ethnicity"]="Asian"
Oct8[grep("Black",Oct8$d8),"Ethnicity"]="Black"
Oct8[grep("Latino",Oct8$d8),"Ethnicity"]="Hispanic or Latino"
Oct8[grep("White",Oct8$d8),"Ethnicity"]="White"
Oct8[grep("Other",Oct8$d8),"Ethnicity"]="Other or Multi-Race"
Oct8[grep("refuse",Oct8$d8),"Ethnicity"]="Refuse"
Oct8$Ethnicity=as.factor(Oct8$Ethnicity)

Oct12[,"Ethnicity"]=NA
Oct12[grep("Asian",Oct12$d8com),"Ethnicity"]="Asian"
Oct12[grep("black",Oct12$d8com),"Ethnicity"]="Black"
Oct12[grep("Latino",Oct12$d8com),"Ethnicity"]="Hispanic or Latino"
Oct12[grep("white",Oct12$d8com),"Ethnicity"]="White"
Oct12[grep("other|multi",Oct12$d8com),"Ethnicity"]="Other or Multi-Race"
Oct12[grep("refuse",Oct12$d8com),"Ethnicity"]="Refuse"
Oct12$Ethnicity=as.factor(Oct12$Ethnicity)

#Q2d
Oct8[,"Year"]=2008
Oct12[,"Year"]=2012
Oct8$Year=as.factor(Oct8$Year)
Oct12$Year=as.factor(Oct12$Year)

Oct8[,"Age"]=Oct8$d1a
Oct12[,"Age"]=Oct12$d1a

Oct8[,"Employment"]=Oct8$d5
Oct12[,"Employment"]=Oct12$d5
Oct8$Employment=factor(Oct8$Employment,levels=c(levels(Oct8$Employment),levels(Oct8$d5a)))
Oct8[grep("not-employed",Oct8$d5),"Employment"]=Oct8[grep("not-employed",Oct8$d5),"d5a"]
Oct8$Employment=as.factor(as.character(Oct8$Employment))
Oct12$Employment=factor(Oct12$Employment,levels=c(levels(Oct12$Employment),levels(Oct12$d5a)))
Oct12[grep("not-employed",Oct12$d5),"Employment"]=Oct12[grep("not-employed",Oct12$d5),"d5a"]
Oct12$Employment=as.factor(as.character(Oct12$Employment))

Oct8[,"Education"]=Oct8$d7
Oct12[,"Education"]=Oct12$d6
Oct8[,"Income"]=Oct8$d11
Oct12[,"Income"]=Oct12$d11
colnames(Oct8)[colnames(Oct8)=="gender"] = "Gender"
colnames(Oct12)[colnames(Oct12)=="gender"] = "Gender"

#Subsetting Required Columns
Oct8s=subset(Oct8,select=c("Party","Preference","Ethnicity","Year","Age","Employment","Income","Gender"))
Oct12s=subset(Oct12,select=c("Party","Preference","Ethnicity","Year","Age","Employment","Income","Gender"))

#Combining Dataframes
dataset=rbind(Oct8s,Oct12s)
head(dataset)

#Q3a
#Themes for plots
theme = theme(legend.text=element_text(size=20),legend.title=element_text(size=20),axis.text.x=element_text(size=20),axis.text.y=element_text(size=20))
theme1 = theme(legend.text=element_text(size=20),legend.title=element_text(size=20),axis.text.x=element_text(size=15),axis.text.y=element_text(size=20))

#Importanct of Age
dataset %>% group_by(Preference) %>% select(Age) %>% table()
dataset %>% group_by(Preference,Age) %>% summarise(count=n()) %>% ggplot(aes(x=Age,y=count)) + geom_bar(aes(fill=Preference),stat="identity") +  scale_fill_manual(values = c("green"," blue","red")) + theme

#Importance of Gender
dataset %>% group_by(Preference) %>% select(Gender) %>% table()
dataset %>% group_by(Preference,Gender) %>% summarise(count=n()) %>% ggplot(aes(x=Gender,y=count)) + geom_bar(aes(fill=Preference),stat="identity") +  scale_fill_manual(values = c("green"," blue","red")) + theme

#Importance of Ethnicity
dataset %>% group_by(Preference) %>% select(Ethnicity) %>% table()
dataset %>% group_by(Preference,Ethnicity) %>% summarise(count=n()) %>% ggplot(aes(x=Ethnicity,y=count)) + geom_bar(aes(fill=Preference),stat="identity") +  scale_fill_manual(values = c("green"," blue","red")) + theme1 

#Importance of Employment
dataset %>% group_by(Preference) %>% select(Employment) %>% table()
dataset %>% group_by(Preference,Employment) %>% summarise(count=n()) %>% ggplot(aes(x=Employment,y=count)) + geom_bar(aes(fill=Preference),stat="identity") +  scale_fill_manual(values = c("green"," blue","red")) + coord_flip() + theme1

#Importance of Income
dataset %>% group_by(Preference) %>% select(Income) %>% table()
dataset %>% group_by(Preference,Income) %>% summarise(count=n()) %>% ggplot(aes(x=Income,y=count)) + geom_bar(aes(fill=Preference),stat="identity") +  scale_fill_manual(values = c("green"," blue","red")) + coord_flip() + theme1


#Q3b
dataset %>% group_by(Year) %>% select(Preference) %>% table()
dataset %>% group_by(Year,Preference) %>% summarise(count=n()) %>% ggplot(aes(x=Year,y=count)) + geom_bar(aes(fill=Preference),stat="identity") +  scale_fill_manual(values = c("green"," blue","red")) + theme

#Q3c
#Change in Preference by Age
dataset %>% group_by(Year,Age) %>% select(Preference) %>% table()
dataset$Age.Year = paste(dataset$Age,"-",dataset$Year)
plot=melt(dataset[,c("Age.Year","Preference")],id.vars = 1,value.name = "Preference")
plot$Age.Year=as.factor(plot$Age.Year)
plot$value=as.factor(plot$Preference)
plot %>% group_by(Age.Year,Preference) %>% summarise(count=n()) %>% ggplot(aes(x=Age.Year,y=count)) + geom_bar(aes(fill=Preference),stat="identity")+  scale_fill_manual(values = c("green"," blue","red")) + coord_flip() + theme1

#Change in Preference by Gender
dataset %>% group_by(Year,Gender) %>% select(Preference) %>% table()
dataset$Gender.Year = paste(dataset$Gender,"-",dataset$Year)
plot=melt(dataset[,c("Gender.Year","Preference")],id.vars = 1,value.name = "Preference")
plot$Gender.Year=as.factor(plot$Gender.Year)
plot$value=as.factor(plot$Preference)
plot %>% group_by(Gender.Year,Preference) %>% summarise(count=n()) %>% ggplot(aes(x=Gender.Year,y=count)) + geom_bar(aes(fill=Preference),stat="identity")+  scale_fill_manual(values = c("green"," blue","red")) + coord_flip() + theme1

#Change in Preference by Ethnicity
dataset %>% group_by(Year,Ethnicity) %>% select(Preference) %>% table()
dataset$Ethnicity.Year = paste(dataset$Ethnicity,"-",dataset$Year)
plot=melt(dataset[,c("Ethnicity.Year","Preference")],id.vars = 1,value.name = "Preference")
plot$Ethnicity.Year=as.factor(plot$Ethnicity.Year)
plot$value=as.factor(plot$Preference)
plot %>% group_by(Ethnicity.Year,Preference) %>% summarise(count=n()) %>% ggplot(aes(x=Ethnicity.Year,y=count)) + geom_bar(aes(fill=Preference),stat="identity")+  scale_fill_manual(values = c("green"," blue","red")) + coord_flip() + theme1

#Change in Preference by Employment
dataset %>% group_by(Year,Employment) %>% select(Preference) %>% table()
dataset$Employment.Year = paste(dataset$Employment,"-",dataset$Year)
plot=melt(dataset[,c("Employment.Year","Preference")],id.vars = 1,value.name = "Preference")
plot$Employment.Year=as.factor(plot$Employment.Year)
plot$value=as.factor(plot$Preference)
plot %>% group_by(Employment.Year,Preference) %>% summarise(count=n()) %>% ggplot(aes(x=Employment.Year,y=count)) + geom_bar(aes(fill=Preference),stat="identity")+  scale_fill_manual(values = c("green"," blue","red")) + coord_flip() + theme1

#Change in Preference by Income
dataset %>% group_by(Year,Income) %>% select(Preference) %>% table()
dataset$Income.Year = paste(dataset$Income,"-",dataset$Year)
plot=melt(dataset[,c("Income.Year","Preference")],id.vars = 1,value.name = "Preference")
plot$Income.Year=as.factor(plot$Income.Year)
plot$value=as.factor(plot$Preference)
plot %>% group_by(Income.Year,Preference) %>% summarise(count=n()) %>% ggplot(aes(x=Income.Year,y=count)) + geom_bar(aes(fill=Preference),stat="identity")+  scale_fill_manual(values = c("green"," blue","red")) + coord_flip() + theme1


