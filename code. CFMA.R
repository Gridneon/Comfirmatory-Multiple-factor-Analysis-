library(dplyr)
library(car)

library(lavaan)

#call for data
data<-read.csv("mydata1.csv")
View(data)  #view of original data 
names(data)

set.seed(123)

#taking countries needed for the project
fran_1<-subset(data,data$c_abrv=="FR")
germ_1<-subset(data,data$c_abrv=="GE")

#preparing for cleaning
data_to_clean<-rbind(fran_1,germ_1);View(data_to_clean)
names(data_to_clean)
str(data_to_clean)


#needed variable to MGFC
selected_data <- data_to_clean%>%
  select(c_abrv,v133,v134,v135,v136,v137,v138,v139,v140,v141,v98,v99,v100,v101,
         v82,v83,v84,v72,v73,v74,v75,v76,v205,v206,v207,v32,v33,v34,v35,
         v36,v37)
table(selected_data$c_abrv)


#cleaning Data
sample_data<-selected_data %>%
  group_by(c_abrv) %>%
  sample_n(size =1000 );sample_data  # Equal samples (200 from each)

data_clean <- na.omit(sample_data)
table(data_clean$c_abrv)

#prepared data for renaming
finalsample_data<- data_clean%>%
  group_by(c_abrv) %>%
  sample_n(size =500 )
table(finalsample_data$c_abrv)

# renamed variable 
names(finalsample_data)[1:10]<-c("ctry_abr","fair1","fair2",
                                 "fair3","fair4","fair5",
                                 "fair6","fair7","fair8","fair9")

names(finalsample_data)[11:14]<-c("poli_view1","poli_view2",
                                  "poli_view3","poli_view4")

names(finalsample_data)[11:14]<-c("poli_view1","poli_view2",
                                  "poli_view3","poli_view4")

names(finalsample_data)[15:17]<-c("resp_norm1","resp_norm2",
                                  "resp_norm3")

names(finalsample_data)[23:25]<-c("obedien_athy1","obedien_athy2",
                                  "obedien_athy3")

names(finalsample_data)[26:31]<-c("trust1","trust2","trust3",
                                  "trust4","trust5","trust6")
#begin analysis with this data 
data_to_analyse<-finalsample_data[,-c(18:22)]
names(data_to_analyse)
View(data_to_analyse)

str(data_to_analyse)


#changing to factor for Political view 
data_to_analyse$ctry_abr<-factor(data_to_analyse$ctry_abr)
data_to_analyse$poli_view1<-factor(data_to_analyse$poli_view1)
data_to_analyse$poli_view2<-factor(data_to_analyse$poli_view2)
data_to_analyse$poli_view3<-factor(data_to_analyse$poli_view3)
data_to_analyse$poli_view4<-factor(data_to_analyse$poli_view4)


#changing to factor for respect for traditionl norms 

data_to_analyse$resp_norm1<-factor(data_to_analyse$resp_norm1)
data_to_analyse$resp_norm2<-factor(data_to_analyse$resp_norm2)
data_to_analyse$resp_norm3<-factor(data_to_analyse$resp_norm3)

#changing to factor for obeying authorities 
data_to_analyse$obedien_athy1<-factor(data_to_analyse$obedien_athy1)
data_to_analyse$obedien_athy2<-factor(data_to_analyse$obedien_athy2)
data_to_analyse$obedien_athy3<-factor(data_to_analyse$obedien_athy3)


#changing to factor for fairness
data_to_analyse$fair1<-factor(data_to_analyse$fair1 )
data_to_analyse$fair2<-factor(data_to_analyse$fair2 )
data_to_analyse$fair3<-factor(data_to_analyse$fair3 )
data_to_analyse$fair4<-factor(data_to_analyse$fair4 )
data_to_analyse$fair5<-factor(data_to_analyse$fair5 )
data_to_analyse$fair6<-factor(data_to_analyse$fair6 )
data_to_analyse$fair7<-factor(data_to_analyse$fair7 )
data_to_analyse$fair8<-factor(data_to_analyse$fair8 )
data_to_analyse$fair9<-factor(data_to_analyse$fair9 )

str(data_to_analyse)
names(data_to_analyse)

#recode the scale for political view 
data_to_analyse$poli_view1r<-recode(data_to_analyse$poli_view1,
                               "1=3;2=2;3=1;8=0;9=0")    

data_to_analyse$poli_view2r<-recode(data_to_analyse$poli_view2,
                                    "1=3;2=2;3=1;8=0;9=0")
data_to_analyse$poli_view3r<-recode(data_to_analyse$poli_view3,
                                    "1=3;2=2;3=1;8=0;9=0")    

data_to_analyse$poli_view4r<-recode(data_to_analyse$poli_view4,
                                    "1=3;2=2;3=1;8=0;9=0") 

#recode the scale for respecting traditional norm
data_to_analyse$resp_norm1r<-recode(data_to_analyse$resp_norm1,
                                    "1=5;2=4;3=3;4=2;5=1") 

data_to_analyse$resp_norm2r<-recode(data_to_analyse$resp_norm2,
                                    "1=5;2=4;3=3;4=2;5=1")

data_to_analyse$resp_norm3r<-recode(data_to_analyse$resp_norm3,
                                    "1=5;2=4;3=3;4=2;5=1")

#recode the scale for obedience to authority
data_to_analyse$obedien_athy1r<-recode(data_to_analyse$obedien_athy1,
                                    "1=4;2=3;3=2;4=1") 

data_to_analyse$obedien_athy2r<-recode(data_to_analyse$obedien_athy2,
                                    "1=4;2=3;3=2;4=1")

data_to_analyse$obedien_athy3r<-recode(data_to_analyse$obedien_athy3,
                                    "1=4;2=3;3=2;4=1") 


#recode the scale for trust
data_to_analyse$trust1r<-recode(data_to_analyse$trust1,
                                       "1=4;2=3;3=2;4=1") 

data_to_analyse$trust2r<-recode(data_to_analyse$trust2,
                                       "1=4;2=3;3=2;4=1")

data_to_analyse$trust3r<-recode(data_to_analyse$trust3,
                                       "1=4;2=3;3=2;4=1") 
data_to_analyse$trust4r<-recode(data_to_analyse$trust4,
                                "1=4;2=3;3=2;4=1") 

data_to_analyse$trust5r<-recode(data_to_analyse$trust5,
                                "1=4;2=3;3=2;4=1")

data_to_analyse$trust6r<-recode(data_to_analyse$trust6,
                                "1=4;2=3;3=2;4=1")



#data to model on 
data_to_modelon<-data_to_analyse[,-c(11:26)]
names(data_to_modelon)
str(data_to_modelon)
resp_norm=~resp_norm1r + resp_norm2r +resp_norm3r
poli_view=~poli_view1r +poli_view2r +poli_view3r +poli_view4r
trust=~trust1r +trust2r +trust3r +trust4r +trust5r +trust6r

fair=~fair1 + fair2 + fair4+ fair5 + 
  fair6 + fair7 + fair8 + fair9
#running the model
model1<-'
      resp_norm=~resp_norm1r + resp_norm2r +resp_norm3r
      poli_view=~poli_view1r +poli_view2r +poli_view3r +poli_view4r
      trust=~trust1r +trust2r +trust3r +trust4r +trust5r +trust6r
      fair=~fair1 + fair2+ fair5 + fair6 + fair7 + fair8 
'
#running the CFA with the total data set 
fit.model1<-cfa(model1,data=data_to_modelon,std.lv=T,ordered = T)
?cfa
summary(fit.model1,fit.measures=T,standardized=T,rsquare=T)


#running the CFA by group-structural equivalent - configural invariance 
fit.model1.configural<-cfa(model1,data=data_to_modelon,std.lv=T,ordered = T,group="ctry_abr")
?cfa
summary(fit.model1.configural,fit.measures=T,standardized=T,rsquare=T)


#running the CFA by group-metric equivalence/Invariance
#constraining the factor loading to be equal 
fit.model1.metric<-cfa(model1,data=data_to_modelon,std.lv=T,ordered = T,
                       group="ctry_abr",group.equal="loadings")
?cfa
summary(fit.model1.metric,fit.measures=T,standardized=T,rsquare=T)

anova(fit.model1.configural,fit.model1.metric)

fitmeasures(fit.model1.configural,c("cfi","tli","rmsea","srmr","chisq","df"))
fitmeasures(fit.model1.metric,c("cfi","tli","rmsea","srmr","chisq","df"))


fit.model1.scalar<-cfa(model1,data=data_to_modelon,std.lv=T,ordered = T,
                       group="ctry_abr",group.equal=c("loadings","intercepts"))
?cfa
summary(fit.model1.scalar,fit.measures=T,standardized=T,rsquare=T)

anova(fit.model1.metric,fit.model1.scalar)
fitmeasures(fit.model1.metric,c("cfi","tli","rmsea","srmr","chisq","df"))
fitmeasures(fit.model1.scalar,c("cfi","tli","rmsea","srmr","chisq","df"))





