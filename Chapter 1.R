getwd()
employees<-read.csv("employee-data.csv", 
                    fileEncoding = "Latin1", 
                    check.names = F, nrows=10, header=TRUE)
employees
library(tidyverse)
employees2

employees2 %>% 
  group_by(annual.salary)%>% 
  summarize(count=n(), av.salary=mean(annual.salary, na.rm=TRUE))%>% 
  filter(count>0)
billboard<-read.csv("billboard.csv", header=T, nrows=5)
billboard<-as.tibble(billboard)
billboard

billboard %>% gather(x1st.week:x76th.week,  key="week", value= "rank", na.rm=T)%>%
  arrange(artist.inverted)
tb<-read.csv("tb.csv", header=TRUE, nrows=5)
tb<-as.tibble(tb)
tb

gathered.data<- tb%>%gather(m.014:f.65, key="gender", value="cases", na.rm=T)%>%
  arrange(year)
gathered.data
view(gathered.data)
tb.separated<- gathered.data%>%separate(gender, into=c("sex", "age"))
tb.separated

tb.separated$age<-str_replace_all(tb.separated$age, "0","0-")
tb.separated$age<-str_replace_all(tb.separated$age, "15","15-")
tb.separated$age<-str_replace_all(tb.separated$age, "25","25-")
tb.separated$age<-str_replace_all(tb.separated$age, "35","35-")
tb.separated$age<-str_replace_all(tb.separated$age, "45","45-")
tb.separated$age<-str_replace_all(tb.separated$age, "55","55-")
tb.separated$age<-str_replace_all(tb.separated$age, "65","65-100")
tb.separated2<-tb.separated%>%separate(age, into=c("age.low", "age.high"))

tb.separated2
tb.sepaarate.unite<- tb.separated2%>%unite("age", c("age.low", "age.high"))
tb.sepaarate.unite
tb

weather.data<-read.csv("weather-untidy.csv", header=T)
weather.data
weather.data<-as.tibble(weather.data)
weather<-read.csv("Weather.csv", header=T)
weather
weather<-as.tibble(weather)
weather
weather.spread<- spread(weather, key="element", value="value")
weather.spread
#DATA VISUALIZATION (GGPLOT2)
hdi<- read.csv("hdi-cpi.csv", stringsAsFactors=F)
hdi<-as.tibble(hdi)
library(tidyverse)
hdi

scatter.p<-ggplot(hdi)
scatter.p

scatter.p<-ggplot(hdi, aes(CPI.2015, HDI.2015))
scatter.p

scatter.p+ geom_point(aes(color=Region), size=3)+facet_grid(Region ~.)
stat_smooth()+coord_cartesian(xlim=c(0.75,1))

scatter.p+ geom_point(aes(color=Region), size=3)
stat_smooth()+theme_minimal()
library(tidyverse)
histogram<-read.csv("titanic.csv", header=5)
histogram
histogram<- as_tibble(histogram)
hitogram
histogram$Survived<-as.factor(histogram$Survived)
histogram$Pclass<-as.factor(histogram$Pclass)
histogram$Sex<-as.factor(histogram$Sex)
histogram$Embarked<-as.factor(histogram$Embarked)
histogram

histogrm<- ggplot(data=histogram, aes(x=Age))
histogrm+ geom_histogram(binwidth=5, color="darkslategray",
                         
                         fill="darkslategray4", alpha=0.5)+
  ggtitle("Age distribution on the Titanic")+
  labs(y="No. of passengers", x="Age")+
  theme_minimal()
#barchart

barchart<- ggplot(data=histogram, aes(x=Survived, fill=Survived))
bar+geom_bar()+ theme_light()+
  labs(y="Passenger count",x="gender", title="survival rate on the titanic")
   facet_wrap(Sex ~ Pclass)
   
#box and whiskers plot
whiskers.plot<- ggplot(histogram, aes(x= "Survived", y= "Age"))
          whiskers.plot + geom_boxplot(outlier.colour="red", outlier.shape= 4)+ geom_jitter(width=0.2, ase(color=sex))+
              labs(title="Survival on the titanic based on age")+
              theme_light()+coord_flip()

#scatter plot
  library (tidyverse)
corruption<- read_csv(".csv", header=5)
  corruption<- ggplot(corruption, aes(CPI.2015, HDI.2015))
  corruption+ geom_point(aes(color=Region), shape=21,
                         fill="white", size=3, stroke=2) +theme_light()+
    labs(x="Corruption INDEX, 2015",
         Y="human development index, 2015",
         title="corruption and human development")+
    stat_smooth(se=FALSE)
#central tendencies
victor<-c(1,2,3,4,5,6,5)
bon<- c(2,3,1,2,3,4,5)
library(tidyverse)
bondu<- tibble(victor, bon)
bondu

mean(bondu$bon)
mean(bondu$victor)

median(bondu$bon)
median(bondu$victor)

mode(bondu$bon)
mode(bondu$victor)

x<- table(bondu$bon)
x
names(x)[which(x==max(x))]

summary(bondu)
 #skewness of data

bara<- c(3,4,5,6,8,9,13,89)
bura<- c(3,7,9,46,34,56,2,3)
bora<- c(33, 4, 5, 76,34,34, 43,5)
df<- tibble(bara, bura, bora)

bara.hist<- ggplot(df, aes(x=bara, fill="fl"))+
          geom_histogram(binwidth=10, color="white")+theme_classic()+ 
            labs(title = "Its nothing really")
          
bara.hist


bura.hist<- ggplot(df, aes(x=bura, fill="fl"))+
  geom_histogram(binwidth=10, color="white")+theme_classic()+ 
  labs(title = "Its nothing really")

bura.hist

bora.hist<- ggplot(df, aes(x=bora, fill="fl"))+
  geom_histogram(binwidth=10, color="white")+theme_classic()+ 
  labs(title = "Its nothing really")

bora.hist

bon.mxn<- bon*1.3
bon.mxn

bondu$bon.mxn<-c(2.6, 3.9, 1.3, 2.6, 3.9, 5.2, 6.5)
bondu

 
sapply(bondu, mean)
lapply(bondu, mean)

sapply(bondu, var)
sapply(bondu, sd)

coef.var<- sapply(bondu, sd)/sapply(bondu, mean)
coef.var


land.data<- read.csv("landdata-states.csv", stringsAsFactors=F)

land.data

land.data%>% subset(Date== 2001.25)%>% ggplot(aes(y=Structure.Cost,
                                                   x=log(Land.Value)))+
  geom_point()+theme_light()+ labs(x="land value (transformed)",
                                   y="Structure Cost(USD)", 
                                   tittle="Relationship between land")
cor(land.data$Structure.Cost, land.data$Land.Value)
cor.test(land.data$Structure.Cost, land.data$Land.Value)

#INFERENTIAL STATISTICS

sal<-read.csv("ztest-a.csv", stringsAsFactors=TRUE)
summary(sal)

sal
#single population

z.test<- function(a, mu, sd){
  zeta=(mean(a)-mu)/(sd/sqrt(length(a)))
  return (zeta)
}
z.test(a=sal$salary, mu=113000, sd=15000)

data2<-read.csv("ttest-a.csv", stringsAsFactors= TRUE)

library(psych)

install.packages("psych")

describe(data2)
T.test<- function (a, hmean){
  t<- (mean(a)-hmean)/sd(a)/sqrt(length(a))
  return(t)
}
T.test(a= data2$Open.rate, hmean= 0.4)

length(data2$Open.rate)
          #two populations
#dependent ttest

magn<- read.csv("dependent-samples.csv")
magn


describe(magn)

magns<- t.test(magn$Before, magn$After, paired=TRUE, alternative= "less")

magns
describe(magn)
#independent ttest
grades<- read.csv("independent-samples.csv")
grades

grades<- gather(grades, Engineering,Management,
          key="course",
          value="grades")
grades

ind.tests<- t.test(grades~course, data=grades, mu= -4)
ind.tests

#LINEAR REGRESSION ANALYSIS
college<- read.csv("regression-example.csv")

describe(college)

college

linear.model<- lm(GPA~SAT, data=college)

point.sg<- ggplot(college, aes(SAT, GPA))+
  geom_point()+
  theme_light()+
  labs(x="SAT Scores",
       y="GPA upon graduation",
       title= "SAT and GPA")+
  stat_smooth(method="lm", se=FALSE)
point.sg
summary(point.sg)
summary(linear.model)

#functions

Applicants<-20
if(Applicants< 2){ 
  print("the qualification requirements are extreme")
  
} else{print("the qualification requirements are fair")}


money.owed<- 6000
if(money.owed< 500){
  print("should pay after one month")
  print("send notice today")
  money.owed<-money.owed*1.2
  print(money.owed)} else if((money.owed> 5000) & (money.owed<10000)){
print("should pay after a year")
  } else{print("Keep collateral")}

  
