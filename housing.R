setwd()
housing_train=read.csv("C:/Users/Namdeo/Downloads/Additional Datasets/Additional Datasets/housing_train.csv",stringsAsFactors = F)
housing_test=read.csv("C:/Users/Namdeo/Downloads/Additional Datasets/Additional Datasets/housing_test.csv",stringsAsFactors = F)
library(dplyr)
glimpse(housing_train)

housing_test$Price=NA
housing_train$data='train'
housing_test$data='test'

housing_all=rbind(housing_train,housing_test)

table(housing_all$CouncilArea)
table(housing_all$SellerG)

names(housing_all)[sapply(housing_all,function(x) is.character(x))]


round(tapply(housing_all$Price,housing_all$CouncilArea,mean,na.rm=T))

lapply(housing_all,function(x) sum(is.na(x)))

housing_all$Address = NULL
housing_all$Postcode = NULL
housing_all$Car = NULL 
housing_all$BuildingArea = NULL
housing_all$YearBuilt = NULL



CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

names(housing_all)[sapply(housing_all,function(x) is.character(x))]

cat_cols = c("Suburb"   ,   "Type"       , "Method"   ,   "SellerG"  ,   "CouncilArea")

for(cat in cat_cols){
  housing_all=CreateDummies(housing_all,cat,50)
  
}

for(col in names(housing_all)){
  if(sum(is.na(housing_all[,col]))>0 & !(col %in% c("data","Price"))){
    housing_all[is.na(housing_all[,col]),col]=mean(housing_all[housing_all$data=='train',col],na.rm=T)
  }
}

housing_train=housing_all %>% filter(data=='train') %>% select(-data)
housing_test=housing_all %>% filter(data=='test') %>% select (-data,-Price)

set.seed(2)
s=sample(1:nrow(housing_train),0.7*nrow(housing_train))
housing_train1=housing_train[s,]
housing_train2=housing_train[-s,]

glimpse(housing_train1)

fit=lm(Price~.,data = housing_train1)
library(car)
sort(vif(fit),decreasing = T)

fit=lm(Price~.-CouncilArea_-Distance,data = housing_train1)

fit=step(fit)

summary(fit)
formula(fit)

fit = lm(Price ~ Rooms + Bedroom2 + Bathroom + Landsize + Suburb_Hadfield + 
            Suburb_HeidelbergWest + Suburb_OakleighSouth + 
           Suburb_CoburgNorth + Suburb_HeidelbergHeights + Suburb_Malvern + 
           Suburb_Moorabbin + 
           Suburb_Maidstone + Suburb_AirportWest + 
           Suburb_Bulleen +  Suburb_SunshineNorth + 
           Suburb_WestFootscray + Suburb_AvondaleHeights + Suburb_Fawkner + 
           Suburb_AltonaNorth + Suburb_Armadale + Suburb_Williamstown + 
           Suburb_SunshineWest + Suburb_Ivanhoe + Suburb_TemplestoweLower + 
           Suburb_KeilorEast + Suburb_Prahran + 
            Suburb_Kensington + Suburb_Sunshine + 
           Suburb_Toorak + Suburb_Maribyrnong + Suburb_Doncaster + Suburb_MooneePonds + 
           Suburb_Hampton + Suburb_Balwyn + Suburb_Camberwell + 
           Suburb_PascoeVale + Suburb_BrightonEast + 
            Suburb_Coburg + Suburb_Northcote + Suburb_Kew + 
           Suburb_Brighton + Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + 
           Suburb_SouthYarra + Suburb_Preston + Suburb_BentleighEast + 
           Suburb_Reservoir + Type_u + Type_h + Method_SP + Method_S + 
           SellerG_Kay +  
           SellerG_Greg +  SellerG_RT + 
            SellerG_Marshall + SellerG_Barry + SellerG_Jellis + 
           CouncilArea_Monash + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
           CouncilArea_Melbourne + CouncilArea_Banyule + CouncilArea_PortPhillip + 
           CouncilArea_Yarra + CouncilArea_Maribyrnong +  
           CouncilArea_MooneeValley + CouncilArea_Moreland + CouncilArea_Boroondara,data=housing_train1)
summary(fit)

rmse= mean((housing_train2$Price-predict(fit,newdata=housing_train2))**2) %>% sqrt()
rmse

fit.final=fit=lm(Price ~ .,
                 data=housing_train)
fit.final=step(fit.final)

pred.IR=predict(fit.final,newdata=housing_test)

write.csv(pred.IR,"housingPrice.csv",row.names = F)

df=read.csv("C:/Users/Namdeo/Documents/housingPrice.csv")
df1=round(df$x,digits = 0)
df1
write.csv(df1,"housingPrice2.csv",row.names = F)

df$x = replace(df$x,which(df$x),mean(df$x))

df1[] = lapply(df1,x)
df1
x=mean(df$x)
x

###
library(ggplot2)
housing_train1 %>%
  mutate(pred_IR=predict(fit,newdata=housing_train1)) %>%
  ggplot(aes(x=Price,y=pred_IR))+geom_point(alpha=0.6)

model_string=paste(fit$coefficients,names(fit$coefficients),sep="*",collapse = " + ")
model_eq=strwrap(sub("\\*\\(Intercept\\)","",gsub("+ -","- ",model_string,fixed=TRUE)))
model_eq

plot(fit,which=1)
plot(fit,which=2)

df=data.frame(res=fit$residual)
ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm ,args = list(mean=mean(df$res),sd=sd(df$res)),color="green")

shapiro.test(fit$residuals)

plot(fit,which=3)
plot(fit,which=4)



table(S$store_Type)
table(unique(S$Areaname))
sum(S$Areaname)
prop.table(table(S$Areaname,S$store),1)
var(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4,S$store_Type,na.rm = FALSE)
var(S$s)

library(ggplot2)
ggplot(df, aes(store_train$sales0)) + geom_density(color="red")+
  stat_function(fun=dnorm, args=list(mean=mean(store_train$sales0),
                                     sd=sd(store_train$sales0)),
                color="green")+
  ggtitle("Visual Normality Test for sales0")
library(nortest)
ad.test(store_train$sales4)

var.test(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4[S$store_Type == "Grocery Store"],S$sales0+S$sales1+S$sales2+S$sales3+S$sales4[S$store_Type == "Supermarket Type3"])
t.test(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4~S$store_Type == "Grocery Store",paired=FALSE,var.equal=FALSE)
var.test(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4,S$store_Type == "Grocery Store")

boxplo
boxplot(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4)
boxplot +geom_boxplot()
geom_boxplot(outlier.shape = NA)
outlier.limits=function(x,k){
  x.q1=quantile(x)[2]
  x.q3=quantile(x)[4]
  x.iqr=IQR(x)
  ll=x.q1-k*x.iqr
  ul=x.q3+k*x.iqr
  limits=c(ll,ul)
  names(limits)=NULL
  return(limits)
}
c=1.5
print("Outlier Limits For fnlwgt are :")

outlier.limits(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4,c)

n1=outlier.limits(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4,c)
print("Number of outliers according to these limits for fnlwgt:")

sum(S$sales0+S$sales1+S$sales2+S$sales3+S$sales4<n1[1] | S$sales0+S$sales1+S$sales2+S$sales3+S$sales4>n1[2])