DMR=tbl_df(Dubai_Marina_Ready_DB)
summary(DMR)
cor(DMR$Price,DMR$Size)
sd(DMR$`Price/Size`)
dcount(DMR$Size)
ggplot(dmr1,aes(dmr1$Price))+geom_histogram(binwidth = 500000,col='black',fill= 'Dark blue')+xlim(c(0,10000000))+labs(title = "Histogram for Price", x="Price")
options(scipen=999)
unique(DMR$`No of Bedrooms`)
DMR %>%
  group_by(`No of Bedrooms`) %>%
  summarise(N=(n()) %>%
  ggplot(DMR,aes(x=DMR$`No of Bedrooms`,y=N()))+geom_col()

DMR %>%
  group_by(Bedrooms) %>%
  na.omit(Bedrooms %>%
  summarise(N=sum(Price)) %>%
  ggplot(DMR,aes(Bedrooms,N))+geom_col()


 DMR %>%
   group_by(Developer) %>%
   na.omit() %>%
   summarise(N=n()) %>%
   top_n(2) %>%
   ggplot(aes(Developer,N,fill=Developer))+geom_col()

DMR %>%
  group_by(Bedrooms) %>%
  na.omit() %>%
  summarise(N= sum(Price)) %>%
  top_n(5) %>%
  ggplot(aes(x=reorder(Property,-N),y=N, fill=Property))+geom_col()+labs(title = "Top 5 Properties",x= "Property", y = "No Of Transactions")+theme(axis.text.x= element_blank()) 

DMR %>%
  group_by(Bedrooms,Price) %>%
  na.omit() %>%
  summarise(N=sum(Price)) %>%
  ggplot(aes(Bedrooms,paste(format(round(N / 1e6, 1), trim = TRUE), "M"),fill= Bedrooms))+geom_col()+scale_x_continuous(breaks= DMR$Bedrooms)
  
  cor.test(DMR$Price,DMR$Size)       
summary(lm(formula = DMR$Price~DMR$Size) 
summary(lm(formula=DMR$Price~DMR$Size+DMR$Property))          
predict(lm(formula=DMR$Price~DMR$Size+DMR$Property)),newdata = data.frame(sqft = 2000), interval = "prediction"          
predict(lm(formula = DMR$Price~DMR$Size+DMR$Property)), newdata= data.frame(sqft=2000),interval="prediction"

options(scipen=999)
ggplot(data=DMR, aes(x=DMR$Price, y=DMR$Size))+geom_point()+theme_light()

g1=skew(DMR$Price)
cor.test()

hist(g1)

ggplot(DMR,aes(x=Type,y=Price))+geom_boxplot()
ggplot(DMR,aes(x=Type,y=log(Price))+geom_boxplot()
ggplot(DMR,aes(x=Size,y=Price))+geom_point()+facet_grid(.~Bedrooms)
plot(lm(DMR$Price~DMR$Size))
abline(plot(DMR$Price,DMR$Size))
abline(lm(g1~g2))
DMR=mutate(DMR,Logprice= log(Price))
DMR=mutate(DMR,Logsizee = log(Size))

g1=select(log(DMR$Price),log(DMR$Size))
g1=select(DMR,log(DMR$Price),log(DMR$Size))      
g1=log(DMR$Price)
g2=log(DMR$Size)
plot(g1,g2)

abline(lm(DMR$Logprice~DMR$Logsizee))

abline(lm(DMR$Logprice~DMR$Logsizee))
abline(8.1183  ,      0.8499)
cor(DMR$`Price/Size`,DMR$Price)
cor(DMR$`Price/Size`,DMR$Size)
abline(plot(lm(DMR$Logprice~DMR$Size)))
abline(plot(y=DMR$Logprice,x=DMR$Size))
mod=lm(DMR$Price~DMR$Size)
summary(mod)
mod2=lm(DMR$Price~DMR$Size+DMR$Property)
summary(mod2)

anova(mod,mod2)
predict(lm(DMR$Price ~ DMR$Size), newdata = data.frame(DMR$Size= 2000), interval = "prediction")




plot(DMR, col="navy",na.omit(DMR))

sample = sample.split(DMR,SplitRatio = 0.80)
train1 =subset(DMR,sample ==TRUE)
test1=subset(DMR, sample==FALSE)
summary(train1)
summary(test1)
fit1=lm(Price ~.,data=train1)
fit1=lm(data = train1,Price~.)
cor(DMR1$Price,DMR$Bedrooms)

DMR %>%
  na.omit(Bedrooms) %>%
  cor(DMR$Price,DMR$Bedrooms)

DMR %>%
  na.omit(Bedrooms) %>%
  cor(Price,Bedrooms)
cor(DMR$Price,na.omit(DMR$Bedrooms))
DSB=select(DMR,na.omit(DMR$Bedrooms))
dsb=tbl_df(c(DMR,na.omit(DMR$Bedrooms)))    
dmr1=na.omit(DMR)
cor(dmr1$Price,dmr1$Bedrooms)
dmr1=tbl_df(dmr1)

d1=lm(dmr1$Price~dmr1$Size)
summary(d1)
d2=lm(dmr1$Price~dmr1$Size+dmr1$Property+dmr1$Quarter)
summary(d2)
anova(d1,d2)
predict(lm(dmr1$Price ~ dmr1$Size+dmr1$Property), newdata = data.frame(Size = 2000), interval = "confidence") %>%
  View()
fit1=lm(Price~Size+Quarter,data=dmr1)
ggplot(data=dmr1, aes(fit1$residuals)) +
  geom_histogram(binwidth = 1000000, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +xlab()
  ggtitle("Histogram for Model Residuals")
options(scipen=999)
predict(fit1, data.frame(Size = 2000, Quarter='2015-Q4'))
ggplot(data = dmr1, aes(x = Size, y = Price+Property)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")
options(scipen=999)
tsclean(dmr1)
decompose(dmr1)
adf.test(dmr1)
stl(dmr1$`Price/Size`)

dd=dmr1 %>%
  group_by(Year) %>%
  summarise(N=median(Price/Size))
View(dd) 



dmr2$Year=as.numeric(dmr2$Year)
dmr2$Bedrooms=as.character(dmr2$Bedrooms)
dmr2=select(dmr1,'Price','Size','Price/Size','Bedrooms','Property','Quarter','Year','Developer','Logprice')
corrplot(dmr2)
ggpairs(data=dmr2, title="Dubai Marina data",binwidth=100000)
corrplot(dmr3)
dmr3=matrix(dmr2)
corrplot.mixed(dmr2)
corrplot(cor(dmr2),method = "number")

cor(dmr2$Price,dmr2$Size)
ggpairs(dmr2)
ggpairs(dmr2)+scatter.hist()
scatter.hist(dmr2)
ggscatmat(dmr2,  color = NULL, alpha = 1,corMethod = "pearson")
pairs(data=dmr2,main="Simple Scatterplot Matrix")
View(dmr1)
cor(dmr2)

ggplot(dmr2,aes(Bedrooms,Logprice,group=Bedrooms))+geom_boxplot(col='Dark Blue')+labs(y = "Inprice", x="Bedrooms")+scale_x_continuous(breaks= dmr2$Bedrooms)
ggplot(dmr2,aes(Developer,Logprice,))+geom_boxplot(col='Dark Blue')+labs(y="inprice",x="developer")+theme(axis.text.x = element_text(angle = 90,size=6))
ggplot(dmr2,aes(Type,Logprice))+geom_boxplot(col='Dark Blue')+labs(y = "Inprice", x="Type")
ggplot(dmr2,aes(Community,Logprice))+geom_boxplot(col='Dark Blue')+labs(y = "Inprice", x="Community")
ggplot(dmr2,aes(Property,Logprice))+geom_boxplot(col='Dark Blue',)+labs(y="inprice",x="Property")+theme(axis.text.x = element_text(angle = 90,size=4))
ggplot(dmr2,aes(Year))+geom_histogram(stat = 'count',fill="Dark Blue")
cor.test(dmr2$Price,dmr2$Size)
options(scipen=999)
summary(ff)
ggplot(data=dmr2, aes(ff$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

dmr2 %>%
  group_by(Year) %>%
  summarise(N=n()) %>%
  ggplot(aes(Year,N))+geom_col(fill="Dark blue")

ggplot(data=dmr2, aes(x=Size, y=Price))+geom_point(col='Dark Blue')+theme_light()+ 
  geom_smooth(method='lm',formula=y~x)
cor.test(dmr2$Size,dmr2$Price)
cors=cor(dmr2)
heatmap(cors, symm = TRUE)
corrplot.mixed(cors, order="hclust")

ab=lm(Price~Size,dat=dmr2)
summary(aa)
anova(ab,aa)
lm(Price~Size+Bedrooms,dat=dmr2)
predict(aa, data.frame(Size=c(1000, 2000, 3000),Property='Marina View Towers - A'))
summary(anova(aa,ab))
summary(aa)
ggplot(ab,aes(aa$residuals,))+geom_histogram(binwidth = 1000000)
