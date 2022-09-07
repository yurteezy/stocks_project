########################
##  semistralni prace ##

########################




##PRIKLAD 1##

library("readxl")
tabl <- read_xlsx("C:/Users/iurn00/Desktop/Учеба 1 сем/R-ko//semestralka123.xlsx")
tabl$price <- as.numeric(tabl$price)
tabl$ytd_change <- as.numeric(tabl$ytd_change)
tabl$volume_price <- as.numeric(tabl$volume_price)
tabl$historical_max <- sample(c(TRUE,FALSE),size=nrow(tabl),replace = TRUE)
tabl$possible_purch <- c(NA)

##PRIKLAD 2##

cat("typ udaje s kterymi pracuju je",typeof(tabl))
cat("databaze ma",ncol(tabl),"promenych")
cat("databaze ma",nrow(tabl),"pozorovani")
cat('shrnuti databazi') 
summary(tabl)
    

##PRIKLAD 3##

sapply(tabl, class)

##PRIKLAD 4##

n=seq(1,nrow(tabl))

rownames(tabl) <- paste("ID",n,sep="")
rownames(tabl)


##PRIKLAD 5##

colnames(tabl)


##PRIKLAD 6##

cat("Kdy by dataset mel NA tak bych pouzila funkce na.omit")


##PRIKLAD 7##

tabl$skupina <- sample(c("A","B","C","D"), nrow(tabl), TRUE)
tabl$skupina

##PRIKLAD 8##

cat("pozorovani pismena B v poradi",which(tabl$skupina=="B"))
cat("pocet pozorovani pisnena B",length(which(tabl$skupina=="B")))

##PRIKLAD 9##

A <- which(tabl$skupina=="A")
B <- which(tabl$skupina=="B")
C <- which(tabl$skupina=="C")
D <- which(tabl$skupina=="D")

qq <- function(x){
  cat("minimum promennou = " , min(x))
  cat("  maximum promennou = ", max(x))
  cat("  prumer promennou = ", mean(x))
}
qq(tabl$ytd_change[A])
qq(tabl$ytd_change[B])
qq(tabl$ytd_change[C])
qq(tabl$ytd_change[D])

##PRIKLAD 10##

#prvni tabulka
cetnost <- table(tabl$skupina)
relativne <- round(prop.table(table(tabl$skupina)),2)
procent <- paste(round(100*relativne, 2), "%", sep="")
cat("tabulka cetnosti sloupce skupina  ")
rbind (cetnost,relativne,procent)

#druha tabulka
aa <- table(tabl$sector,tabl$historical_max)
colnames(aa) <- c("on hist.max  ","not on hist.max")
overvaluetion_of_industry <- paste(round(100*aa[,2]/sum(tabl$historical_max==TRUE), 2), "%", sep="")
print("kontingencni tabulka")
cbind(aa,overvaluetion_of_industry)

## filtrovani > mean
biggest_growth <- tabl$company[which(tabl$ytd_change>mean(tabl$ytd_change))]
growth_ytd <- tabl$ytd_change[tabl$ytd_change>mean(tabl$ytd_change)]
cat("nadprumerny rust firem")
cbind(as.data.frame(biggest_growth),growth_ytd)

##sort order

tt <- as.data.frame(tabl$company[order(ordered(tabl$price))])
yy <- (cbind(tt,sort(tabl$price)))
colnames(yy) <- c("Company","Price in $")
print("6 nejlevnejsich firem ze seznamu")
head(yy)

##subset

ad <- as.data.frame(subset(tabl$company,tabl$historical_max==FALSE))
colnames(ad) <- "Neni na historickem maximume"
ad


##Priklad 11## ciklus ktery pridava sloupec s rekomendaci




for (i in 1:nrow(tabl)){
if (tabl$ytd_change[i]<15)
  {
  tabl$possible_purch[i]="buy"
}
  else if(tabl$ytd_change[i]<40) {  tabl$possible_purch[i]="hold" }
  
  else {  tabl$possible_purch[i]="sell" }
  }
tabl

##Priklad 12##

zamest=0

for (i in 1:nrow(tabl)) {
  if (tabl$sector[i]=="Financials" ) { 
    next 
    }
  zamest <- zamest+tabl$total_employee[i]
  
}
cat("pocet zamestnancu ktere pracuji v nefinancnich firmech =",zamest)


##Priklad 13## jaky pocet firem poradne ma 1/5 vsech zamestnancu

q<-1
fd=1
rw <- (sum(tabl$total_employee))/5

repeat{
   fd <- fd + tabl$total_employee[q]

  if(fd > rw){
    break 
  }
  else{ q=q+1 }
  }
cat(q,"prvnich firem ma 1/5 poctu vsech zamestnancu ze seznamu")


##Priklad 14## vyber 5 nahodnych akcii a 100 simulaci ktere nam ukazou jaky by prirust mohli bysme mit

l=1
kk=0
while (l <100) {
  kk <- kk+((sum(sample(tabl$ytd_change,5)))/5)
  l=l+1}
  cat("prirust by byl",kk/100)

  
##Priklad 15##
  cs=0
  cs1=0
  en=0
  en1=0
  fn=0
  fn1=0
  hc=0
  hc1=0
  ind=0
  ind1=0
  it=0
  it1=0
  mt=0
  mt1=0
  cn=0
  cn1=0
  
for (i in 1:nrow(tabl)){
  if (tabl$sector[i]=="Communication Services")
  {cs = cs + tabl$ytd_change[i]
  cs1=cs1+1}
  else if (tabl$sector[i]=="Consumer")
  {cn=cn+tabl$ytd_change[i]
    cn1=cn1+1}
  else if (tabl$sector[i]=="Energy")
  {en=en+tabl$ytd_change[i]
    en1=en1+1}
  else if (tabl$sector[i]=="Financials")
  {fn=fn+tabl$ytd_change[i]
    fn1=fn1+1}
  else if (tabl$sector[i]=="Health Care")
  {hc=hc+tabl$ytd_change[i]
    hc1=hc1+1}
  else if (tabl$sector[i]=="Industrials")
  {ind=ind+tabl$ytd_change[i]
    ind1=ind1+1}
  else if (tabl$sector[i]=="Information Technology") 
  { it=it+tabl$ytd_change[i]
    it1=it1+1}
  else { mt=mt+tabl$ytd_change[i]
      mt1=mt1+1
  
}
}
  tabl$sector <- as.factor(tabl$sector)
  jmeno_ind<- c(levels(tabl$sector))
  
  prumer_ind<- round(c(cs/cs1,  
cn/cn1,
en/en1,
fn/fn1,
hc/hc1,
ind/ind1,
it/it1,
mt/mt1),2)

  print("zmeny cen na akcii v prumyslech")
cbind(jmeno_ind,prumer_ind)

  ##Priklad 16##

fmean <- function(x){
  sum(x)/length(x)
    }
fmean(tabl$price)

  ##Priklad 17##

fmean1 <- function(x){
  if (class(x)=="numeric"){
    sum(x)/length(x)
  }
  else "neni mozne vzpocitat aritmeticky prumer z udaju, ktere nejsou cisla"
}
fmean1(tabl$skupina)
cat("arifmeticky prumer =", fmean1(tabl$price))


  ##Priklad 18##

gmean = function(x){prod(x)^(1/length(x))}
cat("geometricky prumer =",gmean(tabl$price))


  ##Priklad 19##

hmean = function(x) {
  if (typeof(x)=="list") {
    (nrow(x)*ncol(x))/sum(1/x)
  } else
  length(x)/sum(1/x)
  }
cat("harmonicky prumer =" ,hmean(tabl$price))

  ##Priklad 20##

qr<-c()
cx<-c()
numf <- function(x){
        cx<-(sapply(x, class))
          qr<- which(cx=="numeric")
          x[qr]
      }
numerickaMatice <- numf(tabl)
numerickaMatice

##Priklad 21##

print("harmonicky prumer ze vsech hodnot")
hmean(numerickaMatice)


print("harmonicky prumer jednotlivych radku")
for (i in 1:nrow(numerickaMatice)){
hrow  <-  hmean(numerickaMatice[i,])
print(hrow)
}


print("harmonicky prumer jednotlivych sloupcu")
for (i in 1:ncol(numerickaMatice)){
hcol <-  hmean(numerickaMatice[i]) 
print(hcol)}


##Priklad 22##

pr22 <- function(x){
  okr <- round(x)
  okr%%7
  
}
pr22(numerickaMatice)



##Priklad 23##
library("ggplot2")


## 1 base

lvls <- rainbow(length(levels(tabl$sector)))
y1<-table(tabl$sector)
f1<-levels(tabl$sector)
f1<-paste(f1,(round(y1/sum(y1)*100)),"%",sep=" ")


pie(y1, labels = f1,
    main="Pie Chart of sectors",
    col=lvls, 
    cex=0.8)

## 2 base

tb1 <- table(tabl$historical_max, tabl$possible_purch)
barplot(tb1, main="The number of recommendations by historical max",
        xlab="recommendation", col=c("darkblue","red"),
        legend = rownames(tb1), beside=TRUE)

## 3 base

r <- tabl[order(tabl$ytd_change),]
dotchart(r$ytd_change,labels=r$company,cex=.7,
         main="Change of stocks price",
         xlab="ytd change in %", gcolor="blue", color="darkgreen")

## 1 ggplot

ggplot(tabl, aes(x=skupina, y=price, fill=skupina)) + 
geom_boxplot(fill = "white",color="violet")+
  geom_jitter(color="#3366FF", size=2)+
  ggtitle("price of stocks by groups") +
  xlab("Skupina")
  

## 2 ggplot

ggplot(tabl, aes(x=volume_price, y=ytd_change)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  ggtitle("Evolution of something")



