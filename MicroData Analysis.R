library(data.table)
library(sqldf)
library(bit64)
library(ggplot2)
library(survey)

#### FUNCTIONS ####

sView = function(dt)
{
  View(dt[1:max(length(dt),1000)])
}
gorToInt = function(c) #Takes a gor string and gets the character value of the first character.
{
  as.integer(factor(substr(tolower(c),1,1),levels=letters))
}
addCellNos = function(dt,ruref.,sic,emp,gor) #Last four variables take on string values of column names.
{ #Defined this as a seperate function as I need to run this query at least twice.
  dt[,strruref:=as.character(get(ruref.))] #Sqldf doesn't like integer64.
  uni = sqldf(paste0("
    select A.*, B.cell
    from dt A left join strata B
    on A.",sic,">=B.lower_sic and A.",sic,"<=B.upper_sic and
        A.",emp,">=B.lower_size and A.",emp,"<=B.upper_size and
            ",gor,">=lowerintgor and ",gor,"<=upperintgor") )
  #SQL join to attach cell numbers to RUs.
  uni=data.table(uni)
  uni[,c(ruref.,"strruref"):=.(as.integer64(strruref),NULL)]
  eval(parse(text=paste0("setkey(uni,",ruref.,")")))
  dt[,strruref:=NULL]
  return(uni)
}
SICtoInd = function(sic)
{
  ifelse(sic < 68000,
    ifelse(sic < 41000,
      ifelse(sic < 35000,
        ifelse(sic < 10000,
          ifelse(sic < 5000, "A", "B"),
          "C"
        ),
        ifelse(sic < 36000, "D", "E")
      ),
      ifelse(sic < 55000,
        ifelse(sic < 49000,
          ifelse(sic < 45000, "F", "G"),
          "H"
        ),
        ifelse (sic < 64000,
          ifelse(sic < 58000, "I", "J"),
          "K"
        )
      )
    ),
    ifelse(sic < 86000,
      ifelse(sic < 77000,
        ifelse(sic < 69000, "L", "M"),
        ifelse(sic < 85000,
          ifelse(sic < 84000, "N", "O"),
          "P"
        )
      ),
      ifelse(sic < 94000,
        ifelse(sic < 90000, "Q", "R"),
        ifelse(sic < 99000,
          ifelse(sic < 97000, "S", "T"),
          "U"
        )
      )
    )
  )
}
createFilteredData = function(data,break.down=breakdown) #Filter data.
{
  filt = data
  
  if ("q203" %in% break.down)
  {
    filt = filt[q203!=167]
    
  } else {
    if ("q202" %in% break.down)
    {
      filt = filt[q203!="00"]
    } else {
      filt = filt[q202=="00"]
    }
  }
  if ("q203" %in% break.down)
  {
    filt = filt[q203!=167]
  } else {
    filt = filt[q203==167]
  }
  
  filt
}

#### SELECTION PARAMETERS ####

set.seed(420690)
n=9000 #Random sample size.
breakdown="q202" #The way in which we're breaking down our data. Eg: c("q201","q202")
thresholds = c(seq(500,25,-25),1)
bootstrap = 0 #Number of bootstrap samples to draw when estimating new variances.

#if (breakdown=="") breakdown="q201" else breakdown=c("q201",breakdown)

#### IMPORT DATA ####

micro.file = "D:/AITIS/aitis_final_base_data_2015.csv"
universe.file = "D:/AITIS/rx201512.csv"
dead.ru.file = "D:/AITIS/idbr455out.csv"
stratum.definitions.file = "D:/AITIS/Our PRN for 2015 MAIN.csv"
abs.universe.file = "D:/AITIS/ABSUniverse2015.csv"


strata = fread(stratum.definitions.file, select=c("cell", "seltype","lower_class","upper_class","lower_size",	"upper_size", "Universe15","lower_reg","upper_reg"))
strata = strata[!is.na(Universe15)]
setkey(strata, cell)
strata[,c('upperintgor','lowerintgor'):=.(gorToInt(upper_reg),gorToInt(lower_reg))]


divtable = data.table(a=1:100,b=SICtoInd(1:100*1000),key="a");


micro.data = fread(micro.file,
                     select=c("ru_ref","q202","q203","current_sic","q201","q204","q204_i","q204_j","imp_class", "concern","o_weight","a_weight","contributor_region","employee_count","cell_selection"),
                     colClasses = c(receipt_date="character",imp_class="character",q202="character"))
setkey(micro.data,ru_ref,q203,q202,q201)
micro.data[,gorint:=gorToInt(contributor_region)]
#micro.data = addCellNos(micro.data,"ru_ref","current_sic","employee_count","gorint")
micro.data[,division:=current_sic %/% 1000];
micro.data[,industry:=divtable[.(division),b]]
micro.data[,cell:=-1]
micro.data[concern %in% c(55,60), cell := as.numeric(imp_class)]
micro.data=micro.data[!is.na(q204_j)]

universe = fread(universe.file,
                select=c("ruref","gor","curturn","curempment","cursic07","prn","status"))
setkey(universe, ruref)
universe[,gorint:=gorToInt(gor)]
universe = universe[ruref<60000000000] #Remove enterprise groups.
universe = universe[status %in% 1:3][,status:=NULL] #Legal status in N.
#universe = addCellNos(universe, "ruref", "cursic07","curempment","gorint")
universe[,division:=cursic07 %/% 1000];
universe[,indust:=divtable[.(division),b]]


deadrus = read.csv(dead.ru.file,header=FALSE)
allrus = deadrus$V1
deadrus = deadrus$V1[!(gsub(' ','',as.character(deadrus$V2)) == "")]

#### PROCESSING ####

#Filter data...
micro.filtered = createFilteredData(micro.data)

rus = micro.filtered[,unique(ru_ref)]

micro.prn = micro.filtered[concern %in% c(55,60),unique(ru_ref)]
#micro.prn.reduced = universe[.(micro.prn)][!is.na(prn),ruref] #Remove units dead in current year.

micro.prn.data = micro.filtered[.(micro.prn),.("q204_i"=sum(q204_i),"current_sic"=current_sic[1],"gorint"=gorint[1],"cell"=cell[1],"industry"=industry[1],"employee_count"=employee_count[1],"q202"=q202[1],"q203"=q203[1]),keyby=c("ru_ref",breakdown)]
#micro.prn.data.reduced = micro.prn.data[.(micro.prn.reduced)] #Remove units dead in current year.

micro.data.summ = micro.filtered[,.("q204_i"=sum(q204_i),"q204_j"=sum(q204_j),'o_weight'=o_weight[1],'a_weight'=a_weight[1],'cell'=cell[1]),by=c('ru_ref',breakdown)]
micro.data.summ = merge(micro.data.summ,universe,by.x = "ru_ref",by.y = "ruref",all.x=TRUE)
graph = ggplot(data=micro.data.summ,aes(y=log10(q204_i+1),x=log10(1+curturn),colour=q201))
graph + geom_point(size=0.5) + geom_abline(slope=1,colour='#2255FF',size=1.3)

#temp = merge(x=current.data.summ[q201=='P'], y=current.data.summ[q201=='R'], by='ru_ref', all=TRUE)
#setnames(temp,c("q204_i.x","q204_i.y"),c("q204_P","q204_R"))
#graph = ggplot(data=temp,aes(y=log10(1+q204_P),x=log10(1+q204_R)))
#graph + geom_point() + geom_abline(slope=1,colour='#2255FF',size=1.3)

#### SELECTION ####

totals = micro.data.summ[,.(tot=sum(q204_j*o_weight*a_weight,na.rm = T)),keyby=breakdown]

number.to.keep = numeric(length(thresholds)+1)
count = 1
Var = number.to.keep
CV = number.to.keep

total.perc = merge(micro.data[,.(dummy=1),by=breakdown],data.table(dummy=1,thresholds),by='dummy',allow.cartesian = T)
  #Creates a new totals table keyed by breakdown and threshold.
eval(parse(text=paste0("setkey(total.perc,thresholds,",paste(breakdown,collapse=","),")")))
total.perc[,c('q204','perc','dummy'):=.(0,0,NULL)]

ru.total.trade = micro.prn.data[,.(q=sum(q204_i)),by=ru_ref] #Convenience table: aggregate trade for each RU.
micro.data.summ2 = micro.data.summ[,.(q204_i=sum(q204_i),o_weight=o_weight[1],cell=cell[1]),keyby=ru_ref]
setnames(micro.data.summ2,'cell','celln')

tt = micro.data[q202=='00',sum(q204_i*o_weight*a_weight,na.rm = T)] #Total trade.

Sh = micro.data[q202=='00' & !is.na(q204_j) & concern!=70,.(q204_i=sum(q204_i),o_weight=o_weight[1],cell=cell[1],a=a_weight[1]),by=ru_ref][,.(Sh = var(q204_i*o_weight),nh = .N,a=a[1]),keyby=cell]
setkey(Sh,cell)
Sh[nh==1, Sh:=0]
Sh = subset(strata,select=c('cell','Universe15'))[Sh]
setkey(Sh,cell)
setnames(Sh,'Universe15','Nh')
Sh[cell<1,Nh:=nh]
Sh[,Nh2:=Nh]
Var[count] = Sh[,sum(Nh * {a-1} * Sh,na.rm = T)]
CV[count] = 100*sqrt(Var[count])/tt
Sh[,Nt:=0]

units = micro.prn.data[,.(ru_ref=unique(ru_ref),weight=1),keyby=cell]
setnames(units,'cell','cel')
micro.data[,tempcell:=cell]
strata[,tempN:=Universe15]

for (threshold in thresholds)
{
  count=count+1
  
  big.units = ru.total.trade[threshold<=q,ru_ref]
  new.big.units = ru.total.trade[threshold<=q & q<500]
  number.to.keep[count] = length(big.units)
  
  
  NoToRemove = micro.data[.(new.big.units),.(Nt=.N),keyby=cell]
  Sh[NoToRemove[,.(cell)],c('Nh','Nt'):=.(Nh2 - NoToRemove[,Nt],NoToRemove[,Nt])] #Decrease the cell counts for those that have been added to ref list.
  Sh[,nh:=pmin(nh,Nh)]
  micro.data[.(new.big.units),tempcell:=-1]
  if(bootstrap>0)
  {
    for (k in 1:bootstrap)
    {
      units[.(new.big.units),c('weight','count'):=.(1/2,0)] # Half because the rotation period is 2.
      Sh[cell>0,Sh:=var(c(micro.data.summ2[.(units[.(cell)][!ru_ref %in% new.big.units,ru_ref]),q204_i*o_weight,by=celln][,V1],
        micro.data.summ2[.(units[.(cell),ru_ref[runif(Nt,1,length(ru_ref)+1)]]),q204_i*o_weight])),by=cell]
      Var[count] = Var[count] + Sh[,sum(Nh * {a-1} * Sh,na.rm = T)]
      if(!k%%5) print(paste0("Bootstrap: ",k))
    }
  }
  Var[count] = Var[count]/bootstrap
  CV[count] = CV[count] + 100*sqrt(Var[count])/tt
  
  this.temp = micro.data.summ[.(big.units),.(tot=sum(q204_i)),keyby=breakdown]
  total.perc[c(list(threshold),as.list(subset(this.temp,select = breakdown))), 
             q204:=this.temp[,tot]]
  
  print(paste0("Threshold: ",threshold))
}

thresholds = c(500,thresholds)

indexes = as.list(subset(total.perc,select=breakdown))
total.perc[,perc:=100*q204/totals[indexes,tot]]

u = micro.data[q202!='00' & q203!=167,.(cov=100*sum(q204_j,na.rm=T)/sum(q204_j*o_weight*a_weight,na.rm=T)),by=breakdown]
a = merge(x=total.perc[.(1)],y=total.perc[.(500)],by=breakdown)
a[,improvement:=perc.x-perc.y]
v = merge(a,u,by=breakdown)
v[,cov.improved:=cov+improvement]
b = micro.data[q204_i>1,.(N=length(unique(ru_ref))),by=breakdown]
c = micro.data[q204_i>=500,.(N=length(unique(ru_ref))),by=breakdown]
d = merge(x=b,y=c,by=breakdown)
d[,Ndiff:=N.x - N.y]
e = merge(x=a,y=d,by=breakdown)
micro.data[q202!='00' & q203!=167,cont:=100*q204_i / sum(q204_j*o_weight*a_weight,na.rm = T),by=breakdown]
maxconts = micro.data[concern %in% c(55,60),.(cont=sum(cont,na.rm=T)),by=c(breakdown,'ru_ref')]
maxconts = maxconts[,.(maxcont=max(cont)),keyby=ru_ref]
maxconts = maxconts[!.(micro.data[q202=='00',.(q=sum(q204_i)),by=ru_ref][q>=500,ru_ref])]

ograph = ggplot(data=data.table(Threshold=thresholds,CV)[CV!=0],aes(x=Threshold,y=CV))
ograph + geom_line() + ggtitle("Coefficient of Variation (CV) of total trade as a function of Threshold") +
  labs(y="CV (%)",x="Threshold (£1000's)")
realog = ggplot(data=data.table(No=number.to.keep,CV)[-1][!is.nan(CV)],aes(x=No,y=CV))
realog + geom_line() + ggtitle("Coefficient of Variation (CV) as a function of Number of Businesses Above Threshold") +
  labs(y="CV (%)",x="Number of Businesses Above Threshold")


graph = ggplot(data=data.table(thresholds,number.to.keep)[-1],aes(thresholds,number.to.keep))
graph + geom_line() + ylim(0,max(number.to.keep)) + ggtitle("Number of PRN businesses in 2015 with at least threshold (£1000s) of ITIS in any direction") + 
  labs(y = "Number of businesses",x="Threshold (£1000's)")

graph = ggplot(data=total.perc[q201=='P']#[,industry:=as.factor(industry)]
               ,aes_string(x="thresholds",y="perc"))#,col=breakdown[2]))
graph + geom_line() +# 
    ggtitle("Percentage of total trade (purchases) for the PRN sample units exceeding a specified threshold") + labs(y="Coverage (purchases / %)",x="Threshold (£1000's)")
graph = ggplot(data=total.perc[q201=='R']#[,industry:=as.factor(industry)]
               ,aes_string(x="thresholds",y="perc"))#,col=breakdown[2]))
graph + geom_line() +# ylim(2.20,2.35) + #scale_y_continuous(breaks=c(1.92,1.94,1.96,1.98,2.00,2.02,2.04)) + 
  ggtitle("Percentage of total trade (receipts) for the PRN sample units exceeding a specified threshold") + labs(y="Coverage (receipts / %)",x="Threshold (£1000's)")