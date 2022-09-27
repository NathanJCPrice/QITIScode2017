library(data.table)
library(bit64)
library(ggplot2)

#### PARAMETERS ####

current.reference.list.inclusion.threshold=500 #In pounds-thousands. Currently £500k.

breakdown="industry" #The way in which we're breaking down our data. Eg: c("q201","q202").
#This variable is used as the default break.down in functions when break.down is left unspecified.
if (breakdown=="") breakdown="q201" else breakdown=c("q201",breakdown) #Automatically adds in q201 to the start of breakdown. You may want to comment this out.

micro.file = "D:/AITIS/aitis_final_base_data_2015.csv" #Micro data that we're using to update the reference list.
dead.ru.file = "D:/AITIS/idbr455out.csv" #List of dead rus.
file.to.write.to = "D:/AITIS/extra_reference_list_units.csv" #Where to output the units that need to be added to the reference list.

#### UTILITY FUNCTIONS ####

SICtoInd = function(sic)#Converts a vector of 5-digit SICs to sections.
{#This function is slow so we create divtable further down to perform a quick join instead.
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


#### IMPORT DATA ####

divtable = data.table(a=1:100,b=SICtoInd(1:100*1000),key="a"); #Convenience table, helps assign sections to divisions.
bestconts = list(); #Convenience list. This is where we store all the contribution tables that we don't want to calculate over and over.

micro.data = fread(micro.file, #Import microdata.
                  select=c("ru_ref","q202","q203","current_sic","q201","q204","q204_i","q204_j","imp_class", "concern","o_weight","a_weight","contributor_region","employee_count"),
                  colClasses = c(ru_ref="integer64",receipt_date="character",imp_class="character",q202="character"))
setkey(micro.data,ru_ref,q203,q202,q201) #No particular reason for this order apart from ru_ref being in front.
micro.data[,division:=current_sic %/% 1000] #Get divisions from SICs.
micro.data[,industry:=divtable[.(division),b]] #Get sections from divtable.

deadrus = read.csv(dead.ru.file,header=FALSE) #See the QITIS Enlargement documentation for details on the format of this file.
allrus = deadrus$V1
deadrus = deadrus$V1[!(gsub(' ','',as.character(deadrus$V2)) == "")]
deadrus = as.integer64(deadrus)

#We're not going to output those units that will already be added to the ref list, i.e. >£500k.
first.to.ref.list = micro.data[q202=='00' & concern %in% c(55,60),.(q=sum(q204_i)),by=ru_ref][q>=current.reference.list.inclusion.threshold,unique(ru_ref)]

#### MAIN FUNCTIONS ####

#Input a threshold, a percentage, and this function outputs all those units with a contribution greater than the threshold relative to the breakdown.
enlargeSample=function(threshold, inputrus=integer64(), break.down=breakdown)
{
  quay = paste(break.down,collapse = "") #Defines the key for accessing the relevant contribution table in bestcont.
  if (all(names(bestconts)!=quay)) #If the the table hasn't been created yet once, do it now. Doing this saves us processing the same results over and over.
  {
    temp = micro.data[q202!='00' & q203!=167] #Filter out summarative entries.
    temp[,contribution:=100*q204_i/sum(q204_i*o_weight*a_weight),by=break.down]
    temp = temp[concern %in% c(55,60),.(contribution=sum(contribution)),by=c('ru_ref',break.down)] #Aggregate all the smaller contributions.
    bestconts[[quay]] <<- temp[,.(maxcont=max(contribution)),by=ru_ref] #Calculate all units best contributions.
    setkey(bestconts[[quay]],maxcont)
  }
  newrus = bestconts[[quay]][maxcont>=threshold][!.(first.to.ref.list)][,ru_ref] #Select all units with max contribution greater than the threshold.
  if (length(newrus)) newrus = newrus[!newrus %in% deadrus]#Strip dead units from the output.
  rus=sort(unique(c(newrus,as.integer64(inputrus),-1)))[-1] #Take the union of the newrus and inputrus.
  return(rus)
}

#Perform enlargement but based on achieving a target size instead of by threshold.
enlargeSampleBySize=function(target.samplesize, inputrus=integer64(), break.down=breakdown)
{
  ## Binary search.
  prevsamplesize=-5
  rus=numeric()
  samplesize=0
  threshold=50
  tolerance=25
  while(abs(samplesize-target.samplesize)>0 & (abs(samplesize-prevsamplesize)>0 | tolerance>2^-20))
  {
    prevsamplesize=samplesize
    rus=enlargeSample(threshold,inputrus,break.down)
    samplesize=length(rus)
    
    message(paste('Threshold:', signif(threshold), 'Sample size:', samplesize,sep=' '))
    
    if(samplesize > target.samplesize)
    {
      threshold = threshold+tolerance
    } else {
      threshold = threshold-tolerance
    }
    
    tolerance = tolerance/2
    
  }
  return(rus)
}

#Enlarge by q201 then product then country then industry all based on the same threshold.
performFullEnlargement=function(threshold, inputrus=integer64(), silent=F)
{
  rus = enlargeSample(threshold,inputrus,break.down=c("q201"))
  if (!silent) message("Enlarged by q201.")
  rus = enlargeSample(threshold,rus,break.down=c("q202"))
  if (!silent) message("Enlarged by product.")
  rus = enlargeSample(threshold,rus,break.down=c("q203"))
  if (!silent) message("Enlarged by country.")
  rus = enlargeSample(threshold,rus,break.down=c("industry"))
  if (!silent) message("Enlarged by industry (section).")
  return(rus)
}

#Runs performFullEnlargement with different thresholds until it finds the appropriate sample size.
performFullEnlargementBySize=function(target.samplesize, inputrus=integer64())
{
  ## Binary search.
  prevsamplesize=-5
  rus=numeric()
  samplesize=0
  threshold=50
  tolerance=25
  while(abs(samplesize-target.samplesize)>0 & (abs(samplesize-prevsamplesize)>0 | tolerance>2^-20))
  {
    prevsamplesize=samplesize
    rus=performFullEnlargement(threshold,inputrus,silent=T)
    samplesize=length(rus)
    
    message(paste('Threshold:', signif(threshold), 'Sample size:', samplesize,sep=' '))
    
    if(samplesize > target.samplesize)
    {
      threshold = threshold+tolerance
    } else {
      threshold = threshold-tolerance
    }
    
    tolerance = tolerance/2
    
  }
  return(rus)
}

#Estimates next year's coverage gains if the reference list is to be enlarged using a specified list of RUs.
estimateCoverageImprovements=function(inputrus, break.down=breakdown)
{
  if (!length(inputrus)) return(micro.data[,.(Coverage.Percentage = 0),by=break.down])
  temp = micro.data[q202!='00' & q203!=167,.(Coverage.Percentage = 100*sum(q204_i*(ru_ref %in% inputrus))/sum(q204_i*o_weight*a_weight)),by=break.down]
  setorderv(temp,"Coverage.Percentage",order=-1) #Order so that the best improvements are listed first.
  temp
}

#Estimates what the new coverages will be by breakdown if the reference list is expanded for the upcoming year.
estimateNewCoverages = function(inputrus, break.down=breakdown)
{
  #Start by estimating this year's coverages.
  all.cov = micro.data[q202!='00' & q203!=167,.(cov = 100*sum(q204_i)/sum(q204_j*o_weight*a_weight,na.rm=T)),keyby=break.down]
  if (length(inputrus)) #Make sure the input vector is non-empty.
  {
    #Estimate the improvements.
    cov.improvements = estimateCoverageImprovements(inputrus,break.down)
    setkeyv(cov.improvements,break.down)
    new.cov = merge(cov.improvements,all.cov,by=break.down) #Join the tables together.
    new.cov[,.(Coverage=Coverage.Percentage+cov),by=break.down] #Add this year's coverages to the estimated improvement and output.
  } else {
    all.cov #If non input just output this year's coverage estimates.
  }
}

#Plots some helpful graphs which will help determine the improvement gained from adding certain numbers of units.
plotImprovementGraphs = function(thresholds = c(1/100:10,seq(0.1,1,0.01)),break.down=breakdown)
{ 
  thresholds=sort(thresholds) #These need to be in order.
  bigtabletoastytime = micro.data[,.(thresholdd=thresholds),by=break.down] #The cartesian product of thresholds and the levels of break.down.
  setkeyv(bigtabletoastytime,c('thresholdd',break.down))
  ns = integer() #Vector of sample enlargement sizes.
  for (thresh in thresholds)
  {
    message(paste0("Threshold: ",thresh))
    tempru = performFullEnlargement(thresh,silent = T)
    ns = c(ns,length(tempru)) #Record enlargement size.
    tempimprov = estimateCoverageImprovements(tempru,break.down)
    setkeyv(tempimprov,break.down)
    bigtabletoastytime[.(thresh),cov:=tempimprov[,Coverage.Percentage]] #Record coverages by break.down for this threshold.
  }
  #Might be worth just running the following line and viewing the table.
  #We're going to find each fo these - 'min','max','mean','median' - values taken over the coverages by break.down for each threshold.
  smallertabletoastytime = CJ(threshold=thresholds,vari=c('min','max','mean','median')) 
  setkey(smallertabletoastytime,threshold)
  for (f in c('min','max','mean','median')) #Loop over the function names.
  {
    #Apply f to the coverages by group.
    smallertabletoastytime[vari==f, cover:=bigtabletoastytime[.(threshold),do.call(f,args=.(cov))],by=threshold]
  }
  #Add the enlargement sizes to teh thresholds.
  smallertabletoastytime[,n:=sort(rep(ns,length(unique(vari))),decreasing=T)]
  
  gg = ggplot(data=smallertabletoastytime,aes(x=threshold,y=cover,col=vari))
  print(gg + geom_line() + labs(x="Contribution threshold (%)",y="Coverage improvement (%)") + ggtitle("Number of extra units to add as a function of contribution threshold"))
  
  gg = ggplot(data=smallertabletoastytime[,.(n=unique(n)),by=threshold],aes(x=threshold,y=n))
  print(gg + geom_line() + labs(x="Contribution threshold (%)",y="Number to add") + ggtitle("Number of extra units to add as a function of threshold"))
  
  gg = ggplot(data=smallertabletoastytime,aes(x=n,y=cover,col=vari))
  print(gg + geom_line() + labs(x="Number to add",y="Coverage improvement (%)") + ggtitle("Coverage improvement as a function of extra units added"))
}
  
#Output a list of RUs we want to add to the reference list.
saveRUList=function(inputrus,file.to.write=file.to.write.to)
{
  tooutput=data.frame(sort(inputrus)); names(tooutput)='ru_ref';
  write.csv(tooutput,file.to.write,row.names = FALSE)
}