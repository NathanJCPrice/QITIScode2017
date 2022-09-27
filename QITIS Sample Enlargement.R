### Code written by Nathan Price (Methodology, SD+E BAR). 
### Feel free to email me on Nathan.Price@ons.gov.uk if you have a question / need advice. ###

library(data.table)
library(bit64)

microdata.file="aitis_final_base_data_2015.csv" #Location of the microdata file.
dead.ru.list.file="idbr455out.csv" #Location of a list of RUs with dates next to the dead ones.
starting.sample.file="Annual to Quarterly - 2015 annual to 2017 Q1.csv" #Current sample list.
annual.total.country.import.estimates.file="geog totals finals 2015 imports.csv" #Estimated import totals.
annual.total.country.export.estimates.file="geog totals finals 2015 exports.csv" #Estimated export totals.
countries.of.interest.file="Country list.csv" #This is a list of our \emph{countries of interest}.
industry.totals.file='Industry Totals.csv' #A list of the industry (section) totals
output.file='Enlarged RU List QITIS 2017.csv'



importData=function()
{
  countrynames<<-read.csv(countries.of.interest.file) #Read in the countries that QITIS is interested in.
  countries <<- countrynames$Code #Forget the other details.
  countrynames <<- countrynames$Country
  
  microdata<<-fread(microdata.file,select=c('ru_ref','current_sic','q201','q202','q204_i','q203','concern','gor'),colClasses = c(q202='factor')) #Import the relevant columns into a data table.
  setkey(microdata,concern,q202)
  
  microdata<<-microdata[!(concern==70),] #Filter out ABS data.
  microdata<<-microdata[!(q202=='00' | q202=='0'),] #Filter out these.
  
  setkey(microdata,q203,ru_ref)
  
  ru.country.data<<-microdata[q203 %in% countries,] #Filter out entries not related to our countries of interest.
  
  setkey(ru.country.data, ru_ref)
  
  deadrus<<-read.csv(dead.ru.list.file,header=FALSE)
  deadrus <<- deadrus$V1[!(gsub(' ','',as.character(deadrus$V2)) == "")] #Find where the dates are and filter out those RUs.
  ru.country.data<<-ru.country.data[!(ru_ref %in% as.integer64(deadrus))] #as.integer64 is used as ru_ref is imported as a data table while our deadrus is imported as a data frame.
  
  oldrus <<- read.csv(starting.sample.file,header=TRUE)
  #oldrus <<- (1:1)[0]
  oldrus <<- sort(unique(oldrus[,1]))
  oldrus <<- oldrus[!(oldrus %in% deadrus)] #Remove dead RUs.
  oldrus <<- oldrus[as.integer64(oldrus) %in% microdata[,ru_ref]] #Stops any RUs from slipping into the Quarterly panel without first appearing in the AITIS panel.
  
  setkey(ru.country.data,q203,q201,ru_ref)
  ru.country.data <<- ru.country.data[,sum(q204_i),by=key(ru.country.data)] #Calculate each RUs combination total.
  setnames(ru.country.data,'V1','q204') #Name our new column 'q204'.
  setkey(ru.country.data,q203,q201)
  surveytotals=ru.country.data[,sum(q204),by=key(ru.country.data)] #Calculate combination survey totals.
  setnames(surveytotals,'V1','q204') #Name our new column 'q204'.
  setkey(surveytotals,q203,q201)
  getSurveyTotal=function(c,q){surveytotals[.(c,q),q204]} #Define a handy function that returns the survey total for a specific combination.
  ru.country.data[,surveycontribution:=100*q204/getSurveyTotal(q203,q201)] #Calculate each RUs contribution to each combination.
  maxsurveycontribution<<-ru.country.data[,max(surveycontribution),by=ru_ref] #Calculate the maximum survey contribution of each RU.
  setkey(maxsurveycontribution,V1)
  
  ####Read in the annual published estimates and filter out those that aren't relevant.
  ptots <<- read.csv(annual.total.country.import.estimates.file)
  rtots <<- read.csv(annual.total.country.export.estimates.file)
  ptots <<- ptots[ptots$Country.code %in% countries,]
  rtots <<- rtots[rtots$Country.code %in% countries,]
  #The following function may appear a little bit wacky but is very benign. It takes vector inputs of countries and q201 codes and outputs a vector of relevant totals for each.
  getAnnualTotal=function(c,q){u=c; for (i in 1:length(c)){u[i]=ifelse(q[i]=='P',ptots$Total.exports[ptots$Country.code==c[i]],rtots$Total.exports[rtots$Country.code==c[i]])}; return(u);}
  #Calculate annual contribution for each RU and each combination.
  ru.country.data[,annualcontribution:=100*q204/getAnnualTotal(q203,q201)]
  setkey(ru.country.data, ru_ref, q203, q201)
  
  
  ru.industry.data <<- microdata
  #Order the table on the following variables, the variables being ordered by importance...
  setkey(ru.industry.data,q203)
  #Filter the ABS data (concern=70) and the world data (q203=167).
  ru.industry.data<<-ru.industry.data[q203==167]
  #Convert the SIC into a division.
  ru.industry.data[,current_sic := floor(current_sic/1000)]
  setkey(ru.industry.data,ru_ref,current_sic)
  
  ru.industry.data<<-ru.industry.data[!(ru_ref %in% as.integer64(deadrus))]
  
  setkey(ru.industry.data,ru_ref,current_sic)
  section<<-read.csv(industry.totals.file) #Get section totals.
  section$And[is.na(section$And)]<<-999999; section$And.End[is.na(section$And.End)]<<-999999; #Changes na values to a very large number which helps avoid complications elsewhere.
  
  L = function(x){return(x[length(x)])} #Faster than the tail function.
  getSection = function(y) #Defines a function that, given a division, returns a number corresponding to a section.
  {
    if(length(y)>1){
      for(i in 1:length(y))
      {
        y[i]=getSection(y[i])
      }
      return(y)
    }
    if (y %in% max(section$Start[section$Start <= y]):max(section$End[section$Start <= y]))
    {
      return(L(section$Section[section$Start <= y]))
    } else if (y %in% max(section$And[section$And <= y]):max(section$And.End[section$And <= y]))
    {
      return(L(section$Section[section$And <= y]))
    } else {return(NA)}
  }
  
  ru.industry.data[,sect:=levels(getSection(1))[getSection(current_sic)]] #Add in a section column to inddataperm.
  
  indoeuropeansettlers=ru.industry.data[,sum(q204_i),by=c('sect','q201')] #Get survey combination totals.
  setkey(indoeuropeansettlers,sect,q201)
  getTotal = function(s,q) #A handy function that returns survey totals for a given combination.
  {
    return(indoeuropeansettlers[.(s,q),V1])
  }
  
  ru.industry.totals<-ru.industry.data[,100*sum(q204_i)/getTotal(sect,q201),by=c('ru_ref','sect','q201')] #Calculate survey contributions.
  setnames(ru.industry.totals,'V1','Cont')
  ru.best.ind.contribution<<-ru.industry.totals[,max(Cont),by=c('ru_ref','sect')] #Get the maximum survey contribution for each combination.
  setnames(ru.best.ind.contribution,'V1','BestContribution')
  setkey(ru.best.ind.contribution,BestContribution)
}



enlargeByCountry=function(inputrus, target.threshold=NULL, target.samplesize=NULL)
{
  if(!is.null(target.threshold))
  {
    newrus=maxsurveycontribution[V1>target.threshold,ru_ref]
    rus=sort(unique(c(newrus,as.integer64(inputrus),-1)))[-1]# Take the union of newrus and oldrus to get the new sample's RU list. 
    #The -1? Why is it there? Well, because R is sometimes nonsensical, the 'unique' function applied to an empty integer64 vector throws an error, so we put in the -1 just in case we're performing an analysis which leads to oldrus and newrus both being empty. We use the [-1] part to drop it as we know that RU reference numbers are always >0 so -1 will come first in this vector and so always be dropped.
    return(rus)
  }
  if(!is.null(target.samplesize))
  {
    prevsamplesize=-5
    rus=numeric()
    samplesize=0
    threshold=50
    tolerance=25
    while(abs(samplesize-target.samplesize)>0 & (abs(samplesize-prevsamplesize)>0 | tolerance>2^-20))
    {
      prevsamplesize=samplesize
      rus=enlargeByCountry(inputrus,threshold)
      samplesize=length(rus)
      
      print(paste('Threshold:', signif(threshold), 'Sample size:', samplesize,sep=' '))
      
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
  print('Neither threshold nor samplesize specified.')
  return(Lazy.Mans.Way.Of.Throwing.An.Error)
}

enlargeByIndustry=function(inputrus, target.threshold=NULL, target.samplesize=NULL)
{
  if(!is.null(target.threshold))
  {
    sample=ru.best.ind.contribution[BestContribution>target.threshold]
    rus=sort(unique(c(sample[,ru_ref],as.integer64(inputrus),-1)))[-1]# Take the union of newrus and oldrus to get the new sample's RU list. 
    #The -1? Why is it there? Well, because R is sometimes nonsensical, the 'unique' function applied to an empty integer64 vector throws an error, so we put in the -1 just in case we're performing an analysis which leads to oldrus and newrus both being empty. We use the [-1] part to drop it as we know that RU reference numbers are always >0 so -1 will come first in this vector and so always be dropped.
    return(rus)
  }
  if(!is.null(target.samplesize))
  {
    prevsamplesize=-5
    rus=numeric()
    samplesize=0
    threshold=50
    tolerance=25
    while(abs(samplesize-target.samplesize)>0 & (abs(samplesize-prevsamplesize)>0 | tolerance>2^-20))
    {
      prevsamplesize=samplesize
      rus=enlargeByIndustry(inputrus,threshold)
      samplesize=length(rus)
      
      print(paste('Threshold:', signif(threshold), 'Sample size:', samplesize,sep=' '))
      
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
  print('Neither threshold nor samplesize specified.')
  return(Lazy.Mans.Way.Of.Throwing.An.Error)
}



getIndustryCoverages=function(ruX)
{
  inddata=ru.industry.data[ru_ref %in% ruX]
  
  
  #Create a data frame with as many rows as there are divisions.
  dfi=data.frame(1:length(unique(inddata[,current_sic])))
  #Name columns.
  names(dfi)=c('Total.Exports')
  dfi$Total.Imports=dfi$Total.Exports
  #Reorder the data table.
  setkey(inddata,current_sic,q201,ru_ref)
  #Create the industry column as a list of industries.
  dfi['Industry']=sort(unique(inddata[,current_sic]))
  #Loop through industries and calculate the survey totals for each.
  for(i in sort(unique(inddata[,current_sic])))
  {
    #Exports.
    dfi$Total.Exports[dfi$Industry==i] = inddata[current_sic==i & q201=='R',sum(q204_i)]
    #Imports.
    dfi$Total.Imports[dfi$Industry==i] = inddata[current_sic==i & q201=='P',sum(q204_i)]
  }
  
  #Creates the data frame that will be outputted.
  IndustryCoverages=data.frame(matrix(NA,ncol=3,nrow=nrow(section)))
  names(IndustryCoverages) = c('IndustrySection','Coverage.Exports','Coverage.Imports')
  IndustryCoverages$IndustrySection=section$Section
  #Loops through sections dataframe collating industry totals into section totals.
  for (i in 1:nrow(section))
  {
    #The sections table contains information on the start and end of the intervals for which section division values fall.
    IndustryCoverages$Coverage.Exports[i] = sum(dfi$Total.Exports[dfi$Industry %in% section$Start[i]:section$End[i]])
    IndustryCoverages$Coverage.Imports[i] = sum(dfi$Total.Imports[dfi$Industry %in% section$Start[i]:section$End[i]])
    
    #This is used in the case of aggregating sections. We can define a second interval in the sections table and then those two sections will be merged.
    if(!is.na(section$And[i]))
    {
      IndustryCoverages$Coverage.Exports[i] = IndustryCoverages$Coverage.Exports[i] + sum(dfi$Total.Exports[dfi$Industry %in% section$And[i]:section$And.End[i]])
      IndustryCoverages$Coverage.Imports[i] = IndustryCoverages$Coverage.Imports[i] + sum(dfi$Total.Imports[dfi$Industry %in% section$And[i]:section$And.End[i]])
    }
    #print(section$Start[i]:section$End[i])
    
    #Calculate the annual coverage for the industries.
    IndustryCoverages$Coverage.Exports[i] = 100*IndustryCoverages$Coverage.Exports[i]/(section$Exports[i])
    IndustryCoverages$Coverage.Imports[i] = 100*IndustryCoverages$Coverage.Imports[i]/(section$Imports[i])
  }
  #Output the table.
  return(IndustryCoverages)
}

getCountryCoverages=function(ruX)
{
  #Filter to keep only those entries corresponding to the RUs in ruX and then sum the contributions over combinations.
  result=ru.country.data[ru_ref %in% ruX][,sum(annualcontribution),by=c('q203','q201')]
  setkey(result,q203,q201)
  
  
  dfc=data.frame(countrynames) #Create the data frame to output.
  names(dfc)='Country'
  dfc$Total.Imports=dfc$Total.Exports=0 #Initialise relevant columns.
  for (i in 1:length(countrynames))
  {
    #For each combination output the total contribution into the relevant cell.
    dfc$Total.Imports[i]=ifelse(is.na(result[.(countries[i],'P'),V1]),0,result[.(countries[i],'P'),V1])
    dfc$Total.Exports[i]=ifelse(is.na(result[.(countries[i],'R'),V1]),0,result[.(countries[i],'R'),V1])
  }
  return(dfc) #Done. :)
}



plotCountryGraphs=function(inputrus,thresholds,ylim=c(40,90))
{
  samplesizes=numeric()
  meancoverages=numeric()
  mincoverages=numeric()
  mediancoverages=numeric()
  for(threshold in thresholds)
  {
    rus=enlargeByCountry(inputrus,target.threshold = threshold)
    samplesizes=c(samplesizes,length(rus)) #Record the latest sample size.
    coverages=ru.country.data[ru_ref %in% rus,sum(annualcontribution),by=c('q203','q201')][,V1] #Calculate the combination coverages.
    meancoverages=c(meancoverages,mean(coverages)) #Record the latest mean coverage.
    mincoverages=c(mincoverages,min(coverages)) #     "     minimum  " .
    mediancoverages=c(mediancoverages,median(coverages)) #     "     median  " .
    
    #points(threshold+coverages*0,coverages)
  }
  
  plot(thresholds,samplesizes,type = 'l',xlab = '% Contribution Threshold (Survey)', ylab = 'Sample Size', main = 'Sample size against required contribution threshold (Country)')
  plot(thresholds,mincoverages,type = 'l',ylim=ylim,xlab = '% Contribution Threshold (Survey)',ylab = 'Combination coverage (Annual)',main='Combination coverage against required contribution threshold (Country)')
  lines(thresholds,meancoverages,col='red')
  lines(thresholds,mediancoverages,col='blue')
  
  plot(samplesizes,meancoverages,col='red',type = 'l',xlab = 'Samplesize',ylab = 'Combination coverage (Annual)',main='Sample Size against Mean Coverage (Country)')
  lines(samplesizes,mediancoverages,col='blue')
}

plotIndustryGraphs = function(inputrus,thresholds,ylim=c(40,90))
{
  samplesizes=numeric()
  meancoverages=numeric()
  mincoverages=numeric()
  mediancoverages=numeric()
  for(threshold in thresholds)
  {
    rus=enlargeByCountry(inputrus,target.threshold = threshold)
    samplesizes=c(samplesizes,length(rus)) #Record the latest sample size.
    coverages=unlist(getIndustryCoverages(rus)[c('Coverage.Exports','Coverage.Imports')]) #Calculate the combination coverages.
    meancoverages=c(meancoverages,mean(coverages)) #Record the latest mean coverage.
    mincoverages=c(mincoverages,min(coverages)) #     "     minimum  " .
    mediancoverages=c(mediancoverages,median(coverages)) #     "     median  " .
    
    #points(threshold+coverages*0,coverages)
  }
  
  plot(thresholds,samplesizes,type = 'l',xlab = '% Contribution Threshold (Survey)', ylab = 'Sample Size', main = 'Sample size against required contribution threshold (Industry)')
  plot(thresholds,mincoverages,type = 'l',ylim=ylim,xlab = '% Contribution Threshold (Survey)',ylab = 'Combination coverage (Annual)',main='Combination coverage against required contribution threshold (Industry)')
  lines(thresholds,meancoverages,col='red')
  lines(thresholds,mediancoverages,col='blue')
  
  plot(samplesizes,meancoverages,col='red',type = 'l',xlab = 'Samplesize',ylab = 'Combination coverage (Annual)',main='Sample Size against Mean Coverage (Industry)')
  lines(samplesizes,mediancoverages,col='blue')
}



saveRUList=function(inputrus,file.to.write.to)
{
  tooutput=data.frame(sort(inputrus)); names(tooutput)='ru_ref';
  write.csv(tooutput,file.to.write.to,row.names = FALSE)
}