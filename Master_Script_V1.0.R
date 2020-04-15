##required packages
library(openxlsx)

###add ontology read

##add final metadata, implement automated inference 
##make sure metdata and variable definitions are not overwritten if present!!!!!

###


###set working directory 
wd=getwd()
wd=gsub("scripts","", wd)
setwd(wd)


####make directories for data
path.data.raw=paste(getwd(),"/data/raw/",sep="")
path.data.proc=paste(getwd(),"/data/processed/",sep="")
path.data.op=paste(getwd(),"/data/definitions_protocols/",sep="")

path.writing=paste(getwd(),"/writing/",sep="")
path.results.raw=paste(getwd(),"/results/raw",sep="")
path.results.tab=paste(getwd(),"/results/tables",sep="")
path.results.im=paste(getwd(),"/results/figures",sep="")
path.scripts=paste(getwd(),"/scripts/",sep="")


dir.create(path.data.raw, showWarnings=F, recursive=T)
dir.create(path.data.proc, showWarnings=F, recursive=T)
dir.create(path.data.op, showWarnings=F, recursive=T)
dir.create(path.writing, showWarnings=F, recursive=T)
dir.create(path.results.raw, showWarnings=F, recursive=T)
dir.create(path.results.tab, showWarnings=F, recursive=T)
dir.create(path.results.im, showWarnings=F, recursive=T)
dir.create(path.scripts, showWarnings=F, recursive=T)


####check presence of data files and list
data.list.wd= list.files(wd)
data.list.wd = data.list.wd[grep("metadata", data.list.wd,invert=T)]
data.list.wd = data.list.wd[grep("readme", data.list.wd,invert=T)]
writing.list.wd=data.list.wd[unique(c(grep(".doc", data.list.wd),grep(".docx", data.list.wd),grep(".txt", data.list.wd),grep(".rtf", data.list.wd)))]
data.list.wd= data.list.wd[unique(c(grep(".csv", data.list.wd),grep(".xls", data.list.wd)))]

data.list.raw= list.files(path.data.raw)
writing.list= list.files(path.writing)
##remove metedata
data.list.raw= data.list.raw[grep("metadata",data.list.raw,invert=T)]
data.list.proc= list.files(path.data.proc)

##if raw folder is empty, move data from main folder to raw

if(length(data.list.raw)==0){
file.copy(data.list.wd, path.data.raw)	
file.remove(data.list.wd)	

data.list.raw= list.files(path.data.raw)
##remove metedata
data.list.raw= data.list.raw[grep("metadata",data.list.raw,invert=T)]
	
}


if(length(writing.list)==0){

file.copy(writing.list.wd, path.writing)	
file.remove(writing.list.wd)	
	
}
	


##now read all excel workbooks and sheets within workbooks

for(wb in data.list.raw){


if(grepl(".xlsx",wb)){

wb.name=gsub(".xlsx","", wb)	
file=paste(path.data.raw, wb,sep="/")
sheets=getSheetNames(file)	
	



}	



if(grepl(".csv",wb)){

wb.name=gsub(".csv","", wb)	
file=paste(path.data.raw, wb,sep="/")
sheets="data"	
	
}	



##create workbook to store data and metadata
meta.wb.name=paste(wb.name,"_metadata",sep="")
meta.wb <- createWorkbook(meta.wb.name)


##store variable names
var.names=c()
unit.vec=c()
sheet.names=c()

for(st in sheets){

if(grepl(".xlsx",wb)){
t1<-read.xlsx(file,detectDates=T,sheet=st, colNames=F)
}

if(grepl(".csv",wb)){
t1<-read.csv(file, header =F,as.is =T)
}

##skip empty worksheets
if(is.null(t1)) next

##count NA to detect header in excel sheet
#na count function that sets "" to NA (for csv)
na.count.fun=function(x){
x[which(x=="")]=NA
out=sum(!is.na(x))	
return(out)
	}

na.count=apply(t1,1, na.count.fun)
ncol=max(na.count)
head.row=which(na.count==ncol)[1]

##make dataframe from sheet with colum names
colnames=as.character(t1[head.row,])

###remove NA from names
miss.name=which(is.na(colnames)|colnames=="NA")
colnames[miss.name]=paste("X",1:length(miss.name),sep="")

###fish units from names using $  ##when units flag that specification of methods and defenitions of measurements (e.g. reported vs measured, dr vs fresh weight.)
colnames=lapply(colnames,function(x) unlist(strsplit(x,split="\\$")))

contains.unit=unlist(lapply(colnames,function(x) length(x)>1 ))

units=rep("",length(colnames))

units[contains.unit]=unlist(lapply(colnames[contains.unit],function(x) x[2]))

colnames=unlist(lapply(colnames,function(x) x[1]))

new.frame= t1[(head.row+1):nrow(t1),1:length(colnames)]
colnames(new.frame)= colnames


var.names=c(var.names, colnames)
unit.vec=c(unit.vec, units)
sheet.names=c(sheet.names,rep(st,length(colnames)))


addWorksheet(meta.wb, st)
writeData(meta.wb, st, new.frame)


}

##make data frame with workbook name, sheet names and variables and add column for definitions

var.frame=data.frame("workbook"= wb.name,"sheet"= sheet.names,"variable"= var.names,"unit"=unit.vec,"definition"=NA,"unique identifier"=0,"personal information"=0, stringsAsFactors = F)	

#set up metadata
#mid.names=c("ID","Country","Name region","Name site","Minimum latitude","Maximum latitude", "Minimum longitude","Maximum longitude","Experiment/survey","Type of experiment","Type of survey","On-farm/on-station","Crops","Animals","Soil type")
#meta.data.frame=read.xlsx(paste(path.data.op,"Required_Metadata.xlsx",sep="/"))

field.vec=c("Data ID", "Official title of the dataset", "Project name", "Description of project", "Author", "Author ID(ORCID)", "Contributor(s)", "Subject matter of research/Vocabulary", "Data origin", "Funder(s) or sponsor(s) of project", "creation date (m/d/yyyy)", "Embargo end date", "Citation", "keywords (AGROVOC)", "Country(ies) covered", "Point longitude coord. in Dec. Degrees ", "Agro-Ecological Zone(s)(FAO) covered", "Years covered by data", "Crops covered by data", "Animals covered by data", "Start date of data collection ", "End date of data collection ", "License (default=CC-BY)", "Permission given by email", "Rights", "Contact email")

field_name.vec=c("data.id", "data.title", "project.name", "project.description", "author", "orcid", "contributors", "subject.research", "data.origin", "donor", "date.creation", "date.embargo", "citation", "keywords.agrovoc", "countries", "longitude", "aez", "years", "crops", "animals", "date.collect.start", "date.collect.end", "licence", "permission", "rights", "contact.mail")

meta.data.frame=data.frame(field= field.vec,field_name= field_name.vec,values=NA, stringsAsFactors = F)	


###check if workbook with variable definitions and metadata already present in processed and make sure not overwritten
exist.wb= data.list.proc[grep(wb.name, data.list.proc)]

if(length(exist.wb)>0){
##read.data and extract
exist.file=paste(path.data.proc,exist.wb,sep="/")
existing.varframe=read.xlsx(exist.file,detectDates=T,sheet= "variable definitions")
existing.meta.data.frame=read.xlsx(exist.file,detectDates=T,sheet= "meta data")	

existing.meta.data.frame.id=apply(existing.meta.data.frame[,1:2],1,paste,collapse=";")
meta.data.frame.id=apply(meta.data.frame[,1:2],1,paste,collapse=";")


existing.varframe.id=apply(existing.varframe[,1:3],1,paste,collapse=";")
var.frame.id=apply(var.frame[,1:3],1,paste,collapse=";")


##overwrite new frames with existing values
meta.data.frame[na.omit(match(existing.meta.data.frame.id,meta.data.frame.id)),]<-existing.meta.data.frame[na.omit(match(meta.data.frame.id,existing.meta.data.frame.id)),]
var.frame[na.omit(match(existing.varframe.id,var.frame.id)),]<-existing.varframe[na.omit(match( var.frame.id,existing.varframe.id)),]

##rbind additional rows
meta.data.frame=rbind(meta.data.frame, existing.meta.data.frame[which(!existing.meta.data.frame.id%in% meta.data.frame.id),])
var.frame =rbind(var.frame, existing.varframe[which(!existing.varframe.id%in%var.frame.id),])

	
}


##now add metadata to metadata workbook
addWorksheet(meta.wb, "meta data")
addWorksheet(meta.wb, "variable definitions")

writeData(meta.wb, "meta data", meta.data.frame)
writeData(meta.wb, "variable definitions", var.frame)

##now save workbook
saveWorkbook(meta.wb,file = paste(path.data.proc,meta.wb.name,".xlsx",sep=""), overwrite = T) ##later do not overrwrite but make sure merged/aggregated



}

