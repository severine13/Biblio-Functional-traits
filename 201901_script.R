# bibliometrix trait based
# S. Martini 
# May 2018

# Field Tag	Description
# AU	Authors
# TI	Document Title
# SO	Publication Name (or Source)
# JI	ISO Source Abbreviation
# DT	Document Type
# DE	Authors' Keywords
# ID	Keywords associated by SCOPUS or ISI database
# AB	Abstract
# C1	Author Address
# RP	Reprint Address
# CR	Cited References
# TC	Times Cited
# PY	Year
# SC	Subject Category
# UT	Unique Article Identifier
# DB	Bibliographic Database

library(bibliometrix)
library(ggplot2)

repobib="F:/Oceano/publications/publis_martini/2018_review_trait_based/bibliometrix_data/last"
repobib="F:/Oceano/sev-postdocs/LOV2017/traits/bibliometrix_data/20190122/2019histo"
setwd(repobib)
# Read files in the repository with ".bib"
files <- list.files(pattern = "\\.bib$")


# read and load all files
out.file = ""

for (i in 1:length(files)){
Dataset = readFiles(files[i])
M = isibib2dfb(Dataset)
out.file = rbind(out.file,M)
}


####
out.file$SC=gsub("\\\\&",";",out.file$SC)
  listSC=strsplit(as.character(out.file$SC),";")
  listSC=lapply(listSC, function(l) trim(l))
  nSC=unlist(lapply(listSC,length))  # num. of authors per paper
  fracSC=unlist(sapply(nSC,function(x){rep(1/x,x)}))  # fractional frequencies
  SC=unlist(listSC)
  #AU=gsub(" ", "", unlist(listAU), fixed = TRUE)
  #if (M$DB[1]=="ISI"){AU=gsub(" ", "", unlist(listAU), fixed = TRUE)} # delete spaces
  #if (M$DB[1]=="SCOPUS"){AU=sub(" ",",",unlist(listAU),fixed=TRUE);AU=gsub(" ","",AU,fixed=TRUE)}
  # Authors=sort(table(AU),decreasing=TRUE)
  # Authors_frac=aggregate(fracAU,by=list(AU),'sum')
  # names(Authors_frac)=c("Author","Frequency")
  # Authors_frac=Authors_frac[order(-Authors_frac$Frequency),]
  # FirstAuthors=lapply(listAU,function(l) l[1])
  # listAUU=strsplit(as.character(M$AU[nAU>1]),sep)
  # AuMultiAuthoredArt=length(unique(gsub(" ", "", unlist(listAUU), fixed = TRUE)))

 
# results <- biblioAnalysis(out.file, sep = ";")
# S <- summary(object = results, k = 10, pause = FALSE)
# plot(x = results, k = 10, pause = FALSE)
  
##########
outfile=dplyr::filter(out.file,!PY %in% c("","2019"))
ggplot(outfile,aes(x=PY,y=..count..))+geom_bar()+labs(x="Year",y="Number of articles")+theme_classic()+ theme(text=element_text(size=18),axis.text.x=element_text(angle=45,hjust=1))

#treemap
titi=sort(table(SC))
toto=as.data.frame(titi[(length(titi)-15):length(titi)])

toto$labs=paste(toto$SC," - ", round(toto$Freq,2),sep="")

treemap(dtf = toto,
        index=c("labs"),
        vSize="Freq",
        vColor="Freq",
       # palette="Spectral",
        type="index",
        border.col=c("white", "white"),
        fontsize.title = 22,
        fontsize.labels = toto$Freq/4.2,
        algorithm="pivotSize",
        title ="Treemap of the main fields citing trait-based approaches")


# # other try
# library(treemapify)
# treemap_coords <- treemapify(toto, area="Freq")
# 
# 
# ggplot2::ggplot(treemap, ggplot2::aes(area = gdp_mil_usd, fill = region)) +
#   geom_treemap()


# 1993 - 13078
datev=""
# number of publications in marine / freshwater global per year
datev$weight=rev(c(67509,67228,62204,58386,55452,51120,48151,44488,41124,38604,35410,32031,28632,26279,25100,23015,21815,20757,20118,19362,17857,17273,15566,14209,12760,12377))
datev$PY=as.character(c(1991,1992,seq(1994,2017)))

out.file=merge(out.file,datev)

#######
dataset=count(out.file,"PY")
dataset$weight=datev$weight
dataset$ratio=dataset$freq/dataset$weight

ggplot(dataset,aes(x=PY,y=ratio))+geom_bar(stat="identity")+labs(x="Year",y="Number of articles")+theme_classic()+ theme(text=element_text(size=18),axis.text.x=element_text(angle=45,hjust=1))


#################################################
out.file$SC[grep("ECOLOGY",strsplit(out.file$SC, "[;]"))]="ECOLOGY"
out.file$SC[grep("BIODIVERSITY",strsplit(out.file$SC, "[;]"))]="ECOLOGY"
out.file$SC[grep("ZOOLOGY",strsplit(out.file$SC, "[;]"))]="ZOOLOGY"
out.file$SC[grep("ANATOMY",strsplit(out.file$SC, "[;]"))]="ZOOLOGY"
out.file$SC[grep("MICROBIOLOGY",strsplit(out.file$SC, "[;]"))]="MICROBIOLOGY"
out.file$SC[grep("TECHNOLOGY",strsplit(out.file$SC, "[;]"))]="TECHNOLOGY"
out.file$SC[grep("PLANT SCIENCE",strsplit(out.file$SC, "[;]"))]="PLANT SCIENCE"
out.file$SC[grep("FORESTRY",strsplit(out.file$SC, "[;]"))]="PLANT SCIENCE"
out.file$SC[grep("ENVIRONMENTAL SCIENCE",strsplit(out.file$SC, "[;]"))]="ECOLOGY"
out.file$SC[grep("LIFE SCIENCE",strsplit(out.file$SC, "[;]"))]="ECOLOGY"
out.file$SC[grep("GENETICS",strsplit(out.file$SC, "[;]"))]="GENETICS"
out.file$SC[grep("HEREDITY",strsplit(out.file$SC, "[;]"))]="GENETICS"

out.file$SC[grep("NEUROSCIENCES",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("BIOCHEMISTRY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("GEOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("EVOLUTIONARY BIOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("DEVELOPMENTAL BIOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("PHARMACY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("GENERAL",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("GEOCHEMISTRY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("AGRICULTURE",strsplit(out.file$SC, "[;]"))]="PLANT SCIENCE"
out.file$SC[grep("FISHERIES",strsplit(out.file$SC, "[;]"))]="MARINE"
out.file$SC[grep("ENTOMOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("PALEONTOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("PARASITOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("MICROSCOPY",strsplit(out.file$SC, "[;]"))]="TECHNOLOGY"
out.file$SC[grep("HISTORY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("INFECTIOUS DISEASES",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("MATERIALS SCIENCE",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("WATER RESOURCES",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("ANTHROPOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("ARCHAEOLOGY",strsplit(out.file$SC, "[;]"))]="OTHER"
out.file$SC[grep("MATHEMATICS",strsplit(out.file$SC, "[;]"))]="OTHER"

out.file$SC[grep("COMPUTER SCIENCE",strsplit(out.file$SC, "[;]"))]="TECHNOLOGY"
out.file$SC[grep("FRESHWATER BIOLOGY",strsplit(out.file$SC, "[;]"))]="FRESHWATER"
out.file$SC[grep("OCEANOGRAPHY",strsplit(out.file$SC, "[;]"))]="MARINE"
out.file$SC[grep("MARINE",strsplit(out.file$SC, "[;]"))]="MARINE"
# titre journal ou titre
out.file$SC[grep("MARINE",strsplit(out.file$ID, "[ ]"))]="MARINE"
out.file$SC[grep("MARINE",strsplit(out.file$SO, "[ ]"))]="MARINE"
out.file$SC[grep("MARINE",strsplit(out.file$TI, "[ ]"))]="MARINE"

out.file$SC[grep("FRESHWATER",strsplit(out.file$TI, "[ ]"))]="FRESHWATER"
out.file$SC[grep("FRESHWATER",strsplit(out.file$SO, "[ ]"))]="FRESHWATER"
out.file$SC[grep("FRESHWATER",strsplit(out.file$ID, "[ ]"))]="FRESHWATER"



### 
out.file=out.file[-which(out.file$SC=="PLANT SCIENCE"),]

library(plyr)
counts <- ddply(out.file, .(out.file$SC, out.file$PY), nrow)
names(counts) <- c("SC", "PY", "Freq")
counts=merge(counts,datev)
counts$ratio=counts$Freq/counts$weight*100
ggplot(counts,aes(x=PY,y=ratio,fill=SC))+geom_bar(stat="identity",position="fill")+labs(x="Year",y="Ratio of articles")+scale_fill_brewer(palette = "Set3")+theme_classic()+ theme(text=element_text(size=18),axis.text.x=element_text(angle=45,hjust=1))

counts$SC=factor(counts$SC,levels=c("ECOLOGY","FRESHWATER","MARINE","TECHNOLOGY","GENETICS","ZOOLOGY","MICROBIOLOGY","OTHER"))
# save plots
jpeg("filled_plot_ratio1.jpg",width=1000,height=400)
ggplot(counts,aes(x=PY,y=ratio,fill=SC))+geom_bar(stat="identity",position="fill")+labs(x="Year",y="Ratio of articles")+scale_fill_brewer(palette = "Set3")+theme_classic()+ theme(text=element_text(size=18),axis.text.x=element_text(angle=45,hjust=1),legend.title=element_blank())
dev.off()

jpeg("filled_plot_ratio2.jpg",width=1000,height=400)
ggplot(counts,aes(x=PY,y=ratio,fill=SC))+geom_bar(stat="identity")+labs(x="Year",y="Percentage of articles")+scale_fill_brewer(palette = "Set3")+theme_classic()+ theme(text=element_text(size=18),legend.title=element_blank(),axis.text.x=element_text(angle=45,hjust=1))
dev.off()


