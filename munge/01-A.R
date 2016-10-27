#description     :ASRZW analysis preprocessing script.
#author		       : Rylan Shearn
#date            :20161015
#version         :0
#usage		       :this will be run automatically when running analyses from within the src/ folder
#notes           :Contact RS for input data
#r_version       :3.3.1 (2016-06-21) -- "Bug in Your Hair"
#==============================================================================

#########################################################################
#uncomment data loading to do more intensive tests

#read in pipoly data
#nature15380.s2 <- read.csv(file = "data/nature15380-s2.csv")
#read in zw data
#rsbl20120083supp1 <- read.csv(file = "data/rsbl20120083supp1.csv")
#read in taxonomy information data
#BirdLife.Checklist.Version.8 <- read.csv(file = "data/BirdLife_Checklist_Version_8.csv")
#read in data from Szekely et al (2014)
#this data also contains data for all species that were in Owens and Bennett (1994)
#rspb20140342supp2 <- read.csv(file = "data/rspb20140342supp2.csv")

#########################################################################
#rename dataframes
tax <- BirdLife.Checklist.Version.8 #taxonomy info
zw <- rsbl20120083supp1 #zw data
pipoly <- nature15380.s2 #pipoly et al (2015) data
szekely <- rspb20140342supp2 #data from Szekely et al (2014) - also in Owens and Bennett (1994)
owens <- OwensBennett1994 #owens & bennett (1994) data

#clear original dataframes
rm(BirdLife.Checklist.Version.8, rsbl20120083supp1, nature15380.s2, rspb20140342supp2, OwensBennett1994)

#########################################################################
# adjust tax data

#re-assign column names of tax data as the second row (first was not real header)
tax <- sapply(tax, as.character) #convert to character df
colnames(tax) <- tax[1, ] #assign names
colnames(tax) <- gsub(' ', '.', colnames(tax)) #replace space with '.'
tax <- as.data.frame(tax[-1,]) #remove 1st row

#subset tax data
tax <- subset(tax, select=c(Order,Family.name,Family,Scientific.name,Authority,Synonyms))
#change name of name column
colnames(tax)[which(names(tax) == "Scientific.name")] <- "Name"
#convert Order text to lower case
tax$Order <- tolower(tax$Order)
#function to capitalize first letter of column
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

#convert Order text first letter to upper case
tax$Order <- capFirst(tax$Order)
#split synonyms into multiple columns using tidyr
library(tidyr)
tax <-separate(data = tax, col = Synonyms, into = c("Synonym1","Synonym2","Synonym3","Synonym4"), sep = "\\; ")
#ensure blank cells have 'NA'
tax <- apply(tax, 2, function(x) gsub("^$|^ $", NA, x))

#########################################################################
# adjust szekely data

#change name of ASR column
colnames(szekely)[which(names(szekely) == "Adult.sex.ratio")] <- "ASR"

#for row in ASR
#if empty
#insert male mortality/fem mort rate
szekely <- within(szekely,
                  ASR.mort.fill <- ifelse(!is.na(ASR),ASR,Male.adult.mortality.rate/(Male.adult.mortality.rate + Female.adult.mortality.rate))
)

#for row in ASR: insert male mortality/fem mort rate regardless of original data
szekely <- within(szekely,
                  ASR.mort.overwrite <- ifelse(!is.na(Male.adult.mortality.rate),Male.adult.mortality.rate/(Male.adult.mortality.rate + Female.adult.mortality.rate),ASR)
)

#for row in ASR: insert male mortality/fem mort rate in new column
szekely <- within(szekely,
                  ASR.mort <- Male.adult.mortality.rate/(Male.adult.mortality.rate + Female.adult.mortality.rate)
)

#add suffix to end of column names
colnames(szekely) <- paste(colnames(szekely), "szekely", sep = ".")
#remove suffix from name column
names(szekely)[1] <- c("Name")
#remove rows that have empty name
szekely <- szekely[!(is.na(szekely$Name) | szekely$Name==""), ]

#########################################################################
# adjust owens data

#rename adult mortality column names to match szekely data
colnames(owens)[which(names(owens) == "mortalityrate.adult.male")] <- "Male.adult.mortality.rate"
colnames(owens)[which(names(owens) == "mortalityrate.adult.female")] <- "Female.adult.mortality.rate"

#make ASR column based on adult mortality
owens$ASR <- owens$Male.adult.mortality.rate/(owens$Male.adult.mortality.rate + owens$Female.adult.mortality.rate)

#add suffix to end of column names
colnames(owens) <- paste(colnames(owens), "owens", sep = ".")
#remove suffix from name column
names(owens)[1] <- c("Name")

#########################################################################
# adjust pipoly data

#change name of ASR column and others
colnames(pipoly)[which(names(pipoly) == "Adult.sex.ratio..")] <- "ASR"
colnames(pipoly)[which(names(pipoly) == "Body.size..")] <- "Body.size"
colnames(pipoly)[which(names(pipoly) == "Sexual.size.dimorphism..")] <- "Sexual.size.dimorphism"
colnames(pipoly)[which(names(pipoly) == "abs.Latitude..")] <- "abs.Latitude"
colnames(pipoly)[which(names(pipoly) == "Adult.sex.ratio...Latitude.reference")] <- "Adult.sex.ratio.Latitude.reference"
colnames(pipoly)[which(names(pipoly) == "Body.size...Sexual.size.dimorphism.reference")] <- "Body.size.Sexual.size.dimorphism.reference"
#pull out only birds from ratio data
pipoly <- pipoly[(pipoly$Taxon=="bird"),]
#add suffix to end of column names
colnames(pipoly) <- paste(colnames(pipoly), "pipoly", sep = ".")
#remove suffix from name column
names(pipoly)[1] <- c("Name")
#replace underscores with spaces for Name column
pipoly$Name <- gsub('_', ' ', pipoly$Name)

#########################################################################
# merge adjusted pipoly, owens and szekely data

asrData <- data.frame(merge(szekely,pipoly, by="Name", all=TRUE))
# merge to owens data
asrData <- data.frame(merge(asrData,owens, by="Name", all=TRUE))

#combine the ratios from szekely and owens (owens has some not included in szekely)

#copy szekely columns to new ones
asrData$ASR.mort.fill <- asrData$ASR.mort.fill.szekely
asrData$ASR.mort.overwrite <- asrData$ASR.mort.overwrite.szekely
asrData$ASR.mort <- asrData$ASR.mort.szekely

#for row in ASR.mort.fill, if empty, insert male mortality/fem mort rate from owens into ASR.mort.fill
asrData <- within(asrData,
                  ASR.mort.fill <- ifelse(is.na(ASR.mort.fill),ASR.owens,ASR.mort.fill)
)

#for row in ASR.mort.overwrite, if empty, insert male mortality/fem mort rate from owens into ASR.mort.overwrite
#this is basically ASR overwritten with mortality based ASR where possible,
#but giving preference to Szekely estimates rather than Owens
asrData <- within(asrData,
                  ASR.mort.overwrite <- ifelse(is.na(ASR.mort.overwrite),ASR.owens,ASR.mort.overwrite)
)

#for row in ASR.mort.szekely, if empty, insert male mortality/fem mort rate from owens into ASR.mort
#this is mortality based ASR without being complemented by other estimate types,
#but giving preference to Szekely estimates rather than Owens
asrData <- within(asrData,
                  ASR.mort <- ifelse(is.na(ASR.mort.szekely),ASR.owens,ASR.mort)
)

#remove rows of ASR that have identical names (fail safe)
asrData <- subset(asrData, !duplicated(asrData[,1]))
               
#########################################################################
# adjust zw data 

# replace '_' with '.' for colnames
colnames(zw) <- gsub('_', '.', colnames(zw)) 

#########################################################################
# merge ASR with ZW

#merge species with ratio and zw, including missing data elements
datafull <- merge(data.frame(merge(asrData,zw, by="Name", all.y=TRUE)), #species with ratio but not ZW length
                  data.frame(merge(zw,asrData, by="Name", all.y=TRUE)), #species with zw length but not ratio
                  all=TRUE)
#merge species taxonomy information with sex ratio and zw data
datafull <- merge(tax,datafull, all.y=TRUE)
#remove duplicated rows of datafull
datafull <- datafull[!duplicated(datafull), ]

#########################################################################
# Replacing incorrect scientific names (synonyms)

#take subset of datafull - taking only rows without NA for any syns
notax <- subset(datafull, !is.na(Synonym1) | !is.na(Synonym2) | !is.na(Synonym3) | !is.na(Synonym4)) 
#remove subset tax columns
syns <- notax[ , !(names(notax) %in% c("Order","Family", "Family.name", "Authority", "Synonyms", "Synonym1", "Synonym2", "Synonym3", "Synonym4"))]

#function for merging synonyms and removing NA rows
mergeSyn <- function(x,print=TRUE) {
  #overwrite temp with syns
  temp <- syns
  #rename temp 'Name' column as x
  colnames(temp)[which(names(temp) == "Name")] <- x
  #merge subset with tax data based on 'Synonym1' column into new df
  result <- data.frame(merge(tax,temp, by=x, all.y=TRUE))
  #remove rows with NA for order
  result <- subset(result, !is.na(Order)) 
  return(result)
}

#run synonym merging function for all synonyms while pasting them together, overwriting syns
syns <- rbind(notax, mergeSyn("Synonym1"), mergeSyn("Synonym2"), mergeSyn("Synonym3"), mergeSyn("Synonym4")) 
#remove rows that had any synonyms in datafull
datafull <- subset(datafull, is.na(Synonym1) & is.na(Synonym2) & is.na(Synonym3) & is.na(Synonym4)) 
#rbind new renamed structure for found synonyms back to datafull
datafull <- rbind(datafull,syns)


#########################################################################
## data labelling prep for Z.W ratio distribution plots

#add column of labels indicating if value present for ASR.szekely, ASR.mort.overwrite, ASR.fill, ASR.pipoly
datafull <- within(datafull,
                   ASR.szekely.status <- ifelse(!is.na(ASR.szekely),
                                                "From szekely et al. (2014)",
                                                "Not from szekely et al. (2014)")
)

#add column of labels indicating if value is not NA for ASR.pipoly
datafull <- within(datafull,
                   ASR.pipoly.status <- ifelse(!is.na(ASR.pipoly),
                                               "From Pipoly et al. (2015)",
                                               "Not from Pipoly et al. (2015)")
)

#add column of labels indicating if value is not NA for ASR.owens
datafull <- within(datafull,
                   ASR.owens.status <- ifelse(!is.na(ASR.owens),
                                                 "From Owens & Bennett (1994)",
                                               "Not from Owens & Bennett (1994)")
)

#add column of labels indicating if value present for ASR.szekely
datafull <- within(datafull,
                   ASR.status <- ifelse(!is.na(ASR.szekely),"Other",NA)
)
#add to same column, labels indicating if value present for ASR.mort but absent from ASR.szekely
datafull <- within(datafull,
                   ASR.status <- ifelse(!is.na(ASR.mort) & is.na(ASR.szekely),"Mortality only",ASR.status)
)
#add to same column, labels indicating if value present for ASR.szekely and ASR.mort
datafull <- within(datafull,
                   ASR.status <- ifelse(!is.na(ASR.szekely) & !is.na(ASR.mort),"Mortality + Other",ASR.status)
)
#add to same column, labels indicating if value not present for ASR.mort.fill
datafull <- within(datafull,
                   ASR.status <- ifelse(is.na(ASR.mort.fill),"No ASR data",ASR.status)
)
#########################################################################
## preparing data for distribution plots

#add column of labels indicating if value present for ZW
datafull <- within(datafull,
                   Z.W.status <- ifelse(!is.na(Z.W),"With Z/W data","Without Z/W data")
)

#take subset into new dataframe
data_long <- subset(datafull, select=c(Name, 
                                       ASR.szekely,
                                       ASR.mort,
                                       ASR.mort.overwrite,
                                       ASR.mort.fill,
                                       ASR.status,
                                       Z.W,
                                       Z.W.status))
#make name column a factor (requires tidyr package)
data_long$Name <- factor(data_long$Name)

#transforming to long format so that sex ratio types will be on different rows rather than columns
data_long <- gather(data_long,
                    asr.origin,
                    sex.ratio,
                    ASR.szekely:ASR.mort.fill,
                    factor_key=TRUE)

#calculate means for subsets of Z.W.status ~ asr.origin combinations (means to plot on histograms) - reqs plyr
asrMeans <- ddply(data_long,~ Z.W.status  +  asr.origin ,summarise,asr.mean=mean(sex.ratio,na.rm=TRUE)) 

#calculate means for subsets of Z.W.status ~ asr.origin combinations (means to plot on histograms) - reqs plyr
zwMeans <- ddply(data_long,~ ASR.status ,summarise,zw.mean=mean(Z.W,na.rm=TRUE)) 

#########################################################################
# scatter plot data preparation

# cut down data before plotting
#remove all rows that are NA for ASR.fill.szekely and Z.W (unpaired data)
datafull <- datafull[!with(datafull,is.na(ASR.mort.fill)& is.na(ASR.mort.overwrite)& is.na(Z.W)),]
#requires reshape2 package
library(reshape2)
datamelt <- reshape2:::melt.data.frame(data = datafull, id.vars = c("Order",
                                              "Name",
                                              "Z.W"),
                 measure.vars = c("ASR.szekely",
                                  "ASR.mort.fill",
                                  "ASR.mort.overwrite",
                                  "Hatching.sex.ratio.szekely",
                                  "Fledging.sex.ratio.szekely",
                                  "Male.adult.mortality.rate.szekely",
                                  "Female.adult.mortality.rate.szekely",
                                  "Male.polygamy.score.szekely",
                                  "Female.polygamy.score.szekely",
                                  "Male.participation.in.pre.hatching.care.szekely",
                                  "Male.participation.in.post.hatching.care.szekely",
                                  "Male.body.mass..g..szekely",
                                  "Female.body.mass..g..szekely",
                                  "Chick.developmental.mode.szekely",
                                  "Clutch.size.szekely",
                                  "Egg.mass..g..szekely",
                                  "Sexual.size.dimorphism.pipoly",
                                  "Body.size.pipoly",
                                  "abs.Latitude..pipoly",
                                  "body.mass.male.owens",
                                  "mortalityrate.juvenile.male.owens",
                                  "mortalityrate.juvenile.female.owens",
                                  "sexbias.plumage.dimorphism.owens",
                                  "sexbias.nest.building.owens",
                                  "sexbias.clutch.incubation.owens",
                                  "sexbias.brood.provisioning.owens",
                                  "sexbias.passive.brood.defence.owens",
                                  "sexbias.active.brood.defence.owens",
                                  "Genomesize",
                                  "SpMass",
                                  "Z.total.first"),
                 variable.name = "Predictor", value.name = "Value")

datamelt.sex <- reshape2:::melt.data.frame(data = datafull, id.vars = c("Order",
                                                                    "Name",
                                                                    "Z.W"),
                                       measure.vars = c("ASR.szekely",
                                                        "ASR.mort.fill",
                                                        "ASR.mort.overwrite",
                                                        "Hatching.sex.ratio.szekely",
                                                        "Fledging.sex.ratio.szekely",
                                                        "Male.adult.mortality.rate.szekely",
                                                        "Female.adult.mortality.rate.szekely",
                                                        "mortalityrate.juvenile.male.owens",
                                                        "mortalityrate.juvenile.female.owens"),
                                       variable.name = "Predictor", value.name = "Value")

#convert datamelt values to numeric
datamelt.sex$Z.W <- as.numeric(datamelt.sex$Z.W)
datamelt.sex$Value <- as.numeric(datamelt.sex$Value)

datamelt.sexd <- reshape2:::melt.data.frame(data = datafull, id.vars = c("Order",
                                                                        "Name",
                                                                        "Z.W"),
                                           measure.vars = c("Male.polygamy.score.szekely",
                                                            "Female.polygamy.score.szekely",
                                                            "Male.participation.in.pre.hatching.care.szekely",
                                                            "Male.participation.in.post.hatching.care.szekely",
                                                            "Male.body.mass..g..szekely",
                                                            "Female.body.mass..g..szekely",
                                                            "Sexual.size.dimorphism.pipoly",
                                                            "body.mass.male.owens",
                                                            "body.mass.female.owens",
                                                            "sexbias.plumage.dimorphism.owens",
                                                            "sexbias.nest.building.owens",
                                                            "sexbias.clutch.incubation.owens",
                                                            "sexbias.brood.provisioning.owens",
                                                            "sexbias.passive.brood.defence.owens",
                                                            "sexbias.active.brood.defence.owens"),
                                           variable.name = "Predictor", value.name = "Value")

#convert datamelt values to numeric
datamelt.sexd$Z.W <- as.numeric(datamelt.sexd$Z.W)
datamelt.sexd$Value <- as.numeric(datamelt.sexd$Value)

datamelt.phen <- reshape2:::melt.data.frame(data = datafull, id.vars = c("Order",
                                                                         "Name",
                                                                         "Z.W"),
                                           measure.vars = c("Chick.developmental.mode.szekely",
                                                            "Clutch.size.szekely",
                                                            "Egg.mass..g..szekely",
                                                            "Body.size.pipoly",
                                                            "abs.Latitude..pipoly",
                                                            "Genomesize",
                                                            "SpMass"),
                                           variable.name = "Predictor", value.name = "Value")

#convert datamelt values to numeric
datamelt.phen$Z.W <- as.numeric(datamelt.phen$Z.W)
datamelt.phen$Value <- as.numeric(datamelt.phen$Value)

##Function for line equation
lm_eqn = function(x){
  m = lm(Value ~ Z.W, x);
  eq <- substitute(r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#put equations for each predictor into dataframe
#eqns <- by(datamelt, datamelt$Predictor, lm_eqn)
#eqndf <- data.frame(eq = unclass(eqns), Predictor = names(eqns))
#paste facet names with Rsq values into data frame
#eqndf$lab = paste(eqndf$Predictor, "(", "R^2 =", eqndf$eq, ")", sep=" ")

#put equations for each predictor into dataframe
eqns <- by(datamelt.sex, datamelt.sex$Predictor, lm_eqn)
eqndf.sex <- data.frame(eq = unclass(eqns), Predictor = names(eqns))
#paste facet names with Rsq values into data frame
eqndf.sex$lab = paste(eqndf.sex$Predictor, "(", "R^2 =", eqndf.sex$eq, ")", sep=" ")

#put equations for each predictor into dataframe
eqns <- by(datamelt.sexd, datamelt.sexd$Predictor, lm_eqn)
eqndf.sexd <- data.frame(eq = unclass(eqns), Predictor = names(eqns))
#paste facet names with Rsq values into data frame
eqndf.sexd$lab = paste(eqndf.sexd$Predictor, "(", "R^2 =", eqndf.sexd$eq, ")", sep=" ")

#put equations for each predictor into dataframe
eqns <- by(datamelt.phen, datamelt.phen$Predictor, lm_eqn)
eqndf.phen <- data.frame(eq = unclass(eqns), Predictor = names(eqns))
#paste facet names with Rsq values into data frame
eqndf.phen$lab = paste(eqndf.phen$Predictor, "(", "R^2 =", eqndf.phen$eq, ")", sep=" ")
