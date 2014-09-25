## Master Import Script
## Ben Mazzotta
## 2014 09 25


## Cost of Cash Global
## ATM costs study.


# #####
# Data to import:
#   1. World Bank ATMs
#   2. IMF interest rates
#   3. World Bank Population
#   4. World Bank PPP conversion factors
#   5. GDP per capita for spice


#     0. Preliminaries
require(data.table)
require(foreign)
require(countrycode)
require(ggplot2)

# ## Run me in the data subfolder
# setwd("../data")

#     1. World Bank ATMs

atmcap <- read.delim(file="atmpercapita.csv", sep=",", skip=2, header=T)
str(atmcap)
summary(atmcap)

atmcap <- data.table(atmcap)
atmcap[,estimate:= rowMeans(.SD, na.rm=T), .SDcols=c("X2010","X2011","X2012")]

summary(atmcap)
qplot(estimate, data=atmcap, stat="bin", binwidth=10)


#     2. World Bank Population

pop <- read.delim(file="population1960-2013.csv", skip=2, header=T, sep=",")
pop <- data.table(pop)
pop <- pop[,c(1:4, 54:58), with=F]
str(pop)

pop[,estimate:=rowMeans(.SD, na.rm=T), .SDcols=c(5:9)]
summary(pop)


#     3. Price level ratios
#          i.e., ratio of PPP to market exchange rate


#       import the world bank CSV file
ppplevel <- data.table(read.delim(file="ppplevel.csv", skip=2, header=T, sep=","))
#       Keep only years 2008--2012
ppplevel <- ppplevel[,c(1:4,54:58), with=F]
str(ppplevel)

#       Average years 2008--2012 "estimate"
ppplevel[,estimate:=rowMeans(.SD, na.rm=T), .SDcols=c(5:9)]
summary(ppplevel)
with(ppplevel, table(Country.Name, estimate>1))


##    I also did PPP conversion factors; not what we want

pppconv <- read.delim(file="pppconversion.csv", skip=2, header=T, sep=",")
pppconv <- data.table(pppconv)
pppconv <- pppconv[,c(1:4,54:58), with=F]
str(pppconv)

pppconv[,estimate:= rowMeans(.SD, na.rm=T), .SDcols=c(5:9)]
summary(pppconv)

#     4. Interest rates 

##      These were very hard to select. Most datasets are not useful.
##        - rejected World Bank's "Lending Rate" series
##        -

intrate <- read.delim(file="IFS.csv", sep=",", header=T, skip=0)
intrate <- data.table(intrate)
str(intrate)

attr(intrate$Concept, "levels") <- c("Deposit","Discount","Lending","Money market")
str(intrate)


#     Take the average of available observations by concept and country.
intrate[Concept=="Deposit",  deposit:= rowMeans(.SD, na.rm=T), .SDcols=c(10:12)]
intrate[Concept=="Discount",  discount:= rowMeans(.SD, na.rm=T), .SDcols=c(10:12)]
intrate[Concept=="Lending",  lending:= rowMeans(.SD, na.rm=T), .SDcols=c(10:12)]
intrate[Concept=="Money market",  moneymarket:= rowMeans(.SD, na.rm=T), .SDcols=c(10:12)]
summary(intrate)

#     Now whittle down to one observation per country.

#     This step finds the non-missing entries by country. Replicate to all country entries.
setkey(intrate,Country)
intrate[,deposit:= min(deposit, na.rm=T), by="Country"]
intrate[,discount:= min(discount, na.rm=T), by="Country"]
intrate[,lending:= min(lending, na.rm=T), by="Country"]
intrate[,moneymarket:= min(moneymarket, na.rm=T), by="Country"]
summary(intrate)

#     Not all countries report all measures. This step corrects "Inf" entries to NA.
intrate[deposit==Inf,deposit:= NA]
intrate[discount==Inf,discount:= NA]
intrate[lending==Inf,lending:= NA]
intrate[moneymarket==Inf,moneymarket:= NA]
summary(intrate)

#     Keep just the first entry by Country and all four rates.
names(intrate)
interest <- unique(intrate[,c("Country", "deposit", "discount", "lending", "moneymarket"), with=F])

#     Euro area discount rates reported by ECB
#       Which countries?
eurozone <- c("Austria","Belgium","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland","Italy","Latvia","Luxembourg","Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")

#       What rate?
eurointerest <- interest["Euro Area",discount][[2]]

# #       Check the join/query works
# interest[Country %in% eurozone & is.na(discount),]

#       Replace missing values w/ eurozone discount rate
interest[Country %in% eurozone & is.na(discount), discount:=eurointerest]

#       US discount rate is the numeraire
numeraire <- interest["United States", discount][[2]]
interest[,estimate:=discount/numeraire]
summary(interest)

#       If the discount rate multiplier is unavailable ... 
USmoneyrate <- interest["United States", moneymarket][[2]]
interest[is.na(estimate),estimate := moneymarket/USmoneyrate]
summary(interest)

USlendingrate <- interest["United States", lending][[2]]
interest[is.na(estimate),estimate := lending/USlendingrate]
summary(interest)



# #       Verify
# interest[Country %in% eurozone,]

summary(interest)


##      5. GDP per capita

gdpcap <- read.delim(file="gdpcap.csv", sep=",", header=T, skip=2)
gdpcap <- data.table(gdpcap)
gdpcap <- gdpcap[,c(1:4, 54:58), with=F]
str(gdpcap)
gdpcap[,estimate:= rowMeans(.SD, na.rm=T), .SDcols=c(5:9)]
summary(gdpcap)

## Save workspace to ~/data/current.Rdata

save.image("current.Rdata")

