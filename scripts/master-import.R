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

# setnames(interest, "Country.Name", "Country")
# #####
# West African monetary union similarly misses discount rates. US numeraire has no deposit rate.
# Eastern Carib monetary union similarly misses discount rates. US numeraire has no deposit rate.
# Lets add those back in ASAP

waemu <- c("Benin","Burkina Faso","Cote d'Ivoire", "Guinea-Bissau","Mali","Niger","Senegal","Togo")

# View(interest[is.na(discount) & is.na(estimate),])
View(interest[Country %in% waemu, ])


##        WAEMU deposit rates are 3.5 across the board.
##        Where does that put them on the distribution? 
summary(interest)

##        Rates really aren't all that correlated.
cor(interest[complete.cases(interest),.SD, .SDcols=c("deposit","discount","lending","moneymarket")])

##        Linear models are pretty useless but plausible.
lm.discrate01 <- lm(discount~deposit+lending+moneymarket, data=interest)
summary(lm.discrate01)
lm.discrate02 <- lm(discount~deposit, data=interest)
summary(lm.discrate02)
discount_waemu <- predict(lm.discrate02, interest[Country %in% waemu,], se.fit=TRUE)
interest[Country %in% waemu, discount := discount_waemu$fit]


rm(list=c("lm.discrate01","lm.discrate02","waemu","eurointerest","eurozone","discount_waemu"))




# Which countries still missing?
#       Use the discount when available. When not, adjust for different variances of interest series.
interest[,ratiodiscount := discount / interest["United States", discount][[2]]]
interest[,ratiolending := lending / interest["United States", lending][[2]]]
interest[,ratiomoney := moneymarket / interest["United States", moneymarket][[2]]]

#       US discount rate is the numeraire
interest[,estimate := ratiodiscount]
summary(interest)

#         91 missing observations. We can do better.
sd(interest$ratiodiscount, na.rm=T)
sd(interest$ratiolending, na.rm=T)
sd(interest$ratiomoney, na.rm=T)

#         These are factors to correct for the different variance in interest rate levels.
sd_lenddisc = sd(interest$ratiodiscount, na.rm=T) / sd(interest$ratiolending, na.rm=T)
sd_moneydisc = sd(interest$ratiodiscount, na.rm=T) / sd(interest$ratiomoney, na.rm=T)

# ## Check that it works; it does.
# var(interest$ratiodiscount, na.rm=T)
# var(interest$ratiomoney, na.rm=T)
# var(interest$ratiolending, na.rm=T)
# var(interest$ratiomoney * sd_moneydisc, na.rm=T)
# var(interest$ratiolending * sd_lenddisc, na.rm=T)


#       If the discount rate multiplier is unavailable ... 
#           First try the lending rate
#           Adjusted for the difference in variance between lending and discount
interest[is.na(estimate),estimate := ratiolending * sd_lenddisc ]
summary(interest)

#       If the discount rate multiplier is unavailable ... 
#           Next try the moneymarket rate
#           Similarly adjusted for variance
interest[is.na(estimate),estimate := ratiomoney * sd_moneydisc]
summary(interest)

rm(list=c("sd_moneydisc","sd_lenddisc","numeraire"))

# #       Verify
# interest[Country %in% eurozone,]

setnames(interest,"Country","Country.Name")
setkey(ATMcost, Country.Name); setkey(interest, Country.Name)


summary(interest)


##      5. GDP per capita

gdpcap <- read.delim(file="gdpcap.csv", sep=",", header=T, skip=2)
gdpcap <- data.table(gdpcap)
gdpcap <- gdpcap[,c(1:4, 54:58), with=F]
str(gdpcap)
gdpcap[,estimate:= rowMeans(.SD, na.rm=T), .SDcols=c(5:9)]
summary(gdpcap)


# #####
# Cleaning the data

# 1. USA has no value for ATMs per capita; thus no total ATMs estimate.

#       Which estimates are also blank?
write.csv(file="clean-atmspercapita.csv", atmcap[is.na(estimate),.SD, .SDcols=c("Country.Name","Country.Code", "X2012","estimate")])

#       Bahrain, Bermdua, Cuba, Eritrea, Guinea, Gambia, Nifer, Puerto Rico, DPR Korea, Senegal, Somalia, Togo, USA
#       Fix USA by hand.
#       EFT Data sheet cited 425k ATMs in 2010; when USA pop was 308.745 million = 137.65 ATM per 100k population.

setkey(atmcap, "Country.Code")
atmcap["USA",estimate:=137.65]
summary(atmcap)


## Save workspace to ~/data/current.Rdata

save.image("current.Rdata")

