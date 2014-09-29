## Analyze ATM costs internationally
## Ben Mazzotta
## 29 September 2014


### OBSOLETE
###     ALL DONE INSIDE ATMCOSTS.RMD


# #####
# Goals
# 1. Gather data sources into a single object.
# 2. Generate national per-ATM estimates.
# 3. Generate national cost totals.

## Preliminaries
require(data.table)
require(ggplot2)


## Combine estimates into the ATMcost object.

tables()

## Maintenance multiplier
##      Model: Multiply USA maintenance cost component by PPP price level

## Data live in data.table ppplevel
##      Create a new variable named c_maint
##      Merge on "Country.Code"
setkey(ATMcost, "Country.Code"); setkey(ppplevel, "Country.Code")
ATMcost <- ATMcost[ppplevel[,.SD, .SDcols=c("Country.Code","estimate")],]
setnames(ATMcost, "estimate", "c_maint")

##        Now c_maint gives the PPP level ratio of each country relative to the USA.

tables()

# ##      Check USA is the numeraire
# ATMcost["USA",]
summary(ATMcost)

## Interest multiplier
##      Model: Multiply USA interest cost component by interest rate ratio

ATMcost[,c_int:= interest$estimate]
names(ATMcost);names(interest)

setnames(interest,"Country","Country.Name")
names(ATMcost);names(interest)
tables()
setkey(ATMcost, "Country.Name")
setkey(interest, "Country.Name")

dim(ATMcost)
dim(ppplevel)
dim(interest)

str(ATMcost[interest[,.SD,.SDcols=c("Country.Name","estimate")],])

