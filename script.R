##switch on library
library(ggplot2)
library(dplyr)
source("C:/Documents and Settings/Администратор/RepData_PeerAssessment2/multiplot.R")
##take data
URL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
datafile <- "stormData.csv.bz2"
download.file(URL, datafile)
stormdata <- read.csv(bzfile("stormData.csv.bz2"), strip.white = TRUE)
##take only USA states
stormdata <- stormdata[stormdata$STATE %in% state.abb,]
##take data to analyze consequences to population health
stormdata_population_health <- stormdata[,c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES")]
##extracting year of event
stormdata_population_health$BGN_DATE <- as.character(stormdata_population_health$BGN_DATE)
testregexp <- regexpr("[0-9]{4}", stormdata_population_health$BGN_DATE)
testmatch <- regmatches(stormdata_population_health$BGN_DATE, testregexp)
stormdata_population_health$BGN_DATE <- as.integer(stormdata_population_health$BGN_DATE <- testmatch)
##group by year and event
stormdata_population_health$EVTYPE <- tolower(stormdata_population_health$EVTYPE)
#fat_by_event <- subset(stormdata_population_health, FATALITIES > 0, select = c(BGN_DATE, EVTYPE, FATALITIES))
#fat_sum <- fat_by_event %>% group_by(BGN_DATE, EVTYPE) %>% summarize(FATALITIES = sum(FATALITIES))
#inj_by_event <- subset(stormdata_population_health, INJURIES > 0, select = c(BGN_DATE, EVTYPE, INJURIES))
#inj_sum <- inj_by_event %>% group_by(BGN_DATE, EVTYPE) %>% summarize(INJURIES = sum(INJURIES))
fat_and_inj <- subset(stormdata_population_health, (FATALITIES > 0 | INJURIES > 0), select = c(BGN_DATE, EVTYPE, FATALITIES, INJURIES))
#fat_and_inj_sum <- aggregate(fat_and_inj[,3:4], fat_and_inj[,1:2], sum)
##search words to group them into events
type <- vector(mode = "list", length = 6)
names(type) <- c("storm", "extreme_cold", "heat", "in_sea", "landslide", "other")
type[[1]] <- c("storm", "lightning", "thunder", "dust", "wind", "hurricane", "tornado", "funnel cloud", "flood", "rain", "fog", "mudslide", "fld", "stream")
type[[2]] <- c("frost", "chill", "precip","freez", "glaze", "sleet", "hypothermia", "low temp", "ice", "icy", "snow", "cold", "blizzard", "swirl", "hail", "winter", "wintry")
type[[3]] <- c("heat wave", "dry", "warm", "high temp", "drought", "heat", "hyperthermia", "fire")
type[[4]] <- c("drowning", "surf", "rip current", "sea", "ocean", "tsunami", "marine mishap", "marine", "wave", "waterspout", "water")
type[[5]] <- c("landslide", "avalance", "aval")
type[[6]] <- c("other", "high")
for (t in names(type)) {
    #temp <- unique(fat_and_inj_sum$EVTYPE[grep(word, fat_and_inj_sum$EVTYPE)])
    for (word in type[[t]]) {
        fat_and_inj$EVTYPE <- gsub(paste(".*",word,".*", sep = ""), t, fat_and_inj$EVTYPE)
    }
}
fat_and_inj_sum <- aggregate(fat_and_inj[,3:4], fat_and_inj[,1:2], sum)
##create graphics
i <- ggplot(data = fat_and_inj_sum, 
            aes(y = INJURIES, x = BGN_DATE, colour = EVTYPE)) + 
    ggtitle("Injuries by time in every event") + 
    geom_line() +
    geom_point()
f <- ggplot(data = fat_and_inj_sum, 
            aes(y = FATALITIES, x = BGN_DATE, colour = EVTYPE)) + 
    ggtitle("Fatalities by time in every event") + 
    geom_line() +
    geom_point()
g <- multiplot(i, f, cols = 2)
g
##take data to analyze consequences to economic
stormdata_economic <- subset(stormdata, (CROPDMG > 0 | PROPDMG > 0), select = c(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP))
##transform economic damage fields
stormdata_economic$EVTYPE <- tolower(stormdata_economic$EVTYPE)
stormdata_economic$PROPDMGEXP <- tolower(stormdata_economic$PROPDMGEXP)
stormdata_economic$CROPDMGEXP <- tolower(stormdata_economic$CROPDMGEXP)
#it doesn't work
#f <- function(value_a, value_b)
#    {
#    if (value_b > 0) {
#        if (value_a == "k")
#            value_b <- as.numeric(value_b) * 1000
#        if (value_a == "m")
#            value_b <- as.numeric(value_b) * 10^6
#        if (value_a == "b")
#            value_b <- as.numeric(value_b) * 10^9
#        if (value_a == "h")
#            value_b <- as.numeric(value_b) * 100
#            }   
#}
#prop_seq <- mapply(stormdata_economic$PROPDMG, stormdata_economic$PROPDMGEXP, FUN = f)
#crop_seq <- mapply(stormdata_economic$CROPDMG, stormdata_economic$CROPDMGEXP, FUN = f)
#it works
prop_seq <- apply(stormdata_economic, 1, function(x) { if (x["PROPDMGEXP"] == "k") { as.numeric(x["PROPDMG"]) * 1000 } else {if (x["PROPDMGEXP"] == "m") {as.numeric(x["PROPDMG"])*10^6} else {if (x["PROPDMGEXP"] == "b") {as.numeric(x["PROPDMG"])*10^9} else {if (x["PROPDMGEXP"] == "h") {as.numeric(x["PROPDMG"])*100}}}}} )
crop_seq <- apply(stormdata_economic, 1, function(x) { if (x["CROPDMGEXP"] == "k") { as.numeric(x["CROPDMG"]) * 1000 } else {if (x["CROPDMGEXP"] == "m") {as.numeric(x["CROPDMG"])*10^6} else {if (x["CROPDMGEXP"] == "b") {as.numeric(x["CROPDMG"])*10^9} else {if (x["CROPDMGEXP"] == "h") {as.numeric(x["CROPDMG"])*100}}}}} )
stormdata_economic$PROPDMG <- as.numeric(paste(prop_seq, sep = " "))
stormdata_economic$CROPDMG <- as.numeric(paste(crop_seq, sep = " "))
stormdata_economic <- stormdata_economic[,c("EVTYPE", "PROPDMG", "CROPDMG")]
##search words to group them into events
type <- vector(mode = "list", length = 6)
names(type) <- c("storm", "extreme_cold", "heat", "in_sea", "landslide", "other")
type[[1]] <- c("storm", "dam break", "gustnado", "tropical depression", "landspout", "tstmw", "burst", "wet", "turbulence", "lig", "thunder", "dust", "wind", "hurricane", "tornado", "funnel cloud", "flood", "rain", "fog", "torndao", "fld", "stream")
type[[2]] <- c("frost", "cool", "chill", "precip","freez", "mix", "glaze", "sleet", "hypothermia", "low temp", "ice", "icy", "snow", "cold", "blizzard", "swirl", "hail", "winter", "wintry")
type[[3]] <- c("heat wave", "smoke", "dry", "warm", "high temp", "drought", "heat", "hyperthermia", "fire")
type[[4]] <- c("drowning", "beach", "surf", "surge", "coast", "rip current", "tide", "seiche", "sea", "ocean", "tsunami", "marine mishap", "marine", "wave", "waterspout", "water")
type[[5]] <- c("landslide", "avalance", "aval", "rockslide", "slide", "land")
type[[6]] <- c("other", "high", "\\?", "urban", "county")
for (t in names(type)) {
    #temp <- unique(stormdata_economic$EVTYPE[grep(word, stormdata_economic$EVTYPE)])
    for (word in type[[t]]) {
        stormdata_economic$EVTYPE <- gsub(paste(".*",word,".*", sep = ""), t, stormdata_economic$EVTYPE)
    }
}
stormdata_economic_sum <- aggregate(stormdata_economic[,2:3], stormdata_economic[1], sum, na.rm = TRUE)
##create graphics
p <- ggplot(data = stormdata_economic_sum, 
            aes(x = EVTYPE, y = PROPDMG, fill = PROPDMG)) + 
    ggtitle("Property damage by event") + 
    geom_bar(position = "dodge", alpha = 0.5, stat = "identity") + 
    ylab("Property damage in $") + xlab("Event group")
c <- ggplot(data = stormdata_economic_sum, 
            aes(x = EVTYPE, y = CROPDMG, fill = CROPDMG)) + 
    ggtitle("Crop damage by event") + 
    geom_bar(position = "dodge", alpha = 0.5, stat = "identity") + 
    ylab("Crop damage in $") + xlab("Event group")
g2 <- multiplot(p, c, cols = 2)
g2