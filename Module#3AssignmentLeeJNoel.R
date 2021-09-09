set1 <- c(10,2,3,2,4,2,5)
set2 <-c(20,12,13,12,14,12,15)

#set 1 central Tendency
Mean1 <- mean(set1)
Median1 <- median(set1)
Mode1 <- print(ModeSet1)

#created function to retrieve Mode For set 1
getmodeset1 <- function(set1){
  uniqgetmode <- unique(set1)
  uniqgetmode[which.max(tabulate(match(set1,uniqgetmode)))]
}

#Mode for set 1
ModeSet1 <- getmodeset1(set1)

#Variation set 1
Range1 <- range(set1)
Interquartile1 <- IQR(set1)
Variance1 <- var(set1)
Stdev1 <- sd(set1)

#Set 1 Summary
summary

#list for Central tendency of set1
Central_Tendency <- list(Mean1, Median1, Mode1)
names(Central_Tendency) <- c("Mean","Median","Mode")
Central_Tendency

#list for variation of set1
Variation <- list(Range1, Interquartile1, Variance1, Stdev1)
names(Variation) <- c("Range","Interquatile","Variance","Standard Deviation")
Variation

#set 2 Central Tendency
Mean2 <- mean(set2)
Median2 <- median(set2,trim = 0, na.rm = FALSE)
Mode2 <- print(ModeSet2)

#created function to retrieve Mode For set 2
getmodeset2 <- function(set2){
  uniqgetmode <- unique(set2)
  uniqgetmode[which.max(tabulate(match(set2,uniqgetmode)))]
}

#Mode for set 2
ModeSet2 <- getmodeset2(set2)


#Variation set 2
Range2 <- range(set2)
Interquartile2 <- IQR(set2)
Variance2 <- var(set2)
Stdev2 <- sd(set2)

#Set 2 Summary
summary(set2)

#list for Central tendency of set2
Central_Tendency <- list(Mean2, Median2, Mode2)
names(Central_Tendency) <- c("Mean","Median","Mode")
Central_Tendency

#list for variation of set2
Variation <- list(Range2, Interquartile2, Variance2, Stdev2)
names(Variation) <- c("Range","Interquatile","Variance","Standard Deviation")
Variation