rm(list=ls())

# make an nx2 matrix of ETF-vol pair returns, bucketed by time Window 
mkReturns<-function(filename, volIndex, underlyingIndex, timeWindow) {
  mat<-data.matrix(read.csv(filename))
  retmat<-matrix(NA,nrow=nrow(mat),ncol=2)
  t = timeWindow
  for (r in 1:(nrow(mat)-t)) {
    c = volIndex
    retmat[r,1] = unname((mat[r+t,c] - mat[r,c])/mat[r,c])
    c = underlyingIndex
    retmat[r,2] = unname((mat[r+t,c] - mat[r,c])/mat[r,c])
  }
  retmat<-na.omit(retmat)
  return(retmat)
}

# make an predictive nx2 matrix of ETF-vol pair returns, bucketed by time Window
# the volatility will lag behind the ETF by past times the timeWindow
mkPredictiveReturns<-function(filename, volIndex, underlyingIndex, pastWindow, futureWindow) {
  mat<-data.matrix(read.csv(filename))
  retmat<-matrix(NA,nrow=nrow(mat),ncol=2)
  t = pastWindow
  t2 = pastWindow + futureWindow
  for (r in 1:(nrow(mat)-t2)) {
    c = volIndex
    retmat[r,1] = unname((mat[r+t,c] - mat[r,c])/mat[r,c])
    c = underlyingIndex
    retmat[r,2] = unname((mat[r+t2,c] - mat[r+t,c])/mat[r+t,c])
  }
  retmat<-na.omit(retmat)
  return(retmat)
}

# border = cutoff at border of the cont table, such as say 20%
# increment = stepping between columns in cont table, such as say 5%
# Makes a contingency table from -border to +border with bucket size = incrementpp
mkContTable<-function(mymatrix, border, increment) {
  # equal number of rows & cols
  rows = 2+ (2*border/increment)
  table<-matrix(0,nrow=rows, ncol=rows)
  # walk thru return matrix, populate contingency table
  for(r in 1:nrow(mymatrix)) {
    e1 = mymatrix[r,1]
    e2 = mymatrix[r,2]
    i1 = ceiling(abs(e1)/increment)
    i2 = ceiling(abs(e2)/increment)
    if (e1 > 0) r1 = min(rows, (rows/2) + i1)
    else r1 = max(1, (rows/2) - i1 + 1)
    
    if (e2 > 0) c1 = min(rows, (rows/2) + i2)
    else c1 = max(1, (rows/2) - i2 + 1)
    
    table[r1,c1] = table[r1,c1] + 1
    #print(paste(e1,",",e2,",",i1,",",i2,",",r1,",",c1))
  }
  return(table)
}

# Collapse a large contingency table into a 2x2 table
collapseTable<- function(bigtable) {
  twobytwo = matrix(0,nrow=2,ncol=2)
  r = nrow(bigtable)
  r2 = r/2
  sum = 0
  for(i in 1:r2) {
    for(j in 1:r2) {
      sum = sum + bigtable[i,j]
      #print(paste(i, ",", j,",", bigtable[i,j]))
    }
  }
  twobytwo[1,1] = sum
  
  sum = 0
  for(i in 1:r2) {
    for(j in (1+r2):r) {
      sum = sum + bigtable[i,j]
    }
  }
  twobytwo[1,2] = sum
  
  sum = 0
  for(i in (1+r2):r) {
    for(j in 1:r2) {
      sum = sum + bigtable[i,j]
    }
  }
  twobytwo[2,1] = sum
  twobytwo[2,2] = sum(bigtable) - sum(twobytwo)
  return(twobytwo)
}

testTable<-function() {
  mymat = matrix(c(-11,-11,-9,-9,9,9,11,11,9,-3), 5,2,byrow=TRUE)
  table = mkContTable(mymat,10,10)
  print(table)
}


# PLEASE CHANGE THIS PATH TO WHEREVER THE FILE IS LOCATED ON YOUR MACHINE
myfile = "~/Desktop/526/groud.csv"

# read file & convert to ETF-vol pair returns over 7-day & 30-day time windows
SPYWeekly = mkReturns(myfile, 2,6,7)
SPYMonthly = mkReturns(myfile, 2,6,30)

DIAWeekly = mkReturns(myfile, 4,9,7)
DIAMonthly = mkReturns(myfile, 4,9,30)

QQQWeekly = mkReturns(myfile, 3,12,7)
QQQMonthly = mkReturns(myfile, 3,12,30)

# Some contingency experiments -
#make contingency tables with 5% intervals, max 20%
#spy5wk = mkContTable(SPYWeekly, .20, .05)
#spy5mon = mkContTable(SPYMonthly, .20, .05)

#make contingency tables with 10% intervals, max 20%
#spy10wk = mkContTable(SPYWeekly, .20, .10)
#spy10mon = mkContTable(SPYMonthly, .20, .10)

#make contingency tables with 2% intervals, max 10%
#spy2wk = mkContTable(SPYWeekly, .10, .02)
#spy2mon = mkContTable(SPYMonthly, .10, .02)

#BEST Contigency table: USE THE 1% WEEKLY interval with 3% max
spyBest = mkContTable(SPYWeekly, .03, .01)
diaBest = mkContTable(DIAWeekly, .03, .01)
qqqBest = mkContTable(QQQWeekly, .03, .01)

#make 2x2 tables
spy2x2 = collapseTable(spyBest)
dia2x2 = collapseTable(diaBest)
qqq2x2 = collapseTable(qqqBest)

# CONCLUSION 1. VERY STRONG CONCURRENT SIGNAL
# Concurrent 2x2 tables show that 
# if VIX decreases during a time window, 
# SPY increases during SAME time window

# Conversely, if VIX increases during a time window, 
# SPY decreases during SAME time window

# Very strong signal ( 5x ) in both cases

# But can we look into the future ? Make predictive returns ?
# Try to predict tomorrow index based on today's vix
SPYPred1 = mkPredictiveReturns(myfile, 2,6,1,1)
spyPred1Table = collapseTable(mkContTable(SPYPred1, .03, .01))

# Try to predict tomorrow index based on past week vix
SPYPred2 = mkPredictiveReturns(myfile, 2,6,7,1)
spyPred2Table = collapseTable(mkContTable(SPYPred2, .03, .01))

# Try to predict tomorrow index based on past two weeks vix
SPYPred3 = mkPredictiveReturns(myfile, 2,6,14,1)
spyPred3Table = collapseTable(mkContTable(SPYPred3, .03, .01))

# Try to predict weekly index return, based on past month vix
SPYPred4 = mkPredictiveReturns(myfile, 2,6,28,7)
spyPred4Table = collapseTable(mkContTable(SPYPred4, .03, .01))

# CONCLUSION 2. VERY WEAK PREDICTIVE SIGNAL
# Trying to predict future index returns based on past volatility no better than coin flip
# Past/Future Time windows seem to matter very little
