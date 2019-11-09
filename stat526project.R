rm(list=ls())

# make an nx2 matrix of ETF-vol pair returns, bucketed by time Window 
mkReturns<-function(filename, volIndex, underlyingIndex, timeWindow) {
  mat<-data.matrix(read.csv(filename))
  retmat<-matrix(0,nrow=nrow(mat),ncol=2)
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

# border = cutoff at border of the cont table, such as say 20%
# increment = stepping between columns in cont table, such as say 5%
# Makes a contingency table from -border to +border with bucket size = incrementpp
mkContTable<-function(mymatrix, border, increment) {
  # equal number of rows & cols
  rows = 2+ (2*border/increment)
  table<-matrix(0,nrow=rows, ncol=rows)
  # walk thru return matrix, populate contingecy table
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

#make contingency tables with 5% intervals, max 20%
spy5wk = mkContTable(SPYWeekly, .20, .05)
spy5mon = mkContTable(SPYMonthly, .20, .05)

#make contingency tables with 10% intervals, max 20%
spy10wk = mkContTable(SPYWeekly, .20, .10)
spy10mon = mkContTable(SPYMonthly, .20, .10)

#make contingency tables with 2% intervals, max 10%
spy2wk = mkContTable(SPYWeekly, .10, .02)
spy2mon = mkContTable(SPYMonthly, .10, .02)

#BEST BET IS TO USE THE 1% WEEKLY interval with 3% max
spyBest = mkContTable(SPYWeekly, .03, .01)
diaBest = mkContTable(DIAWeekly, .03, .01)
qqqBest = mkContTable(QQQWeekly, .03, .01)


