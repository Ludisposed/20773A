# LESSON 1
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

parContext <- RxLocalParallel()
rxSetComputeContext(parContext)

# Run function multiple times
g <- function() 3 * runif(1) + rnorm(1)
y0 <- rxExec(FUN = g, timesToRun = 10)
print(y0)

# Add Arguments
x <- runif(10)
f <- function(x) 3 * x + rnorm(1)
y1 <- rxExec(FUN = fs, elemArgs = xs)
print(y1)

# Adding multple arguments
h <- function(x1, x2) 3 * x1 + 2 * x2 + rnorm(1)
xx <- lapply(1:10, function(x) list(x1 = runif(1), x2 = runif(1)))
y2 <- rxExec(FUN = h, elemArgs = xx)
print(y2)

# doRSR package
library(doRSR)
registerDoRSR()
foreach(i=1:3) %dopar% sqrt(i)

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# LESSON 2
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# RevoPemaR

# Structure ->
# fields, 
# methods,
#     init
#     processData
#     updateResult
#     processresult

library(RevoPemaR)
PemaMean2 <- setPemaClass(
  Class = "PemaMean2",
  contains = "PemaBaseClass",
  fields = list(
    sum = "numeric",
    totalObs = "numeric",
    totalValidObs = "numeric",
    mean = "numeric",
    varName = "character"
  ),
  methods = list(
    initialize = function(varName = "") {
      usingMethods(.pemaMethods)
      varName <<- varName
      sum <<- 0
      totalObs <<- 0
      totalValidObs <<- 0
      mean <<- 0
    },
    processData = function(dataList) {
      sum <<- sum + sum(as.numeric(dataList[[varName]]),
                        na.rm = TRUE)
      totalObs <<- totalObs + length(dataList[[varName]])
      totalValidObs <<- totalValidObs + sum(!is.na(dataList[[varName]]))
      invisible(NULL)
    },
    updateResults = function(PemaMean2Obj) {
      sum <<- sum + PemaMean2Obj$sum
      totalObs <<- totalObs + PemaMean2Obj$totalObs
      totalValidObs <<- totalValidObs + PemaMean2Obj$totalValidObs
      invisible(NULL)
    },
    getVarsToUse = function() {
      varName 
    },
    processReults = function() {
      if (totalValidObs > 0) {
        mean <<- sum / totalValidObs
      } else {
        mean <<- as.numeric(NA)
      }
      return (mean)
    }
  )
)

meanPema2Obj <- PemaMean2()
set.seed(12345)
pemaCompute(pemaObj = meanPema2Obj,
            data = data.frame(x = rnorm(1000)), 
            varName = "x")

meanPema2Obj$mean
meanPema2Obj$totalValidObs