optCutoff <- function(pred, truth, namePos,
                      perfMeasure = "Youden's J statistic",
                      max = TRUE, parallel = FALSE, ncores,
                      delta = 0.01){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))
  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))

  W <- range(pred)
  cutoffs <- c(W[1]-delta, pred, W[2]+delta)
  perf1 <- perfMeasures(pred = pred, truth = truth, namePos = namePos,
                        cutoff = cutoffs[1])
  ind.perf <- which(perf1[,1] == perfMeasure)
  if(parallel){
    stopifnot(requireNamespace("foreach", quietly = TRUE))
    stopifnot(requireNamespace("parallel", quietly = TRUE))
    stopifnot(requireNamespace("doParallel", quietly = TRUE))
    if(missing(ncores)){
      cores <- parallel::detectCores()
      cl <- parallel::makeCluster(cores[1]-1)
    }else{
      cl <- parallel::makeCluster(ncores)
    }
    doParallel::registerDoParallel(cl)
    `%dopar%` <- foreach::`%dopar%`
    perfs <- foreach::foreach(i = seq_along(cutoffs)) %dopar% {
      MKmisc::perfMeasures(pred = pred, truth = truth, namePos = namePos,
                           cutoff = cutoffs[i])[ind.perf,2]
    }
    parallel::stopCluster(cl)
  }else{
    perfs <- numeric(length(cutoffs))
    for(i in seq_along(cutoffs)){
      perfs[i] <- MKmisc::perfMeasures(pred = pred, truth = truth,
                                       namePos = namePos,
                                       cutoff = cutoffs[i])[ind.perf, 2]
    }
  }
  if(max){
    ind.max <- which.max(perfs)
    res <- unlist(c(cutoffs[ind.max], perfs[ind.max]))
  }else{
    ind.min <- which.min(perfs)
    res <- c(cutoffs[ind.min], perfs[ind.min])
  }
  names(res) <- c("Optimal Cut-off", perfMeasure)
  res
}
