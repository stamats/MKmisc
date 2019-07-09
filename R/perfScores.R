perfScores <- function(pred, truth, namePos, weight = 0.5, wBS = weight){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))
  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))
  stopifnot(length(weights) == 1)
  stopifnot(length(wBS) == 1)
  if(weight < 0 | weight > 1) stop("'weight' has to be in [0, 1]")
  if(wBS < 0 | wBS > 1) stop("'wBS' has to be in [0, 1]")

  AUC <- AUC(pred, group = as.integer(truth == namePos))
  GINI <- 2*AUC - 1
  if(any(pred > 1) | any(pred < 0)){
    pred <- exp(pred)/(1 + exp(pred))
  }
  BS <- mean((pred-as.integer(truth == namePos))^2)
  BSP <- mean((1-pred[truth == namePos])^2)
  BSN <- mean(pred[truth != namePos]^2)
  WBS <- wBS*BSP + (1-wBS)*BSN
  BBS <- 0.5*BSP + 0.5*BSN
  value <- c(AUC, GINI, BS, BSP, BSN, WBS, BBS)
  score <- c("area under the ROC curve (AUC)", "Gini index", "Brier score",
               "positive Brier score", "negative Brier score",
               "weighted Brier score", "balanced Brier score")
  data.frame(Score = score, Value = round(value, 3))
}
