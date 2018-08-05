perfMeasures <- function(pred, pred.group, truth, namePos, cutoff = 0.5){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))
  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))
  stopifnot(is.numeric(cutoff))
  stopifnot(length(cutoff) == 1)
  if(missing(pred.group)){
    pred.group <- character(length(pred))
    pred.group[pred >= cutoff] <- namePos
    nam <- levels(truth)
    nameNeg <- nam[nam != namePos]
    pred.group[pred < cutoff] <- nameNeg
    pred.group <- factor(pred.group)
    pred.group <- factor(pred.group, levels = c(nameNeg, namePos))
  }
  stopifnot(length(pred.group) == length(truth))
  if(!is.factor(pred.group)) pred.group <- factor(pred.group)
  stopifnot(nlevels(pred.group) == 2)
  stopifnot(levels(pred.group) == levels(truth))
  stopifnot(namePos %in% levels(pred.group))

  pred.pos <- pred.group == namePos
  pred.neg <- pred.group != namePos
  truth.pos <- truth == namePos
  truth.neg <- truth != namePos
  TP <- sum(pred.pos & truth.pos)
  TN <- sum(pred.neg & truth.neg)
  FP <- sum(pred.pos & truth.neg)
  FN <- sum(pred.neg & truth.pos)

  ACC <- (TN + TP)/(TP + TN + FP + FN)
  PCC <- ACC
  PMC <- 1 - ACC
  ERATE <- PMC
  SENS <- TP/(TP + FN)
  SPEC <- TN/(TN + FP)
  PREV <- (TP + FN)/(TP + TN + FP + FN)
  BACC <- 0.5*SENS + 0.5*SPEC
  INF <- SENS + SPEC - 1
  YOUDEN <- INF
  PPV <- TP/(TP + FP)
  NPV <- TN/(TN + FN)
  MARK <- PPV + NPV - 1
  F1 <- 2*PPV*SENS/(PPV + SENS)
  MCC <- sign(INF)*sqrt(INF*MARK)
  PP <- (TP + FP)/(TP + TN + FP + FN)
  EACC <- PREV*PP + (1 - PREV)*(1 - PP)
  COHEN <- (ACC - EACC)/(1-EACC)
  AUC <- AUC(pred, group = as.integer(truth == namePos))
  GINI <- 2*AUC - 1
  if(all(pred <= 1) & all(pred >= 0)){
    BS <- mean((pred-as.integer(truth == namePos))^2)
    BSP <- mean((1-pred[truth == namePos])^2)
    BSN <- mean(pred[truth != namePos]^2)
    BBS <- 0.5*BSP + 0.5*BSN
  }else{
    pred <- exp(pred-cutoff)/(1 + exp(pred-cutoff))
    BS <- mean((pred-as.integer(truth == namePos))^2)
    BSP <- mean((1-pred[truth == namePos])^2)
    BSN <- mean(pred[truth != namePos]^2)
    BBS <- 0.5*BSP + 0.5*BSN
  }
  value <- c(ACC, PCC, PMC, ERATE, SENS, SPEC, PREV, BACC, INF, YOUDEN, PPV,
             NPV, MARK, F1, MCC, PP, EACC, COHEN, AUC, GINI, BS, BSP, BSN, BBS)
  measure <- c("accuracy (ACC)", "probabiliy of correct classification (PCC)",
               "probability of missclassification (PMC)", "error rate",
               "sensitivity", "specificity", "prevalence", "balanced accuracy (BACC)",
               "informedness", "Youden's J statistic", "positive predictive value (PPV)",
               "negative predictive value (NPV)", "markedness", "F1 score",
               "Matthews' correlation coefficient (MCC)",
               "proportion of positive predictions",
               "expected accuracy", "Cohen's kappa coefficient",
               "area under the ROC curve (AUC)", "Gini index", "Brier score",
               "positive Brier score", "negative Brier score",
               "balanced Brier score")
  data.frame(Measure = measure, Value = round(value, 3))
}
