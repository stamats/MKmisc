perfMeasures <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                         weight = 0.5, wACC = weight, wPV = weight, wBS = weight){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))
  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))
  stopifnot(is.numeric(cutoff))
  stopifnot(length(cutoff) == 1)
  stopifnot(length(weights) == 1)
  stopifnot(length(wACC) == 1)
  stopifnot(length(wPV) == 1)
  stopifnot(length(wBS) == 1)
  if(weight < 0 | weight > 1) stop("'weight' has to be in [0, 1]")
  if(wACC < 0 | wACC > 1) stop("'wACC' has to be in [0, 1]")
  if(wPV < 0 | wPV > 1) stop("'wPV' has to be in [0, 1]")
  if(wBS < 0 | wBS > 1) stop("'wBS' has to be in [0, 1]")

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
  NIR <- max(PREV, 1-PREV)
  BACC <- 0.5*SENS + 0.5*SPEC
  WACC <- wACC*SENS + (1-wACC)*SPEC
  INF <- SENS + SPEC - 1
  YOUDEN <- INF
  PLR <- SENS/(1-SPEC)
  NLR <- (1-SENS)/SPEC
  PPV <- TP/(TP + FP)
  NPV <- TN/(TN + FN)
  MARK <- PPV + NPV - 1
  WPV <- wPV*PPV + (1-wPV)*NPV
  BPV <- 0.5*PPV + 0.5*NPV
  F1 <- 2*PPV*SENS/(PPV + SENS)
  MCC <- sign(INF)*sqrt(INF*MARK)
  PP <- (TP + FP)/(TP + TN + FP + FN)
  EACC <- PREV*PP + (1 - PREV)*(1 - PP)
  COHEN <- (ACC - EACC)/(1-EACC)
  DR <- TP/(TP + TN + FP + FN)
  AUC <- AUC(pred, group = as.integer(truth == namePos))
  GINI <- 2*AUC - 1
  if(any(pred > 1) | any(pred < 0)){
    pred <- exp(pred-cutoff)/(1 + exp(pred-cutoff))
  }
  BS <- mean((pred-as.integer(truth == namePos))^2)
  BSP <- mean((1-pred[truth == namePos])^2)
  BSN <- mean(pred[truth != namePos]^2)
  WBS <- wBS*BSP + (1-wBS)*BSN
  BBS <- 0.5*BSP + 0.5*BSN
  value <- c(ACC, PCC, PMC, ERATE, SENS, SPEC, PREV, NIR, WACC, BACC, INF, YOUDEN,
             PLR, NLR, PPV, NPV, MARK, WPV, BPV, F1, MCC, PP, EACC, COHEN, DR,
             AUC, GINI, BS, BSP, BSN, WBS, BBS)
  measure <- c("accuracy (ACC)", "probabiliy of correct classification (PCC)",
               "probability of missclassification (PMC)", "error rate",
               "sensitivity", "specificity", "prevalence", "no information rate",
               "weighted accuracy (wACC)", "balanced accuracy (BACC)",
               "informedness", "Youden's J statistic", "positive likelihood ratio (PLR)",
               "negative likelihood ratio (NLR)", "positive predictive value (PPV)",
               "negative predictive value (NPV)", "markedness",
               "weighted predictive value", "balanced predictive value",
               "F1 score", "Matthews' correlation coefficient (MCC)",
               "proportion of positive predictions",
               "expected accuracy", "Cohen's kappa coefficient", "detection rate",
               "area under the ROC curve (AUC)", "Gini index", "Brier score",
               "positive Brier score", "negative Brier score",
               "weighted Brier score", "balanced Brier score")
  data.frame(Measure = measure, Value = round(value, 3))
}
