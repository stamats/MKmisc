\name{perfMeasures}
\alias{perfMeasures}
\alias{perfScores}
\title{ Compute Performance Measures and Scores for Binary Classification }
\description{
  The function computes various performance weasures and scores for binary classification.
}
\usage{
perfMeasures(pred, pred.group, truth, namePos, cutoff = 0.5,
             weight = 0.5, wACC = weight, wPV = weight)
perfScores(pred, truth, namePos, weight = 0.5, wBS = weight)
}
\arguments{
  \item{pred}{ numeric values that shall be used for classification; e.g. probabilities
  to belong to the positive group. }
  \item{pred.group}{ vector or factor including the predicted group. If missing,
  \code{pred.group} is computed from \code{pred}, where \code{pred >= cutoff} is
  classified as positive.}
  \item{truth}{ true grouping vector or factor. }
  \item{namePos}{ value representing the positive group.}
  \item{cutoff}{ cutoff value used for classification.}
  \item{weight}{ weight used for computing weighted values. Must be in [0,1].}
  \item{wACC}{ weight used for computing the weighted accuracy. Must be in [0,1].}
  \item{wPV}{ weight used for computing the weighted predictive value. Must be in [0,1].}
  \item{wBS}{ weight used for computing the weighted Brier score. Must be in [0,1].}
}
\details{
  The function \code{perfMeasures} computes various performance measures.
  The measures are:
  accuracy (ACC), probabiliy of correct classification (PCC), probability of
  missclassification (PMC), error rate, sensitivity, specificity, prevalence,
  no information rate, weighted accuracy (wACC), balanced accuracy (BACC),
  informedness, Youden's J statistic, positive likelihood ratio (PLR),
  negative likelihood ratio (NLR), positive predictive value (PPV),
  negative predictive value (NPV), markedness, weighted predictive value,
  balanced predictive value, F1 score, Matthews' correlation
  coefficient (MCC), proportion of positive predictions, expected accuracy,
  Cohen's kappa coefficient, and detection rate.

  These performance measures have in common that they require a dichotomization
  (discretization) of a computed continuous classification function.

  The function \code{perfScores} computes various performance Scores.
  The scores are:
  area under the ROC curve (AUC), Gini index, Brier score, positive Brier score,
  negative Brier score, weighted Brier score, and balanced Brier score.

  If the predictions (\code{pred}) are not in the interval [0,1] the standard
  logistic function is applied to transform the values of \code{pred - cutoff}
  to [0,1].
}
\value{
  \code{data.frame} with names of the performance measures, respectivey scores
  and their respective values.
}
\references{
  G.W. Brier (1950). Verification of forecasts expressed in terms of probability.
  \emph{Mon. Wea. Rev.} \bold{78}, 1-3.

  K.H. Brodersen, C.S. Ong, K.E. Stephan, J.M. Buhmann (2010). The balanced
  accuracy and its posterior distribution. In \emph{Pattern Recognition} (ICPR),
  20th International Conference on, 3121-3124 (IEEE, 2010).

  J.A. Cohen (1960). A coefficient of agreement for nominal scales.
  \emph{Educational and Psychological Measurement} \bold{20}, 3746.

  T. Fawcett (2006). An introduction to ROC analysis.
  \emph{Pattern Recognition Letters} \bold{27}, 861-874.

  T.A. Gerds, T. Cai, M. Schumacher (2008). The performance of risk prediction
  models. \emph{Biom J} \bold{50}, 457-479.

  D. Hand, R. Till (2001). A simple generalisation of the area under the ROC
  curve for multiple class classification problems.
  \emph{Machine Learning} \bold{45}, 171-186.

  J. Hernandez-Orallo, P.A. Flach, C. Ferri (2011). Brier curves: a new cost-
  based visualisation of classifier performance. In L. Getoor and T. Scheffer (eds.)
  \emph{Proceedings of the 28th International Conference on Machine Learning} (ICML-11),
  585???592 (ACM, New York, NY, USA).

  J. Hernandez-Orallo, P.A. Flach, C. Ferri (2012). A unified view of performance
  metrics: Translating threshold choice into expected classification loss.
  \emph{J. Mach. Learn. Res.} \bold{13}, 2813-2869.

  B.W. Matthews (1975). Comparison of the predicted and observed secondary
  structure of t4 phage lysozyme. \emph{Biochimica et Biophysica Acta} (BBA) -
  Protein Structure \bold{405}, 442-451.

  D.M. Powers (2011). Evaluation: From Precision, Recall and F-Factor to ROC,
  Informedness, Markedness and Correlation. \emph{Journal of Machine Learning
  Technologies} \bold{1}, 37-63.

  N.A. Smits (2010). A note on Youden's J and its cost ratio.
  \emph{BMC Medical Research Methodology} \bold{10}, 89.

  B. Wallace, I. Dahabreh (2012). Class probability estimates are unreliable for
  imbalanced data (and how to fix them). In \emph{Data Mining} (ICDM), IEEE 12th
  International Conference on, 695-04.

  J.W. Youden (1950). Index for rating diagnostic tests.
  \emph{Cancer} \bold{3}, 32-35.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## example from dataset infert
fit <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
pred <- predict(fit, type = "response")

## with group numbers
perfMeasures(pred, truth = infert$case, namePos = 1)
perfScores(pred, truth = infert$case, namePos = 1)

## with group names
my.case <- factor(infert$case, labels = c("control", "case"))
perfMeasures(pred, truth = my.case, namePos = "case")
perfScores(pred, truth = my.case, namePos = "case")

## on the scale of the linear predictors
pred2 <- predict(fit)
perfMeasures(pred2, truth = infert$case, namePos = 1, cutoff = 0)
perfScores(pred2, truth = infert$case, namePos = 1)

## using weights
perfMeasures(pred, truth = infert$case, namePos = 1, weight = 0.3)
perfScores(pred, truth = infert$case, namePos = 1, weight = 0.3)
}
\keyword{univar}
