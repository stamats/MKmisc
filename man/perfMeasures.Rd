\name{perfMeasures}
\alias{perfMeasures}
\title{ Compute Performance Measures for Binary Classification }
\description{
  The function computes various performance weasures for binary classification.
}
\usage{
perfMeasures(pred, pred.group, truth, namePos)
}
\arguments{
  \item{pred}{ numeric vector in $[0,1]$ with predictions (probability to belong to
  positive group). }
  \item{pred.group}{ vector or factor including the predicted group. If missing,
  \code{pred.group} is computed from \code{pred}, where \code{pred >= 0.5} is
  classified as positive.}
  \item{truth}{ true grouping vector or factor. }
  \item{namePos}{ value representing the positive group.}
}
\details{
  The function computes various performance measures. The measures are:
  accuracy (ACC), probabiliy of correct classification (PCC), probability of
  missclassification (PMC), error rate, sensitivity, specificity, prevalence,
  balanced accuracy (BACC), informedness, Youden's J statistic,
  positive predictive value (PPV), negative predictive value (NPV), markedness,
  F1 score, Matthews' correlation coefficient (MCC),
  proportion of positive predictions, expected accuracy,
  Cohen's kappa coefficient, area under the ROC curve (AUC), Gini index,
  Brier score, positive Brier score, negative Brier score, and balanced Brier
  score.
}
\value{
  \code{data.frame} with names of the performance measures and their respective
  values.
}
\references{
  G.W. Brier (1950). Verification of forecasts expressed in terms of probability.
  \emph{Mon. Wea. Rev.} \bold{78}, 1???3.

  K.H. Brodersen, C.S. Ong, K.E. Stephan, J.M. Buhmann (2010). The balanced
  accuracy and its posterior distribution. In \emph{Pattern Recognition} (ICPR),
  20th International Conference on, 3121-3124 (IEEE, 2010).

  J.A. Cohen (1960). A coefficient of agreement for nominal scales.
  \emph{Educational and Psychological Measurement} \bold{20}, 3746.

  T. Fawcett (2006). An introduction to ROC analysis.
  \emph{Pattern Recognition Letters} \bold{27}, 861???874.

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

## with group names
my.case <- factor(infert$case, labels = c("control", "case"))
perfMeasures(pred, truth = my.case, namePos = "case")
}
\keyword{univar}
