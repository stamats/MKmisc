hsu.t.test <- function(x, ...){
    UseMethod("hsu.t.test")
}
hsu.t.test.default <- function (x, y, alternative = c("two.sided", "less", "greater"),
                                mu = 0, conf.level = 0.95, ...){
  alternative <- match.arg(alternative)
  if (!missing(mu) && (length(mu) != 1 || is.na(mu)))
    stop("'mu' must be a single number")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
                               conf.level < 0 || conf.level > 1))
    stop("'conf.level' must be a single number between 0 and 1")

  dname <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  yok <- !is.na(y)
  xok <- !is.na(x)
  y <- y[yok]
  x <- x[xok]
  nx <- length(x)
  if(nx < 2) stop("not enough 'x' observations")
  mx <- mean(x)
  vx <- var(x)
  ny <- length(y)
  if(ny < 2) stop("not enough 'y' observations")
  my <- mean(y)
  vy <- var(y)
  estimate <- c(mx, my, sqrt(vx), sqrt(vy))
  names(estimate) <- c("mean of x", "mean of y", "SD of x", "SD of y")
  if(nx < 6 || ny < 6)
    warning("For Hsu t-test the sample size per group should be > 5.")
  s <- sqrt(vx + vy)
  se <- sqrt(vx/nx + vy/ny)
  df <- min(nx, ny) - 1
  if(se < 10 * .Machine$double.eps * max(abs(mx), abs(my)))
      stop("data are essentially constant")
  tstat <- (mx - my - mu)/se

  if(alternative == "less") {
    pval <- pt(tstat, df)
    cint <- c(-Inf, tstat + qt(conf.level, df))
  }
  else if(alternative == "greater") {
    pval <- pt(tstat, df, lower.tail = FALSE)
    cint <- c(tstat - qt(conf.level, df), Inf)
  }
  else{
    pval <- 2 * pt(-abs(tstat), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tstat + c(-cint, cint)
  }
  cint <- mu + cint * se
  names(tstat) <- "t"
  names(df) <- "df"
  names(mu) <- "difference in means"
  attr(cint, "conf.level") <- conf.level
  rval <- list(statistic = tstat, parameter = df, p.value = pval,
               conf.int = cint, estimate = estimate, null.value = mu,
               stderr = se, alternative = alternative,
               method = "Hsu Two Sample t-test",
               data.name = dname)
  class(rval) <- "htest"
  rval
}

hsu.t.test.formula <- function (formula, data, subset, na.action, ...)
{
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop("grouping factor must have exactly 2 levels")
  DATA <- setNames(split(mf[[response]], g), c("x", "y"))
  y <- do.call("hsu.t.test", c(DATA, list(...)))
  y$data.name <- DNAME
  if (length(y$estimate) == 2L)
    names(y$estimate) <- paste("mean in group", levels(g))
  y
}
