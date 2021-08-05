power.welch.t.test <- function (n = NULL, delta = NULL, sd1 = 1, sd2 = 1,
                                sig.level = 0.05, power = NULL,
                                alternative = c("two.sided", "one.sided"),
                                strict = FALSE, tol = .Machine$double.eps^0.25) {
  if (sum(sapply(list(n, delta, sd1, sd2, power, sig.level), is.null)) != 1)
    stop("exactly one of 'n', 'delta', 'sd1', 'sd2', 'power', and 'sig.level' must be NULL")
  if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > sig.level | sig.level > 1))
    stop("'sig.level' must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1))
    stop("'power' must be numeric in [0, 1]")
  if (!is.null(sd1) && !is.numeric(sd1) || any(0 > sd1))
    stop("'sd1' must be a positive numeric")
  if (!is.null(sd2) && !is.numeric(sd2) || any(0 > sd2))
    stop("'sd2' must be a positive numeric")
  alternative <- match.arg(alternative)
  tside <- switch(alternative, one.sided = 1, two.sided = 2)
  if (tside == 2 && !is.null(delta))
    delta <- abs(delta)
  p.body <- if (strict && tside == 2)
    quote({
      nu <- (n - 1) * (sd1^2+sd2^2)^2/(sd1^4 + sd2^4)
      qu <- qt(sig.level/tside, nu, lower.tail = FALSE)
      sd <- sqrt(sd1^2 + sd2^2)
      pt(qu, nu, ncp = sqrt(n) * delta/sd, lower.tail = FALSE) +
        pt(-qu, nu, ncp = sqrt(n) * delta/sd, lower.tail = TRUE)
    })
  else quote({
    nu <- (n - 1) * (sd1^2+sd2^2)^2/(sd1^4 + sd2^4)
    sd <- sqrt(sd1^2 + sd2^2)
    pt(qt(sig.level/tside, nu, lower.tail = FALSE), nu, ncp = sqrt(n)*delta/sd,
       lower.tail = FALSE)
  })
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(n))
    n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+07),
                 tol = tol, extendInt = "upX")$root
  else if (is.null(sd1))
    sd1 <- uniroot(function(sd1) eval(p.body) - power, delta *
                    c(1e-07, 1e+07), tol = tol, extendInt = "downX")$root
  else if (is.null(sd2))
    sd2 <- uniroot(function(sd2) eval(p.body) - power, delta *
                    c(1e-07, 1e+07), tol = tol, extendInt = "downX")$root
  else if (is.null(delta))
    delta <- uniroot(function(delta) eval(p.body) - power,
                     sqrt(sd1^2 + sd2^2) * c(1e-07, 1e+07), tol = tol, 
                     extendInt = "upX")$root
  else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) -
                           power, c(1e-10, 1 - 1e-10), tol = tol, extendInt = "yes")$root
  else stop("internal error", domain = NA)
  NOTE <- "n is number in *each* group"
  METHOD <- "Two-sample Welch t test power calculation"
  structure(list(n = n, delta = delta, sd1 = sd1, sd2 = sd2, sig.level = sig.level,
                 power = power, alternative = alternative, note = NOTE,
                 method = METHOD), class = "power.htest")
}
