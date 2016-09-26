power.nb.test <- function(n = NULL, mu0, mu1, RR, duration = 1, theta, ssize.ratio = 1,
                         sig.level = 0.05, power = NULL,
                         alternative = c("two.sided", "one.sided"),
                         approach = 3){
  if(sum(sapply(list(n, power), is.null)) != 1)
    stop("exactly one of 'n' and 'power' must be NULL")
  if(!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1))
    stop("'sig.level' must be numeric in (0, 1)")
  if(!is.null(power) && (!is.numeric(sig.level) || any(0 > sig.level | sig.level > 1)))
    stop("'power' must be numeric in (0, 1)")
  if(missing(mu0))
    stop("'mu0' must be given")
  if(missing(mu1) && missing(RR))
    stop("either 'mu1' or 'RR' must be given")
  if(missing(theta))
    stop("'theta' must be given")
  if(missing(mu1)) mu1 <- mu0*RR
  if(missing(RR)) RR <- mu1/mu0
  if(!is.numeric(mu0) || mu0 <= 0)
    stop("'mu0' must be numeric and > 0")
  if(!is.numeric(mu1) || mu1 <= 0)
    stop("'mu1' must be numeric and > 0")
  if(!is.numeric(duration) || duration <= 0)
    stop("'duration' must be numeric and > 0")
  if(!is.numeric(theta) || theta <= 0)
    stop("'theta' must be numeric and > 0")
  if(!is.numeric(ssize.ratio) || ssize.ratio <= 0)
    stop("'ssize.ratio' must be numeric and > 0")
  alternative <- match.arg(alternative)
  sided <- switch(alternative, one.sided = 1, two.sided = 2)
  k <- 1/theta
  if(!(approach %in% 1:3))
    stop("'approach' must be equal to 1, 2, or 3")
  if(approach == 1) V0 <- (1 + ssize.ratio)/(duration*ssize.ratio*mu0) + (1 + ssize.ratio)*k/ssize.ratio
  if(approach == 2) V0 <- 1/duration*(1/mu0 + 1/(ssize.ratio*mu1)) + (1 + ssize.ratio)*k/ssize.ratio
  if(approach == 3) V0 <- (1+ssize.ratio)^2/(duration*ssize.ratio*(mu0 + ssize.ratio*mu1)) + (1 + ssize.ratio)*k/ssize.ratio
  V1 <- 1/duration*(1/mu0 + 1/(ssize.ratio*mu1)) + (1 + ssize.ratio)*k/ssize.ratio
  if(is.null(power)){
    if(sided == 1) power <- pnorm((sqrt(n)*abs(log(mu1/mu0)) - qnorm(1-sig.level)*sqrt(V0))/sqrt(V1))
    if(sided == 2) power <- pnorm((sqrt(n)*abs(log(mu1/mu0)) - qnorm(1-sig.level/2)*sqrt(V0))/sqrt(V1))
  } else if(is.null(n)) {
    beta <- 1 - power
    if(sided == 1) n <- ((qnorm(1-sig.level)*sqrt(V0) + qnorm(1-beta)*sqrt(V1))/log(mu1/mu0))^2
    if(sided == 2) n <- ((qnorm(1-sig.level/2)*sqrt(V0) + qnorm(1-beta)*sqrt(V1))/log(mu1/mu0))^2
  } else stop("internal error", domain = NA)
  n1 <- ssize.ratio*n

  METHOD <- "Power calculation for comparing two negative binomial rates"
  NOTE <- "n = sample size of control group, n1 = sample size of treatment group"
  structure(list(n = n, n1 = n1, mu0 = mu0, RR = RR, theta = theta, duration = duration,
                 sig.level = sig.level, power = power, alternative = alternative,
                 note = NOTE, method = METHOD), class = "power.htest")
}
