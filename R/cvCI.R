cvCI <- function(x, conf.level = 0.95, method = "miller", na.rm = FALSE){
    stopifnot(is.numeric(x))
    if (!is.na(pmatch(method, "miller")))
        method <- "miller"
    METHODS <- c("miller", "sharma", "curto", "mckay", "vangel",
                 "panichkitkosolkul", "medmiller", "medmckay", "medvangel",
                 "medcurto", "gulhar")
    method <- pmatch(method, METHODS)

    if (is.na(method))
        stop("invalid method")
    if (method == -1)
        stop("ambiguous method")

    if(length(conf.level) != 1)
        stop("'conf.level' has to be of length 1 (confidence level)")
    if(conf.level < 0.5 | conf.level > 1)
        stop("'conf.level' has to be in [0.5, 1]")

    alpha <- 1 - conf.level
    zquant <- qnorm(1-alpha/2)
    cv <- CV(x = x, na.rm = na.rm)
    Infos <- NULL
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)

    if(method == 1){ # Miller 1991
        cv.var <- cv^2*(0.5 + cv^2)
        cv.se <- sqrt(cv.var/(n-1))
        CI.lower <- cv - zquant*cv.se
        CI.upper <- cv + zquant*cv.se
        Infos <- cv.se
        names(Infos) <- "standard error of CV"
        METH <- "Miller (1991) confidence interval"
    }
    if(method == 2){ # Sharma and Krishna 1994
      CI.lower <- 1/(1/cv + zquant/sqrt(n))
      CI.upper <- 1/(1/cv - zquant/sqrt(n))
      METH <- "Sharma and Krishna (1994) confidence interval"
    }
    if(method == 3){ # Curto and Pinto 2009
      cv.var <- cv^2*(0.5 + cv^2)
      cv.se <- sqrt(cv.var/n)
      CI.lower <- cv - zquant*cv.se
      CI.upper <- cv + zquant*cv.se
      Infos <- cv.se
      names(Infos) <- "standard error of CV"
      METH <- "Curto and Pinto (2009) confidence interval"
    }
    if(method == 4){ # McKay 1932
      q1 <- qchisq(1-alpha/2, df = n-1)
      q2 <- qchisq(alpha/2, df = n-1)
      CI.lower <- cv*sqrt((q2/n-1)*cv^2+q2/(n-1))
      CI.upper <- cv*sqrt((q1/n-1)*cv^2+q1/(n-1))
      METH <- "McKay (1932) confidence interval"
    }
    if(method == 5){ # Vangel 1996 (modified McKay)
      q1 <- qchisq(1-alpha/2, df = n-1)
      q2 <- qchisq(alpha/2, df = n-1)
      CI.lower <- cv*sqrt(((q2+2)/n-1)*cv^2+q2/(n-1))
      CI.upper <- cv*sqrt(((q1+2)/n-1)*cv^2+q1/(n-1))
      METH <- "Vangel (1996) confidence interval"
    }
    if(method == 6){ # Panichkitkosolkul 2009 (modified McKay)
      q1 <- qchisq(1-alpha/2, df = n-1)
      q2 <- qchisq(alpha/2, df = n-1)
      cv.ml <- cv*sqrt((n-1)/n)
      CI.lower <- cv.ml*sqrt(((q2+2)/n-1)*cv.ml^2+q2/(n-1))
      CI.upper <- cv.ml*sqrt(((q1+2)/n-1)*cv.ml^2+q1/(n-1))
      METH <- "Panichkitkosolkul (2009) confidence interval"
    }
    sd.med <- sqrt(sum((x-median(x))^2)/(n-1))
    cv.med <- sd.med/mean(x)
    if(method == 7){ # median modified Miller
      cv.med.var <- cv.med^2*(0.5 + cv.med^2)
      cv.med.se <- sqrt(cv.med.var/(n-1))
      CI.lower <- cv - zquant*cv.med.se
      CI.upper <- cv + zquant*cv.med.se
      Infos <- cv.med.se
      names(Infos) <- "standard error of CV"
      METH <- "Median modified Miller confidence interval"
    }
    if(method == 8){ # median modified McKay
      q1 <- qchisq(1-alpha/2, df = n-1)
      q2 <- qchisq(alpha/2, df = n-1)
      CI.lower <- cv.med*sqrt((q2/n-1)*cv.med^2+q2/(n-1))
      CI.upper <- cv.med*sqrt((q1/n-1)*cv.med^2+q1/(n-1))
      METH <- "Median modified McKay confidence interval"
    }
    if(method == 9){ # median modified Vangel (modified McKay)
      q1 <- qchisq(1-alpha/2, df = n-1)
      q2 <- qchisq(alpha/2, df = n-1)
      CI.lower <- cv.med*sqrt(((q2+2)/n-1)*cv.med^2+q2/(n-1))
      CI.upper <- cv.med*sqrt(((q1+2)/n-1)*cv.med^2+q1/(n-1))
      METH <- "Median modified Vangel confidence interval"
    }
    if(method == 10){ # median modified Curto and Pinto 2009
      cv.med.var <- cv^2*(0.5 + cv^2)
      cv.med.se <- sqrt(cv.med.var/n)
      CI.lower <- cv - zquant*cv.med.se
      CI.upper <- cv + zquant*cv.med.se
      Infos <- cv.med.se
      names(Infos) <- "standard error of CV"
      METH <- "Median modified Curto and Pinto confidence interval"
    }
    if(method == 11){ # Gulhar et al (2012)
      q1 <- qchisq(1-alpha/2, df = n-1)
      q2 <- qchisq(alpha/2, df = n-1)
      CI.lower <- cv*sqrt(n-1)/sqrt(q1)
      CI.upper <- cv*sqrt(n-1)/sqrt(q2)
      METH <- "Gulhar et al (2012) confidence interval"
    }

    CI <- matrix(c(CI.lower, CI.upper), nrow = 1)
    rownames(CI) <- "CV"
    colnames(CI) <- c(paste(alpha/2*100, "%"), paste((1-alpha/2)*100, "%"))
    attr(CI, "conf.level") <- conf.level
    names(cv) <- "CV"

    return(structure(list("estimate" = cv, "conf.int" = CI, "Infos" = Infos,
                          "method" = METH),
                     class = "confint"))
}
