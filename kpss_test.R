kpss.test <-
  function(x, null = c("Level", "Trend"), lshort = TRUE)
  {
    if((NCOL(x) > 1) || is.data.frame(x))        
      stop("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    null <- match.arg(null)
    x <- as.vector(x, mode="double")
    n <- length(x)
    if(null == "Trend") {
      t <- 1:n
      e <- residuals(lm(x ~ t))
      table <- c(0.216, 0.176, 0.146, 0.119)
    }
    else if(null == "Level") {
      e <- residuals(lm(x ~ 1))
      table <- c(0.739, 0.574, 0.463, 0.347)
    }
    tablep <- c(0.01, 0.025, 0.05, 0.10)
    s <- cumsum(e)
    eta <- sum(s^2)/(n^2)
    s2 <- sum(e^2)/n
    if(lshort)
      l <- trunc(4*(n/100)^0.25)
    else
      l <- trunc(12*(n/100)^0.25)
    s2 <- .C(tseries_pp_sum,
             as.vector(e, mode="double"),
             as.integer(n),
             as.integer(l),
             s2=as.double(s2))$s2
    STAT <- eta/s2
    PVAL <- approx(table, tablep, STAT, rule=2)$y
    if(!is.na(STAT) &&
       is.na(approx(table, tablep, STAT, rule=1)$y))
      if(PVAL == min(tablep))
        warning("p-value smaller than printed p-value")
    else
      warning("p-value greater than printed p-value")
    PARAMETER <- l
    METHOD <- paste("KPSS Test for", null, "Stationarity")
    names(STAT) <- paste("KPSS", null)
    names(PARAMETER) <- "Truncation lag parameter"
    structure(list(statistic = STAT,
                   parameter = PARAMETER,
                   p.value = PVAL,
                   method = METHOD,
                   data.name = DNAME),
              class = "htest")
  }