set.seed(944)
y <- rnorm(100)
g <- sample(letters[1:3], size = 100, replace = TRUE)
sex <- sample(c("M", "F"), size = 100, replace = TRUE)
dx <- sample(c("Active", "Control"), size = 100, replace = TRUE)
d <- data.frame(y = y, g = g, sex = sex, dx = dx)

tbl <-    function(x, ...) UseMethod("tbl")

tbl.default <- function(rowvar, ...) {
  counts <- base::table(rowvar, ...)
  percents <- prop.table(counts)
  m <- matrix(c(counts, percents), 
              byrow = FALSE, nrow = length(counts),
              dimnames = list(Level = names(counts),
                c("Number", "Percent")))
  structure(m,
            class = c("tbl", "matrix"))
}

## This must be able to handle the cases where 
## the levels are not the same for each
## group
tbl.formula <- function(formula,
                 data = NULL, ...,
                 subset, na.action = NULL,
		 overall = FALSE,
                 overall.label = "Overall") {
  if (missing(formula) || (length(formula) != 3)) 
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$... <- m$overall <- m$overall.label <- NULL
  m$na.action <- na.action
  require(stats, quietly = TRUE)
  m[[1]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  # assume groups are factors
  # but make the response a factor
  y <- mf[[response]]
  if(!inherits(y, "factor"))
    y <- factor(y)
  
  s <- split(y, mf[-response]) 
  if(overall) {
    if(any(names(s) == overall.label))
    stop(sQuote(overall.label), " is an existing level")
    s <- c(Overall = list(unlist(s)), s)
  }  
  structure(lapply(s, FUN = tbl, ...),
            class = "tbl")
}

format.tbl <- function(x, format = "f", digits = 0, omit.zero.pct = TRUE, ...) {

  ## This is the workhorse to paste the percent after the number
  f <- function(y, format = "f", digits = 1, omit.zero.pct = TRUE, ...) {
     n <- y[, "Number"]
     p <- y[, "Percent"]
     p <- formatC(100 * p, format = format, digits = digits, ...)
     dn <- dimnames(y)
     m <- matrix(paste(n, " (", p, ")", sep = ""),
                 ncol = 1,
                 dimnames = list(Level = dn[[1]], "Number (%)"))
     if(omit.zero.pct) {
       m[n==0] <- "0"
     }  
     m
  }
  
  if(!is.list(x)) 
    return(f(x, digits = digits, omit.zero.pct = omit.zero.pct, ...))
  ##dims <- dim(x)
  ##nr <- dims[1]
  nc <- length(x)
  rval <- vector("list", nc)
  for (i in 1:nc) {
    rval[[i]] <- f(x[[i]], digits = digits, 
                   omit.zero.pct = omit.zero.pct, ...)
  }
  xx <- do.call("cbind", rval)
  dimnames(xx) <- list("No. (%) in each level" = row.names(x[[1]]),
                       "Group" = names(x))
  xx
}

print.tbl <- function(x, quote = FALSE, ...){
  print(format(x, ...), quote = quote)
  invisible(x)
}

as.table.tbl <- function(x, ...){
  if(is.matrix(x)){
    xx <- x[, "Number", drop = F]
    dimnames(xx) <- list("Level" = row.names(x),
                         "Number")
  }
  else {
    nc <- length(x)
    rval <- vector("list", nc)
    ## for each column in the table, grab the numbers
    for (i in 1:nc) rval[[i]] <- x[[i]][, "Number"]
    xx <- do.call("cbind", rval)
    dimnames(xx) <- list("Number in each level" = row.names(x[[1]]),
                         "Group" = names(x))
  }
  xx
}

summary.tbl <- function(object, test = c("none", "chisq", "fisher", "prop.trend"), 
                        correct = FALSE, 
                        n.minus.1 = TRUE,
                        dim.events = 2, # columns are events/non-events
                        ...){
  
  test <- match.arg(test)
  xx <- as.table(object)
  tt <- switch(test,
               "none" = NULL,
               "chisq" = {
                 if(!correct && n.minus.1)
                   warning("Continuity correction overrides 'N-1' method in chi-squared test")
                 tmp <- chisq.test(xx, correct = correct, ...)
                 if(n.minus.1 && min(tmp$expected) < 1)
                   warning("'N-1' method not recommended when expected counts are below 1")
                 if(!correct && n.minus.1 && nrow(xx) == 2 & ncol(xx) == 2) {
                   tmp$method <- "Pearson's Chi-squared test using 'N-1' method"
                   tmp$statistic <- tmp$statistic * sum(xx)/(sum(xx) - 1)
                   tmp$p.value <- pchisq(tmp$statistic, tmp$parameter, lower.tail = FALSE)
                 }
                 tmp
               },
               
               "fisher" = fisher.test(xx, ...),
               "prop.trend" = {
                 if(dim(xx)[dim.events] != 2)
                   stop("Test for trend requires a table with either two columns or two rows")
                 if(dim.events == 2) {
                   events <- xx[,1]
                   trials <- apply(xx, 1, sum)
                 }
                 else {
                   events <- xx[1, ]
                   trials <- apply(xx, 2, sum)
                 }
                 tt <- prop.trend.test(events, trials, ...)
                 })
                   
  out <- list(table = object, htest = tt)
  class(out) <- c("summary.tbl")
  out
}  

format.summary.tbl <- function(x, ...){
  fx <- format(x$table, ...)
  out <- cbind(fx, "P"= c(formatP(x$htest$p.value), rep("", nrow(fx) - 1)))
  dimnames(out) <- list("No. (%) in each level" = row.names(fx),
                        "Group" = c(dimnames(fx)[[2]], "P"))
  print(out, quote = FALSE)
  cat("\nNote: P-value based on", x$htest$method, "\n") 
  invisible(out)
}
