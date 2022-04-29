## Example call is something like:
## yat(Species ~ mean(Sepal.Length) + ... + mean(Petal.Width), iris)


## run yat tests interactively
test.yat <- function(){
    lines <- readLines("../tests/yat-tests.R")
    lines <- sub("library(yat)", "## library(yat)", lines, fixed = TRUE)
    lines <- sub("yat:::", "", lines, fixed = TRUE)
    source("yat.R", echo = TRUE)
    source(textConnection(lines), echo = TRUE)
}


## formula split in that y ~ x + z ==>  list(y ~ x, y ~ z)
## formsplit <- function(x) {
##     char <- as.character(x)
##     lhs <- char[2]
##     rhss <- strsplit(char[3L], "+", fixed = TRUE)[[1]]
##     lapply(rhss, function(x) formula(paste(lhs, x, sep = " ~ ")))
## }

## This version like above but handles multiple lhs variables and the
## result is a matrix whose elements are a list whose first element is
## a formula. So `vs + am ~ mean(mpg) + mean(hp)` becomes
##
##      [,1]           [,2]          
## [1,] vs ~ mean(mpg) am ~ mean(mpg)
## [2,] vs ~ mean(hp)  am ~ mean(hp) 
##
## To get this to be 1-dimensional I think one sets the `dim` attribute
## to NULL.

formsplit <- function(x) {
    char <- as.character(x)
    lhss <- strsplit(char[2], "+", fixed = TRUE)
    rhss <- strsplit(char[3], "+", fixed = TRUE)
    outr <- outer(lhss[[1]], rhss[[1]], FUN = paste, sep = " ~ ")
    apply(t(outr), 1:2, formula) # Gives a matrix
}

## Determin if a character vector representing the right hand
## side of a formula such as `"mediqr(breaks,???)"` has a 'p='
## argument and if so what the value of the argument is. (It
## could be TRUE or another function name.
## 
## parse.for.p.arg <- function(x){ <-- There's a better way!
##     ## a comma then possibly space then 'p' then
##     ## possibly space then equals then possibly space then
##     ## capture everything up to either a comma or a ')'
##     pattern <- "^.*,[[:space:]]*p[[:space:]]*=[[:space:]]*(.*)[,)]"
##     if (!grepl(pattern, x)) {
##         return(NULL)
##     }
##     sub(pattern, "\\1", x)
## }
match.call.via.string <- function(string) {
    stopifnot(length(string) == 1L && is.character(string))
    the.function.as.str <- all.names(str2lang(string))[1]
    if(!exists(the.function.as.str)) {
        stop("The string: ",
             sQuote(string),
             " calls an unknown function")
    }
    the.function <- get(the.function.as.str)
    if (typeof(the.function) %in% c("builtin", "special")) {
        stop("The string: ",
             sQuote(string),
             " calls a primitive function which we cannot handle")
    }
    the.call <- try(match.call(the.function, call = str2lang(string)))
    if (inherits(the.call, "try-error")) {
        stop("The string:\n",
             sQuote(string),
             " caused a problem. Are the arguments correct?")
    }
    the.call
}


## Sometimes we have a list of vectors and we want to stack it.
## For example
##    List of 2
## $ x: Factor w/ 3 levels "L","M","H": 1 1 1 1 1 1 1 1 1 2 ...
## $ y: Factor w/ 3 levels "L","M","H": 1 1 1 1 1 1 1 1 1 2 ...
##
## This makes a data frame with names ind for group and values for
## values

## like utils::stack(list(A = ..., B = ...)) except utils::stack
## wants vectors (and that means not factors). (Not sure why.)
stack.data.list <- function(data.list) {
    out <- data.frame(values = unlist(data.list),
                      ind = rep(names(data.list),
                                times = sapply(data.list, length)),
                      stringsAsFactors = TRUE)
    row.names(out) <- NULL
    out
}


## Take the string version of the rhs and a list of data frames
## and get indicated p-value. Or return NULL

get.p.value <- function(rhs.string, data.list) {
    call.rhs <- match.call.via.string(rhs.string)
    p <- call.rhs$p
    if (length(data.list) == 1L || is.null(p)) {
        return(NULL)
    }

    ## These are supported p-value functions:
    p.fun.list <- list(tt = t.test,
                       ct = chisq.test)
    ok.p <- names(p.fun.list)
    p <- try(match.arg(p, ok.p), silent = TRUE)
    if (inherits(p, "try-error")) {
        message(sQuote("p"),
                " argument in ",
                rhs.string,
                " should be among following:")
        print(noquote(cbind(`p=` = ok.p)))
        stop()
    }
    p.fun <- p.fun.list[[p]]
    p.args <- eval(call.rhs$p.args)

    rhs.list <- lapply(data.list, function(dat) eval(call.rhs$x, dat))
    if (p == "tt") {
        names(rhs.list) <- c("x", "y")
    } else if (p == "ct") {
        rhs.list <- stack.data.list(rhs.list)
        names(rhs.list) <- c("x", "y")        
    }
    do.call(p.fun, c(rhs.list, p.args))$p.value
}



## Take a formula of the form y ~ f(x) and do f(x) for each value of y
## Also works with 1 ~ f(x) which gives an overall summary
rhs.by.lhs <- function(formula, data = NULL) {
    check.formula(formula)
    char <- as.character(formula)

    ## (Found in boxplot.formula and aggregate.formula)
    ## We may not have a data frame, only variables and so
    ## we want to use the formula to generate a data frame
    ## It's better (?) to do it 
    ## m <- match.call(expand.dots = FALSE)                                 
    ## if (is.matrix(eval(m$data, parent.frame())))                         
    ##     m$data <- as.data.frame(data)                                    
    

    temp.rhs <- formula(paste(char[1], char[2]))
    data.list <-
        if (identical(temp.rhs, ~ 1)) {
            list(Total = data)
        } else {
            split(data, temp.rhs) # e.g., split(warpbreaks, ~ wool)
        }

    ## See what arguments were used in the rhs
    p.value <- get.p.value(char[3], data.list)
    
    if (length(data.list) == 1L & !is.null(p.value)) {
        warning(sQuote("p.fun"),
                " argument found in ",
                sQuote(char[3]),
                " but ",
                sQuote(paste(char[2], char[1], char[3])),
                " has no groups on the left hand side")
    }

    ## apply by rhs
    results <- lapply(data.list,
                      model.frame, # I know more now. Do I limit myself with this?
                      formula = formula(paste(char[1], char[3])))
    df <- as.data.frame(results, stringsAsFactors = FALSE)
    names(df) <- names(data.list)
    ## Some functions include labels and categories as a comment
    ## attribute which model.frame doesn't blitz
    cmt <- comment(df[[1L]])
    ##
    if (is.null(cmt)) {
        df$variable <- sapply(results, names)[1:nrow(df)]
        df$category <- NA_character_
    } else {
        df$variable <- cmt[1L]
        df$category <- if (length(cmt) == 1L) NA_character_ else cmt[-1L]
    }
    ## Put `variable` and `category` first
    df <- df[, c("variable", "category",
                 setdiff(names(df), c("variable", "category")))]
    df$p.value <- p.value
    df
}
## rhs.by.lhs(wool ~ mediqr(breaks, p = t.test), warpbreaks)
## rhs.by.lhs(1 ~ mediqr(breaks, p = t.test), warpbreaks)

## --------------------------------------------------------------------------------
          
p.check <- function(x) {
    any(grepl("^p\\.value", names(x)))
}

add.p.value.column.if.needed <- function(x) {
    p.value.status <- sapply(x, p.check)
    if (any(p.value.status) & !all(p.value.status)) {
        for (i in which(!p.value.status)) {
            x[[i]]$p.value <- NA_real_
        }
    }
    x
}

## --------------------------------------------------------------------------------

## Take a list `x` of data frames and how many rows
## and do something along the lines of
## dataframe(x, nrows = 2)
##
## [[1]]
##    variable category        0        1
## 1 mean(mpg)       <NA> 16.61667 24.55714
##
## [[2]]
##   variable category        0        1
## 1 mean(hp)       <NA> 189.7222 91.35714
##
## [[3]]
##    variable category        0        1     2
## 1 mean(mpg)       <NA> 17.14737 24.39231  22.5
##
## [[4]]
##   variable category        0        1      2
## 1 mean(hp)       <NA> 160.2632 126.8462   88.1
arrange.results <- function(x, nrows, ncols) {
    ## data frame we get if there is only one
    ## lhs variable
    ## We can't rbind if only some data frames have p.values
    x. <- add.p.value.column.if.needed(x[1:nrows])
    out <- out.first <- do.call(rbind, x.)

    ## Now merge in repeatedly other rhs variables (if any)
    if (ncols > 1) {
        ## indices of ultimate arrangement
        indices <- matrix(1:(nrows * ncols), nrow = nrows, byrow = FALSE)
        
        for (i in 2:ncols) {
            ## One element could have a p.value column and the other not
            ## so check if any do:
            ## browser()
            x. <- add.p.value.column.if.needed(x[indices[,i]])
            out.next <- do.call(rbind, x.)
            
            out <- merge(out,
                         out.next,
                         by = c("row.names", "variable", "category"))
            out$Row.names <- NULL
            out
        }
    }

    out
}

## This is how it works:
## yat(wool ~ meansd(log(breaks),
##                   p = "tt",
##                   p.args = list(var.equal = TRUE)),
##    data = warpbreaks)
##
## - Parse the formula so it's an array of group ~ f(x) (which this is)
## - Parse a string version of the f(x) to see if the p.fun
##   argument is not NULL
## - If it is not null, it's assumed to be a function and so we need
##   to apply that p-value to 


check.formula <- function(formula) {
    ## From aggregate.formula
    if (missing(formula) || !inherits(formula, "formula")) 
        stop(sQuote("formula"), " missing or incorrect")
    if (length(formula) != 3L) 
        stop(sQuote("formula"), " must have both left and right hand sides")
    formula
}
check.data <- function(data) {
    if (is.null(data))
        stop(sQuote("yat"),
             " needs a ",
             sQuote("data"),
             " argument",
             call. = FALSE)
    if (is.matrix(data))
        data <- as.data.frame(data)
    data
}


## yat stands for yet another table
yat <- function(formula, data = NULL) {
    formula <- check.formula(formula)
    data <- check.data(data)
    forms <- formsplit(formula)
    dims <- dim(forms)
    dim(forms) <- NULL
    smry <- lapply(forms, rhs.by.lhs, data = data)
    arrange.results(smry, dims[1], dims[2])
}




## ---------------------------------------------------------------------------

## Summary functions that format their output. The functions can
## have two special arguments. One is 'p.fun' and it is used
## to specify the function used for calculating a p-value. The
## other is p.args which is a list of arguments passed to 'p.fun'.
## both are NULL by default.
##

## take off the sign for -0 or -0.0, -0.00, etc.
unsign.zero <- function(x) {
    sub("^-(0*\\.?0*)$", "\\1", x)
}



## To recap: The above is an example. A summary function needs to 
## have a second argument that's lab, some formatting
## arguments, a p argument which specifies the p-value function
## or a short string that represents a p-value function like
## "t" for "t.test", 
## and '...' which will be passed to the p-value function

## The above doesn't work because when doing rhs.by.lhs(wool ~ med(breaks, p = t.test))
## the only time I'm calling med() is when I have a subgroup. I might want
## rhs.by.lhs(wool ~ med(breaks), p = t.test) but how do I get the p OUTSIDE
## of med? What I really want is to have an expression like quote(wool ~ med(sqrt(breaks), p = t.test))
## and be able to peel off or turn off the `p = t.test` argument. It's kind of the
## reverse of do.call(med, list(sqrt(breaks), p = t.test)). Can I do 


## Number in each group (as in a header)
nequals <- function(x, lab = NULL) {
    n <- table(x)
    out <- sprintf("%s (n=%d)", names(n), n)
    comment(out) <- c(if (is.null(lab)) {
                          deparse1(match.call()$x)
                      }else {
                          lab
                      }, names(n))
    out <- setNames(out, comment(out)[-1L])
    ## But now also prepend the 'lab' to the first element's name    
    names(out)[1] <- paste(lab, names(out)[1], sep = ": ")
    out
}


## Number and percentage
npct <- function(x, lab = NULL, ref = NULL, dig = 0, p = NULL, p.args = NULL, ...) {
    n <- table(x)
    p <- 100 * proportions(n)
    if (!is.null(ref)) {
        n <- n[as.character(ref)]
        p <- p[as.character(ref)]
    }
    out <- sprintf("%s (%s%%)",
                   formatC(n, format = "d"),
                   formatC(p, format = "f", digits = dig))
    ## First element of the comment character vector is the label
    ## the rest are the levels/category. A length-one comment has no
    ## category
    comment(out) <- c(if (is.null(lab)) {
                          deparse1(match.call()$x)
                      } else {
                          lab
                      },
                      names(n))
    out <- setNames(out, comment(out)[-1L])
    ## But now also prepend the 'lab' to the first element's name    
    names(out)[1] <- paste(lab, names(out)[1], sep = ": ")
    out
}

## This shows breaks being listed second
## yat(1 + wool ~
##       npct(tension, lab = "Tension") +
##       mediqr(breaks),
##     data = warpbreaks)

## +   variable category       Total           A           B
## 1     <NA>       <NA> 26 (18, 34) 26 (20, 36) 24 (18, 29)
## 2  Tension          H    18 (33%)     9 (33%)     9 (33%)
## 3  Tension          L    18 (33%)     9 (33%)     9 (33%)
## 4  Tension          M    18 (33%)     9 (33%)     9 (33%)

mediqr <- function(x, lab = NULL, dig = 0, p = NULL, p.args = NULL, ...) {
    smry <- quantile(x, c(0.5, 0.25, 0.75), na.rm = TRUE)
    out <- sprintf("%s (%s, %s)",
                   unsign.zero(formatC(smry[1], format = "f", digits = dig)),
                   unsign.zero(formatC(smry[2], format = "f", digits = dig)),
                   unsign.zero(formatC(smry[3], format = "f", digits = dig)))
    comment(out) <- if (is.null(lab)) {
                        deparse1(match.call()$x)
                    }else {
                        lab
                    }
    setNames(out, lab)
}

meansd <- function(x, lab = NULL, dig = 0, p = NULL, p.args = NULL, ...) {
    smry <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    out <- sprintf("%s (%s)",
                   unsign.zero(formatC(smry[1], format = "f", digits = dig)),
                   unsign.zero(formatC(smry[2], format = "f", digits = dig)))
    comment(out) <- if (is.null(lab)) {
                        deparse1(match.call()$x)
                    }else {
                        lab
                    }
    setNames(out, lab)
}
