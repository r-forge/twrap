## Example call is something like:
## yat(Species ~ mean(Sepal.Length) + ... + mean(Petal.Width), iris)

## formula split in that y ~ x + z ==>  list(y ~ x, y ~ z)
formsplit <- function(x) {
    char <- as.character(x)
    lhs <- char[2]
    rhss <- strsplit(char[3L], "+", fixed = TRUE)[[1]]
    lapply(rhss, function(x) formula(paste(lhs, x, sep = " ~ ")))
}

## This is like formsplit1 but handles multiple lhs variables and the
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

## Take a formula of the form y ~ f(x) and do f(x) for each value of y
## Can version 2 handle formulas with a constant 1 ~ mean(mpg)?
rhs.by.lhs <- function(formula, data) {
    char <- as.character(formula)
    newrhs <- formula(paste(char[1], char[2]))
    groups <-
        if (identical(newrhs, ~ 1)) {
            list(Total = data)
        } else {
            ## split by lhs. Uses split method for data.frame
            split(data, formula(paste(char[1], char[2])))
        }
    ## apply by rhs
    results <- lapply(groups,
                      model.frame,
                      formula = formula(paste(char[1], char[3])))
    df <- as.data.frame(results)
    names(df) <- names(groups)
    ## Some functions include labels and categories as comments
    ## attribute which model.frame doesn't blitz
    cmt <- comment(df[[1L]])

    if (is.null(cmt)) {
        df$variable <- sapply(results, names)[1:nrow(df)]
        df$categories <- NA_character_
    } else {
        df$variable <- cmt[1L]
        df$categories <- if (length(cmt) == 1L) NA_character_ else cmt[-1L]
    }
    ## Put `variable` and `categories` first
    df[, c("variable", "categories",
           setdiff(names(df), c("variable", "categories")))]
}

## Take a list `x` of data frames and how many rows
## and do something along the lines of
## dataframe(x, nrows = 2)
##
## [[1]]
##    variable categories        0        1
## 1 mean(mpg)       <NA> 16.61667 24.55714
##
## [[2]]
##   variable categories        0        1
## 1 mean(hp)       <NA> 189.7222 91.35714
##
## [[3]]
##    variable categories        0        1     2
## 1 mean(mpg)       <NA> 17.14737 24.39231  22.5
##
## [[4]]
##   variable categories        0        1      2
## 1 mean(hp)       <NA> 160.2632 126.8462   88.1
arrange.results <- function(x, nrows, ncols) {
    ## data frame we get if there is only one
    ## lhs variable
    out <- do.call(rbind, x[1:nrows])

    ## Now merge in repeatedly other rhs variables (if any)
    if (ncols > 1) {
        ## indices of ultimate arrangement
        indices <- matrix(1:(nrows * ncols), nrow = nrows, byrow = FALSE)
        
        for (i in 2:ncols) {
            out.next <- do.call(rbind, x[indices[,i]])
            out <- merge(out,
                         out.next,
                         by = c("variable", "categories"))
        }
    }
    out
}

## yat stands for yet another table
yat <- function(formula, data) {
    forms <- formsplit(formula)
    dims <- dim(forms)
    dim(forms) <- NULL
    smry <- lapply(forms, rhs.by.lhs, data = data)
    arrange.results(smry, dims[1], dims[2])
}


## One of a few summary functions that format their output
mediqr <- function(x, lab = NULL, dig = 0) {
    smry <- quantile(x, c(0.5, 0.25, 0.75), na.rm = TRUE)
    out <- sprintf("%s (%s, %s)",
                   formatC(smry[1], format = "f", digits = dig),
                   formatC(smry[2], format = "f", digits = dig),
                   formatC(smry[3], format = "f", digits = dig))
    comment(out) <- if (is.null(lab)) NA_character_ else lab
    setNames(out, lab)
}

meansd <- function(x, lab = NULL, dig = 0) {
    smry <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
    out <- sprintf("%s (%s)",
                   formatC(smry[1], format = "f", digits = dig),
                   formatC(smry[2], format = "f", digits = dig))
    comment(out) <- if (is.null(lab)) NA_character_ else lab
    setNames(out, lab)
}

npct <- function(x, lab = NULL, ref = NULL, dig = 0) {
    n <- table(x)
    p <- 100 * proportions(n)
    if (!is.null(ref)) {
        n <- n[ref]
        p <- p[ref]
    }
    out <- sprintf("%s (%s%%)",
                   formatC(n, format = "d"),
                   formatC(p, format = "f", digits = dig))
    ## First element of the comment character vector is the label
    ## the rest are the levels/categories. A length-one comment has no
    ## categories
    comment(out) <- c(if (is.null(lab)) NA_character_ else lab, names(n))
    out <- setNames(out, comment(out)[-1L])
    ## But now also prepend the 'lab' to the first element's name    
    names(out)[1] <- paste(lab, names(out)[1], sep = ": ")
    out
}
