## Example call is something like:
## yat(Species ~ mean(Sepal.Length) + ... + mean(Petal.Width), iris)

## formula split in that y ~ x + z ==>  list(y ~ x, y ~ z)
formsplit <- function(x) {
    char <- as.character(x)
    lhs <- char[2]
    rhss <- strsplit(char[3L], "+", fixed = TRUE)[[1]]
    lapply(rhss, function(x) formula(paste(lhs, x, sep = " ~ ")))
}

## Take a formula of the form y ~ f(x) and do f(x) for each value of y
rhs.by.lhs <- function(formula, data) {
    char <- as.character(formula)
    ## split by lhs
    groups <- split(data, formula(paste(char[1], char[2])))
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
    } else{
        df$variable <- cmt[1L]
        df$categories <- if (length(cmt) == 1L) NA_character_ else cmt[-1L]
    }
    ## Put `variable` and `categories` first
    df[, c("variable", "categories",
           setdiff(names(df), c("variable", "categories")))]
}

## yat stands for yet another table
yat <- function(formula, data) {
    forms <- formsplit(formula)
    smry <- lapply(forms, rhs.by.lhs, data = data)
    do.call(rbind, smry)
}

## Summary functions that format their output
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
