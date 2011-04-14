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
