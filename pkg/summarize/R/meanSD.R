meanSD <- function(x, ...) UseMethod("meanSD")

meanSD.default <- function(x, na.rm = TRUE) {
  m <- mean(x, na.rm = na.rm)
  s <- sd(x, na.rm = na.rm)
  structure(c(m, s),
            names = c("Mean", "SD"),
            class = "meanSD")
}

meanSD.formula <- function(formula,
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
  s <- split(mf[[response]], mf[-response])
  
  if(overall) {
    if(any(names(s) == overall.label))
    stop(sQuote(overall.label), " is an existing level")
    s <- c(Overall = list(unlist(s)), s)
  }  
  structure(sapply(s, FUN = meanSD, ...),
            class = "meanSD")
}
