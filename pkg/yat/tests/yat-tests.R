## Tests for yat and unexported functions yat uses
library(yat)
library(stats)

## ----------------------------------------------------------------------
## formsplit 

observed <- yat:::formsplit(A ~ B + C)
expected <- structure(list(formula(A ~ B),
                           formula(A ~ C)),
                      .Dim = 2:1)
stopifnot(all.equal(observed, expected))

observed <- yat:::formsplit(1 + A + B ~ C + D + E)
expected <-
    structure(list(formula(1 ~ C), formula(1 ~ D), formula(1 ~ E),
                   formula(A ~ C), formula(A ~ D), formula(A ~ E),
                   formula(B ~ C), formula(B ~ D), formula(B ~ E)),
              .Dim = c(3, 3))
stopifnot(all.equal(observed, expected))

## ----------------------------------------------------------------------
## match.call.via.string


g <- function(x) x + 12
observed <- yat:::match.call.via.string("g(32)")
expected <- call("g", x = 32)
stopifnot(identical(observed, expected))

string <- "mediqr(breaks, p = \"tt\", p. = list(var.equal = TRUE))"
observed <- yat::: match.call.via.string(string)
expected <- call("mediqr",
                 x = quote(breaks),
                 p = "tt",
                 p.args = quote(list(var.equal = TRUE)))
stopifnot(identical(observed, expected))

string <- "lm(breaks ~ tension*wool, d = warpbreaks, subset = wool != 'A')"
observed <- yat:::match.call.via.string(string)


expected <- call("lm",
                 formula = breaks ~ tension*wool,
                 data = quote(warpbreaks),
                 subset = quote(wool != "A"))

## Not suer why the objects aren't identical
stopifnot(identical(deparse1(observed), deparse1(expected)))


## ----------------------------------------------------------------------
## stack.data.list

observed <- yat:::stack.data.list(list(A = 1:3, B = 4:9, C = 10:20))
expected <- data.frame(values = unlist(list(A = 1:3, B = 4:9, C = 10:20)),
                       ind = c(rep("A", length(1:3)),
                               rep("B", length(4:9)),
                               rep("C", length(10:20))),
                       stringsAsFactors = TRUE)
row.names(expected) <- NULL
identical(observed, expected)


## When all the elements are the same length
n <- 10
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
observed <- yat:::stack.data.list(list(x1 = x1, x2 = x2, x3 = x3))
expected <- stack(list(x1 = x1, x2 = x2, x3 = x3))
stopifnot(identical(observed, expected))



## ----------------------------------------------------------------------
## get.p.value

observed <- yat:::get.p.value("mediqr(breaks, p = 't', p.args = list(var.equal = TRUE))",
                              split(warpbreaks, ~ wool))
expected <- t.test(breaks ~ wool, warpbreaks, var.equal = TRUE)$p.value
stopifnot(identical(observed, expected))


observed <- yat:::get.p.value("npct(cyl, p = 'ct', p.args = list(correct = FALSE))",
                              split(mtcars, ~ am))
expected <- with(mtcars, chisq.test(cyl, am, correct = FALSE)$p.value)
stopifnot(identical(observed, expected))

set.seed(1)
observed <- yat:::get.p.value("npct(cyl, p = 'ct', p.args = list(simulate.p.value = TRUE, B = 1e5))",
                              split(mtcars, ~ am))
set.seed(1)
expected <- with(mtcars,
                 chisq.test(am, cyl, simulate.p.value = TRUE, B = 1e5)$p.value)
stopifnot(identical(observed, expected))

## ----------------------------------------------------------------------
## rhs.by.lhs

observed <- yat:::rhs.by.lhs(1 ~ mean(Sepal.Length), iris)
expected <- structure(list(variable = "mean(Sepal.Length)",
                           category = NA_character_, 
                           Total = mean(iris$Sepal.Length)),
                      class = "data.frame",
                      row.names = 1L)
stopifnot(identical(observed, expected))

d <- data.frame(y = rnorm(1e5))
observed <- yat:::rhs.by.lhs(1 ~ meansd(y, dig = 1), data = d)
expected <- structure(list(variable = "y",
                           category = NA_character_, 
                           Total = structure("0.0 (1.0)",
                                             comment = "y")),
                      row.names = 1L,
                      class = "data.frame")
stopifnot(identical(observed, expected))

observed <- yat:::rhs.by.lhs(1 ~ mediqr(breaks), warpbreaks)

expected <- structure(list(variable = "breaks",
                           category = NA_character_, 
                           Total = structure("26 (18, 34)",
                                             comment = "breaks")),
                      class = "data.frame",
                      row.names = 1L)
stopifnot(identical(observed, expected))


observed <- yat:::rhs.by.lhs(wool ~ meansd(breaks, "Breaks", p = "t"), warpbreaks)
expected <- structure(list(variable = "Breaks",
                           category = NA_character_, 
                           A = structure(c(Breaks = "31 (16)"), # checked
                                         comment = "Breaks"), 
                           B = structure(c(Breaks = "25 (9)"),  # checked
                                         comment = "Breaks"), 
                           p.value = t.test(breaks ~ wool,
                                            data = warpbreaks)$p.value),
                      row.names = 1L,
                      class = "data.frame")
stopifnot(identical(observed, expected))


observed <- yat:::rhs.by.lhs(1 ~ npct(am, "Manual transmission", dig = 1, ref = 0), data = mtcars)
expected <- structure(list(variable = "Manual transmission",
                           category = "0", 
                           Total = structure(c(`Manual transmission: 0` = "19 (59.4%)"),
                                             comment = c("Manual transmission", 
                                                         "0"))),
                      row.names = 1L,
                      class = "data.frame")
stopifnot(identical(observed, expected))

## ----------------------------------------------------------------------
## p.check

x <- list(A = data.frame(a = 1, b = 2, p.value = 0.293),
          B = data.frame(a = 3, b = 4),
          C = data.frame(a = 10, b = 18, p.value = NA_real_))
observed <- sapply(x, yat:::p.check)
expected <- c(A = TRUE, B = FALSE, C = TRUE)
stopifnot(identical(observed, expected))


## ----------------------------------------------------------------------
## add.p.value.column.if.needed

observed <- yat:::add.p.value.column.if.needed(x)
expected <- list(A = structure(list(a = 1, b = 2, p.value = 0.293),
                               class = "data.frame",
                               row.names = c(NA, -1L)),
                 B = structure(list(a = 3, b = 4, p.value = NA_real_),
                               row.names = c(NA, -1L),
                               class = "data.frame"),
                 C = structure(list(a = 10, b = 18, p.value = NA_real_),
                               class = "data.frame",
                               row.names = c(NA, -1L)))
stopifnot(identical(observed, expected))




## ----------------------------------------------------------------------
## arrange.results

mylist <- list(data.frame(variable = "Apple",
                          category = c("Honey Crisp",
                                         "Pink Lady"),
                          organic = c("10 (15%)", "20 (85%)"),
                          conventional = c("3 (5%)", "50 (95%)")),
               data.frame(variable = "Pear",
                          category = NA_character_,
                          organic = "7.1",
                          conventional = "2.5"))

observed <- yat:::arrange.results(mylist, 2, 1)
expected <-
    structure(list(variable = c("Apple", "Apple", "Pear"),
                   category = c("Honey Crisp", "Pink Lady", NA),
                   organic = c("10 (15%)", "20 (85%)", "7.1"), 
                   conventional = c("3 (5%)", "50 (95%)", "2.5")),
              row.names = c(NA, -3L),
              class = "data.frame")
stopifnot(identical(observed, expected))
    

set.seed(393)
n <- 20
d <- data.frame(group = sample(letters[1:6], size = n, replace = TRUE),
                x1 = rpois(n, 3),
                x2 = rpois(n, 5),
                x3 = rpois(n, 7))
data.list <- lapply(yat:::formsplit(1 + group ~
                                        median(x1) +
                                        median(x2) +
                                        median(x3)),
                    yat:::rhs.by.lhs, d)

observed <- yat:::arrange.results(data.list, 3, 2)

expected <- structure(list(variable = c("median(x1)", "median(x2)", "median(x3)"),
                           category = c(NA_character_, NA_character_, NA_character_),
                           Total = c(2, 4, 7.5),
                           a = c(4, 4.5, 8),
                           b = c(2L, 5L, 8L), 
                           c = c(2.5, 3.5, 7),
                           d = c(2.5, 3.5, 8),
                           e = c(3L, 3L, 8L), 
                           f = c(1, 5.5, 5)),
                      row.names = c(NA, -3L),
                      class = "data.frame")
stopifnot(all.equal(observed, expected))


## ----------------------------------------------------------------------
## yat

data(mtcars)
observed <- yat(1 + wool + tension ~ mean(breaks), warpbreaks)
expected <- data.frame(variable = "mean(breaks)",
                       category = NA_character_,
                       Total = mean(warpbreaks$breaks),
                       A = with(subset(warpbreaks, wool == "A"), mean(breaks)),
                       B = with(subset(warpbreaks, wool == "B"), mean(breaks)),                       
                       L = with(subset(warpbreaks, tension == "L"), mean(breaks)),
                       M = with(subset(warpbreaks, tension == "M"), mean(breaks)),
                       H = with(subset(warpbreaks, tension == "H"), mean(breaks)),
                       stringsAsFactors = FALSE)
stopifnot(identical(observed, expected))

observed <- yat(1 + wool ~ mediqr(breaks, "Breaks", p = "t"), warpbreaks)
expected <- structure(list(variable = "Breaks",
                           category = NA_character_, 
                           Total = c(Breaks = "26 (18, 34)"),
                           A = c(Breaks = "26 (20, 36)"), 
                           B = c(Breaks = "24 (18, 29)"),
                           p.value = 0.109830321713655),
                      row.names = c(NA, -1L),
                      class = "data.frame")
stopifnot(all.equal(observed, expected))

## When the Total and by-wool data sets are merged I want to
## keep Tension appearing before breaks
observed <- yat(1 + wool ~ npct(tension, "Tension") + mediqr(breaks), warpbreaks)

expected <- structure(list(variable = c("Tension",
                                        "Tension",
                                        "Tension", 
                                        "breaks"),
                           category = c("L", "M", "H", NA),
                           Total = c(`Tension: L` = "18 (33%)", 
                                     M = "18 (33%)",
                                     H = "18 (33%)",
                                     "26 (18, 34)"),
                           A = c(`Tension: L` = "9 (33%)", 
                                 M = "9 (33%)",
                                 H = "9 (33%)",
                                 "26 (20, 36)"),
                           B = c(`Tension: L` = "9 (33%)", 
                                 M = "9 (33%)",
                                 H = "9 (33%)",
                                 "24 (18, 29)")),
                      row.names = c(1L, 2L, 3L, 4L),
                      class = "data.frame")
stopifnot(identical(observed, expected))

## This was broken; it gave this:
##   variable category           A           B
## 1   breaks     <NA> 26 (20, 36) 24 (18, 29)
## 2   breaks     <NA> 26 (20, 36) 24 (18, 29)
## because I was reordering after a merge. But no merge needed here

observed <- yat(wool ~ mediqr(breaks) + meansd(breaks), warpbreaks)
expected <- structure(list(variable = c("breaks", "breaks"),
                           category = c(NA_character_, NA_character_),
                           A = structure(c("26 (20, 36)", "31 (16)"), comment = "breaks"), 
                           B = structure(c("24 (18, 29)", "25 (9)"), comment = "breaks")),
                      row.names = c(NA, -2L),
                      class = "data.frame")
stopifnot(identical(observed, expected))

observed <- yat(1 + wool ~ mediqr(breaks) + meansd(breaks), warpbreaks)


## ----------------------------------------------------------------------
## unsign.zero

stopifnot(identical(yat:::unsign.zero("-0.0"), "0.0"))
stopifnot(identical(yat:::unsign.zero("-0.00"), "0.00"))
stopifnot(identical(yat:::unsign.zero("-0.0003"), "-0.0003"))
stopifnot(identical(yat:::unsign.zero("-0000.000"), "0000.000"))
stopifnot(identical(yat:::unsign.zero("-.0"), ".0"))

## ----------------------------------------------------------------------
## Below is failing because when I merge the total piece with the
## by 'g' piece, the missing values in variables and categories means
## I can't merge correctly. This makes me wonder if I should separate
## the formatting from the calculations. There could be a third
## column called estimates with values such as c("est", "lower", "upper") or
## what have you in which I would stack rather than format values such as
## mediqr. This is only a 
## d <- data.frame(y = rnorm(1e4),
##                 g = sample(letters[1:4], size = 1e4, replace = TRUE))
## yat(1 + g ~ quantile(y, c(0.1, 0.2, 0.5, 0.8, 0.9)) + mean(y), d) # problem
## yat(1 + g ~ median(y) + mean(y), d) # OK






## #
                                        # data(esoph)
## observed <- yat(1 ~ npct(agegp, lab = "Hello, World!"), esoph)
## expected <- data.frame(variable = "npct(agegp)",
##                        category = levels(esoph$agegp),
##                        Total = tabulate(unclass(esoph$agegp)))
## expected
                                           
