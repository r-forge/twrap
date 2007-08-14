### thead takes a formula representing the column
### heads of a twrap (complete table) object
### and creates a data frame with the column heads
### as rows and columns indicating the row, col,
### rowspan, and colspan of the headers

thead <- function(formula) {
  rhs <- as.character(formula)[2]
  tf <- terms(f, keep.order = TRUE)

  ## return unique headers
  uniqHeads <- as.character(attr(tf, "variables"))[-1]
  Heads <- attr(tf, "term.labels")
  Heads
  
}

### test cases
f <- ~ A + B:(C+D) + P
f <- ~ A + B + P + C + D + P
f <- ~ A:(B:(C+D) + E:(C+D)) + P
f <- ~ B:(C+D) + E:(C+D) + P
f <- ~ B:(C+D) + E:(C2+D2) + P
f <- ~ `Big dog` + `Little cat`

### In all cases variables within an interaction term in the
### formula are re-ordered by the ordering of the
### '"variables"' attribute, which is the order in which the
### variables occur in the formula.

### Can I identify the header hierachy? It would be
### nice if the term.labels were not rearranged
