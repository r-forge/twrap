### thead takes a formula representing the column
### heads of a twrap (complete table) object
### and creates a data frame with the column heads
### as rows and columns indicating the row, col,
### rowspan, and colspan of the headers

thead <- function(formula) {
  tf <- terms(f, keep.order = TRUE)

  ## return unique headers
  heads <- as.character(attr(tf, "variables"))[-1]
  
  
}

### test cases
f <- ~ A + B:(C+D) + P
f <- ~ A + B + P + C + D + P
f <- ~ A:(B:(C+D) + E:(C+D)) + P
f <- ~ `Big dog` + `Little cat`
