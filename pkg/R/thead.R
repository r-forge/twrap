### Early code to generate a thead object via a formula interface
f <- ~ a + b:(c + d) + P

### would print as: 
### -------------
### a |   b   | P
###   |-------|
###   | c | d |
### -------------


### Simple toy examples
f <- ~ a + b:(c + d) + P          # works 
f <- ~ a + b:(c + d) + e:(f + g)  # works 
f <- ~ Characteristic + Control + Disease:(aMCI + AD) + P # works
f <- ~ `Patient characteristic` + P

### Can I handle duplicates?
f <- ~ A + B + P + E + F + P
f <- ~ `Pumpkin pie` + B + P + E + F + P

fixdups <- function(f) {

  if(!is.formula(f))
    stop(sQuote("f"), " must be a formula")

  ft <- terms(f, keep.order = TRUE, simplify = FALSE)
  ftl <- attr(ft, "term.labels")
  fchar <- as.character(f)[2]

  ftl.count <- numeric(length(ftl))

  ## ftl.count[1] <-

    
  fchar.split <- strsplit(fchar, 
                          split = c(" *\\+ *"),
                          perl = TRUE)

  ## pseudo:   
  for (i in 1:length(ftl))
     find out how many times term i is in fchar
     if more than one, then sub() that many times minus 1.
     need to track what to peel off from each after making it
     unique. That is, need to know how to revert.


  
  fchar <- as.character(formula)[2]	
  
  ## split on non-word characters (\W) that may
  ## be preceded or followed by whitespace
  f2 <- strsplit(f, "\\s*\\W\\s*", perl = TRUE)[[1]]
  return(f2)

  f2. <- make.unique(f2)
  old <- f2[f2 != f2.]
  new <- f2.[f2 != f2.]
  for(i in 1:length(old))
     f <- sub(paste("", old[i], sep = "") 
              paste(new[i], f))
          
}
fixdups(f)

### f <- ~ A + B:(C + D:(E + F)) + P
### 
### ----------------        
### A |   B     | P
###   | -----   |
###   | C   D   |
###   |   ----- |
###   |   E   F |
### ----------------



ff <- terms(f, keep.order = TRUE)
attr(ff, "intercept") <- 0

### 'm' for matrix
m <- attr(ff, "factors")

### 'tl' for terms.labels
tl <- attr(ff, "term.labels")

### rows and columns in 'm'
ncols <- ncol(m)
nrows <- max(attr(ff, "order"))

### dimnames of m
rn <- gsub("`", "", row.names(m))
cn <- dimnames(m)[[2]]
### take the 'lowest-level' variable in the term
cn <- sub(".*:(.*$)", "\\1", cn, perl = TRUE)
dimnames(m)[[2]] <- cn

### summarize m
rn.sums <- apply(m, 1, FUN = function(x) sum(x != 0))
cn.sums <- apply(m, 2, FUN = function(x) sum(x != 0))
cn.sums <- attr(ff, "order")

### order of all variables ###
### not just terms         ###
### leads to rowspans?     ###
o <- rn.sums
o[cn] <- cn.sums

h <- data.frame(rowstart = rep(NA, length(rn)),
                colstart = NA,
                rowspan = NA,
                colspan = NA,
                footnote = NA)
row.names(h) <- rn

## can I generate 'pats' using paste() based on nrows?
pats <- c("(.*):.*",
          ".*:(.*).*")

### peel of highest level variable from term
rowstart1 <- gsub("(.*):.*", "\\1", tl, perl = TRUE)
h[rowstart1, "rowstart"] <- 1

### peel of lowest level variable from term
rowstart2 <- gsub(".*:(.*)$", "\\1", grep(":", tl, value = TRUE))
h[rowstart2, "rowstart"] <- 2

### beautiful!
h[, "colstart"] <- 
  apply(m, 1, 
        FUN = function(x) min((1:ncols)[x != 0]))

h[, "rowspan"] <- 1 + nrows - o
h[, "colspan"] <- rn.sums

## h <- h[order(h[, "rowstart"], h[, "colstart"]),]

T <- function(x, file) {
  cat("<html>\n<table border = \"1\">\n", file = file)
  	
  rseq <- unique(h$rowstart)	
  for(i in rseq) {
  	x <- subset(h, rowstart == i)
    cat("<tr>\n", file = file, append = TRUE)
    cat(paste(" <th ", 
          "rowspan = \"",
          x$rowspan, 
          "\" colspan = \"",
          x$colspan,
          "\"> ",
          row.names(x), 
          " </th>\n",
          sep = ""), file = file, append = TRUE)
    cat("</tr>\n", file = file, append = TRUE)
    }  
  
  cat("</table>\n</html>\n", file = file, append = TRUE)		
}
T(h, "test.html")
