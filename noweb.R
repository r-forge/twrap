## Very simple interface to noweb

## Convert a literate program to source code by
## extracing the code chunks
notangle <- function(file.nw,    # noweb file
                     file.out = "",   # output file
                     name = NULL,  # name of code chunks
                     notangle.cmd = getOption("notangle.cmd"),
                     ...) {
  if(!file.exists(file.nw))
    stop(sQuote(file.nw), " does not exist")
  
  if (is.null(notangle.cmd) || !nzchar(notangle.cmd)) {
      notangle.cmd <- "notangle"
      ##From bitmap() but doesn't work here because
      ##notangle doesn't have a '-help' option
      ##rc <- system(paste(notangle.cmd, "-help > /dev/null"))
      ##if (rc != 0) 
      ##  stop("sorry, ", sQuote("notangle"), " cannot be found")
    }
  cmd <- paste(notangle.cmd,
               " ",
               ifelse(is.null(name), "",
                      paste("-R", name, sep = "")),
               " ",
               file.nw,
               ifelse(file.out == "", "",
                      paste(" > ", file.out, sep = "")),
               collapse = " ")
  rc <- system(cmd, ...)
  invisible(rc)                 
}

## Get chunk names from a noweb file
chunks <- function(file.nw) {
  lines <- readLines(file.nw)
  names(lines) <- 1:length(lines)
  sub("^<<(.*)>>=", "\\1", 
      grep("^<<", lines, value = TRUE),
      perl = TRUE) 
}
