readItemList <- function(fileName) {

  d <- NULL

  if (is.character(fileName) && file.exists(fileName)) {
    try({
      f <- file(fileName, open = "r")
      d <- readLines(f)
      close(f)
    })
  } else {
    print("ERROR")
  }

  l <- NULL

  if (!is.null(d)) {
    l <- strsplit(d, "\\s+")
  }

  return(l)

}
