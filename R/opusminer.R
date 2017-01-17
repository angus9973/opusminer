# Header (from original opus_miner.cpp)
opusHeader <- paste("OPUS Miner: Filtered Top-k Association Discovery of Self-Sufficient Itemsets",
                    "Version 1.2",
                    "Copyright (C) 2012-2016 Geoffrey I Webb",
                    "This program comes with ABSOLUTELY NO WARRANTY. This is free software,",
                    "and you are welcome to redistribute it under certain conditions.",
                    "See the GNU General Public Licence <http://www.gnu.org/licenses/> for details.",
                    "",
                    "If you publish results obtained by using this software please cite:",
                    "  Webb, G.I. & Vreeken, J. (2014) Efficient Discovery of the Most Interesting Associations.",
                    "  ACM Transactions on Knowledge Discovery from Data. 8(3), Art. no. 15.")

# Read itemlist-format files
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

# Wrap opusCPP() (via opusHelper())
opusR <- function(fileName,
                  printClosures = FALSE,
                  filterItemsets = TRUE,
                  k = 100,
                  searchByLift = FALSE,
                  correctForMultCompare = TRUE,
                  redundancyTests = TRUE) {

  if (is.character(fileName) && file.exists(fileName)) {

    input <- readItemList(fileName)

    k <- ifelse(k < 1, 1, k)

    args <- c(printClosures,
              filterItemsets,
              searchByLift,
              correctForMultCompare,
              redundancyTests)

    output <- .opusHelper(input,
                             k,
                             args)

    cat(header, sep = "\n")

    return(output)

  } else {
    cat("ERROR")
  }
}
