# Header (from original opus_miner.cpp)
opusHeader <- c("OPUS Miner: Filtered Top-k Association Discovery of Self-Sufficient Itemsets",
                "Version 1.2",
                "Copyright (C) 2012-2016 Geoffrey I Webb",
                "This program comes with ABSOLUTELY NO WARRANTY. This is free software,",
                "and you are welcome to redistribute it under certain conditions.",
                "See the GNU General Public Licence <http://www.gnu.org/licenses/> for details.",
                "",
                "If you publish results obtained by using this software please cite:",
                "  Webb, G.I. & Vreeken, J. (2014) Efficient Discovery of the Most Interesting Associations.",
                "  ACM Transactions on Knowledge Discovery from Data. 8(3), Art. no. 15.")

# add: "convert" parameter, do conversion as in file-reading but on already-existing data
# converstion itemlist -> tidlist
opusR_ <- function(f = NULL,         # file
                   d = NULL,         # data
                   k = 100,          # k
                   pc = FALSE, # printClosures
                   fi = TRUE,  # filterItemsets
                   sl = FALSE, # searchByLift
                   cf = TRUE,  # correctForMultCompare
                   rd = TRUE)  # redundancyTests
  {

  cat("HEADER", "\n\n")

  k <- ifelse(k < 1, 1, k)
  p <- c(pc, fi, sl, cf, rd)

  output <- NULL

  if (is.character(f) && file.exists(f)) {

    try({

      cat("Reading file...")

      T1 <- proc.time()[3]

      # type = character
      raw <- readChar(f, file.info(f)$size, TRUE)

      # type = vector of character (1st and only element of list of length 1)
      raw <- strsplit(raw, split = "\n", fixed = TRUE)[[1]]

      # *** or strsplit(..., split = "\\s+") ***
      # type = list of vector of character
      items <- strsplit(raw, " ", fixed = TRUE)

      noOfTransactions <- length(items)

      # type = vector of character
      index <- unique(unlist(items, FALSE, FALSE))

      noOfItems <- length(index)

      # replace item name (character) with item index (integer)
      # faster than "lapply(items, match, index)"
      # type = list of integer
      items_int <- unname(
        split(
          match(unlist(items), index),
          rep(
            seq_along(items),
            lengths(items)
          )
        )
      )

      # intermediate steps to allow "jagged" transpose

      # type = vector of integer
      items_int_flat <- unlist(items_int, recursive = FALSE, use.names = FALSE)

      # exchange item index (integer) with transaction number (integer)
      # subtract 1 to index from 0 (for C++)
      # flatten
      # type = vector of integer
      trans <- rep(seq_along(items_int), lengths(items_int)) - as.integer(1)

      # C <- split(A, B): place each element of A, A[i], at C[[B[i]]]
      # ie, "swap" the transaction number with the item index
      # ie, like a "jagged" transpose of the transaction numbers and the item index nubmers
      # type = list of vector of integer
      tidList <- unname(split(trans, items_int_flat))

      T2 <- proc.time()[3]

      cat(" (", round(T2 - T1, 2), " seconds)\n\n", sep = "")
      output <- .opusHelper(tidList, noOfItems, noOfTransactions, k, p)

      T3 <- proc.time()[3]

      cat("Total time: ", round(T3 - T1, 2), " seconds.\n", sep = "")

      output$itemset <- lapply(output$itemset, function(v){index[v + 1]}) # "decode"

      if (pc == FALSE) {
        output$closure <- NULL
      } else {
        output$closure <- lapply(output$closure, function(v){index[v + 1]}) # "decode"
      }

      T4 <- proc.time()[3]

      cat("**Decode time: ", round(T4 - T3, 2), " seconds.**\n")

    }) # try({...
  } # if (is.character(f) && file.exists(f)) {...

  return(output)
}

getRecords <- function(itemset, index = NULL) {
  if (is.null(index)) {
    index = 1:length(itemset[[1]])
  }
  tmp <- as.data.frame(sapply(itemset, "[", index))
  tmp$count <- as.integer(tmp$count)
  tmp$value <- as.numeric(tmp$value)
  tmp$p <- as.numeric(tmp$p)
  tmp$self_sufficient <- as.logical(tmp$self_sufficient)
  return(tmp)
}
