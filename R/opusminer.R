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

opusR_ <- function(f = NULL,         # file
                   d = NULL,         # data
                   k = 100,          # k
                   p = c(pc = FALSE,  # printClosures
                         fi = TRUE, # filterItemsets
                         sl = FALSE,
                         cf = TRUE,  # correctForMultCompare
                         rd = TRUE) # redundancyTests
                   ) {

  cat("HEADER", "\n\n")

  k <- ifelse(k < 1, 1, k)

  # check

  # tmp <- NULL # to capture tidList for comparison
  output <- NULL

  if (is.character(f) && file.exists(f)) {

    try({

      cat("Reading file...")

      # T1 <- Sys.time()
      T1 <- proc.time()[3]

      # type = character
      raw <- readChar(f, file.info(f)$size, TRUE)

      # strsplit returns list (of vector of character)
      # in this case: list of length one; get first (and only) vector only
      # type = vector of character
      raw <- strsplit(raw, split = "\n", fixed = TRUE)[[1]]

      # type = list of vector of character
      items <- strsplit(raw, " ", fixed = TRUE) # fixed vector of split = ...

      # ***NEW*** (alt)
      noOfTransactions <- length(items)

      # type = vector of character
      index <- unique(unlist(items, FALSE, FALSE))

      # ***NEW*** (alt)
      noOfItems <- length(index)

      # replace item value (character) with index value (integer)
      # much faster than "lapply(items, match, index)"
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

      # flatten
      # type = vector of integer
      items_int_flat <- unlist(items_int, recursive = FALSE, use.names = FALSE)

      # exchange item index value (integer) with transaction number (integer)
      # subtract 1 to index from 0 (for C++)
      # flatten
      # type = vector of integer
      trans <- rep(seq_along(items_int), lengths(items_int)) - as.integer(1)

      # C <- split(A, B): place each element of A, A[i], at C[[B[i]]]
      # ie, "swap" the transaction number with the item index number
      # ie, like a "jagged" transpose of the transaction numbers and the item index nubmers
      # type = list of vector of integer
      tidList <- unname(split(trans, items_int_flat))

      # T2 <- Sys.time()
      T2 <- proc.time()[3]

      cat(" (", round(T2 - T1, 2), " seconds)\n\n", sep = "")
      output <- .opusHelper(tidList, noOfItems, noOfTransactions, k, p)

      # T3 <- Sys.time()
      T3 <- proc.time()[3]

      cat("Total time: ", round(T3 - T1, 2), " seconds.\n", sep = "")

      # tmp <- tidList
      # tmp <- index

      # OPTIMISE... (duplicate/reverse split method, above?)
      # Test for LARGE output sizes
      output <- lapply(output, function(i){i + 1}) # index from 1 (cf 0)
      output <- lapply(output, function(v){index[v]}) # "decode"

      # need to get other components of output as well

      # or above two together:
      # output <- lapply(output, function(v){index[v + 1]}) # "decode"

      # T4 <- Sys.time()
      T4 <- proc.time()[3]

      cat("**Decode time: ", round(T4 - T3, 2), " seconds.**\n")

    }) # try({...
  } # if (is.character(f) && file.exists(f)) {...

  # output <- lapply(output, function(x){x + 1})
  # reversed <- index[unlist(output)]

  # return(list(output, tmp))
  return(output)
}

# Wrap opusCPP() (via opusHelper())
opusR <- function(tidList,
                  nItems,
                  nTrans,
                  printClosures = FALSE,
                  filterItemsets = TRUE,
                  k = 100,
                  searchByLift = FALSE,
                  correctForMultCompare = TRUE,
                  redundancyTests = TRUE) {

  # if (is.character(fileName) && file.exists(fileName)) {

    # input <- readItemList(fileName)

    k <- ifelse(k < 1, 1, k)

    args <- c(printClosures,
              filterItemsets,
              searchByLift,
              correctForMultCompare,
              redundancyTests)

    # cat(opusHeader, sep = "\n")

    # output <- .opusHelper(input,
    #                       k,
    #                       args)

    # load_data(input)

    .opusHelper(tidList, nItems, nTrans, k, args)

    # return(output)

  # } else {
  #   cat("ERROR")
  # }
}

# comparison, opus_miner.exe takes 4:42 to load kosarak.dat
# this takes ~30s
# or less?
opusR_file_whole_FAST <- function(fileName) {

  # Doesn't need to be initialised (here or anywhere): restrict to "try" block
  # tidList <- list()

  if (is.character(fileName) && file.exists(fileName)) {

    try({

      # type = character
      raw <- readChar(fileName,
                      nchars = file.info(fileName)$size,
                      useBytes = TRUE)

      # strsplit returns list (of vector of character)
      # in this case: list of length one; get first (and only) vector only
      # type = vector of character
      raw <- strsplit(raw, split = "\n", fixed = TRUE)[[1]]

      # type = list of vector of character
      items <- strsplit(raw, " ", fixed = TRUE) # fixed vector of split = ...
      # items <- strsplit(raw, "\\s+") # regexp: slow

      # ***NEW*** (alt)
      numTrans <- length(items)

      # # ***NEW***
      # numItems <- length(items)

      # type = vector of character
      index <- unique(unlist(items, FALSE, FALSE))

      # ***NEW*** (alt)
      numItems <- length(index)

      # replace item value (character) with index value (integer)
      # much faster than "lapply(items, match, index)"
      # type = list of integer
      items_int <- unname(
        split(
          match(unlist(items), index),
          rep(
            # seq(1, length(items), 1), # slow
            seq_along(items), # fast
            lengths(items)
          )
        )
      )

      # flatten
      # type = vector of integer
      items_int_flat <- unlist(items_int, recursive = FALSE, use.names = FALSE)

      # exchange item index value (integer) with transaction number (integer)
      # subtract 1 to index from 0 (for C++)
      # flatten
      # type = vector of integer
      trans <- rep(seq_along(items_int), lengths(items_int)) - as.integer(1)

      # C <- split(A, B): place each element of A, A[i], at C[[B[i]]]
      # ie, "swap" the transaction number with the item index number
      # ie, like a "jagged" transpose of the transaction numbers and the item index nubmers
      # type = list of vector of integer
      tidList <- unname(split(trans, items_int_flat))

      # # ***NEW***
      # numTrans <- length(tidList)

      # replace *tidList* with *unname(split(...))* ?
      # load_data_whole(tidList, numItems, numTrans)

      opusR(tidList, nItems = numItems, nTrans = numTrans)

      # print(numItems)
      # print(numTrans)

    })
  }
  # Move up to try({...}) block...
  # load_data_whole(tidList)

  # return(opusR_2())
  # opusR_2()
  # toDO:
  # - convert r object on cpp side properly
  # - call CPP main function
  # - reverse CPP value via index
}

# convert itemList to tidlist
itemListToTIDList <- function (itemList) {
  tidList <- list()
  for (i in 1:length(itemList)) {
    for (j in 1:length(itemList[[i]]))
    {
      # if k is an int --> works as an index of the list position
      # if k is a chr --> works by naming the list entry lst["name"] = ...
      k <- itemList[[i]][j]
      if (k <= length(tidList)) {
        # print(tidList[[k]])
        tidList[[k]] <- c(tidList[[k]], i)
      } else {
        tidList[[k]] <- i
      }
      # tidList[[itemList[[i]][j]]] <- c(1, 3)
    }
  }
  return(tidList)
}
