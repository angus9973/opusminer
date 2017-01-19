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

    cat(opusHeader, sep = "\n")

    output <- .opusHelper(input,
                          k,
                          args)

    return(output)

  } else {
    cat("ERROR")
  }
}

# Read itemlist-format files, index unique items, create tidlist
readItemList2 <- function(fileName) {

  index <- NULL # index
  tidList <- list()
  ix <- 0

  # what about duplicate items??? eg, trans_1 : 1, 2, 3, 3, 3, 4, 6, 9...

  if (is.character(fileName) && file.exists(fileName)) {
    try({
      f <- file(fileName, open = "r")
      l <- readLines(f, n = 1) # line
      while (length(l) != 0) {

        v <- unlist(strsplit(l, "\\s+")) # vector of items
        # print(v)
        n <- v[!(v %in% index)]  # items not previously indexed
        index <- c(index, n)
        u <- match(v, index)

        # cat("u: ", u, "\n")
        # v <- as.numeric(v)
        m <- max(u)
        if (length(tidList) < m) {
          tidList[[m]] <- ix
          u <- u[-match(m, u)]
        }
        tidList[u] <- lapply(tidList[u], append, ix)
        ix <- ix + 1
        # for (i in 1:length(v)) {
        #   if (!(v[i] %in% index)) {
        #     index <- c()
        #   }
        # }
        # check if v[i] is in index
        # if not, add to index, add to tidlist (that is, add the index no as an integer)
        # then return completed tidlist (preserve index to decode tidlist into original input)
        l <- readLines(f, n = 1) # line
      }
      close(f)
    })
  } else {
    print("ERROR")
  }

  return(list(index, tidList))
}

# piecewise
opusR_piece <- function(fileName) {
  index <- NULL
  ix <- as.integer(0)
  if (is.character(fileName) && file.exists(fileName)) {
    try({
      f <- file(fileName, open = "r")
      l <- readLines(f, n = 1)
      while(length(l) != 0) {
        v <- unlist(strsplit(l, "\\s+"))
        t1 <- system.time(n <- v[!(v %in% index)])
        t2 <- system.time(index <- c(index, n))
        print(t1)
        print(t2)
        u <- match(v, index)
        # print(u)
        # print(ix)
        load_data_piece(u, ix)
        ix <- ix + 1
        l <- readLines(f, n = 1)
      }
      close(f)
    })
  }
  # call cpp, return
  # decode return via index
  # return decoded
}

# suspect indexing is too slow...
opusR_file_piece <- function(fileName) {
  index <- NULL
  ix <- as.integer(0)
  # TT <- c(0, 0, 0)
  TTB <- 0
  if (is.character(fileName) && file.exists(fileName)) {
    try({
      f <- file(fileName, open = "r")
      l <- readLines(f, n = 1)
      while(length(l) != 0) {
        raw <- strsplit(l, "\\s+")
        # v <- unlist(raw, FALSE, FALSE)

        # need to speed this up
        # T1 <- Sys.time()
        # n <- v[!(v %in% index)]
        # T2 <- Sys.time()
        # index <- c(index, n)
        # T3 <- Sys.time()
        # u <- match(v, index)
        # T4 <- Sys.time()
        #
        # TT[1] <- TT[1] + (T2 - T1)
        # TT[2] <- TT[2] + (T3 - T2)
        # TT[3] <- TT[3] + (T4 - T3)


        # index <- unique(c(index, v))
        TB1 <- Sys.time()
        # u <- match(v, index)
        TB2 <- Sys.time()
        #
        TTB <- TTB + (TB2 - TB1)


        # print(u)
        # print(ix)
        # load_data_piece(u, ix)
        ix <- ix + 1
        l <- readLines(f, n = 1)
      }
      close(f)
    })
  }

  print(TTB)

  # tst
  # return(index)

  # call cpp, return
  # decode return via index
  # return decoded
  # print(TT)
}

# comparison, opus_miner.exe takes 4:42 to load kosarak.dat
# this takes ~30s
# or less?
opusR_file_whole <- function(fileName) {

  tidList <- list()
  TT <- list()

  if (is.character(fileName) && file.exists(fileName)) {

    try({

      TT[[1]] <- Sys.time()
      f <- file(fileName, open = "r")

      TT[[2]] <- Sys.time()
      # type = vector of character
      raw <- readLines(f)

      TT[[3]] <- Sys.time()
      # print(typeof(raw))
      # print(class(raw))
      # print(mode(raw))


      # type = list of vector of character
      items <- strsplit(raw, "\\s+")

      TT[[4]] <- Sys.time()
      # type = vector of character
      index <- unique(unlist(items, FALSE, FALSE))

      TT[[5]] <- Sys.time()
      # replace item value (character) with index value (integer)
      # type = list of integer
      # items_int <- lapply(items, match, index)
      # alternative (test if faster - looks like it):
      items_int <- unname(
                     split(
                       match(unlist(items), index),
                       rep(
                         seq(1, length(items), 1),
                         lengths(items)
                         )
                       )
                     )
      # above:
      #

      TT[[6]] <- Sys.time()
      # flatten
      # type = vector of integer
      items_int_flat <- unlist(items_int, recursive = FALSE, use.names = FALSE)

      TT[[7]] <- Sys.time()
      # exchange item index value (integer) with transaction number (integer)
      # subtract 1 to index from 0 (for C++)
      # flatten
      # type = vector of integer
      trans <- rep(seq_along(items_int), lengths(items_int)) - as.integer(1)

      TT[[8]] <- Sys.time()
      # forget the english meaning of "split"
      # split(a, b):
      # (1) returns a list
      # (2) puts the elements in *a* in the position (list index) specified in *b*
      # "swap" the transaction index with the item index number
      # a bit like a "jagged" transpose of the transaction numbers
      # type = list of vector of integer
      tidList <- unname(split(trans, items_int_flat))

      TT[[9]] <- Sys.time()
      close(f)

    })
  }
  load_data_whole(tidList)
  TT[[10]] <- Sys.time()
  # toDO:
  # - call CPP
  # - reverse CPP value via index

  # to check memory footprint in try({}) block, above
  # return(tidList)
  # return(index)
  # print(TT[[2]] - TT[[1]])
  cat(format(diff(unlist(TT)), digits = 6), sep = "\n")
}

# comparison, opus_miner.exe takes 4:42 to load kosarak.dat
# this takes ~30s
# or less?
opusR_file_whole_FAST <- function(fileName) {

  tidList <- list()
  TT <- list()

  if (is.character(fileName) && file.exists(fileName)) {

    try({

      TT[[1]] <- Sys.time()
      # f <- file(fileName, open = "r")
      raw <- readChar(fileName, nchars = file.info(fileName)$size, useBytes = TRUE)

      TT[[2]] <- Sys.time()
      # type = vector of character
      # raw <- readLines(f)

      raw2 <- strsplit(raw, split = "\n", fixed = TRUE)

      TT[[3]] <- Sys.time()
      # print(typeof(raw))
      # print(class(raw))
      # print(mode(raw))


      # type = list of vector of character
      items <- strsplit(raw, "\\s+")

      TT[[4]] <- Sys.time()
      # type = vector of character
      index <- unique(unlist(items, FALSE, FALSE))

      TT[[5]] <- Sys.time()
      # replace item value (character) with index value (integer)
      # type = list of integer
      # items_int <- lapply(items, match, index)
      # alternative (test if faster - looks like it):
      items_int <- unname(
        split(
          match(unlist(items), index),
          rep(
            seq(1, length(items), 1),
            lengths(items)
          )
        )
      )
      # above:
      #

      TT[[6]] <- Sys.time()
      # flatten
      # type = vector of integer
      items_int_flat <- unlist(items_int, recursive = FALSE, use.names = FALSE)

      TT[[7]] <- Sys.time()
      # exchange item index value (integer) with transaction number (integer)
      # subtract 1 to index from 0 (for C++)
      # flatten
      # type = vector of integer
      trans <- rep(seq_along(items_int), lengths(items_int)) - as.integer(1)

      TT[[8]] <- Sys.time()
      # forget the english meaning of "split"
      # split(a, b):
      # (1) returns a list
      # (2) puts the elements in *a* in the position (list index) specified in *b*
      # "swap" the transaction index with the item index number
      # a bit like a "jagged" transpose of the transaction numbers
      # type = list of vector of integer
      tidList <- unname(split(trans, items_int_flat))

      TT[[9]] <- Sys.time()
      close(f)

    })
  }
  load_data_whole(tidList)
  TT[[10]] <- Sys.time()
  # toDO:
  # - call CPP
  # - reverse CPP value via index

  # to check memory footprint in try({}) block, above
  # return(tidList)
  # return(index)
  # print(TT[[2]] - TT[[1]])
  cat(format(diff(unlist(TT)), digits = 6), sep = "\n")
}

# getListIndex <- function(L) {
#   index <- rep(seq_along(L), lengths(L))
#   return(index)
# }

# Make index
makeIndex_itemList <- function(itemList) {
  # return(matrix(unique(unlist(itemList)), ncol = 1, dimnames = list(NULL, "items")))
  return(unique(unlist(itemList))) # , dimnames = list(NULL, "items"))
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
