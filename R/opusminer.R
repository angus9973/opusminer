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
        n <- v[!(v %in% index)]
        index <- c(index, n)
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

opusR_int <- function(fileName) {
  index <- NULL
  tidList <- list()
  ix <- as.integer(0)
  if (is.character(fileName) && file.exists(fileName)) {
    try({
      f <- file(fileName, open = "r")
      l <- readLines(f, n = 1)
      while(length(l) != 0) {
        v <- unlist(strsplit(l, "\\s+"))
        n <- v[!(v %in% index)]
        index <- c(index, n)
        u <- match(v, index)
        m <- max(u)
        if (length(tidList) < m) {
          tidList[[m]] <- ix
          u <- u[-match(m, u)]
        }
        tidList[u] <- lapply(tidList[u], append, ix)
        ix <- ix + 1
        l <- readLines(f, n = 1)
      }
      close(f)
    })
  }
  load_data_int(tidList)
  # call cpp, return
  # decode return via index
  # return decoded
  # print(tidList[[1]][5])
  return(tidList)
}

opusR_int_FAST <- function(fileName) {
  index <- NULL
  tidList <- list()
  # ix <- as.integer(0)
  if (is.character(fileName) && file.exists(fileName)) {
    try({
      f <- file(fileName, open = "r")
      d <- readLines(f)
      d <- strsplit(d, "\\s+")
      # print(d)
      index <- unique(unlist(d))
      vals <- lapply(d, match, index)
      # print(index)
      # print(vals)

      # tidList[[length(index) + 1]] <- NULL
      # tidList[vals] <- 1

      i <- getListIndex(vals)
      v <- unname(unlist(vals, FALSE, FALSE))

      # tidList[[length(index) + 1]] <- 0
      # tidList[[length(index) + 1]] <- NULL
      #
      # tidList[v] <- i

      tidList <- unname(split(i, v))

      #
      # tmp <- lapply()
      #
      # for (i in 1:length(vals)) {
      #   tmp <- tidList[vals[[i]]]
      #
      #   tidList[vals[[i]]] <- lapply(tidList[vals[[i]]], append, i)
      #   # print(vals[[i]])
      # }


      # while(length(l) != 0) {
      #   v <- unlist(strsplit(l, "\\s+"))
      #   n <- v[!(v %in% index)]
      #   index <- c(index, n)
      #   u <- match(v, index)
      #   m <- max(u)
      #   if (length(tidList) < m) {
      #     tidList[[m]] <- ix
      #     u <- u[-match(m, u)]
      #   }
      #   tidList[u] <- lapply(tidList[u], append, ix)
      #   ix <- ix + 1
      #   l <- readLines(f, n = 1)
      # }
      close(f)
    })
  }
  load_data_int(tidList)
  # call cpp, return
  # decode return via index
  # return decoded
  # print(tidList[[1]][5])
  return(tidList)
}

getListIndex <- function(L) {
  index <- rep(seq_along(L), lengths(L))
  return(index)
}

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


testVector <- function() {
  v <- seq(1, 100, 1)
  load_data_2(v)
}

# testList <- function() {
#   l <- list()
#   for (i in 1:10) {
#     l[[i]] <- seq(1, runif(1, 1, 10), 1)
#   }
#   print(l)
#   load_data_3(l)
# }

makeBigData <- function() {
  # m <- matrix(data = rep(rep("abcdefghijklmnopqrstuvwxyz", 1e4), 1e4), ncol = 1e4)
  v <- rep((runif(500, 0, 9)), 1e6)
  return(v)
}

passBigData <- function(input) {
  load_big(input)
}
