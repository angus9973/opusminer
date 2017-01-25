# ==============================================================================
# TO DO:
#   - check arguments being passed, avoid copying etc. (esp. large objects)
#   - piping?
#   - null items?
#   - input format
# ..............................................................................

# ==============================================================================
# EXTERNAL
# ..............................................................................

# as.tidset <- function(itemlist){}

as.data.frame.itemset <- function(itemset) {
  tmp <- as.data.frame(sapply(itemset, "["))
  tmp$itemset <- sapply(tmp$itemset, paste, collapse = ", ")
  tmp$count <- as.integer(tmp$count)
  tmp$value <- as.numeric(tmp$value)
  tmp$p <- as.numeric(tmp$p)
  tmp$self_sufficient <- as.logical(tmp$self_sufficient)
  if ("closure" %in% names(tmp)) {
    tmp$closure <- sapply(tmp$closure, paste, collapse = ", ")
  }
  return(tmp)
}

# todo:
#   - move console messages here? or to .[function]?
decode <- function(itemset, index) {
  return(lapply(itemset, function(v){index[v + 1]}))
}

# -> as(tidlist, ...)
encode <- function(itemlist) {
  return(.encode(itemlist)[1:2])
}

opus <- function(filename = NULL,
                 itemlist = NULL,
                 tidlist = NULL,
                 k = 100,
                 ...) {

  output <- NULL
  # class(output) <- ...

  k <- ifelse(k < 1, 1, k)
  cpp_arguments <- .arguments(list(...))

  proc_time <- rep(0, 5)

  if (!is.null(filename) | !is.null(itemlist)) {
    try({

      proc_time[1] <- proc.time()[3]

      if (!is.null(filename)) {
        cat("Reading file...")
        tmp <- .encode(read.itemlist(filename))
      } else {
        cat("Reading data...")
        tmp <- .encode(itemlist)
      }

      proc_time[2] <- proc.time()[3]

      cat(" (", round(proc_time[2] - proc_time[1], 2), " seconds)\n\n", sep = "")

      output <- .opus_cpp(tmp$tidlist,
                          tmp$num_items,
                          tmp$num_trans,
                          k,
                          cpp_arguments)

      proc_time[3] <- proc.time()[3]

      cat("Decoding...")

      output$itemset <- decode(output$itemset, tmp$index)

      # or if(cpp_arguments$print_closures == TRUE)...?
      if (!is.null(output$closure[[1]])) {
        output$closure <- decode(output$closure, tmp$index)
      } else {
        output$closure <- NULL
      }

      proc_time[4] <- proc.time()[3]

      cat(" (", round(proc_time[4] - proc_time[3], 2), " seconds)\n\n", sep = "")

      cat("[Total = ", round(proc_time[4] - proc_time[1], 2), " seconds.]\n", sep = "")
    })
  } else if (!is.null(tidlist)) {

    proc_time[1] <- proc.time()[3]

    output <- .opus_cpp(tidlist,
                        length(tidlist),
                        max(unlist(tidlist, FALSE, FALSE)),
                        k,
                        cpp_arguments)

    # or if(cpp_arguments$print_closures == FALSE)...?
    if (is.null(output$closure[[1]])) {
      output$closure <- NULL
    }

    proc_time[2] <- proc.time()[3]

    cat("[Total = ", round(proc_time[2] - proc_time[1], 2), " seconds.]\n", sep = "")

  } else {
    stop("ERROR")
  }

  return(output)
}

# read.itemlist()
# add:
#   - regex
read.itemlist <- function(filename, sep = " ") {
  itemlist <- NULL
  if (is.character(filename) && file.exists(filename)) {
    try ({
      raw <- readChar(filename, file.info(filename)$size, TRUE)
      raw <- strsplit(raw, split = "\n", fixed = TRUE)[[1]]
      itemlist <- strsplit(raw, split = sep, fixed = TRUE)
    })
  }
  return(itemlist)
}

# read.TIDList <- function(filename) {}

# ==============================================================================
# INTERNAL
# ..............................................................................

.arguments <- function(L) {

  default <- list(print_closures = FALSE,
                  filter_itemsets = TRUE,
                  search_by_lift = FALSE,
                  correct_for_mult_compare = TRUE,
                  redundancy_tests = TRUE)

  if (length(L) > 0) {
    # "drop" any elements of L not being both of lenght one and boolean
    L <- L[sapply(L, function(e){length(e) == 1 && is.logical(e)})]
    # replace:
    #   - elements of default with names matching elements of L; with
    #   - elements of L with names matching elements of default.
    default[names(default) %in% names(L)] <- L[names(L) %in% names(default)]
  }

  return(unlist(default, use.names = FALSE))
}

.encode <- function(itemlist) {
  index <- unique(unlist(itemlist, FALSE, FALSE))

  # try:
  #   - swap "unname(...)" for "use.names = FALSE" in "unlist(...)"
  item_index_numbers <-
    unname(
      split(
        match(unlist(itemlist), index),
        rep(
          seq_along(itemlist),
          lengths(itemlist)
          )
        )
      )

  item_index_numbers_flat <- unlist(item_index_numbers,
                                    recursive = FALSE,
                                    use.names = FALSE)

  transaction_numbers_flat <- rep(seq_along(item_index_numbers),
                                  lengths(item_index_numbers)) - as.integer(1)

  tidlist <-
    unname(
      split(
        transaction_numbers_flat,
        item_index_numbers_flat
      )
    )

  return(list(tidlist = tidlist,
              index = index,
              num_items = length(index),
              num_trans = length(itemlist)))
}

# .opus <- function(){}

# .read.itemlist <- function(filename){}

# .read.TIDList <- function(){}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

opusR <- function(f = NULL,   # file
                  d = NULL,   # data : list of numeric
                  k = 100,    # k
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

  if (is.null(d) & is.null(f)) {
    message("[provide filename or indexed tidlist]")
  } else if (is.null(f) & !is.null(d)) {
    try({
      output <- .opusHelper(d, length(d), max(unlist(d)), k, p)
    })
  } else if (is.character(f) && file.exists(f)) {

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
      # return(tidList)

      T2 <- proc.time()[3]

      cat(" (", round(T2 - T1, 2), " seconds)\n\n", sep = "")
      # output <- .opusHelper(tidList, noOfItems, noOfTransactions, k, p)
      output <- .opus_cpp(tidList, noOfItems, noOfTransactions, k, p)

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

readItemList <- function(f) {
  items <- NULL
  if (is.character(f) && file.exists(f)) {
    try({
      raw <- readChar(f, file.info(f)$size, TRUE)
      raw <- strsplit(raw, split = "\n", fixed = TRUE)[[1]]
      items <- strsplit(raw, " ", fixed = TRUE)
    })
  } else {
    message("ERROR")
  }
  return(items)
}

# readTIDSet <- function(){}

itemListToTIDList <- function(items) {

  index <- unique(unlist(items, FALSE, FALSE))

  items_int <- unname(
    split(
      match(unlist(items), index),
      rep(
        seq_along(items),
        lengths(items)
      )
    )
  )

  items_int_flat <- unlist(items_int, recursive = FALSE, use.names = FALSE)

  trans <- rep(seq_along(items_int), lengths(items_int)) - as.integer(1)

  tidList <- unname(split(trans, items_int_flat))

  return(tidList)

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

.header <- c("OPUS Miner: Filtered Top-k Association Discovery of Self-Sufficient Itemsets",
             "Version 1.2",
             "Copyright (C) 2012-2016 Geoffrey I Webb",
             "This program comes with ABSOLUTELY NO WARRANTY. This is free software,",
             "and you are welcome to redistribute it under certain conditions.",
             "See the GNU General Public Licence <http://www.gnu.org/licenses/> for details.",
             "",
             "If you publish results obtained by using this software please cite:",
             "  Webb, G.I. & Vreeken, J. (2014) Efficient Discovery of the Most Interesting Associations.",
             "  ACM Transactions on Knowledge Discovery from Data. 8(3), Art. no. 15.")
