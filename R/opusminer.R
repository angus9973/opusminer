# ==============================================================================
# TO DO:
#   - add
#     - convenience file-reading, output format option list or trans. (arules)
#   - investigate
#     - results arules input vs om input: rounding?
#     - to do with order (ie, sort 1 10 13 2 23 25 etc.) of items as chr...
#     - also discrepancy C++ exe vs R... "value" changing differently?
#     - discrepancy C++ exe vs R is due to init():opus_miner.cpp
#     - also cause of sorted? YES
#     - Q: DISCREPANCY ON FIRST LOAD ALSO (INIT CAUSING PROBLEM)
#          OR ON ONLY 2ND-ONWARDS RUN (INIT NOT RESETING EVERYTHING)???
#     - A: looks like 2nd-onwards runs [check]: ie, init not broad enough
#     - appears that adding TIDCount to init() [and extern TIDCount in
#       find_itemsets...] fixes the issue...
#   - file-reading:
#     - regex?
#   - filie-writing:
#     - write results to file?
#   - test:
#     - null items
#     - duplicate items
#   - package issues:
#     - comments:
#       - R
#       - C++
#     - arules-based functionality conditional on arules being loaded?
# ..............................................................................

# ==============================================================================
# EXTERNAL
# ..............................................................................

# demo START
as_transactions <- function(transactions) {
  return(.as_transactions(transactions))
}

read_transactions <- function(filename) {
  return(.read_transactions(filename))
}
# demo END

# ...
opus <- function(transactions = NULL,
                 k = 100,
                 format = "dataframe",
                 sep = " ",
                 ...) {

  # initialise output
  output <- NULL

  # check arguments
  if (k < 1) {k <- 1}
  cpp_arguments <- .check_cpp_arguments(list(...))

  if (.valid_input(transactions)) {

    time <- rep(0, 4)
    time[1] <- proc.time()[3]

    cat("Reading file/data...")

    if (.valid_filename(transactions)) {
      input <- .encode(.read_transactions(transactions, sep = sep))
    } else if (is(transactions, "list")) {
      input <- .encode(transactions)
    } else if (is(transactions, "transactions")) {
      input <- .encode(as(transactions, "list"))
    }

    time[2] <- proc.time()[3]
    cat(" (", round(time[2] - time[1], 2), " seconds)\n\n", sep = "")

    output <- .opus_cpp(input$tidlist,
                        input$num_items,
                        input$num_trans,
                        k,
                        cpp_arguments)

    time[3] <- proc.time()[3]
    cat("Decoding output...")

    output$itemset <- .decode(output$itemset, input$index)

    if (cpp_arguments["print_closures"] == TRUE) {
      output$closure <- .decode(output$closure, input$index)
    } else {
      output$closure <- NULL
    }

    time[4] <- proc.time()[3]
    cat(" (", round(time[4] - time[3], 2), " seconds)\n\n", sep = "")
    cat("[[Total: ", round(time[4] - time[1], 2), " seconds]]\n\n", sep = "")

    if (format == "dataframe") {
      output <- .as_data_frame(output)
    } else if (format == "itemsets") {
      output <- .as_itemsets(output)
    }

  } else if (!is.null(transactions)) {
    message("invalid filename or transaction data")
  } else {
    message("no filename or transaction data specified")
  }

  return(output)
}

# ==============================================================================
# INTERNAL
# ..............................................................................

.as_data_frame <- function(output) {

  # "transpose" list elements to columns of a data frame
  output <- as.data.frame(sapply(output, "["))

  output$itemset <- sapply(output$itemset, paste, collapse = ", ")
  output$count <- as.integer(output$count)
  output$value <- as.numeric(output$value)
  output$p <- as.numeric(output$p)
  output$self_sufficient <- as.logical(output$self_sufficient)

  if ("closure" %in% names(output)) {
    output$closure <- sapply(output$closure, paste, collapse = ", ")
  }

  return(output)
}

# return list output as an arules itemsets object
.as_itemsets <- function(output) {
  return(
    new("itemsets",
        items = as(.as_transactions(output$itemset), "itemMatrix"),
        quality = .as_data_frame(output)[-1])
  )
}

# return a list of transactions as a sparse matrix (ngCMatrix)
.as_sparse_matrix <- function(transactions) {
  index <- unique(unlist(transactions))
  sm_row_index <- match(unlist(transactions), index)
  sm_col_index <- rep(seq_along(transactions), lengths(transactions))
  return(sparseMatrix(i = sm_row_index, j = sm_col_index))
}

# return a list of transactions as an arules transactions object
.as_transactions <- function(transactions) {
  return(
    new("transactions",
        data = .as_sparse_matrix(transactions),
        itemInfo = data.frame(labels = unique(unlist(transactions)),
                              stringsAsFactors = FALSE))
  )
}

# validate cpp arguments
.check_cpp_arguments <- function(arg) {

  def <- list(print_closures = FALSE,
              filter_itemsets = TRUE,
              search_by_lift = FALSE,
              correct_for_mult_compare = TRUE,
              redundancy_tests = TRUE)

  if (length(arg) > 0) {
    # "drop" any elements of arg not being both of length 1 and boolean
    arg <- arg[sapply(arg, function(e){length(e) == 1 && is.logical(e)})]
    # replace:
    #   - elements of def with names matching elements of arg; with
    #   - elements of arg with names matching elements of def.
    def[names(def) %in% names(arg)] <- arg[names(arg) %in% names(def)]
  }

  return(unlist(def))
}

# return the item labels given an itemset (of item index numbers) and an index
.decode <- function(itemset, index) {
  return(lapply(itemset, function(v){index[v + 1]}))
}

# return a tidlist, index, number of items and number of transactions
.encode <- function(itemlist) {

  index <- unique(unlist(itemlist, FALSE, FALSE))

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

# read transactions from a file (fast)
.read_transactions <- function(filename, sep = " ") {
  transactions <- NULL
  if (.valid_filename(filename)) {
    try ({
      raw <- readChar(filename, file.info(filename)$size, TRUE)

      EOL <- ifelse(grepl("\r", raw),
                    "\r\n",
                    "\n")

      raw <- strsplit(raw, split = EOL, fixed = TRUE)[[1]]
      # raw <- readLines(filename)
      transactions <- strsplit(raw, split = sep, fixed = TRUE)
    })
  } else {
    message("invalid file name")
  }
  return(transactions)
}

# check if given input is a valid filename, list or arules transactions object
.valid_input <- function(i) {
  return(!is.null(i) && (.valid_filename(i) || is(i, "list") || is(i, "transactions")))
}

# check if a given filename is a valid filename
.valid_filename <- function(f) {
  return(length(f) == 1 && is.character(f) && file.exists(f))
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
