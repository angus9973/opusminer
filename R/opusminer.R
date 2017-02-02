# ==============================================================================
# TO DO:
#   - clean-up cpp
#   - doc: add copyright etc. to R source
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

#' @title
#' OPUS Miner: Filtered Top-k Association Discovery of Self-Sufficient Itemsets
#'
#' @description
#' \code{opus} finds the top k productive, non-redundant itemsets on the
#' measure of interest (leverage or lift) using the OPUS Miner algorithm.
#'
#' @details
#' \code{opus} provides an interfact to the OPUS Miner algorithm (implemented in
#' C++) to find the top k productive, non-redundant itemsets by leverage
#' (default) or lift.
#'
#' \code{transactions} should be a filename, list (of transactions, each list
#' element being a vector of character values representing item labels), or an
#' object of class \code{\link[arules]{transactions}} (\code{arules}).
#'
#' Files should be in the format of a list of transactions, one line per
#' transaction, each transaction (ie, line) being a sequence of item labels,
#' separated by the character specified by the parameter \code{sep} (default
#' " ").  (Alternatively, files can be read seaparately using the \code{\link{read_transactions}}
#' function.)
#'
#' \code{format} should be specified as either "dataframe" (the default) or
#' "itemsets", and any other value will return a list.
#'
#' The optional additional parameters are as follows:
#' \itemize{
#'   \item \code{print_closures}
#'         whether to also return the closure for each itemset (default FALSE)
#'   \item \code{filter_itemsets}
#'         whether to filter itemsets that are not independently productive
#'         (default TRUE)
#'   \item \code{search_by_lift}
#'         make lift (rather than leverage) the measure of iterest (default
#'         FALSE)
#'   \item \code{correct_for_mult_compare}
#'         whether to correct alpha for the size of the search space (default
#'         TRUE)
#'   \item \code{redundancy_tests}
#'         whether to allow redundant itemsets (default TRUE)
#' }
#'
#' @references
#' Webb, G.I. & Vreeken, J. (2014) Efficient Discovery of the Most Interesting
#' Associations. ACM Transactions on Knowledge Discovery from Data. 8(3), Art.
#' no. 15.
#'
#' @param transactions A filename, list, or object of class
#'   \code{\link[arules]{transactions}} (\code{arules}).
#' @param k The number of itemsets to return, an integer (default 100).
#' @param format The output format ("dataframe", default, or "itemsets").
#' @param sep The separator between items (for files, default " ").
#' @param ... Optional additional parameters (see details).
#'
#' @return  The top k productive, non-redundant itemsets, with relevant
#'   statistics, in the form of a data frame, object of class
#'   \code{\link[arules]{itemsets}} (\code{arules}), or a list.
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

    # read and index the input data according to its format
    if (.valid_filename(transactions)) {
      input <- .encode(.read_transactions(transactions, sep = sep))
    } else if (is(transactions, "list")) {
      input <- .encode(transactions)
    } else if (is(transactions, "transactions")) {
      input <- .encode(as(transactions, "list"))
    }

    time[2] <- proc.time()[3]
    cat(" (", round(time[2] - time[1], 2), " seconds)\n\n", sep = "")

    # call OPUS Miner (C++)
    output <- .opus_cpp(input$tidlist,
                        input$num_items,
                        input$num_trans,
                        k,
                        cpp_arguments)

    time[3] <- proc.time()[3]
    cat("Decoding output...")

    # decode (deindex) the itemsets
    output$itemset <- .decode(output$itemset, input$index)

    # if relevant, decode (deindex) the closure itemsets
    if (cpp_arguments["print_closures"] == TRUE) {
      output$closure <- .decode(output$closure, input$index)
    } else {
      output$closure <- NULL
    }

    time[4] <- proc.time()[3]
    cat(" (", round(time[4] - time[3], 2), " seconds)\n\n", sep = "")
    cat("[[Total: ", round(time[4] - time[1], 2), " seconds]]\n\n", sep = "")

    # format the output
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

#' @title Read Transaction Data from a File (Fast)
#'
#' @description
#' \code{read_transactions} reads transaction data from a file fast, providing a
#' significant speed increase over alternative methods for larger files.
#'
#' @details
#' \code{read_transactions} uses (internally) the \code{\link[base]{readChar}}
#' function to read transaction data from a file fast.  This is substantially
#' faster for larger files than alternative methods.
#'
#' Files should be in the format of a list of transactions, one line per
#' transaction, each transaction (ie, line) being a sequence of item labels,
#' separated by the character specified by the parameter \code{sep} (default
#' " ").
#'
#' @param filename A filename.
#' @param sep The separator between items (default " ").
#' @param format The output format ("list" or "transactions").
#'
#' @return The transaction data, in the form of a list (of transactions, each
#' list element being a vector of character values representing item labels), or
#' an object of class \code{\link[arules]{transactions}} (\code{arules}).
read_transactions <- function(filename, sep = " ", format = "list") {
  return(.read_transactions(filename, sep, format))
}

# ==============================================================================
# INTERNAL
# ..............................................................................

# return output as a data frame
.as_data_frame <- function(output) {

  # "transpose" list elements to data frame columns
  output <- as.data.frame(sapply(output, "["))

  # set data frame coloumn types
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

# return output as an object of class itemsets (arules)
.as_itemsets <- function(output) {
  return(
    new("itemsets",
        items = as(.as_transactions(output$itemset), "itemMatrix"),
        quality = .as_data_frame(output)[-1])
  )
}

# return a list of transactions as an object of class ngCMatrix (Matrix)
.as_sparse_matrix <- function(transactions) {
  index <- unique(unlist(transactions))
  sm_row_index <- match(unlist(transactions), index)
  sm_col_index <- rep(seq_along(transactions), lengths(transactions))
  return(Matrix::sparseMatrix(i = sm_row_index, j = sm_col_index))
}

# return a list of transactions as an object of class transactions (arules)
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

  # default cpp arguments
  def <- list(print_closures = FALSE,
              filter_itemsets = TRUE,
              search_by_lift = FALSE,
              correct_for_mult_compare = TRUE,
              redundancy_tests = TRUE)

  if (length(arg) > 0) {
    # drop arguments not scalar boolean
    arg <- arg[sapply(arg, function(e){length(e) == 1 && is.logical(e)})]
    # replace:
    #   - elements of def with names matching elements of arg; with
    #   - elements of arg with names matching elements of def.
    def[names(def) %in% names(arg)] <- arg[names(arg) %in% names(def)]
  }

  return(unlist(def))
}

# return the item labels
.decode <- function(itemset, index) {
  return(lapply(itemset, function(v){index[v + 1]}))
}

# return a tidlist, index, number of items and number of transactions
.encode <- function(transactions) {

  # type: vector of character
  index <- unique(unlist(transactions, FALSE, FALSE))

  # replace item labels with item index numbers (fast)
  # type: list of vector of integer
  item_index_numbers <-
    unname(
      split(
        match(unlist(transactions), index),
        rep(
          seq_along(transactions),
          lengths(transactions)
          )
        )
      )

  item_index_numbers_flat <- unlist(item_index_numbers,
                                    recursive = FALSE,
                                    use.names = FALSE)

  # replace item index numbers with transaction numbers (fast)
  transaction_numbers_flat <- rep(seq_along(item_index_numbers),
                                  lengths(item_index_numbers)) - as.integer(1)

  # "transpose" each item index number to the list element given by the
  # corresponding transaction number (fast); type: list of vector of integer
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
              num_trans = length(transactions)))
}

# read transactions from a file (fast)
.read_transactions <- function(filename, sep = " ", format = "list") {

  transactions <- NULL

  if (.valid_filename(filename)) {

    try ({

      raw <- readChar(filename, file.info(filename)$size, TRUE)

      eol <- ifelse(grepl("\r", raw), "\r\n", "\n")

      raw <- strsplit(raw, split = eol, fixed = TRUE)[[1]]

      transactions <- strsplit(raw, split = sep, fixed = TRUE)

      if (format == "transactions") {
        transactions <- .as_transactions(transactions)
      }

    })

  } else {

    message("invalid file name")

  }

  return(transactions)

}

# check: valid filename, list, or object of class transactions (arules)
.valid_input <- function(i) {
  return(!is.null(i) &&
           (.valid_filename(i) || is(i, "list") || is(i, "transactions")))
}

# check: valid filename
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
