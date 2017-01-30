# ==============================================================================
# TO DO:
#   - check arguments being passed, avoid copying etc. (esp. large objects)
#   - piping?
#   - null items?
#   - input format
#   - ...
#   - fast file -> list -> arules format
#   - arules format -> CPP -> output
#   - output -> arules format?
#   - ? arules removes duplicate ITEMS... just says which transactions an item
#     is in, not how many of that item are in a transaction
#     >>> check behaviour of opusMiner
#     >>> may present issue in terms of use of arules structures... (may not
#         make any difference)
#   - ...
#   - move console output to component functions
# ..............................................................................

# ==============================================================================
# EXTERNAL
# ..............................................................................

# ...
# file-reading options?
opus <- function(filename = NULL,
                 transactions = NULL,
                 format = "dataframe",
                 k = 100,
                 ...) {

  # initialise output
  output <- NULL

  # check arguments
  if (k < 1) {k <- 1}
  cpp_arguments <- .check_arguments(list(...))

  # initialise timing
  time <- rep(0, 4)

  if (!is.null(filename) | !is.null(transactions)) {
    try({

      time[1] <- proc.time()[3]

      if (!is.null(filename)) {
        cat("Reading file...")
        input <- .encode(.read_transactions(filename))
      } else if (!is.null(transactions)) {
        cat("Reading data...")
        if (is(transactions, "transactions")) {
          transactions <- as(transactions, "list")
        }
        input <- .encode(transactions)
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

    }) # try({...
  } else {
    stop("No input.")
  }

  if (format == "dataframe") {
    output <- .as_data_frame(output)
  } else if (format == "itemsets") {
    output <- .as_itemsets(output)
  }

  return(output)
}

# ==============================================================================
# INTERNAL
# ..............................................................................

.as_data_frame <- function(output) {

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

# to do:
#   - add *closure* to quality$...
.as_itemsets <- function(output) {
  return(
    new("itemsets",
        items = as(.as_transactions(output$itemset), "itemMatrix"),
        quality = .as_data_frame(output)[ , c("count",
                                              "value",
                                              "p",
                                              "self_sufficient")])
  )
}

.as_sparse_matrix <- function(itemlist) {

  index <- unique(unlist(itemlist))

  sm_row_index <- match(unlist(itemlist),
                        index)

  sm_col_index <- rep(seq_along(itemlist),
                      lengths(itemlist))

  # depends: Matrix
  return(sparseMatrix(i = sm_row_index,
                      j = sm_col_index))

}

.as_transactions <- function(itemlist) {
  return(
    new("transactions",
        data = .as_sparse_matrix(itemlist),
        itemInfo = data.frame(labels = unique(unlist(itemlist)),
                              stringsAsFactors = FALSE))
  )
}

.check_arguments <- function(arg) {

  def <- list(print_closures = FALSE,
              filter_itemsets = TRUE,
              search_by_lift = FALSE,
              correct_for_mult_compare = TRUE,
              redundancy_tests = TRUE)

  if (length(arg) > 0) {
    # "drop" any elements of L not being both of lenght one and boolean
    arg <- arg[sapply(arg, function(e){length(e) == 1 && is.logical(e)})]
    # replace:
    #   - elements of default with names matching elements of L; with
    #   - elements of L with names matching elements of default.
    def[names(def) %in% names(arg)] <- arg[names(arg) %in% names(def)]
  }

  return(unlist(def))
}

.decode <- function(itemset, index) {
  return(lapply(itemset, function(v){index[v + 1]}))
}

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

# add:
#   - regex
.read_transactions <- function(filename, sep = " ") {
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
