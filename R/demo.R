# # DEMO 2017-02-01
#
# # contents:
# #   - input
# #   - parameters
# #   - output
# #   - speed
#
# # input=========================================================================
#
# # read file directly
# results_1 <- opus("mushroom.dat")
#
# # read file separately
# data_om <- read_transactions("mushroom.dat")
# results_2 <- opus(data_om)
#
# # read file as arules transactions object
# data_arules <- arules::read.transactions("mushroom.dat")
# results_3 <- opus(data_arules)
#
# # parameters====================================================================
#
# results_4 <- opus("mushroom.dat", k = 50, print_closures = TRUE)
#
# # output========================================================================
#
# # data frame (default)
# results_5 <- opus("mushroom.dat")
#
# # arules itemsets
# results_6 <- opus("mushroom.dat", format = "itemsets")
#
# # list (if not "dataframe" or "list")
# results_7 <- opus("mushroom.dat", format = "list")
#
# # speed=========================================================================
#
# # vs C++
# results_8 <- opus("kosarak.dat")
#
# # vs arules
# data_arules <- arules::read.transactions("kosarak.dat")
# data_om <- as_transactions(read_transactions("kosarak.dat"))
#
# results_9 <- opus(data_arules)
# results_10 <- opus(data_om)
