/* Open source implementation of the OPUS Miner algorithm which applies OPUS search for Filtered Top-k Association Discovery of Self-Sufficient Itemsets
** Copyright (C) 2012 Geoffrey I Webb
**
** This program is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program.  If not, see <http://www.gnu.org/licenses/>.
**
** Please report any bugs to Geoff Webb <geoff.webb@monash.edu>
*/

#include <Rcpp.h>

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits>
#ifdef _WIN32
  #include <time.h>
#else
  #include <sys/times.h>
  #include <unistd.h>
#endif

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

#include "opus_miner.h"
#include "globals.h"
// #include "load_data.h"
#include "find_itemsets.h"
#include "print_itemsets.h"
#include "filter_itemsets.h"
#include "utils.h"
#include "fisher.h"

std::priority_queue<itemsetRec> itemsets;

void init() {
  alpha = std::vector<double>();                 // globals.cpp
  tids = std::vector<tidset>();                  // globals.cpp
  itemsets = std::priority_queue<itemsetRec>();  // opus_miner.cpp
  itemNames = std::vector<std::string>();        // globals.cpp
  minValue = -std::numeric_limits<float>::max(); // find_itemsets.cpp
  TIDCount = std::map<itemset, int>();

  //tidCount??? // find_itemsets.cpp
  //p_value // globals.h - NO this is a typedef
}

// bool valgt(itemsetRec i1, itemsetRec i2) {
//   return i1.value > i2.value;
// }

Rcpp::GenericVector
#ifdef _WIN32
  __cdecl // Leading double underscore required for GCC compiler
#endif
opus(Rcpp::GenericVector tidList, int numItems, int numTrans, Rcpp::NumericVector k_, Rcpp::LogicalVector args) {
  #ifdef _DEBUG
    _CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
  #endif

  init();
  tids = Rcpp::as< std::vector< tidset > >(tidList);
  noOfItems = numItems;
  noOfTransactions = numTrans;

  std::vector<itemsetRec> is;

  Rcpp::GenericVector output;
  // Rcpp::DataFrame output;

  #ifdef _WIN32
    time_t start_t = time(NULL);
  #else
    struct tms start_t;
    times(&start_t);
  #endif // _WIN32

  k = Rcpp::as<unsigned int>(k_);

  printClosures = args[0];
  filter = args[1];
  searchByLift = args[2];
  correctionForMultCompare = args[3];
  redundancyTests = args[4];

  try {
    // printf("\nLoading data from %s\n", inputFileName);
    // Rcpp::Rcout << "\nLoading data...\n";
    // load_data(input);

    // printf("%d transactions, %d items\n", noOfTransactions, noOfItems);
    // Rcpp::Rcout << noOfTransactions << " transactions, " << noOfItems << " items\n";

    // fprintf(outf, "\n%s: %ld items, %ld transactions\n", inputFileName, static_cast<long>(noOfItems), static_cast<long>(noOfTransactions));

    // printf("Finding itemsets\n");
    Rcpp::Rcout << "Finding itemsets ("
                << noOfTransactions << " transactions, "
                << noOfItems << " items)...\n\n";

    #ifdef _WIN32
      time_t find_start_t = time(NULL);
    #else
      struct tms find_start_t;
      times(&find_start_t);
    #endif // _WIN32

    find_itemsets();

    #ifdef _WIN32
      time_t find_end_t = time(NULL);
    #else
      struct tms find_end_t;
      times(&find_end_t);
    #endif // _WIN32

    Rcpp::Rcout << " (" << find_end_t-find_start_t << " seconds)\n\n";

    // extract the itemsets from the priority queue
    while (!itemsets.empty()) {
      is.push_back(itemsets.top());
      itemsets.pop();
    }

    if (filter) {
      // printf("Filtering itemsets\n");
      Rcpp::Rcout << "Filtering itemsets...";
      filter_itemsets(is);
    }

    #ifdef _WIN32
      time_t print_start_t = time(NULL);

      const long tm = static_cast<long>(print_start_t-find_start_t);
    #else
      struct tms print_start_t;
      times(&print_start_t);

      const long tm = static_cast<long>(print_start_t.tms_utime-find_start_t.tms_utime) / sysconf(_SC_CLK_TCK);
    #endif // _WIN32

    Rcpp::Rcout << " (" << print_start_t-find_end_t << " seconds)\n\n";

    // fprintf(outf, "Found %ld non-redundant productive itemsets in %ld seconds\n", static_cast<long>(is.size()), tm);
    // Rcpp::Rcout << "Found " << static_cast<long>(is.size()) << " non-redundant productive itemsets in " << tm << " seconds\n";

    // ***** need to move index decoding to R *****
    output = get_itemsets(is);
    // output = is;

    // std::sort(is.begin(), is.end(), valgt);
    // output = Rcpp::wrap(is);
    // is.

    // #ifdef _WIN32
    //   time_t end_t = time(NULL);
    // #else
    //   struct tms end_t;
    //   times(&end_t);
    // #endif // _WIN32

    // #ifdef _WIN32
    //   const long t =  static_cast<long>(end_t-start_t);
    //
    //   // printf("%ld seconds (%ld input, %ld search, %ld filter, %ld output)", t, static_cast<long>(find_start_t-start_t), static_cast<long>(find_end_t-find_start_t), static_cast<long>(print_start_t-find_end_t), static_cast<long>(end_t-print_start_t));
    //   Rcpp::Rcout
    //     << t << " seconds ("
    //     << static_cast<long>(find_start_t-start_t) << " input, "
    //     << static_cast<long>(find_end_t-find_start_t) << " search, "
    //     << static_cast<long>(print_start_t-find_end_t) << " filter)";
    // #else
    //   const long ticks_sec = sysconf(_SC_CLK_TCK);
    //   const long t = static_cast<long>(end_t.tms_utime-start_t.tms_utime) / ticks_sec;
    //
    //   // printf("%ld seconds (%ld input, %ld search, %ld filter, %ld output)", t, static_cast<long>(find_start_t.tms_utime-start_t.tms_utime)/ticks_sec, static_cast<long>(find_end_t.tms_utime-find_start_t.tms_utime)/ticks_sec, static_cast<long>(print_start_t.tms_utime-find_end_t.tms_utime)/ticks_sec, static_cast<long>(end_t.tms_utime-print_start_t.tms_utime)/ticks_sec);
    //   Rcpp::Rcout
    //     << t << " seconds ("
    //     << static_cast<long>(find_start_t.tms_utime-start_t.tms_utime)/ticks_sec << " input, "
    //     << static_cast<long>(find_end_t.tms_utime-find_start_t.tms_utime)/ticks_sec << " search, "
    //     << static_cast<long>(print_start_t.tms_utime-find_end_t.tms_utime)/ticks_sec << " filter)";
    // #endif // _WIN32
    // printf(" for %ld itemsets\n", static_cast<long>(is.size()));
    // Rcpp::Rcout << " for " << static_cast<long>(is.size()) << " itemsets\n";
  }
  catch (std::bad_alloc) {
    // fprintf(stderr, "Out of memory\n");
    Rcpp::Rcout << "Out of memory\n";
  }
  catch (...) {
    // fprintf(stderr, "Unhandled exception\n");
    Rcpp::Rcout << "Unhandled exception\n";
  }

  return output;
}

// [[Rcpp::export(.opus_cpp)]]
Rcpp::GenericVector opus_cpp(Rcpp::GenericVector tidList, int numItems, int numTrans, Rcpp::NumericVector k_, Rcpp::LogicalVector args) {
  return opus(tidList, numItems, numTrans, k_, args);
}
