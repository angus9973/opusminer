/* load_data.cpp - a module of OPUS Miner providing load_data, a procedure to read transaction data from a file.
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
*/

#include <Rcpp.h>

#include <stdio.h>
#include <stdlib.h>
#include <map>
#include <string>

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <crtdbg.h>
#endif

#include "globals.h"
#include "load_data.h"

#include "find_itemsets.h"

// void load_data(Rcpp::GenericVector input) {
//
//   std::map<std::string, itemID> itemstrs;
//   std::string s;
//
//   noOfTransactions = 0;
//   noOfItems = 0;
//
//   for (int i = 0; i < input.size(); i++) {
//
//     Rcpp::CharacterVector transaction = input[i];
//
//     for (int j = 0; j < transaction.size(); j++) {
//
//       s = transaction[j];
//
//       itemID thisid;
//       std::map<std::string, itemID>::const_iterator it = itemstrs.find(s);
//
//       if (it == itemstrs.end()) {
//         thisid = itemNames.size();
//         itemstrs[s] = thisid;
//         itemNames.push_back(s);
//         noOfItems = itemNames.size();
//         tids.resize(noOfItems);
//       }
//       else {
//         thisid = it->second;
//       }
//
//       if (tids[thisid].empty() || *tids[thisid].rbegin() != noOfTransactions) {
//         tids[thisid].push_back(noOfTransactions);
//       }
//     }
//     noOfTransactions++;
//   }
// }

// // [[Rcpp::export]]
// void load_data_2(Rcpp::NumericVector input) {
//   std::vector<int> test = Rcpp::as< std::vector<int> >(input);
//   // Rcpp::Rcout << test[0];
//   Rcpp::Rcout << &input;
//   Rcpp::Rcout << &test;
// }

// std::vector<tidset> tidx;
// std::vector< std::vector<int> > tidy;
//
// typedef std::vector<TID> ABC;
// // ABC aa;
// // std::vector<int> bb = aa;
//
// Rcpp::IntegerVector cc;
// ABC dd = Rcpp::as< ABC >(cc);

// *****
// typedef std::vector< std::vector<int> > DEF;
//
// Rcpp::GenericVector ee;
// DEF ff = Rcpp::as< DEF >(ee);
//
// Rcpp::GenericVector gg;
// std::vector< tidset > hh = Rcpp::as< std::vector<tidset> >(gg);
// *****

// // [[Rcpp::export]]
// void load_data_piece(Rcpp::IntegerVector items, int trans) {
//   int s = Rcpp::max(items) + 1; // indexing from 0
//   if (s > tidx.size()) {
//     tidx.resize(s);
//   }
//   for (int i = 0; i < items.size(); i++) {
//   //   tidx[items[i]].push_back(trans);
//     // Rcpp::Rcout << " | s = " << s << ", tix.size = " << tidx.size()
//     //             << ", items[i] = " << items[i];
//     tidx[items[i]].push_back(trans);
//   }
// }

// typedef std::vector<TID> tx;

// void init() {
//   alpha = std::vector<double>();                 // globals.cpp
//   tids = std::vector<tidset>();                  // globals.cpp
//   itemsets = std::priority_queue<itemsetRec>();  // opus_miner.cpp
//   itemNames = std::vector<std::string>();        // globals.cpp
//   minValue = -std::numeric_limits<float>::max(); // find_itemsets.cpp
// }
//
// // [[Rcpp::export]]
// void load_data_whole(Rcpp::GenericVector tidList) {
//   init();
//   tids = Rcpp::as< std::vector< tidset > >(tidList);
// }
