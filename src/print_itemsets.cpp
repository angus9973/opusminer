/* print_itemsets.cpp - a module of OPUS Miner providing print_itemsets, a procedure to print the top-k itemsets to a file.
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
#include <vector>
#include <algorithm>
#include "globals.h"
#include "itemset.h"
#include "utils.h"
#include "find_closure.h"

bool valgt(itemsetRec i1, itemsetRec i2) {
  return i1.value > i2.value;
}

Rcpp::CharacterVector get_itemset(const itemset &is) {
  Rcpp::CharacterVector items;

  itemset::const_iterator item_it;

  for (item_it = is.begin(); item_it != is.end(); item_it++) {
    items.push_back(itemNames[*item_it].c_str());
  }

  return items;
}

Rcpp::GenericVector get_itemsetRec(const itemsetRec &is) {
  Rcpp::CharacterVector items = get_itemset(is);

  // fprintf(f, " [%d,%f", is.count, is.value);
  // fprintf(f, " %g]", is.p);

  Rcpp::GenericVector record =
    Rcpp::GenericVector::create(Rcpp::Named("Itemset") = items,
                                Rcpp::Named("Count") = is.count,
                                Rcpp::Named("Value") = is.value,
                                Rcpp::Named("P") = is.p,
                                Rcpp::Named("Closures") = "***",
                                Rcpp::Named("Self_Sufficient") = "");

  // if (printClosures) {
  //   itemset closure;
  //
  //   find_closure(is, closure);
  //
  //   if (closure.size() > is.size()) {
  //     fprintf(f, " closure: ");
  //     print_itemset(f, closure);
  //   }
  // }

  // putc('\n', f);
  return record;
}

Rcpp::GenericVector get_itemsets(std::vector<itemsetRec> &is) {
  Rcpp::GenericVector output(k);
  int index = 0;

  std::sort(is.begin(), is.end(), valgt);

  std::vector<itemsetRec>::const_iterator it;

  int failed_count = 0;

  for (it = is.begin(); it != is.end(); it++) {
    if (!it->self_sufficient) {
      failed_count++;
    }
    else {
      // print_itemsetRec(f, *it);
      Rcpp::GenericVector record = get_itemsetRec(*it);
      record("Self-Sufficient") = "YES";
      output[index] = record;
      index++;
    }
  }

  if (failed_count) {
    // fprintf(f, "\n%d itemsets failed test for self sufficiency\n", failed_count);
    for (it = is.begin(); it != is.end(); it++) {
      if (!it->self_sufficient) {
        // print_itemsetRec(f, *it);
        Rcpp::GenericVector record = get_itemsetRec(*it);
        record("Self_Sufficient") = "NO";
        output[index] = record;
        index++;
      }
    }
  }
  return output;
}
