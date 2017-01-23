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

Rcpp::GenericVector get_itemsetRec(const itemsetRec &is) {
  Rcpp::IntegerVector items = Rcpp::wrap(is);//get_itemset(is);

  // fprintf(f, " [%d,%f", is.count, is.value);
  // fprintf(f, " %g]", is.p);

  Rcpp::GenericVector record =
    Rcpp::GenericVector::create(Rcpp::Named("itemset") = items,
                                Rcpp::Named("count") = is.count,
                                Rcpp::Named("value") = is.value,
                                Rcpp::Named("p") = is.p);
                                // Rcpp::Named("Closures") = "***",
                                // Rcpp::Named("Self_Sufficient") = "");
  //
  if (printClosures) {
    itemset closure;

    find_closure(is, closure);

    if (closure.size() > is.size()) {
      // fprintf(f, " closure: ");
      // print_itemset(f, closure);
      // record("Closure") = get_itemset(closure);
      record("closure") = Rcpp::wrap(closure);
    }
  }
  //
  // // putc('\n', f);
  return record;
  // return items;
}

Rcpp::GenericVector get_itemsets(std::vector<itemsetRec> &is) {

  Rcpp::List output_itemset(k);
  Rcpp::NumericVector output_count(k);
  Rcpp::NumericVector output_value(k);
  Rcpp::NumericVector output_p(k);
  Rcpp::GenericVector output_closure(k);
  Rcpp::LogicalVector output_self_sufficient(k);

  std::sort(is.begin(), is.end(), valgt);

  std::vector<itemsetRec>::const_iterator it;

  int index = 0;
  int failed_count = 0;

  for (it = is.begin(); it != is.end(); it++) {
    output_itemset[index] = Rcpp::wrap(*it);
    output_count[index] = it->count;
    output_value[index] = it->value;
    output_p[index] = it->p;

    if (printClosures) {
      itemset closure;

      find_closure(*it, closure);

      if (closure.size() > (*it).size()) {
        output_closure[index] = Rcpp::wrap(closure);
      }
    }

    output_self_sufficient[index] = it->self_sufficient;

    index++;
  }

  Rcpp::List output =
      Rcpp::List::create(Rcpp::Named("itemset") = output_itemset,
                         Rcpp::Named("count") = output_count,
                         Rcpp::Named("value") = output_value,
                         Rcpp::Named("p") = output_p,
                         Rcpp::Named("closure") = output_closure,
                         Rcpp::Named("self_sufficient") = output_self_sufficient);

  return output;
}
