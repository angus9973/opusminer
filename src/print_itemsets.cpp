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
  // Rcpp::GenericVector output(k);
  // Rcpp::GenericVector output =
  //   Rcpp::GenericVector::create(Rcpp::Named("itemset", Rcpp::GenericVector(k)),
  //                               Rcpp::Named("count", Rcpp::NumericVector(k)),
  //                               Rcpp::Named("value", Rcpp::NumericVector(k)),
  //                               Rcpp::Named("p", Rcpp::NumericVector(k)),
  //                               Rcpp::Named("closure", Rcpp::GenericVector(k)),
  //                               Rcpp::Named("self_sufficient", Rcpp::LogicalVector(k)));

  Rcpp::GenericVector op_itemset(k);
  Rcpp::NumericVector op_count(k);
  Rcpp::NumericVector op_value(k);
  Rcpp::NumericVector op_p(k);
  Rcpp::GenericVector op_closure(k);
  Rcpp::LogicalVector op_self_sufficient(k);

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
      // Rcpp::IntegerVector record = Rcpp::wrap(*it);  //it->value;//get_itemsetRec(*it);
      // Rcpp::GenericVector record = get_itemsetRec(*it);//Rcpp::wrap(*it);
      // record("self_sufficient") = "YES";
      // output[index] = record;
      ; //[index] = Rcpp::wrap(*it);
      // output(0, index) = Rcpp::wrap(*it);
      // output(1, index) = it->count;
      // output(2, index) = it->value;
      // output(3, index) = it->p;
      // output(4, index) = 123;
      // output(5, index) = true;
      op_itemset[index] = Rcpp::wrap(*it);
      op_count[index] = it->count;
      op_value[index] = it->value;
      op_p[index] = it->p;
      op_closure[index] = 123;
      op_self_sufficient[index] = true;
      index++;
    }
  }

  if (failed_count) {
    // fprintf(f, "\n%d itemsets failed test for self sufficiency\n", failed_count);
    for (it = is.begin(); it != is.end(); it++) {
      if (!it->self_sufficient) {
        // print_itemsetRec(f, *it);
        // Rcpp::IntegerVector record = Rcpp::wrap(*it);//get_itemsetRec(*it);
        // Rcpp::GenericVector record = get_itemsetRec(*it);
        // record("self_sufficient") = "NO";
        // output[index] = record;
        // output(0, index) = Rcpp::wrap(*it);
        // output(1, index) = it->count;
        // output(2, index) = it->value;
        // output(3, index) = it->p;
        // output(4, index) = 123;
        // output(5, index) = false;
        op_itemset[index] = Rcpp::wrap(*it);
        op_count[index] = it->count;
        op_value[index] = it->value;
        op_p[index] = it->p;
        op_closure[index] = 123;
        op_self_sufficient[index] = false;
        index++;
      }
    }
  }

  Rcpp::GenericVector output =
      Rcpp::GenericVector::create(Rcpp::Named("itemset", op_itemset),
                                  Rcpp::Named("count", op_count),
                                  Rcpp::Named("value", op_value),
                                  Rcpp::Named("p", op_p),
                                  Rcpp::Named("closure", op_closure),
                                  Rcpp::Named("self_sufficient", op_self_sufficient));

  return output;
}
