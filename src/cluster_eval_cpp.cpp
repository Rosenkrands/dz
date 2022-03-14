#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

//' Evaluate a cluster according to average distance divided by total profit
//'
//' @param zone A numeric vector of the id's included in the zone
//' @param inst A data.matrix of the instance contatining the columns id, x, y, score. Row number should coincide with point id.
//' @export
// [[Rcpp::export]]
double cluster_eval_cpp(NumericVector zone, NumericMatrix inst) {
  // Determine size of input vector and matrix
  int size_zone = zone.size();
  int total_profit = 0;
  double total_distance = 0;
  int dist_count = 0;

  // Find the scores for points in the zone
  for (int i = 0; i < size_zone; i++) {
    total_profit += inst(zone[i] - 1,3);
  }

  // Find the average distance between points
  for (int i = 0; i < size_zone; i++) {
    for (int j = 0; j < size_zone; j++) {
      if (i != j) {
        double x = inst(zone[i] - 1,1) - inst(zone[j] - 1,1);
        double y = inst(zone[i] - 1,2) - inst(zone[j] - 1,2);

        total_distance += sqrt(pow(x,2) + pow(y,2));
        dist_count += 1;
      }
    }
  }

  // Average distance divided by total profit
  return (total_distance/dist_count)/total_profit;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
inst = test_instances$p7_chao
zone = c(1, 40, 42, 76, 22, 60, 14, 75, 100, 61, 12, 98, 24, 37, 23, 13, 63, 27, 91, 49, 85, 78, 67, 79, 36, 102)

cluster_eval_cpp(
  zone,
  inst$points |> dplyr::select(id, x, y, score) |> data.matrix()
)
*/
