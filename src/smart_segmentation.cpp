
#include <Rcpp.h>
using namespace Rcpp;

#define STREAM_START -1
#define STREAM_FAKE -2

//' C++ workhorse
//' 
//' Calculate the segmentation given a value matrix and minimum streak
//'
//' @param values Matrix whose columns are streams to switch between
//' @param streak Minimum length between switches
// [[Rcpp::export]]
NumericVector smart_segment_cpp (NumericMatrix values, IntegerVector streak) {
  
  // first, handle some simple edge cases
  if (streak.length() != 1) {
    stop("Error: `streak` is a vector of length %i, but should be length 1.", streak.length());
  }
  int min_length = streak[0];
  
  if (min_length > values.rows()) {
    stop("Error: `streak` (%i) is larger than number of entries (%i).", min_length, values.rows());
  }
  
  // TODO: faster cases for min_length = 1 and min_length > values.rows() / 2
  
  // set up for computational efficiency by creating the partial sum arrays
  /*
  NumericMatrix partial_sum = clone(values);
  for (int i = 0; i < partial_sum.cols(); i++) {
    for (int j = 1; j < partial_sum.rows(); j++) {
      partial_sum(j, i) += partial_sum(j - 1, i);
    }
  } 
  */
  
  // Initilaize four matrices, storing best scores and indexes for completed and incomplete streaks respectively.
  NumericMatrix completed_scores(values.rows(), values.cols());
  NumericMatrix completed_indexes(values.rows(), values.cols());
  
  // from 0 to min_length - 1, we have trash!
  for (int stream = 0; stream < values.cols(); stream++) {
    for (int i = 0; i < min_length; i++) {
      completed_scores(i, stream) = R_NegInf;
      if (i == min_length - 1) {
        completed_indexes(i, stream) = STREAM_START;
      } else {
        completed_indexes(i, stream) = STREAM_FAKE;
      }
    }
  }
  
  // i = min_length here
  for (int stream = 0; stream < values.cols(); stream++) {
    double completed_score = 0.0;
    // TODO: use the partial sums for this.
    for (int j = 0; j <= min_length - 1; j++) {
      completed_score += values(j, stream);
    }
    completed_scores(min_length - 1, stream) = completed_score;
  }
  
  // Score these, "normally"
  for (int i = min_length; i < values.rows(); i++) {
    for (int stream = 0; stream < values.cols(); stream++) {
      
      // We have two options - either add value(i, stream) to the previous completed score, or
      // go back and get the best score from the previous option ("freeing up" another score..)
      
      double best_score = values(i, stream) + completed_scores(i - 1, stream);
      int best_option = stream; // that is, tack on to the previous score.
      
      // TODO: use the partial sums for this.
      double completion_score = 0.0;
      for (int j = 0; j < min_length; j++) {
        completion_score += values(i - j, stream);
      }
      
      for (int option = 0; option < completed_scores.cols(); option++) {
        if (option != stream) { // TODO: can this be safely ignored? probably, because there are no uncompleted streaks right?
          double option_score = completion_score + completed_scores(i - min_length, option);
          if (option_score > best_score) {
            best_score = option_score;
            best_option = option;
          }
        }
      }
      completed_scores(i, stream) = best_score;
      completed_indexes(i, stream) = best_option;
    }
    
  }
  
  // dump out all the numerics from the three important value...
  // Rcpp::Rcout << "Streak: " << min_length << std::endl;
  // Rcpp::Rcout << "Input Values:" << std::endl << values << std::endl;
  // Rcpp::Rcout << "Completed Scores:" << std::endl << completed_scores << std::endl;
  // Rcpp::Rcout << "Completed Indexes:" << std::endl << completed_indexes << std::endl;
  
  // Finally, look at all the options at the very end and choose the highest score.
  double best_score = R_NegInf;
  int best_stream = STREAM_FAKE;
  for (int stream = 0; stream < values.cols(); stream++) {
    double stream_score = completed_scores(values.rows() - 1, stream);
    if (best_score < stream_score) {
      best_stream = stream;
      best_score = stream_score;
    } 
  }
  
  // And work backward from there.
  // completed score... the choice made at
  NumericVector retval(values.rows());
  for (int i = 0; i < values.rows(); i++) {
    retval(i) = STREAM_FAKE;
  }
  
  int current_stream = best_stream;
  int index = values.rows() - 1;
  
  while(current_stream >= 0 && index >= 0) {
    retval(index) = current_stream;
    // peek behind at the previous one.
    int prev_stream = completed_indexes(index, current_stream);
    // if it's the same, don't worry, just continue on...
    // if not:
    if (current_stream != prev_stream) {
      // also jump back min_length items
      for (int i = 0; i < min_length - 1; i++) {
        index--;
        retval(index) = current_stream;
      }
      current_stream = prev_stream;
    }
    index--;
  }
  
  if (current_stream != STREAM_START || index != -1) {
    stop("Error: Could not backtrack through indexes correctly. Expected stream start sentinel (-1), found %i. Expected index to be -1, found %i.", current_stream, index);
  }
    
  return retval + 1; // convert to one-indexed R matrixes
}












