// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// code.cpp
void fun();
extern "C" SEXP _statart_fun() {
  BEGIN_CPP11
    fun();
    return R_NilValue;
  END_CPP11
}
// row_stat.cpp
cpp11::doubles_matrix<> df_to_mat(data_frame tb, double na);
extern "C" SEXP _statart_df_to_mat(SEXP tb, SEXP na) {
  BEGIN_CPP11
    return cpp11::as_sexp(df_to_mat(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb), cpp11::as_cpp<cpp11::decay_t<double>>(na)));
  END_CPP11
}
// row_stat.cpp
writable::doubles row_max_dbl(data_frame tb);
extern "C" SEXP _statart_row_max_dbl(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_max_dbl(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// row_stat.cpp
writable::doubles row_min_dbl(data_frame tb);
extern "C" SEXP _statart_row_min_dbl(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_min_dbl(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// row_stat.cpp
writable::doubles row_sum_dbl(data_frame tb);
extern "C" SEXP _statart_row_sum_dbl(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_sum_dbl(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// row_stat.cpp
writable::doubles row_mean_dbl(data_frame tb);
extern "C" SEXP _statart_row_mean_dbl(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_mean_dbl(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// row_stat.cpp
writable::integers row_unique_dbl(data_frame tb);
extern "C" SEXP _statart_row_unique_dbl(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_unique_dbl(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_statart_df_to_mat",      (DL_FUNC) &_statart_df_to_mat,      2},
    {"_statart_fun",            (DL_FUNC) &_statart_fun,            0},
    {"_statart_row_max_dbl",    (DL_FUNC) &_statart_row_max_dbl,    1},
    {"_statart_row_mean_dbl",   (DL_FUNC) &_statart_row_mean_dbl,   1},
    {"_statart_row_min_dbl",    (DL_FUNC) &_statart_row_min_dbl,    1},
    {"_statart_row_sum_dbl",    (DL_FUNC) &_statart_row_sum_dbl,    1},
    {"_statart_row_unique_dbl", (DL_FUNC) &_statart_row_unique_dbl, 1},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_statart(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
