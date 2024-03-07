// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// sum.cpp
writable::doubles row_sum(data_frame tb);
extern "C" SEXP _statart_row_sum(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_sum(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// sum.cpp
writable::doubles row_max(data_frame tb);
extern "C" SEXP _statart_row_max(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_max(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// sum.cpp
writable::doubles row_min(data_frame tb);
extern "C" SEXP _statart_row_min(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_min(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// sum.cpp
writable::integers row_unique(data_frame tb);
extern "C" SEXP _statart_row_unique(SEXP tb) {
  BEGIN_CPP11
    return cpp11::as_sexp(row_unique(cpp11::as_cpp<cpp11::decay_t<data_frame>>(tb)));
  END_CPP11
}
// sum.cpp
double s_sum(doubles vec);
extern "C" SEXP _statart_s_sum(SEXP vec) {
  BEGIN_CPP11
    return cpp11::as_sexp(s_sum(cpp11::as_cpp<cpp11::decay_t<doubles>>(vec)));
  END_CPP11
}
// sum.cpp
double s_mean(doubles vec);
extern "C" SEXP _statart_s_mean(SEXP vec) {
  BEGIN_CPP11
    return cpp11::as_sexp(s_mean(cpp11::as_cpp<cpp11::decay_t<doubles>>(vec)));
  END_CPP11
}
// sum.cpp
double s_max(doubles vec);
extern "C" SEXP _statart_s_max(SEXP vec) {
  BEGIN_CPP11
    return cpp11::as_sexp(s_max(cpp11::as_cpp<cpp11::decay_t<doubles>>(vec)));
  END_CPP11
}
// sum.cpp
double dbl_sd(doubles dbl_vec);
extern "C" SEXP _statart_dbl_sd(SEXP dbl_vec) {
  BEGIN_CPP11
    return cpp11::as_sexp(dbl_sd(cpp11::as_cpp<cpp11::decay_t<doubles>>(dbl_vec)));
  END_CPP11
}
// sum.cpp
int n_distinct10k(doubles vec);
extern "C" SEXP _statart_n_distinct10k(SEXP vec) {
  BEGIN_CPP11
    return cpp11::as_sexp(n_distinct10k(cpp11::as_cpp<cpp11::decay_t<doubles>>(vec)));
  END_CPP11
}
// sum.cpp
double s_median(doubles vec);
extern "C" SEXP _statart_s_median(SEXP vec) {
  BEGIN_CPP11
    return cpp11::as_sexp(s_median(cpp11::as_cpp<cpp11::decay_t<doubles>>(vec)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_statart_dbl_sd",        (DL_FUNC) &_statart_dbl_sd,        1},
    {"_statart_n_distinct10k", (DL_FUNC) &_statart_n_distinct10k, 1},
    {"_statart_row_max",       (DL_FUNC) &_statart_row_max,       1},
    {"_statart_row_min",       (DL_FUNC) &_statart_row_min,       1},
    {"_statart_row_sum",       (DL_FUNC) &_statart_row_sum,       1},
    {"_statart_row_unique",    (DL_FUNC) &_statart_row_unique,    1},
    {"_statart_s_max",         (DL_FUNC) &_statart_s_max,         1},
    {"_statart_s_mean",        (DL_FUNC) &_statart_s_mean,        1},
    {"_statart_s_median",      (DL_FUNC) &_statart_s_median,      1},
    {"_statart_s_sum",         (DL_FUNC) &_statart_s_sum,         1},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_statart(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
