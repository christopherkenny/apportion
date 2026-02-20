#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>


extern void adams(
  const int* const n_tot__,
  const double* const pop__,
  int* const out__,
  const R_len_t pop__dim_1_,
  const R_len_t pop__dim_2_);

SEXP adams_(SEXP _args) {
  // n_tot
  _args = CDR(_args);
  SEXP n_tot = CAR(_args);
  if (TYPEOF(n_tot) != INTSXP) {
    Rf_error("typeof(n_tot) must be 'integer', not '%s'", Rf_type2char(TYPEOF(n_tot)));
  }
  const int* const n_tot__ = INTEGER(n_tot);
  const R_xlen_t n_tot__len_ = Rf_xlength(n_tot);

  // pop
  _args = CDR(_args);
  SEXP pop = CAR(_args);
  if (TYPEOF(pop) != REALSXP) {
    Rf_error("typeof(pop) must be 'double', not '%s'", Rf_type2char(TYPEOF(pop)));
  }
  const double* const pop__ = REAL(pop);
  const int* const pop__dim_ = ({
  SEXP dim_ = Rf_getAttrib(pop, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "pop must be a 2D-array, but length(dim(pop)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int pop__dim_1_ = pop__dim_[0];
  const int pop__dim_2_ = pop__dim_[1];

  if (n_tot__len_ != 1)
    Rf_error("length(n_tot) must be 1, not %0.f",
              (double)n_tot__len_);
  const R_xlen_t out__len_ = (pop__dim_1_) * (pop__dim_2_);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, out__len_));
  int* out__ = INTEGER(out);
  {
    const SEXP _dim_sexp = PROTECT(Rf_allocVector(INTSXP, 2));
    int* const _dim = INTEGER(_dim_sexp);
    _dim[0] = pop__dim_1_;
    _dim[1] = pop__dim_2_;
    Rf_dimgets(out, _dim_sexp);
  }

  adams(
    n_tot__,
    pop__,
    out__,
    pop__dim_1_,
    pop__dim_2_);

  UNPROTECT(2);
  return out;
}

static const R_ExternalMethodDef QuickrEntries[] = {
  {"adams_", (DL_FUNC) &adams_, -1},
  {NULL, NULL, 0}
};

#include <R_ext/Rdynload.h>

void R_init_apportion(DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
  R_useDynamicSymbols(dll, FALSE);
}
