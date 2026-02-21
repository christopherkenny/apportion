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

extern void balinski_young(
  const int* const n_tot__,
  const double* const pop__,
  const int* const apprt__,
  const R_len_t pop__dim_1_,
  const R_len_t pop__dim_2_);

SEXP balinski_young_(SEXP _args) {
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

  // apprt
  _args = CDR(_args);
  SEXP apprt = CAR(_args);
  if (TYPEOF(apprt) != INTSXP) {
    Rf_error("typeof(apprt) must be 'integer', not '%s'", Rf_type2char(TYPEOF(apprt)));
  }
  const int* const apprt__ = INTEGER(apprt);
  const int* const apprt__dim_ = ({
  SEXP dim_ = Rf_getAttrib(apprt, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "apprt must be a 2D-array, but length(dim(apprt)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int apprt__dim_1_ = apprt__dim_[0];
  const int apprt__dim_2_ = apprt__dim_[1];

  if (n_tot__len_ != 1)
    Rf_error("length(n_tot) must be 1, not %0.f",
              (double)n_tot__len_);
  if (pop__dim_1_ != apprt__dim_1_)
    Rf_error("dim(apprt)[1] must equal dim(pop)[1],"
             " but are %0.f and %0.f",
              (double)apprt__dim_1_, (double)pop__dim_1_);
  if (pop__dim_2_ != apprt__dim_2_)
    Rf_error("dim(apprt)[2] must equal dim(pop)[2],"
             " but are %0.f and %0.f",
              (double)apprt__dim_2_, (double)pop__dim_2_);

  balinski_young(
    n_tot__,
    pop__,
    apprt__,
    pop__dim_1_,
    pop__dim_2_);

  return apprt;
}

extern void dean(
  const int* const n_tot__,
  const double* const pop__,
  int* const out__,
  const R_len_t pop__dim_1_,
  const R_len_t pop__dim_2_);

SEXP dean_(SEXP _args) {
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

  dean(
    n_tot__,
    pop__,
    out__,
    pop__dim_1_,
    pop__dim_2_);

  UNPROTECT(2);
  return out;
}

extern void dhondt(
  const int* const n_tot__,
  const double* const pop__,
  const int* const apprt__,
  const R_len_t pop__dim_1_,
  const R_len_t pop__dim_2_);

SEXP dhondt_(SEXP _args) {
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

  // apprt
  _args = CDR(_args);
  SEXP apprt = CAR(_args);
  if (TYPEOF(apprt) != INTSXP) {
    Rf_error("typeof(apprt) must be 'integer', not '%s'", Rf_type2char(TYPEOF(apprt)));
  }
  const int* const apprt__ = INTEGER(apprt);
  const int* const apprt__dim_ = ({
  SEXP dim_ = Rf_getAttrib(apprt, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "apprt must be a 2D-array, but length(dim(apprt)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int apprt__dim_1_ = apprt__dim_[0];
  const int apprt__dim_2_ = apprt__dim_[1];

  if (n_tot__len_ != 1)
    Rf_error("length(n_tot) must be 1, not %0.f",
              (double)n_tot__len_);
  if (pop__dim_1_ != apprt__dim_1_)
    Rf_error("dim(apprt)[1] must equal dim(pop)[1],"
             " but are %0.f and %0.f",
              (double)apprt__dim_1_, (double)pop__dim_1_);
  if (pop__dim_2_ != apprt__dim_2_)
    Rf_error("dim(apprt)[2] must equal dim(pop)[2],"
             " but are %0.f and %0.f",
              (double)apprt__dim_2_, (double)pop__dim_2_);

  dhondt(
    n_tot__,
    pop__,
    apprt__,
    pop__dim_1_,
    pop__dim_2_);

  return apprt;
}

extern void hamilton_vinton(
  const int* const n_tot__,
  const double* const pop__,
  int* const out__,
  const R_len_t pop__dim_1_,
  const R_len_t pop__dim_2_);

SEXP hamilton_vinton_(SEXP _args) {
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

  hamilton_vinton(
    n_tot__,
    pop__,
    out__,
    pop__dim_1_,
    pop__dim_2_);

  UNPROTECT(2);
  return out;
}

extern void huntington_hill(
  const int* const n_tot__,
  const double* const pop__,
  const int* const apprt__,
  const R_len_t pop__dim_1_,
  const R_len_t pop__dim_2_);

SEXP huntington_hill_(SEXP _args) {
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

  // apprt
  _args = CDR(_args);
  SEXP apprt = CAR(_args);
  if (TYPEOF(apprt) != INTSXP) {
    Rf_error("typeof(apprt) must be 'integer', not '%s'", Rf_type2char(TYPEOF(apprt)));
  }
  const int* const apprt__ = INTEGER(apprt);
  const int* const apprt__dim_ = ({
  SEXP dim_ = Rf_getAttrib(apprt, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "apprt must be a 2D-array, but length(dim(apprt)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int apprt__dim_1_ = apprt__dim_[0];
  const int apprt__dim_2_ = apprt__dim_[1];

  if (n_tot__len_ != 1)
    Rf_error("length(n_tot) must be 1, not %0.f",
              (double)n_tot__len_);
  if (pop__dim_1_ != apprt__dim_1_)
    Rf_error("dim(apprt)[1] must equal dim(pop)[1],"
             " but are %0.f and %0.f",
              (double)apprt__dim_1_, (double)pop__dim_1_);
  if (pop__dim_2_ != apprt__dim_2_)
    Rf_error("dim(apprt)[2] must equal dim(pop)[2],"
             " but are %0.f and %0.f",
              (double)apprt__dim_2_, (double)pop__dim_2_);

  huntington_hill(
    n_tot__,
    pop__,
    apprt__,
    pop__dim_1_,
    pop__dim_2_);

  return apprt;
}

extern void webster(
  const int* const n_tot__,
  const double* const pop__,
  const int* const apprt__,
  const R_len_t pop__dim_1_,
  const R_len_t pop__dim_2_);

SEXP webster_(SEXP _args) {
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

  // apprt
  _args = CDR(_args);
  SEXP apprt = CAR(_args);
  if (TYPEOF(apprt) != INTSXP) {
    Rf_error("typeof(apprt) must be 'integer', not '%s'", Rf_type2char(TYPEOF(apprt)));
  }
  const int* const apprt__ = INTEGER(apprt);
  const int* const apprt__dim_ = ({
  SEXP dim_ = Rf_getAttrib(apprt, R_DimSymbol);
  if (Rf_length(dim_) != 2) Rf_error(
    "apprt must be a 2D-array, but length(dim(apprt)) is %i",
    (int) Rf_length(dim_));
  INTEGER(dim_);});
  const int apprt__dim_1_ = apprt__dim_[0];
  const int apprt__dim_2_ = apprt__dim_[1];

  if (n_tot__len_ != 1)
    Rf_error("length(n_tot) must be 1, not %0.f",
              (double)n_tot__len_);
  if (pop__dim_1_ != apprt__dim_1_)
    Rf_error("dim(apprt)[1] must equal dim(pop)[1],"
             " but are %0.f and %0.f",
              (double)apprt__dim_1_, (double)pop__dim_1_);
  if (pop__dim_2_ != apprt__dim_2_)
    Rf_error("dim(apprt)[2] must equal dim(pop)[2],"
             " but are %0.f and %0.f",
              (double)apprt__dim_2_, (double)pop__dim_2_);

  webster(
    n_tot__,
    pop__,
    apprt__,
    pop__dim_1_,
    pop__dim_2_);

  return apprt;
}

static const R_ExternalMethodDef QuickrEntries[] = {
  {"adams_", (DL_FUNC) &adams_, -1},
  {"balinski_young_", (DL_FUNC) &balinski_young_, -1},
  {"dean_", (DL_FUNC) &dean_, -1},
  {"dhondt_", (DL_FUNC) &dhondt_, -1},
  {"hamilton_vinton_", (DL_FUNC) &hamilton_vinton_, -1},
  {"huntington_hill_", (DL_FUNC) &huntington_hill_, -1},
  {"webster_", (DL_FUNC) &webster_, -1},
  {NULL, NULL, 0}
};

#include <R_ext/Rdynload.h>

void R_init_apportion(DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, NULL, QuickrEntries);
  R_useDynamicSymbols(dll, FALSE);
}
