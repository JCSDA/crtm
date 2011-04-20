#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "idl_export.h"

/**************************************************************************
  Helper routines
***************************************************************************/

static IDL_VPTR get_IDL_byte(int b) {
  IDL_VPTR idl_b;
  if ((idl_b = IDL_Gettmp()) == (IDL_VPTR) NULL) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP,
                "could not create temporary variable");
  }
 
  idl_b->type = IDL_TYP_BYTE;
  idl_b->value.c = (UCHAR) b;

  return idl_b;
}

#define MG_C_ABS(z) sqrt(z.r*z.r + z.i*z.i)

#define MG_ARRAY_EQUAL_ARR2ARR(TYPE, TOLERANCE_TYPE, DIFFERENCE_EXPR, ABS)  \
int mg_array_equal_ ## TYPE ## _arr2arr(int n,                              \
                                        TYPE *arr1, TYPE *arr2,             \
                                        TOLERANCE_TYPE tolerance) {         \
  int i;                                                                    \
  for (i = 0; i < n; i++) {                                                 \
    TYPE d, a1 = arr1[i], a2 = arr2[i];                                     \
    DIFFERENCE_EXPR;                                                        \
    if (ABS(d) > tolerance) return 0;                                       \
  }                                                                         \
  return 1;                                                                 \
}

MG_ARRAY_EQUAL_ARR2ARR(UCHAR, UCHAR, d = a1 - a2, abs)
MG_ARRAY_EQUAL_ARR2ARR(IDL_INT, IDL_INT, d = a1 - a2, abs)
MG_ARRAY_EQUAL_ARR2ARR(IDL_LONG, IDL_LONG, d = a1 - a2, abs)
MG_ARRAY_EQUAL_ARR2ARR(float, float, d = a1 - a2, fabs)
MG_ARRAY_EQUAL_ARR2ARR(double, double, d = a1 - a2, fabs)
MG_ARRAY_EQUAL_ARR2ARR(IDL_COMPLEX, float, d.r = a1.r - a2.r; d.i = a1.i - a2.i, MG_C_ABS)
MG_ARRAY_EQUAL_ARR2ARR(IDL_DCOMPLEX, double, d.r = a1.r - a2.r; d.i = a1.i - a2.i, MG_C_ABS)
MG_ARRAY_EQUAL_ARR2ARR(IDL_UINT, IDL_UINT, d = a1 - a2, abs)
MG_ARRAY_EQUAL_ARR2ARR(IDL_ULONG, IDL_ULONG, d = a1 - a2, abs)
MG_ARRAY_EQUAL_ARR2ARR(IDL_LONG64, IDL_LONG64, d = a1 - a2, labs)
MG_ARRAY_EQUAL_ARR2ARR(IDL_ULONG64, IDL_ULONG64, d = a1 - a2, labs)

#define MG_ARRAY_EQUAL_SCALAR2ARR(TYPE, TOLERANCE_TYPE, DIFFERENCE_EXPR, ABS) \
int mg_array_equal_ ## TYPE ## _scalar2arr(int n,                             \
                                           TYPE scalar, TYPE *arr,            \
                                           TOLERANCE_TYPE tolerance) {        \
  int i;                                                                      \
  for (i = 0; i < n; i++) {                                                   \
    TYPE d, a1 = scalar, a2 = arr[i];                                         \
    DIFFERENCE_EXPR;                                                          \
    if (ABS(d) > tolerance) return 0;                                         \
  }                                                                           \
  return 1;                                                                   \
}

MG_ARRAY_EQUAL_SCALAR2ARR(UCHAR, UCHAR, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_INT, IDL_INT, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_LONG, IDL_LONG, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2ARR(float, float, d = a1 - a2, fabs)
MG_ARRAY_EQUAL_SCALAR2ARR(double, double, d = a1 - a2, fabs)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_COMPLEX, float, d.r = a1.r - a2.r; d.i = a1.i - a2.i, MG_C_ABS)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_DCOMPLEX, double, d.r = a1.r - a2.r; d.i = a1.i - a2.i, MG_C_ABS)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_UINT, IDL_UINT, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_ULONG, IDL_ULONG, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_LONG64, IDL_LONG64, d = a1 - a2, labs)
MG_ARRAY_EQUAL_SCALAR2ARR(IDL_ULONG64, IDL_ULONG64, d = a1 - a2, labs)

#define MG_ARRAY_EQUAL_SCALAR2SCALAR(TYPE, TOLERANCE_TYPE, DIFFERENCE_EXPR, ABS) \
int mg_array_equal_ ## TYPE ## _scalar2scalar(TYPE scalar1, TYPE scalar2,        \
                                              TOLERANCE_TYPE tolerance) {        \
  int i;                                                                         \
  TYPE d, a1 = scalar1, a2 = scalar2;                                            \
  DIFFERENCE_EXPR;                                                               \
  if (ABS(d) > tolerance) return 0;                                              \
  return 1;                                                                      \
}

MG_ARRAY_EQUAL_SCALAR2SCALAR(UCHAR, UCHAR, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_INT, IDL_INT, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_LONG, IDL_LONG, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(float, float, d = a1 - a2, fabs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(double, double, d = a1 - a2, fabs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_COMPLEX, float, d.r = a1.r - a2.r; d.i = a1.i - a2.i, MG_C_ABS)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_DCOMPLEX, double, d.r = a1.r - a2.r; d.i = a1.i - a2.i, MG_C_ABS)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_UINT, IDL_UINT, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_ULONG, IDL_ULONG, d = a1 - a2, abs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_LONG64, IDL_LONG64, d = a1 - a2, labs)
MG_ARRAY_EQUAL_SCALAR2SCALAR(IDL_ULONG64, IDL_ULONG64, d = a1 - a2, labs)


#define MG_ARRAY_EQUAL_ARR2ARR_CASE(TYPE_VALUE, TYPE, TOLERANCE_TYPE, IDL_MEMBER, ZERO)          \
    case TYPE_VALUE:                                                                             \
      is_equal = mg_array_equal_ ## TYPE ## _arr2arr(argv[0]->value.arr->n_elts,                 \
                                                     (TYPE *) argv[0]->value.arr->data,          \
                                                     (TYPE *) argv[1]->value.arr->data,          \
                                                     kw.tolerance_present ? kw.tolerance->value.IDL_MEMBER : ZERO); \
      break;

#define MG_ARRAY_EQUAL_SCALAR2ARR_CASE(TYPE_VALUE, TYPE, SCALAR_ARG, ARR_ARG, TOLERANCE_TYPE, IDL_ARGMEMBER, IDL_TOLMEMBER, ZERO) \
    case TYPE_VALUE:                                                                              \
      is_equal = mg_array_equal_ ## TYPE ## _scalar2arr(ARR_ARG->value.arr->n_elts,               \
                                                        (TYPE) SCALAR_ARG->value.IDL_ARGMEMBER,   \
                                                        (TYPE *) ARR_ARG->value.arr->data,        \
                                                        kw.tolerance_present ? kw.tolerance->value.IDL_TOLMEMBER : ZERO); \
      break;

#define MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(TYPE_VALUE, TYPE, TOLERANCE_TYPE, IDL_ARGMEMBER, IDL_TOLMEMBER, ZERO)    \
    case TYPE_VALUE:                                                                              \
      is_equal = mg_array_equal_ ## TYPE ## _scalar2scalar((TYPE) argv[0]->value.IDL_ARGMEMBER,   \
                                                           (TYPE) argv[1]->value.IDL_ARGMEMBER,   \
                                                           kw.tolerance_present ? kw.tolerance->value.IDL_TOLMEMBER : ZERO); \
      break;


IDL_VPTR IDL_mg_array_equal(int argc, IDL_VPTR *argv, char *argk) {
  int is_equal, nargs;
    
  typedef struct {  
    IDL_KW_RESULT_FIRST_FIELD;
    IDL_LONG no_typeconv;    
    IDL_VPTR tolerance; 
    int tolerance_present;
  } KW_RESULT;  
  
  static IDL_KW_PAR kw_pars[] = {  
    { "NO_TYPECONV", IDL_TYP_LONG, 1, IDL_KW_ZERO,  
      0, IDL_KW_OFFSETOF(no_typeconv) },   
    { "TOLERANCE", IDL_TYP_UNDEF, 1, IDL_KW_VIN | IDL_KW_OUT,  
      IDL_KW_OFFSETOF(tolerance_present), IDL_KW_OFFSETOF(tolerance) },   
    { NULL }  
  };  
  
  KW_RESULT kw;

  IDL_ENSURE_SIMPLE(argv[0]);
  IDL_ENSURE_SIMPLE(argv[1]);
  
  nargs = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, (IDL_VPTR *) NULL, 1, &kw); 

  if (kw.no_typeconv && (argv[0]->type != argv[1]->type)) {
    IDL_KW_FREE;
    return get_IDL_byte(0); 
  }

  if (argv[0]->type != argv[1]->type) {
    IDL_KW_FREE;
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP,
                "input parameters must be of the same type");
  }
  
  if (kw.tolerance_present && (kw.tolerance->type != argv[0]->type)) {
    IDL_KW_FREE;
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP,
                "TOLERANCE and input parameters must be of the same type");
  }
  
  // TODO: conversion between two different types
  
  if (argv[0]->flags & IDL_V_ARR && argv[1]->flags & IDL_V_ARR) {
    if (argv[0]->value.arr->n_elts != argv[1]->value.arr->n_elts) {
      IDL_KW_FREE;
      return get_IDL_byte(0);
    }
        
    switch (argv[0]->type) {
      MG_ARRAY_EQUAL_ARR2ARR_CASE(1, UCHAR, UCHAR, c, 0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(2, IDL_INT, IDL_INT, i, 0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(3, IDL_LONG, IDL_LONG, l, 0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(4, float, float, f, 0.0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(5, double, double, d, 0.0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(6, IDL_COMPLEX, float, f, 0.0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(9, IDL_DCOMPLEX, double, d, 0.0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(12, IDL_UINT, IDL_UINT, ui, 0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(13, IDL_ULONG, IDL_ULONG, ul, 0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(14, IDL_LONG64, IDL_LONG64, l64, 0)
      MG_ARRAY_EQUAL_ARR2ARR_CASE(15, IDL_ULONG64, IDL_ULONG64, ul64, 0)
    }
  } else if (argv[0]->flags & IDL_V_ARR && !(argv[1]->flags & IDL_V_ARR)) {
    switch (argv[0]->type) {
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(1, UCHAR, argv[1], argv[0], UCHAR, c, c, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(2, IDL_INT, argv[1], argv[0], IDL_INT, i, i, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(3, IDL_LONG, argv[1], argv[0], IDL_LONG, l, l, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(4, float, argv[1], argv[0], float, f, f, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(5, double, argv[1], argv[0], double, d, d, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(6, IDL_COMPLEX, argv[1], argv[0], float, cmp, f, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(9, IDL_DCOMPLEX, argv[1], argv[0], double, dcmp, d, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(12, IDL_UINT, argv[1], argv[0], IDL_UINT, ui, ui, 0);
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(13, IDL_ULONG, argv[1], argv[0], IDL_ULONG, ul, ul, 0);
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(14, IDL_LONG64, argv[1], argv[0], IDL_LONG64, l64, l64, 0);
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(15, IDL_ULONG64, argv[1], argv[0], IDL_ULONG64, ul64, ul64, 0);
    }
  } else if (!(argv[0]->flags & IDL_V_ARR) && argv[1]->flags & IDL_V_ARR) {
    switch (argv[0]->type) {
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(1, UCHAR, argv[0], argv[1], UCHAR, c, c, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(2, IDL_INT, argv[0], argv[1], IDL_INT, i, i, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(3, IDL_LONG, argv[0], argv[1], IDL_LONG, l, l, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(4, float, argv[0], argv[1], float, f, f, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(5, double, argv[0], argv[1], double, d, d, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(6, IDL_COMPLEX, argv[0], argv[1], float, cmp, f, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(9, IDL_DCOMPLEX, argv[0], argv[1], double, dcmp, d, 0.0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(12, IDL_UINT, argv[0], argv[1], IDL_UINT, ui, ui, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(13, IDL_ULONG, argv[0], argv[1], IDL_ULONG, ul, ul, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(14, IDL_LONG64, argv[0], argv[1], IDL_LONG64, l64, l64, 0)
      MG_ARRAY_EQUAL_SCALAR2ARR_CASE(15, IDL_ULONG64, argv[0], argv[1], IDL_ULONG64, ul64, ul64, 0)
    }
  } else {
    switch (argv[0]->type) {
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(1, UCHAR, UCHAR, c, c, 0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(2, IDL_INT, IDL_INT, i, i, 0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(3, IDL_LONG, IDL_LONG, l, l, 0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(4, float, float, f, f, 0.0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(5, double, double, d, d, 0.0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(6, IDL_COMPLEX, float, cmp, f, 0.0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(9, IDL_DCOMPLEX, double, dcmp, d, 0.0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(12, IDL_UINT, IDL_UINT, ui, ui, 0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(13, IDL_ULONG, IDL_ULONG, ul, ul, 0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(14, IDL_LONG64, IDL_LONG64, l64, l64, 0)
      MG_ARRAY_EQUAL_SCALAR2SCALAR_CASE(15, IDL_ULONG64, IDL_ULONG64, ul64, ul64, 0)
    }
  }

  IDL_KW_FREE;
                                                                                        
  return get_IDL_byte(is_equal);
}


int IDL_Load(void) {  
  /*
   * These tables contain information on the functions and procedures
   * that make up the cmdline_tools DLM. The information contained in these
   * tables must be identical to that contained in mg_introspection.dlm.
   */
  static IDL_SYSFUN_DEF2 function_addr[] = {
    { IDL_mg_array_equal, "MG_ARRAY_EQUAL", 2, 2, IDL_SYSFUN_DEF_F_KEYWORDS, 0 },
  };
  
  /*
   * Register our routines. The routines must be specified exactly the same
   * as in mg_introspection.dlm.
   */
  return IDL_SysRtnAdd(function_addr, TRUE, IDL_CARRAY_ELTS(function_addr));  
}
