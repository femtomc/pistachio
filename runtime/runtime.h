#ifndef RUNTIME_H
#define RUNTIME_H

#include <inttypes.h>
#include <math.h>
#include <stdio.h>

typedef struct FunctionClosure_ {
  void *function;
} FunctionClosure;

int64_t mc_print_inf_f(FunctionClosure *self, int64_t i);
FunctionClosure mc_print_int;

int64_t mc_print_newline_f(FunctionClosure *self, int64_t i);
FunctionClosure mc_print_newline;

double mc_float_of_int_f(FunctionClosure *self, int64_t i);
FunctionClosure mc_float_of_int;

int64_t mc_int_of_float_f(FunctionClosure *self, double d);
FunctionClosure mc_int_of_float;

// truncate = int_of_float
FunctionClosure mc_truncate;

double mc_abs_float_f(FunctionClosure *self, double d);
FunctionClosure mc_abs_float;

double mc_sqrt_f(FunctionClosure *self, double d);
FunctionClosure mc_sqrt;

double mc_sin_f(FunctionClosure *self, double d);
FunctionClosure mc_sin;

double mc_cos_f(FunctionClosure *self, double d);
FunctionClosure mc_cos;

#endif
