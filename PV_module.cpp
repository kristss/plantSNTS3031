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

//' @param T_e Outdoor temperature
//' @param I_sol Solar radiation
//' @export 
// [[Rcpp::export]]
  NumericVector rcpp_SN3031(NumericVector T_e, NumericVector I_sol,
                            double n_nom, double tau_alpha, double U_L,
                            double I_ref, double IAM, 
                            double a_temp, double phi_array, 
                            double n_inv, double phi_soil,
                            double A_MOD, double N_MOD) {

  int n = T_e.size();
  NumericMatrix Out(n,3);
  
  for (int i=0; i < n; i++) {
  // Solcelletemperaturen, T_cell [DegC]
  double T_cell = T_e[i] + (I_sol[i] * (tau_alpha - (n_nom/100)))/U_L;
  // Tap pga. av høy celletemperatur
  double phi_temp = a_temp * (T_cell - 25);
  // Tapskoeffesient
  double f_perf = IAM * (1 - phi_soil/100) * (1 - phi_temp/100) * (1 - phi_array/100) * n_inv/100;
  // Største effekt fra en rad med solceller
  double P_PK = (n_nom * I_ref * A_MOD * N_MOD) / 100;
  // Elektrisk effekt ut av solcelleanlegget
  double P_PV = (P_PK * f_perf * I_sol[i]) / I_ref;
  // Systemvirkningsgraden for hele systemet [%]
  double n_sys = n_nom * f_perf;
  Out(i, 0) = T_cell;
  Out(i, 1) = P_PV;
  Out(i, 2) = n_sys;
  }
  colnames(Out) = CharacterVector::create("T_cell", "P_PV", "n_sys");
  return Out;
  }
    

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
x <- rcpp_SN3031(A_MOD=1, N_MOD=1, n_nom=17, 
                 I_ref=1000, IAM=0.96, U_L=35, 
                 a_temp=0.45,phi_array=5.5,tau_alpha=0.90, 
                 n_inv=98, phi_soil=2, 
                 T_e=df$n_hour,I_sol=df$I_sol_ver)
*/