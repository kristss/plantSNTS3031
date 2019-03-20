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
  NumericVector rcpp_SN3031(NumericVector T_e, NumericVector L_SH,
                            NumericVector T_s, NumericVector P20, NumericVector C20,
                            double P_HP_13, double COP_HP_13, 
                            double T_sh_set, double T_dhw_set, 
                            double H_s, double K, double T_a,
                            double dt) {

  int n = T_e.size();
  NumericMatrix Out(n,8);
  double P_HP_SH;
  double COP_HP_SH;
  double COPX_HP_SH;
  double E_HP_DHW;
  double E_HP_SH;
  
  for (int i=0; i < n; i++) {
  // AWHP
  double T_sh_s0 = T_sh_set;
  double Ts = T_e[i]; // Source temperature
  if (Ts < T_s[0]) {P_HP_SH = P_HP_13 * 0;}
  else if (Ts< T_s[1]) {P_HP_SH = P_HP_13 * (P20[0] + (Ts-T_s[0])*((P20[1] - P20[0])/(T_s[1]-T_s[0])));}
  else if (Ts< T_s[2]) {P_HP_SH = P_HP_13 * (P20[1] + (Ts-T_s[1])*((P20[2] - P20[1])/(T_s[2]-T_s[1])));} 
  else {P_HP_SH = P_HP_13 * P20[2];}
  if (Ts < T_s[0]) {COP_HP_SH = COP_HP_13 * C20[0];}
  else if (Ts< T_s[1]) {COP_HP_SH = COP_HP_13 * (C20[0] + (Ts-T_s[0])*((C20[1] - C20[0])/(T_s[1]-T_s[0])));}
  else if (Ts< T_s[2]) {COP_HP_SH = COP_HP_13 * (C20[1] + (Ts-T_s[1])*((C20[2] - C20[1])/(T_s[2]-T_s[1])));} 
  else {COP_HP_SH = COP_HP_13 * C20[2];}
  // simultanous DHW and space heating
  double t_SH = dt; // hours available for space heating
  double Q_HP_SH = min(NumericVector::create(L_SH[i],(t_SH*P_HP_SH)));
  // simplified part load, space heatin mode
  // if on/off controlled HP
  //COPX_HP_DHW = COP_HP_DHW * X_DHW/(0.9*X_DHW+0.1)
  // simplified part load, dhw mode
  double X_SH = Q_HP_SH / P_HP_SH;
  // if invert controlled HP
  if (X_SH<=0.25) {COPX_HP_SH = COP_HP_SH * (4*X_SH/(0.9*X_SH+0.1));} else {COPX_HP_SH = COP_HP_SH * 1;};
  // if on/off controlled HP
  // COPX_HP_SH = COP_HP_SH * X_SH/(0.9*X_SH+0.1)
  // Added:
  if(!COPX_HP_SH) {E_HP_SH = 0;} else {E_HP_SH = Q_HP_SH/COPX_HP_SH;};
  double E_HP = E_HP_SH;
  double Q_TOT = L_SH[i];
  double Q_HP_TOT = Q_HP_SH;
  double Q_EL_SH = L_SH[i] - Q_HP_SH;
  double Q_EL_TOT = Q_EL_SH;
  Out(i, 0) = Ts;
  Out(i, 1) = E_HP;
  Out(i, 2) = Q_EL_TOT;
  Out(i, 3) = Q_HP_TOT;
  Out(i, 4) = Q_TOT;
  Out(i, 5) = X_SH;
  Out(i, 6) = COP_HP_SH;
  Out(i, 7) = COPX_HP_SH;
  }
  colnames(Out) = CharacterVector::create("T_s","E_HP", "Q_EL", "Q_HP","Q_TOT","X_SH","COP_HP_SH","COPX_HP_SH");
  return Out;
  }
    

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

AAHP <- rcpp_SN3031(T_e=df$T_e,L_SH=df2$P_hyd*200,
                 T_s=c(-20,2,7),
                 P20=c(0.54,0.79,1),
                 C20=c(0.62,0.72,1),
                 P_HP_13=7000, 
                 COP_HP_13=3.7, 
                 T_sh_set=35,
                 T_dhw_set=55, 
                 H_s=H_s, 
                 K=K, 
                 T_a=18,
                 dt=1)
*/