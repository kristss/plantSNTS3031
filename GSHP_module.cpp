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
  NumericVector rcpp_SN3031(NumericVector T_e, NumericVector L_DHW, NumericVector L_SH,NumericVector Month,
                            NumericVector T_s, NumericVector P35, NumericVector C35,
                            NumericVector P55, NumericVector C55,
                            double P_HP_12, double COP_HP_12, 
                            double T_sh_set, double T_dhw_set, 
                            double H_s, double K, double T_a,
                            double dt,
                            double Tavg_s, double Tamp_s, double phi) {

  int n = T_e.size();
  NumericMatrix Out(n,7);
  double P_HP_SH;
  double COP_HP_SH;
  double P_HP_DHW;
  double COP_HP_DHW;
  double COPX_HP_DHW;
  double COPX_HP_SH;
  double E_HP_DHW;
  double E_HP_SH;
  
  for (int i=0; i < n; i++) {
  // AWHP
  double T_sh_s0 = T_sh_set;
  double T_dhw_s0 = T_dhw_set;
  double Q_TANK_DHW = L_DHW[i] + H_s * ((T_dhw_set - K * T_dhw_s0)/(1-K)-T_a);
  double Q_TANK_SH = L_SH[i] + H_s * ((T_sh_set - K * T_sh_s0)/(1-K)-T_a);
  double Ts = Tavg_s-Tamp_s*cos(2*PI/12*(Month[i]-phi-1)); // Source temperature
  if (Ts < T_s[0]) {P_HP_SH = P_HP_12 * 0;}
  else if (Ts< T_s[1]) {P_HP_SH = P_HP_12 * (P35[0] + (Ts-T_s[0])*((P35[1] - P35[0])/(T_s[1]-T_s[0])));}
  else if (Ts< T_s[2]) {P_HP_SH = P_HP_12 * (P35[1] + (Ts-T_s[1])*((P35[2] - P35[1])/(T_s[2]-T_s[1])));} 
  else {P_HP_SH = P_HP_12 * P35[2];}
  if (Ts < T_s[0]) {COP_HP_SH = COP_HP_12 * C35[0];}
  else if (Ts< T_s[1]) {COP_HP_SH = COP_HP_12 * (C35[0] + (Ts-T_s[0])*((C35[1] - C35[0])/(T_s[1]-T_s[0])));}
  else if (Ts< T_s[2]) {COP_HP_SH = COP_HP_12 * (C35[1] + (Ts-T_s[1])*((C35[2] - C35[1])/(T_s[2]-T_s[1])));} 
  else {COP_HP_SH = COP_HP_12 * C35[2];}
  if (Ts < T_s[0]) {P_HP_DHW = P_HP_12 * 0;}
  else if (Ts< T_s[1]) {P_HP_DHW = P_HP_12 * (P55[0] + (Ts-T_s[0])*((P55[1] - P55[0])/(T_s[1]-T_s[0])));}
  else if (Ts< T_s[2]) {P_HP_DHW = P_HP_12 * (P55[1] + (Ts-T_s[1])*((P55[2] - P55[1])/(T_s[2]-T_s[1])));} 
  else {P_HP_DHW = P_HP_12 * P55[2];}
  if (Ts < T_s[0]) {COP_HP_DHW = COP_HP_12 * C55[0];}
  else if (Ts< T_s[1]) {COP_HP_DHW = COP_HP_12 * (C55[0] + (Ts-T_s[0])*((C55[1] - C55[0])/(T_s[1]-T_s[0])));}
  else if (Ts< T_s[2]) {COP_HP_DHW = COP_HP_12 * (C55[1] + (Ts-T_s[1])*((C55[2] - C55[1])/(T_s[2]-T_s[1])));} 
  else {COP_HP_DHW = COP_HP_12 * C55[2];}
  // simultanous DHW and space heating
  double t_DHW = min(NumericVector::create(dt,(Q_TANK_DHW/P_HP_DHW))); // hours used for DHW
  double t_SH = dt-t_DHW; // hours available for space heating
  double Q_HP_DHW = min(NumericVector::create(Q_TANK_DHW,(t_DHW*P_HP_DHW)));
  double Q_HP_SH = min(NumericVector::create(Q_TANK_SH,(t_SH*P_HP_SH)));
  // simplified part load, space heatin mode
  double X_DHW = Q_HP_DHW / P_HP_DHW;
  // if invert controlled HP
  if (X_DHW<=0.25) {COPX_HP_DHW = COP_HP_DHW * (4*X_DHW/(0.9*X_DHW+0.1));} else{COPX_HP_DHW = COP_HP_DHW * 1;};
  // if on/off controlled HP
  //COPX_HP_DHW = COP_HP_DHW * X_DHW/(0.9*X_DHW+0.1)
  // simplified part load, dhw mode
  double X_SH = Q_HP_SH / P_HP_SH;
  // if invert controlled HP
  if (X_SH<=0.25) {COPX_HP_SH = COP_HP_SH * (4*X_SH/(0.9*X_SH+0.1));} else {COPX_HP_SH = COP_HP_SH * 1;};
  // if on/off controlled HP
  // COPX_HP_SH = COP_HP_SH * X_SH/(0.9*X_SH+0.1)
  // Added:
  if(!COPX_HP_DHW) {E_HP_DHW = 0;} else {E_HP_DHW = Q_HP_DHW/COPX_HP_DHW;};
  if(!COPX_HP_SH) {E_HP_SH = 0;} else {E_HP_SH = Q_HP_SH/COPX_HP_SH;};
  double E_HP = E_HP_DHW + E_HP_SH;
  double Q_TOT = Q_TANK_DHW + Q_TANK_SH;
  double Q_HP_TOT = Q_HP_DHW + Q_HP_SH;
  double Q_EL_DHW = Q_TANK_DHW - Q_HP_DHW;
  double Q_EL_SH = Q_TANK_SH - Q_HP_SH;
  double Q_EL_TOT = Q_EL_DHW + Q_EL_SH;
  Out(i, 0) = Ts;
  Out(i, 1) = E_HP;
  Out(i, 2) = Q_EL_TOT;
  Out(i, 3) = Q_HP_TOT;
  Out(i, 4) = Q_HP_DHW;
  Out(i, 5) = COP_HP_DHW;
  Out(i, 6) = COP_HP_SH;
  }
  colnames(Out) = CharacterVector::create("T_s","E_HP", "Q_EL", "Q_HP","Q_HP_DHW","COP_HP_DHW","COP_HP_SH");
  return Out;
  }
    

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

GSHP <- rcpp_SN3031(T_e=df$T_e,L_DHW=df$P_dhw*200,L_SH=df2$P_hyd*200,Month=df$n_month,
                 T_s=c(-5,0,5),
                 P35=c(0.92,1.00,1.15),
                 C35=c(0.78,1.00,1.10),
                 P55=c(0.82,0.91,1.00),
                 C55=c(0.43,0.58,0.73),
                 P_HP_12=8000, 
                 COP_HP_12=5.0, 
                 T_sh_set=35,
                 T_dhw_set=55, 
                 H_s=H_s, 
                 K=K, 
                 T_a=20,
                 dt=1,
                 Tavg_s=0,
                 Tamp_s=2,
                 phi=0)
*/