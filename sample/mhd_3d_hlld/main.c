#include <stdio.h>
#include <math.h>
#include "mhd_3d_hlld.h"

// T_MAX : number of time steps
#define T_MAX (1000)

// specific heat ratio
#define GAMMA (5.0/3.0)
#define GAMMA1 (GAMMA - 1.0)

static double sq(const double x) {
  return x*x;
}

static double norm2(const double x, const double y, const double z) {
  return sq(x) + sq(y) + sq(z);
}

static double velocity(const double rho, const double rhou) {
  return rhou/rho;
}

static double kinetic_energy(const double rho, const double u, const double v, const double w) {
  return 0.5*rho*norm2(u, v, w);
}

static double magnetic_energy(const double B_x, const double B_y, const double B_z) {
  return 0.5*norm2(B_x, B_y, B_z);
}

static double pressure(const double e, const double e_K) {
  return GAMMA1 * (e - e_K);
}

static double temperature(const double rho, const double p) {
  return p/rho;
}

static FILE * file_open(const char *prefix, const int time_step, const int rank) {
  FILE *fp;
  char fn[256];
  sprintf(fn, "%s%d_%d.dat", prefix, time_step, rank);
  fp = fopen(fn, "w");
  if (fp == NULL) {
    fprintf(stderr, "file open failure\n");
    Formura_Finalize();
    exit(1);
  } else {
    fprintf(stderr, "step = %d, open: %s\n", time_step, fn);
  }
  return fp;
}

static void writeData(const Formura_Navi *n) {
  FILE *fp = file_open("data/", n->time_step, n->my_rank);

  for (int iz = n->lower_z; iz < n->upper_z; iz++) {
    for (int iy = n->lower_y; iy < n->upper_y; iy++) {
      for (int ix = n->lower_x; ix < n->upper_x; ix++) {
        const double x = to_pos_x(ix,*n);
        const double y = to_pos_y(iy,*n);
        const double z = to_pos_z(iz,*n);
        const double rho = formura_data.rho[ix][iy][iz];
        const double u = velocity(rho, formura_data.rhou[ix][iy][iz]);
        const double v = velocity(rho, formura_data.rhov[ix][iy][iz]);
        const double w = velocity(rho, formura_data.rhow[ix][iy][iz]);
        const double B_x = formura_data.B_x[ix][iy][iz];
        const double B_y = formura_data.B_y[ix][iy][iz];
        const double B_z = formura_data.B_z[ix][iy][iz];
        const double e_K = kinetic_energy(rho, u, v, w);
        const double e_B = magnetic_energy(B_x, B_y, B_z);
        const double p = pressure(formura_data.e[ix][iy][iz], e_K + e_B);
        const double T = temperature(rho, p);
        const double psi = formura_data.psi[ix][iy][iz];
        fprintf(fp, "%.17e %.17e %.17e %.17e %.17e %.17e %.17e %.17e %.17e %.17e %.17e %.17e %.17e\n"
            , x     // column 0
            , y     // column 1
            , z     // column 2
            , rho   // column 3
            , p     // column 4
            , T     // column 5
            , u     // column 6
            , v     // column 7
            , w     // column 8
            , B_x   // column 9
            , B_y   // column 10
            , B_z   // column 11
            , psi   // column 12
            );
      }
    }
  }
  fclose(fp);
}

int main(int argc, char **argv) {
  Formura_Navi n;
  Formura_Init(&argc, &argv, &n);  // Initialize Formura stepper

  writeData(&n);
  while(n.time_step < T_MAX) {
    Formura_Forward(&n);    // run Formura stepper
  }
  writeData(&n);

  Formura_Finalize();   // Finalize Formura
  return 0;
}

