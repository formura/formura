#include <stdio.h>
#include <math.h>
#include <mpi.h>
#include "diffusion3.h"

#define T_MAX 100

double gauss(double x, double y, double z, double x0, double y0, double z0) {
  return exp(-pow((x-x0)/(5*dx),2.0)-pow((y-y0)/(5*dy),2.0)-pow((z-z0)/(5*dz),2.0));
}

// 初期条件の設定
void setup(Formura_Navi n) {
  double x0 = n.length_x/2.0;
  double y0 = n.length_y/2.0;
  double z0 = n.length_z/2.0;
  for(int ix = n.lower_x; ix < n.upper_x; ix++) {
    double x = to_pos_x(ix,n);
    for(int iy = n.lower_y; iy < n.upper_y; iy++) {
      double y = to_pos_y(iy,n);
      for(int iz = n.lower_z; iz < n.upper_z; iz++) {
        double z = to_pos_z(iz,n);
        q[ix][iy][iz] = gauss(x,y,z,x0,y0,z0);
      }
    }
  }
}

void writeData(Formura_Navi n) {
  int t = n.time_step;
  double x0 = n.length_x/2.0;
  double y0 = n.length_y/2.0;
  double z0 = n.length_z/2.0;
  char fn[256];
  char fn_xy[256];
  char fn_yz[256];
  char fn_zx[256];
  sprintf(fn, "data/%d_%d.dat", t, n.my_rank);
  sprintf(fn_xy, "data/%d_%d-xy.dat", t, n.my_rank);
  sprintf(fn_yz, "data/%d_%d-yz.dat", t, n.my_rank);
  sprintf(fn_zx, "data/%d_%d-zx.dat", t, n.my_rank);
  FILE *fp = fopen(fn, "w");
  FILE *fp_xy = fopen(fn_xy, "w");
  FILE *fp_yz = fopen(fn_yz, "w");
  FILE *fp_zx = fopen(fn_zx, "w");
  for(int ix = n.lower_x; ix < n.upper_x; ix++) {
    double x = to_pos_x(ix,n);
    for(int iy = n.lower_y; iy < n.upper_y; iy++) {
      double y = to_pos_y(iy,n);
      for(int iz = n.lower_z; iz < n.upper_z; ++iz) {
        double z = to_pos_z(iz,n);
        fprintf(fp, "%f %f %f %f\n", x, y, z, q[ix][iy][iz]);
        if (z == z0) {
          fprintf(fp_xy, "%f %f %f %f\n", x, y, z, q[ix][iy][iz]);
        }
        if (x == x0) {
          fprintf(fp_yz, "%f %f %f %f\n", x, y, z, q[ix][iy][iz]);
        }
        if (y == y0) {
          fprintf(fp_zx, "%f %f %f %f\n", x, y, z, q[ix][iy][iz]);
        }
      }
      fprintf(fp, "\n");
      fprintf(fp_yz, "\n");
    }
    fprintf(fp, "\n");
    fprintf(fp_xy, "\n");
    fprintf(fp_zx, "\n");
  }
  fclose(fp);
  fclose(fp_xy);
  fclose(fp_yz);
  fclose(fp_zx);
  printf("Write: %s\n", fn);
}

int main(int argc, char **argv) {
  MPI_Init(&argc,&argv);

  Formura_Navi n;
  Formura_Init(&n, MPI_COMM_WORLD);
  setup(n);

  writeData(n);
  while(n.time_step < T_MAX) {
    Formura_Forward(&n);
    writeData(n);
  }

  MPI_Finalize();
  return 0;
}
