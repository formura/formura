#include <stdio.h>
#include <math.h>
#include <time.h>
#include "pearson2.h"

#define T_MAX 5000
#define T_MON 100

double frand() {
  return rand()/((double)RAND_MAX);
}

void perturbate(Formura_Navi n) {
  for(int ix = n.lower_x; ix < n.upper_x; ix++) {
    for(int iy = n.lower_y; iy < n.upper_y; iy++) {
      if (formura_data.u[ix][iy] != 1.0) {
        formura_data.u[ix][iy] += 0.01*frand();
        formura_data.v[ix][iy] += 0.01*frand();
      }
    }
  }
}

void writeData(Formura_Navi n) {
  char fn[256];
  sprintf(fn, "data/%d_%d.dat", n.time_step, n.my_rank);
  FILE *fp = fopen(fn, "w");
  for(int ix = n.lower_x; ix < n.upper_x; ix++) {
    double x = to_pos_x(ix,n);
    for(int iy = n.lower_y; iy < n.upper_y; iy++) {
      double y = to_pos_y(iy,n);
      fprintf(fp, "%f %f %.16f %.16f\n", x, y, formura_data.u[ix][iy],formura_data.v[ix][iy]);
    }
  }
  fclose(fp);
  printf("Write: %s\n", fn);
}

int main(int argc, char **argv) {
  srand(time(NULL));
  Formura_Navi n;
  Formura_Init(&argc, &argv, &n);

  perturbate(n);

  writeData(n);
  while(n.time_step < T_MAX) {
    Formura_Forward(&n);
    if(n.time_step % T_MON == 0) {
      writeData(n);
    }
  }

  Formura_Finalize();
  return 0;
}
