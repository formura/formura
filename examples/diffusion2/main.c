#include <stdio.h>
#include <math.h>
#include "diffusion2.h"

#define T_MAX 100

void writeData(Formura_Navi n) {
  char fn[256];
  sprintf(fn, "data/%d_%d.dat", n.time_step, n.my_rank);
  FILE *fp = fopen(fn, "w");
  for(int ix = n.lower_x; ix < n.upper_x; ix++) {
    double x = to_pos_x(ix,n);
    for(int iy = n.lower_y; iy < n.upper_y; iy++) {
      double y = to_pos_y(iy,n);
      fprintf(fp, "%f %f %f\n", x, y, formura_data.q[ix][iy]);
    }
    fprintf(fp, "\n");
  }
  fclose(fp);
  printf("Write: %s\n", fn);
}

int main(int argc, char **argv) {
  Formura_Navi n;
  Formura_Init(&argc, &argv, &n);

  writeData(n);
  while(n.time_step < T_MAX) {
    Formura_Forward(&n);
    writeData(n);
  }

  Formura_Finalize();
  return 0;
}
