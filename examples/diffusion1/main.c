#include <stdio.h>
#include <math.h>
#include "diffusion1.h"

#define T_MAX 100

double gauss(double x, double x0) {
  return exp(-pow((x-x0)/(5*dx),2.0));
}

// 初期条件の設定
void setup(Formura_Navi n) {
  double x0 = n.length_x/2.0;
  for(int ix = n.lower_x; ix < n.upper_x; ix++) {
    double x = to_pos_x(ix,n);
    formura_data.q[ix] = gauss(x,x0);
  }
}

void writeData(Formura_Navi n) {
  char fn[256];
  sprintf(fn, "data/%d_%d.dat", n.time_step, n.my_rank);
  FILE *fp = fopen(fn, "w");
  for(int ix = n.lower_x; ix < n.upper_x; ix++) {
    double x = to_pos_x(ix,n);
    fprintf(fp, "%f %f\n", x, formura_data.q[ix]);
  }
  fclose(fp);
  printf("Write: %s\n", fn);
}

int main(int argc, char **argv) {
  Formura_Navi n;
  Formura_Init(&argc, &argv, &n);
  setup(n);

  writeData(n);
  while(n.time_step < T_MAX) {
    Formura_Forward(&n);
    writeData(n);
  }

  Formura_Finalize();
  return 0;
}
