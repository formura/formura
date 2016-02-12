#include <mpi.h>
#include <math.h>
#include "pearson.h"


float U[256][256];
float V[256][256];
float U_0;
float V_0;
;
;
float a[256][256];
float a_0[256][256];
float k;
float F;
float Du;
float Dv;
float dt;
float dx;
float dU_dt[256][256];
float dV_dt[256][256];
float U_next_0[256][256];
float V_next_0[256][256];
;
;



int
Formura_Init (struct Formura_Navigator *navi, MPI_Comm comm)
{
  int ix, iy;
  const int NX = 256, NY = 256;
  U_0 = 0.0;
  V_0 = 0.0;
  for (ix = 0; ix < NX + 0; ++ix) {
    for (iy = 0; iy < NY + 0; ++iy) {
      U[ix][iy] = U_0;
    }
  }

  for (ix = 0; ix < NX + 0; ++ix) {
    for (iy = 0; iy < NY + 0; ++iy) {
      V[ix][iy] = V_0;
    }
  }

  navi->time_step = 0;
  navi->lower_x = 0;
  navi->offset_x = 0;
  navi->upper_x = 256;
  navi->lower_y = 0;
  navi->offset_y = 0;
  navi->upper_y = 256;
}


int
Formura_Forward (struct Formura_Navigator *navi)
{
  int ix, iy;
  const int NX = 256, NY = 256;
  int timestep;
  for (timestep = 0; timestep < 20; ++timestep) {
    for (ix = 0; ix < NX + 0; ++ix) {
      for (iy = 0; iy < NY + 0; ++iy) {
        a[ix][iy] = U[ix][iy];
      }
    }

    for (ix = 0; ix < NX + 0; ++ix) {
      for (iy = 0; iy < NY + 0; ++iy) {
        a_0[ix][iy] = V[ix][iy];
      }
    }

    k = 5.0e-2;
    F = 1.5e-2;
    Du = 2.0e-5;
    Dv = 1.0e-5;
    dt = 1.0;
    dx = 1.0e-2;
    for (ix = 1; ix < NX + -1; ++ix) {
      for (iy = 1; iy < NY + -1; ++iy) {
        dU_dt[ix][iy] =
          (((-(a[ix][iy] * pow (a_0[ix][iy],
                  2.0))) + (F * (1.0 - a[ix][iy]))) + ((Du / pow (dx,
                2.0)) * (((a[ix - 1][iy] - a[ix][iy]) - (a[ix][iy] - a[ix +
                    1][iy])) + ((a[ix][iy - 1] - a[ix][iy]) - (a[ix][iy] -
                  a[ix][iy + 1])))));
      }
    }

    for (ix = 1; ix < NX + -1; ++ix) {
      for (iy = 1; iy < NY + -1; ++iy) {
        dV_dt[ix][iy] =
          (((a[ix][iy] * pow (a_0[ix][iy],
                2.0)) - ((F + k) * a_0[ix][iy])) + ((Dv / pow (dx,
                2.0)) * (((a_0[ix - 1][iy] - a_0[ix][iy]) - (a_0[ix][iy] -
                  a_0[ix + 1][iy])) + ((a_0[ix][iy - 1] - a_0[ix][iy]) -
                (a_0[ix][iy] - a_0[ix][iy + 1])))));
      }
    }

    for (ix = 1; ix < NX + -1; ++ix) {
      for (iy = 1; iy < NY + -1; ++iy) {
        U_next_0[ix][iy] = (a[ix][iy] + (dt * dU_dt[ix][iy]));
      }
    }

    for (ix = 1; ix < NX + -1; ++ix) {
      for (iy = 1; iy < NY + -1; ++iy) {
        V_next_0[ix][iy] = (a_0[ix][iy] + (dt * dV_dt[ix][iy]));
      }
    }

    for (ix = 1; ix < NX + -1; ++ix) {
      for (iy = 1; iy < NY + -1; ++iy) {
        U[ix][iy] = U_next_0[ix][iy];
      }
    }

    for (ix = 1; ix < NX + -1; ++ix) {
      for (iy = 1; iy < NY + -1; ++iy) {
        V[ix][iy] = V_next_0[ix][iy];
      }
    }

  }
  navi->time_step += 20;
}