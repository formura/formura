
#pragma once
#ifdef __cplusplus
extern "C"
{
#endif
#include <mpi.h>


#define NX  300
typedef double rsc_surface;
extern double u[306];
extern double Rsc0[160];
extern double Rsc1[160];
struct Facet_1_src__0_dest__1{double Ridge_1_St_u[2];
double Ridge_1_Om_23_r__0_r__1[2];
double Ridge_1_Om_24_r__0_r__1[2];
double Ridge_1_Om_25_r__0_r__1[2];
};extern struct Facet_1_src__0_dest__1 Facet_1_src__0_dest__1_Send;extern struct Facet_1_src__0_dest__1 Facet_1_src__0_dest__1_Recv;extern MPI_Request req_Facet_1_src__0_dest__1;

extern double Ridge_0_Om_23_r__0_r__1[2];
extern double Ridge_0_Om_24_r__0_r__1[2];
extern double Ridge_0_Om_25_r__0_r__1[2];
typedef double rsc_surface;
struct Facet_m1_src__1_dest__0{double Ridge_m1_St_u[2];
double Ridge_m1_Om_23_r__1_r__0[2];
double Ridge_m1_Om_24_r__1_r__0[2];
double Ridge_m1_Om_25_r__1_r__0[2];
};extern struct Facet_m1_src__1_dest__0 Facet_m1_src__1_dest__0_Send;extern struct Facet_m1_src__1_dest__0 Facet_m1_src__1_dest__0_Recv;extern MPI_Request req_Facet_m1_src__1_dest__0;

extern double Ridge_0_Om_23_r__1_r__0[2];
extern double Ridge_0_Om_24_r__1_r__0[2];
extern double Ridge_0_Om_25_r__1_r__0[2];

struct Formura_Navigator {
int time_step;
int lower_x;
int upper_x;
int offset_x;
MPI_Comm mpi_comm;
int mpi_my_rank;
int mpi_rank__m1;
int mpi_rank__0;
int mpi_rank__1;
};
extern struct Formura_Navigator navi;

int Formura_Init (struct Formura_Navigator *navi, MPI_Comm comm);

void Formura_internal();
void Formura_internal_3();
void Formura_internal_4();
void Formura_internal_5();
void Formura_subroutine_0(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9);
void Formura_subroutine_1(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,int argx8,int argx9);
void Formura_subroutine_2(int argx0,int argx1,int argx2,int argx3,int argx4,rsc_surface * __restrict  argx5,int argx6);

int Formura_Forward (struct Formura_Navigator *navi);
#ifdef __cplusplus
}
#endif
