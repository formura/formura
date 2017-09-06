
#pragma once
#ifdef __cplusplus
extern "C"
{
#endif
#include <mpi.h>


#define NX  200
#define NY  200
typedef double rsc_surface[102];
extern double U[206][206];
extern double V[206][206];
extern double Rsc0[102][102];
extern double Rsc1[102][102];
struct Facet_0_1_src__0_0_dest__0_2{double Ridge_0_1_St_U[200][2];
double Ridge_0_1_St_V[200][2];
double Ridge_0_1_Om_70_r__0_0_r__0_2[48][2];
double Ridge_0_1_Om_71_r__0_0_r__0_2[48][2];
};extern struct Facet_0_1_src__0_0_dest__0_2 Facet_0_1_src__0_0_dest__0_2_Send;extern struct Facet_0_1_src__0_0_dest__0_2 Facet_0_1_src__0_0_dest__0_2_Recv;extern MPI_Request req_Facet_0_1_src__0_0_dest__0_2;

struct Facet_0_1_src__0_0_dest__1_2{double Ridge_0_1_Om_70_r__0_0_r__1_2[2][2];
double Ridge_0_1_Om_71_r__0_0_r__1_2[2][2];
};extern struct Facet_0_1_src__0_0_dest__1_2 Facet_0_1_src__0_0_dest__1_2_Send;extern struct Facet_0_1_src__0_0_dest__1_2 Facet_0_1_src__0_0_dest__1_2_Recv;extern MPI_Request req_Facet_0_1_src__0_0_dest__1_2;

struct Facet_0_1_src__1_0_dest__1_2{double Ridge_0_1_Om_70_r__1_0_r__1_2[100][2];
double Ridge_0_1_Om_71_r__1_0_r__1_2[100][2];
};extern struct Facet_0_1_src__1_0_dest__1_2 Facet_0_1_src__1_0_dest__1_2_Send;extern struct Facet_0_1_src__1_0_dest__1_2 Facet_0_1_src__1_0_dest__1_2_Recv;extern MPI_Request req_Facet_0_1_src__1_0_dest__1_2;

struct Facet_0_1_src__1_0_dest__2_2{double Ridge_0_1_Om_70_r__1_0_r__2_2[2][2];
double Ridge_0_1_Om_71_r__1_0_r__2_2[2][2];
};extern struct Facet_0_1_src__1_0_dest__2_2 Facet_0_1_src__1_0_dest__2_2_Send;extern struct Facet_0_1_src__1_0_dest__2_2 Facet_0_1_src__1_0_dest__2_2_Recv;extern MPI_Request req_Facet_0_1_src__1_0_dest__2_2;

struct Facet_0_1_src__2_0_dest__2_2{double Ridge_0_1_Om_70_r__2_0_r__2_2[52][2];
double Ridge_0_1_Om_71_r__2_0_r__2_2[52][2];
};extern struct Facet_0_1_src__2_0_dest__2_2 Facet_0_1_src__2_0_dest__2_2_Send;extern struct Facet_0_1_src__2_0_dest__2_2 Facet_0_1_src__2_0_dest__2_2_Recv;extern MPI_Request req_Facet_0_1_src__2_0_dest__2_2;

struct Facet_1_0_src__0_0_dest__2_0{double Ridge_1_0_St_U[2][200];
double Ridge_1_0_St_V[2][200];
double Ridge_1_0_Om_70_r__0_0_r__2_0[2][48];
double Ridge_1_0_Om_71_r__0_0_r__2_0[2][48];
};extern struct Facet_1_0_src__0_0_dest__2_0 Facet_1_0_src__0_0_dest__2_0_Send;extern struct Facet_1_0_src__0_0_dest__2_0 Facet_1_0_src__0_0_dest__2_0_Recv;extern MPI_Request req_Facet_1_0_src__0_0_dest__2_0;

struct Facet_1_0_src__0_0_dest__2_1{double Ridge_1_0_Om_70_r__0_0_r__2_1[2][2];
double Ridge_1_0_Om_71_r__0_0_r__2_1[2][2];
};extern struct Facet_1_0_src__0_0_dest__2_1 Facet_1_0_src__0_0_dest__2_1_Send;extern struct Facet_1_0_src__0_0_dest__2_1 Facet_1_0_src__0_0_dest__2_1_Recv;extern MPI_Request req_Facet_1_0_src__0_0_dest__2_1;

struct Facet_1_0_src__0_1_dest__2_1{double Ridge_1_0_Om_70_r__0_1_r__2_1[2][100];
double Ridge_1_0_Om_71_r__0_1_r__2_1[2][100];
};extern struct Facet_1_0_src__0_1_dest__2_1 Facet_1_0_src__0_1_dest__2_1_Send;extern struct Facet_1_0_src__0_1_dest__2_1 Facet_1_0_src__0_1_dest__2_1_Recv;extern MPI_Request req_Facet_1_0_src__0_1_dest__2_1;

struct Facet_1_0_src__0_1_dest__2_2{double Ridge_1_0_Om_70_r__0_1_r__2_2[2][2];
double Ridge_1_0_Om_71_r__0_1_r__2_2[2][2];
};extern struct Facet_1_0_src__0_1_dest__2_2 Facet_1_0_src__0_1_dest__2_2_Send;extern struct Facet_1_0_src__0_1_dest__2_2 Facet_1_0_src__0_1_dest__2_2_Recv;extern MPI_Request req_Facet_1_0_src__0_1_dest__2_2;

struct Facet_1_0_src__0_2_dest__2_2{double Ridge_1_0_Om_70_r__0_2_r__2_2[2][52];
double Ridge_1_0_Om_71_r__0_2_r__2_2[2][52];
};extern struct Facet_1_0_src__0_2_dest__2_2 Facet_1_0_src__0_2_dest__2_2_Send;extern struct Facet_1_0_src__0_2_dest__2_2 Facet_1_0_src__0_2_dest__2_2_Recv;extern MPI_Request req_Facet_1_0_src__0_2_dest__2_2;

struct Facet_1_1_src__0_0_dest__2_2{double Ridge_1_1_St_U[2][2];
double Ridge_1_1_St_V[2][2];
double Ridge_1_1_Om_70_r__0_0_r__2_2[2][2];
double Ridge_1_1_Om_71_r__0_0_r__2_2[2][2];
};extern struct Facet_1_1_src__0_0_dest__2_2 Facet_1_1_src__0_0_dest__2_2_Send;extern struct Facet_1_1_src__0_0_dest__2_2 Facet_1_1_src__0_0_dest__2_2_Recv;extern MPI_Request req_Facet_1_1_src__0_0_dest__2_2;

extern double Ridge_0_0_Om_70_r__0_0_r__1_0[2][48];
extern double Ridge_0_0_Om_70_r__0_0_r__0_1[48][2];
extern double Ridge_0_0_Om_70_r__0_0_r__1_1[2][2];
extern double Ridge_0_0_Om_70_r__1_0_r__2_0[2][48];
extern double Ridge_0_0_Om_70_r__1_0_r__1_1[100][2];
extern double Ridge_0_0_Om_70_r__1_0_r__2_1[2][2];
extern double Ridge_0_0_Om_70_r__2_0_r__2_1[52][2];
extern double Ridge_0_0_Om_70_r__0_1_r__1_1[2][100];
extern double Ridge_0_0_Om_70_r__0_1_r__0_2[48][2];
extern double Ridge_0_0_Om_70_r__0_1_r__1_2[2][2];
extern double Ridge_0_0_Om_70_r__1_1_r__2_1[2][100];
extern double Ridge_0_0_Om_70_r__1_1_r__1_2[100][2];
extern double Ridge_0_0_Om_70_r__1_1_r__2_2[2][2];
extern double Ridge_0_0_Om_70_r__2_1_r__2_2[52][2];
extern double Ridge_0_0_Om_70_r__0_2_r__1_2[2][52];
extern double Ridge_0_0_Om_70_r__1_2_r__2_2[2][52];
extern double Ridge_0_0_Om_71_r__0_0_r__1_0[2][48];
extern double Ridge_0_0_Om_71_r__0_0_r__0_1[48][2];
extern double Ridge_0_0_Om_71_r__0_0_r__1_1[2][2];
extern double Ridge_0_0_Om_71_r__1_0_r__2_0[2][48];
extern double Ridge_0_0_Om_71_r__1_0_r__1_1[100][2];
extern double Ridge_0_0_Om_71_r__1_0_r__2_1[2][2];
extern double Ridge_0_0_Om_71_r__2_0_r__2_1[52][2];
extern double Ridge_0_0_Om_71_r__0_1_r__1_1[2][100];
extern double Ridge_0_0_Om_71_r__0_1_r__0_2[48][2];
extern double Ridge_0_0_Om_71_r__0_1_r__1_2[2][2];
extern double Ridge_0_0_Om_71_r__1_1_r__2_1[2][100];
extern double Ridge_0_0_Om_71_r__1_1_r__1_2[100][2];
extern double Ridge_0_0_Om_71_r__1_1_r__2_2[2][2];
extern double Ridge_0_0_Om_71_r__2_1_r__2_2[52][2];
extern double Ridge_0_0_Om_71_r__0_2_r__1_2[2][52];
extern double Ridge_0_0_Om_71_r__1_2_r__2_2[2][52];
typedef double rsc_surface[102];
struct Facet_m1_m1_src__2_2_dest__0_0{double Ridge_m1_m1_St_U[2][2];
double Ridge_m1_m1_St_V[2][2];
double Ridge_m1_m1_Om_70_r__2_2_r__0_0[2][2];
double Ridge_m1_m1_Om_71_r__2_2_r__0_0[2][2];
};extern struct Facet_m1_m1_src__2_2_dest__0_0 Facet_m1_m1_src__2_2_dest__0_0_Send;extern struct Facet_m1_m1_src__2_2_dest__0_0 Facet_m1_m1_src__2_2_dest__0_0_Recv;extern MPI_Request req_Facet_m1_m1_src__2_2_dest__0_0;

struct Facet_m1_0_src__2_0_dest__0_0{double Ridge_m1_0_Om_70_r__2_0_r__0_0[2][52];
double Ridge_m1_0_Om_71_r__2_0_r__0_0[2][52];
};extern struct Facet_m1_0_src__2_0_dest__0_0 Facet_m1_0_src__2_0_dest__0_0_Send;extern struct Facet_m1_0_src__2_0_dest__0_0 Facet_m1_0_src__2_0_dest__0_0_Recv;extern MPI_Request req_Facet_m1_0_src__2_0_dest__0_0;

struct Facet_m1_0_src__2_1_dest__0_0{double Ridge_m1_0_Om_70_r__2_1_r__0_0[2][2];
double Ridge_m1_0_Om_71_r__2_1_r__0_0[2][2];
};extern struct Facet_m1_0_src__2_1_dest__0_0 Facet_m1_0_src__2_1_dest__0_0_Send;extern struct Facet_m1_0_src__2_1_dest__0_0 Facet_m1_0_src__2_1_dest__0_0_Recv;extern MPI_Request req_Facet_m1_0_src__2_1_dest__0_0;

struct Facet_m1_0_src__2_1_dest__0_1{double Ridge_m1_0_Om_70_r__2_1_r__0_1[2][100];
double Ridge_m1_0_Om_71_r__2_1_r__0_1[2][100];
};extern struct Facet_m1_0_src__2_1_dest__0_1 Facet_m1_0_src__2_1_dest__0_1_Send;extern struct Facet_m1_0_src__2_1_dest__0_1 Facet_m1_0_src__2_1_dest__0_1_Recv;extern MPI_Request req_Facet_m1_0_src__2_1_dest__0_1;

struct Facet_m1_0_src__2_2_dest__0_1{double Ridge_m1_0_Om_70_r__2_2_r__0_1[2][2];
double Ridge_m1_0_Om_71_r__2_2_r__0_1[2][2];
};extern struct Facet_m1_0_src__2_2_dest__0_1 Facet_m1_0_src__2_2_dest__0_1_Send;extern struct Facet_m1_0_src__2_2_dest__0_1 Facet_m1_0_src__2_2_dest__0_1_Recv;extern MPI_Request req_Facet_m1_0_src__2_2_dest__0_1;

struct Facet_m1_0_src__2_2_dest__0_2{double Ridge_m1_0_St_U[2][200];
double Ridge_m1_0_St_V[2][200];
double Ridge_m1_0_Om_70_r__2_2_r__0_2[2][48];
double Ridge_m1_0_Om_71_r__2_2_r__0_2[2][48];
};extern struct Facet_m1_0_src__2_2_dest__0_2 Facet_m1_0_src__2_2_dest__0_2_Send;extern struct Facet_m1_0_src__2_2_dest__0_2 Facet_m1_0_src__2_2_dest__0_2_Recv;extern MPI_Request req_Facet_m1_0_src__2_2_dest__0_2;

struct Facet_0_m1_src__0_2_dest__0_0{double Ridge_0_m1_Om_70_r__0_2_r__0_0[52][2];
double Ridge_0_m1_Om_71_r__0_2_r__0_0[52][2];
};extern struct Facet_0_m1_src__0_2_dest__0_0 Facet_0_m1_src__0_2_dest__0_0_Send;extern struct Facet_0_m1_src__0_2_dest__0_0 Facet_0_m1_src__0_2_dest__0_0_Recv;extern MPI_Request req_Facet_0_m1_src__0_2_dest__0_0;

struct Facet_0_m1_src__1_2_dest__0_0{double Ridge_0_m1_Om_70_r__1_2_r__0_0[2][2];
double Ridge_0_m1_Om_71_r__1_2_r__0_0[2][2];
};extern struct Facet_0_m1_src__1_2_dest__0_0 Facet_0_m1_src__1_2_dest__0_0_Send;extern struct Facet_0_m1_src__1_2_dest__0_0 Facet_0_m1_src__1_2_dest__0_0_Recv;extern MPI_Request req_Facet_0_m1_src__1_2_dest__0_0;

struct Facet_0_m1_src__1_2_dest__1_0{double Ridge_0_m1_Om_70_r__1_2_r__1_0[100][2];
double Ridge_0_m1_Om_71_r__1_2_r__1_0[100][2];
};extern struct Facet_0_m1_src__1_2_dest__1_0 Facet_0_m1_src__1_2_dest__1_0_Send;extern struct Facet_0_m1_src__1_2_dest__1_0 Facet_0_m1_src__1_2_dest__1_0_Recv;extern MPI_Request req_Facet_0_m1_src__1_2_dest__1_0;

struct Facet_0_m1_src__2_2_dest__1_0{double Ridge_0_m1_Om_70_r__2_2_r__1_0[2][2];
double Ridge_0_m1_Om_71_r__2_2_r__1_0[2][2];
};extern struct Facet_0_m1_src__2_2_dest__1_0 Facet_0_m1_src__2_2_dest__1_0_Send;extern struct Facet_0_m1_src__2_2_dest__1_0 Facet_0_m1_src__2_2_dest__1_0_Recv;extern MPI_Request req_Facet_0_m1_src__2_2_dest__1_0;

struct Facet_0_m1_src__2_2_dest__2_0{double Ridge_0_m1_St_U[200][2];
double Ridge_0_m1_St_V[200][2];
double Ridge_0_m1_Om_70_r__2_2_r__2_0[48][2];
double Ridge_0_m1_Om_71_r__2_2_r__2_0[48][2];
};extern struct Facet_0_m1_src__2_2_dest__2_0 Facet_0_m1_src__2_2_dest__2_0_Send;extern struct Facet_0_m1_src__2_2_dest__2_0 Facet_0_m1_src__2_2_dest__2_0_Recv;extern MPI_Request req_Facet_0_m1_src__2_2_dest__2_0;

extern double Ridge_0_0_Om_70_r__1_0_r__0_0[2][52];
extern double Ridge_0_0_Om_70_r__2_0_r__1_0[2][52];
extern double Ridge_0_0_Om_70_r__0_1_r__0_0[52][2];
extern double Ridge_0_0_Om_70_r__1_1_r__0_0[2][2];
extern double Ridge_0_0_Om_70_r__1_1_r__1_0[100][2];
extern double Ridge_0_0_Om_70_r__1_1_r__0_1[2][100];
extern double Ridge_0_0_Om_70_r__2_1_r__1_0[2][2];
extern double Ridge_0_0_Om_70_r__2_1_r__2_0[48][2];
extern double Ridge_0_0_Om_70_r__2_1_r__1_1[2][100];
extern double Ridge_0_0_Om_70_r__0_2_r__0_1[52][2];
extern double Ridge_0_0_Om_70_r__1_2_r__0_1[2][2];
extern double Ridge_0_0_Om_70_r__1_2_r__1_1[100][2];
extern double Ridge_0_0_Om_70_r__1_2_r__0_2[2][48];
extern double Ridge_0_0_Om_70_r__2_2_r__1_1[2][2];
extern double Ridge_0_0_Om_70_r__2_2_r__2_1[48][2];
extern double Ridge_0_0_Om_70_r__2_2_r__1_2[2][48];
extern double Ridge_0_0_Om_71_r__1_0_r__0_0[2][52];
extern double Ridge_0_0_Om_71_r__2_0_r__1_0[2][52];
extern double Ridge_0_0_Om_71_r__0_1_r__0_0[52][2];
extern double Ridge_0_0_Om_71_r__1_1_r__0_0[2][2];
extern double Ridge_0_0_Om_71_r__1_1_r__1_0[100][2];
extern double Ridge_0_0_Om_71_r__1_1_r__0_1[2][100];
extern double Ridge_0_0_Om_71_r__2_1_r__1_0[2][2];
extern double Ridge_0_0_Om_71_r__2_1_r__2_0[48][2];
extern double Ridge_0_0_Om_71_r__2_1_r__1_1[2][100];
extern double Ridge_0_0_Om_71_r__0_2_r__0_1[52][2];
extern double Ridge_0_0_Om_71_r__1_2_r__0_1[2][2];
extern double Ridge_0_0_Om_71_r__1_2_r__1_1[100][2];
extern double Ridge_0_0_Om_71_r__1_2_r__0_2[2][48];
extern double Ridge_0_0_Om_71_r__2_2_r__1_1[2][2];
extern double Ridge_0_0_Om_71_r__2_2_r__2_1[48][2];
extern double Ridge_0_0_Om_71_r__2_2_r__1_2[2][48];

struct Formura_Navigator {
int time_step;
int lower_x;
int upper_x;
int offset_x;
int lower_y;
int upper_y;
int offset_y;
MPI_Comm mpi_comm;
int mpi_my_rank;
int mpi_rank__m1_m1;
int mpi_rank__m1_0;
int mpi_rank__0_m1;
int mpi_rank__0_0;
int mpi_rank__0_1;
int mpi_rank__1_0;
int mpi_rank__1_1;
};
extern struct Formura_Navigator navi;

int Formura_Init (struct Formura_Navigator *navi, MPI_Comm comm);

void Formura_internal();
void Formura_internal_1();
void Formura_internal_2();
void Formura_internal_3();
void Formura_internal_4();
void Formura_internal_5();
void Formura_internal_6();
void Formura_internal_7();
void Formura_internal_8();
void Formura_internal_9();
void Formura_internal_10();
void Formura_internal_11();
void Formura_internal_12();
void Formura_internal_13();
void Formura_internal_14();
void Formura_internal_15();
void Formura_internal_16();
void Formura_internal_17();
void Formura_subroutine_0(int argx0,int argx1,int argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,int argx6,rsc_surface * __restrict  argx7,int argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,int argx12,rsc_surface * __restrict  argx13,int argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,int argx18,rsc_surface * __restrict  argx19,int argx20,int argx21,int argx22,int argx23,int argx24,int argx25);
void Formura_subroutine_1(int argx0,int argx1,int argx2,int argx3,int argx4,int argx5,int argx6,int argx7,int argx8,int argx9,int argx10,int argx11,int argx12,int argx13,int argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,int argx18);
void Formura_subroutine_2(int argx0,int argx1,int argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,int argx6,rsc_surface * __restrict  argx7,int argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,int argx12,rsc_surface * __restrict  argx13,int argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,int argx18,rsc_surface * __restrict  argx19,int argx20,int argx21,int argx22,int argx23,int argx24,int argx25);
void Formura_subroutine_3(int argx0,int argx1,int argx2,int argx3,int argx4,int argx5,int argx6,int argx7,int argx8,int argx9,int argx10,int argx11,int argx12,int argx13,int argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,int argx18);

int Formura_Forward (struct Formura_Navigator *navi);
#ifdef __cplusplus
}
#endif
