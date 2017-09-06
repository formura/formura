
#include <mpi.h>
#include <math.h>
#include <stdbool.h>

#include "sample.h"



double U[206][206];
double V[206][206];
double Rsc0[102][102];
double Rsc1[102][102];
struct Facet_0_1_src__0_0_dest__0_2 Facet_0_1_src__0_0_dest__0_2_Send;struct Facet_0_1_src__0_0_dest__0_2 Facet_0_1_src__0_0_dest__0_2_Recv;MPI_Request req_Facet_0_1_src__0_0_dest__0_2;

struct Facet_0_1_src__0_0_dest__1_2 Facet_0_1_src__0_0_dest__1_2_Send;struct Facet_0_1_src__0_0_dest__1_2 Facet_0_1_src__0_0_dest__1_2_Recv;MPI_Request req_Facet_0_1_src__0_0_dest__1_2;

struct Facet_0_1_src__1_0_dest__1_2 Facet_0_1_src__1_0_dest__1_2_Send;struct Facet_0_1_src__1_0_dest__1_2 Facet_0_1_src__1_0_dest__1_2_Recv;MPI_Request req_Facet_0_1_src__1_0_dest__1_2;

struct Facet_0_1_src__1_0_dest__2_2 Facet_0_1_src__1_0_dest__2_2_Send;struct Facet_0_1_src__1_0_dest__2_2 Facet_0_1_src__1_0_dest__2_2_Recv;MPI_Request req_Facet_0_1_src__1_0_dest__2_2;

struct Facet_0_1_src__2_0_dest__2_2 Facet_0_1_src__2_0_dest__2_2_Send;struct Facet_0_1_src__2_0_dest__2_2 Facet_0_1_src__2_0_dest__2_2_Recv;MPI_Request req_Facet_0_1_src__2_0_dest__2_2;

struct Facet_1_0_src__0_0_dest__2_0 Facet_1_0_src__0_0_dest__2_0_Send;struct Facet_1_0_src__0_0_dest__2_0 Facet_1_0_src__0_0_dest__2_0_Recv;MPI_Request req_Facet_1_0_src__0_0_dest__2_0;

struct Facet_1_0_src__0_0_dest__2_1 Facet_1_0_src__0_0_dest__2_1_Send;struct Facet_1_0_src__0_0_dest__2_1 Facet_1_0_src__0_0_dest__2_1_Recv;MPI_Request req_Facet_1_0_src__0_0_dest__2_1;

struct Facet_1_0_src__0_1_dest__2_1 Facet_1_0_src__0_1_dest__2_1_Send;struct Facet_1_0_src__0_1_dest__2_1 Facet_1_0_src__0_1_dest__2_1_Recv;MPI_Request req_Facet_1_0_src__0_1_dest__2_1;

struct Facet_1_0_src__0_1_dest__2_2 Facet_1_0_src__0_1_dest__2_2_Send;struct Facet_1_0_src__0_1_dest__2_2 Facet_1_0_src__0_1_dest__2_2_Recv;MPI_Request req_Facet_1_0_src__0_1_dest__2_2;

struct Facet_1_0_src__0_2_dest__2_2 Facet_1_0_src__0_2_dest__2_2_Send;struct Facet_1_0_src__0_2_dest__2_2 Facet_1_0_src__0_2_dest__2_2_Recv;MPI_Request req_Facet_1_0_src__0_2_dest__2_2;

struct Facet_1_1_src__0_0_dest__2_2 Facet_1_1_src__0_0_dest__2_2_Send;struct Facet_1_1_src__0_0_dest__2_2 Facet_1_1_src__0_0_dest__2_2_Recv;MPI_Request req_Facet_1_1_src__0_0_dest__2_2;

double Ridge_0_0_Om_70_r__0_0_r__1_0[2][48];
double Ridge_0_0_Om_70_r__0_0_r__0_1[48][2];
double Ridge_0_0_Om_70_r__0_0_r__1_1[2][2];
double Ridge_0_0_Om_70_r__1_0_r__2_0[2][48];
double Ridge_0_0_Om_70_r__1_0_r__1_1[100][2];
double Ridge_0_0_Om_70_r__1_0_r__2_1[2][2];
double Ridge_0_0_Om_70_r__2_0_r__2_1[52][2];
double Ridge_0_0_Om_70_r__0_1_r__1_1[2][100];
double Ridge_0_0_Om_70_r__0_1_r__0_2[48][2];
double Ridge_0_0_Om_70_r__0_1_r__1_2[2][2];
double Ridge_0_0_Om_70_r__1_1_r__2_1[2][100];
double Ridge_0_0_Om_70_r__1_1_r__1_2[100][2];
double Ridge_0_0_Om_70_r__1_1_r__2_2[2][2];
double Ridge_0_0_Om_70_r__2_1_r__2_2[52][2];
double Ridge_0_0_Om_70_r__0_2_r__1_2[2][52];
double Ridge_0_0_Om_70_r__1_2_r__2_2[2][52];
double Ridge_0_0_Om_71_r__0_0_r__1_0[2][48];
double Ridge_0_0_Om_71_r__0_0_r__0_1[48][2];
double Ridge_0_0_Om_71_r__0_0_r__1_1[2][2];
double Ridge_0_0_Om_71_r__1_0_r__2_0[2][48];
double Ridge_0_0_Om_71_r__1_0_r__1_1[100][2];
double Ridge_0_0_Om_71_r__1_0_r__2_1[2][2];
double Ridge_0_0_Om_71_r__2_0_r__2_1[52][2];
double Ridge_0_0_Om_71_r__0_1_r__1_1[2][100];
double Ridge_0_0_Om_71_r__0_1_r__0_2[48][2];
double Ridge_0_0_Om_71_r__0_1_r__1_2[2][2];
double Ridge_0_0_Om_71_r__1_1_r__2_1[2][100];
double Ridge_0_0_Om_71_r__1_1_r__1_2[100][2];
double Ridge_0_0_Om_71_r__1_1_r__2_2[2][2];
double Ridge_0_0_Om_71_r__2_1_r__2_2[52][2];
double Ridge_0_0_Om_71_r__0_2_r__1_2[2][52];
double Ridge_0_0_Om_71_r__1_2_r__2_2[2][52];
struct Facet_m1_m1_src__2_2_dest__0_0 Facet_m1_m1_src__2_2_dest__0_0_Send;struct Facet_m1_m1_src__2_2_dest__0_0 Facet_m1_m1_src__2_2_dest__0_0_Recv;MPI_Request req_Facet_m1_m1_src__2_2_dest__0_0;

struct Facet_m1_0_src__2_0_dest__0_0 Facet_m1_0_src__2_0_dest__0_0_Send;struct Facet_m1_0_src__2_0_dest__0_0 Facet_m1_0_src__2_0_dest__0_0_Recv;MPI_Request req_Facet_m1_0_src__2_0_dest__0_0;

struct Facet_m1_0_src__2_1_dest__0_0 Facet_m1_0_src__2_1_dest__0_0_Send;struct Facet_m1_0_src__2_1_dest__0_0 Facet_m1_0_src__2_1_dest__0_0_Recv;MPI_Request req_Facet_m1_0_src__2_1_dest__0_0;

struct Facet_m1_0_src__2_1_dest__0_1 Facet_m1_0_src__2_1_dest__0_1_Send;struct Facet_m1_0_src__2_1_dest__0_1 Facet_m1_0_src__2_1_dest__0_1_Recv;MPI_Request req_Facet_m1_0_src__2_1_dest__0_1;

struct Facet_m1_0_src__2_2_dest__0_1 Facet_m1_0_src__2_2_dest__0_1_Send;struct Facet_m1_0_src__2_2_dest__0_1 Facet_m1_0_src__2_2_dest__0_1_Recv;MPI_Request req_Facet_m1_0_src__2_2_dest__0_1;

struct Facet_m1_0_src__2_2_dest__0_2 Facet_m1_0_src__2_2_dest__0_2_Send;struct Facet_m1_0_src__2_2_dest__0_2 Facet_m1_0_src__2_2_dest__0_2_Recv;MPI_Request req_Facet_m1_0_src__2_2_dest__0_2;

struct Facet_0_m1_src__0_2_dest__0_0 Facet_0_m1_src__0_2_dest__0_0_Send;struct Facet_0_m1_src__0_2_dest__0_0 Facet_0_m1_src__0_2_dest__0_0_Recv;MPI_Request req_Facet_0_m1_src__0_2_dest__0_0;

struct Facet_0_m1_src__1_2_dest__0_0 Facet_0_m1_src__1_2_dest__0_0_Send;struct Facet_0_m1_src__1_2_dest__0_0 Facet_0_m1_src__1_2_dest__0_0_Recv;MPI_Request req_Facet_0_m1_src__1_2_dest__0_0;

struct Facet_0_m1_src__1_2_dest__1_0 Facet_0_m1_src__1_2_dest__1_0_Send;struct Facet_0_m1_src__1_2_dest__1_0 Facet_0_m1_src__1_2_dest__1_0_Recv;MPI_Request req_Facet_0_m1_src__1_2_dest__1_0;

struct Facet_0_m1_src__2_2_dest__1_0 Facet_0_m1_src__2_2_dest__1_0_Send;struct Facet_0_m1_src__2_2_dest__1_0 Facet_0_m1_src__2_2_dest__1_0_Recv;MPI_Request req_Facet_0_m1_src__2_2_dest__1_0;

struct Facet_0_m1_src__2_2_dest__2_0 Facet_0_m1_src__2_2_dest__2_0_Send;struct Facet_0_m1_src__2_2_dest__2_0 Facet_0_m1_src__2_2_dest__2_0_Recv;MPI_Request req_Facet_0_m1_src__2_2_dest__2_0;

double Ridge_0_0_Om_70_r__1_0_r__0_0[2][52];
double Ridge_0_0_Om_70_r__2_0_r__1_0[2][52];
double Ridge_0_0_Om_70_r__0_1_r__0_0[52][2];
double Ridge_0_0_Om_70_r__1_1_r__0_0[2][2];
double Ridge_0_0_Om_70_r__1_1_r__1_0[100][2];
double Ridge_0_0_Om_70_r__1_1_r__0_1[2][100];
double Ridge_0_0_Om_70_r__2_1_r__1_0[2][2];
double Ridge_0_0_Om_70_r__2_1_r__2_0[48][2];
double Ridge_0_0_Om_70_r__2_1_r__1_1[2][100];
double Ridge_0_0_Om_70_r__0_2_r__0_1[52][2];
double Ridge_0_0_Om_70_r__1_2_r__0_1[2][2];
double Ridge_0_0_Om_70_r__1_2_r__1_1[100][2];
double Ridge_0_0_Om_70_r__1_2_r__0_2[2][48];
double Ridge_0_0_Om_70_r__2_2_r__1_1[2][2];
double Ridge_0_0_Om_70_r__2_2_r__2_1[48][2];
double Ridge_0_0_Om_70_r__2_2_r__1_2[2][48];
double Ridge_0_0_Om_71_r__1_0_r__0_0[2][52];
double Ridge_0_0_Om_71_r__2_0_r__1_0[2][52];
double Ridge_0_0_Om_71_r__0_1_r__0_0[52][2];
double Ridge_0_0_Om_71_r__1_1_r__0_0[2][2];
double Ridge_0_0_Om_71_r__1_1_r__1_0[100][2];
double Ridge_0_0_Om_71_r__1_1_r__0_1[2][100];
double Ridge_0_0_Om_71_r__2_1_r__1_0[2][2];
double Ridge_0_0_Om_71_r__2_1_r__2_0[48][2];
double Ridge_0_0_Om_71_r__2_1_r__1_1[2][100];
double Ridge_0_0_Om_71_r__0_2_r__0_1[52][2];
double Ridge_0_0_Om_71_r__1_2_r__0_1[2][2];
double Ridge_0_0_Om_71_r__1_2_r__1_1[100][2];
double Ridge_0_0_Om_71_r__1_2_r__0_2[2][48];
double Ridge_0_0_Om_71_r__2_2_r__1_1[2][2];
double Ridge_0_0_Om_71_r__2_2_r__2_1[48][2];
double Ridge_0_0_Om_71_r__2_2_r__1_2[2][48];

struct Formura_Navigator navi;

void Formura_decode_mpi_rank (int r, int *ix , int *iy){
int s=r;
*iy=s%1;
s=s/1;
*ix=s%1;
s=s/1;
}
int Formura_encode_mpi_rank ( int ix, int iy){
int s = 0;
s *= 1;
s += (ix%1+1)%1;
s *= 1;
s += (iy%1+1)%1;
return s;}
int Formura_Init (struct Formura_Navigator *navi, MPI_Comm comm){
int ix,iy;
navi->mpi_comm = comm;
{int r; MPI_Comm_rank(comm,&r);navi->mpi_my_rank = r;}
Formura_decode_mpi_rank( navi->mpi_my_rank, &ix , &iy);
navi->mpi_rank__m1_m1=Formura_encode_mpi_rank( ix+(-1),iy+(-1));
navi->mpi_rank__m1_0=Formura_encode_mpi_rank( ix+(-1),iy+(0));
navi->mpi_rank__0_m1=Formura_encode_mpi_rank( ix+(0),iy+(-1));
navi->mpi_rank__0_0=Formura_encode_mpi_rank( ix+(0),iy+(0));
navi->mpi_rank__0_1=Formura_encode_mpi_rank( ix+(0),iy+(1));
navi->mpi_rank__1_0=Formura_encode_mpi_rank( ix+(1),iy+(0));
navi->mpi_rank__1_1=Formura_encode_mpi_rank( ix+(1),iy+(1));
navi->time_step=0;
navi->offset_x=ix*200-2;
navi->lower_x=2;
navi->upper_x=2+200;
navi->offset_y=iy*200-2;
navi->lower_y=2;
navi->upper_y=2+200;
return 0;}


int Formura_Forward (struct Formura_Navigator *navi){
for(int timestep=0;timestep<1;++timestep){
Formura_internal();
MPI_Irecv( (void*) &Facet_1_0_src__0_0_dest__2_0_Recv , sizeof(struct Facet_1_0_src__0_0_dest__2_0) , MPI_BYTE, navi->mpi_rank__1_0, 5 , navi->mpi_comm, &req_Facet_1_0_src__0_0_dest__2_0 );
 MPI_Isend( (void*) &Facet_1_0_src__0_0_dest__2_0_Send , sizeof(struct Facet_1_0_src__0_0_dest__2_0) , MPI_BYTE, navi->mpi_rank__m1_0, 5 , navi->mpi_comm, &req_Facet_1_0_src__0_0_dest__2_0 );

MPI_Irecv( (void*) &Facet_1_0_src__0_0_dest__2_1_Recv , sizeof(struct Facet_1_0_src__0_0_dest__2_1) , MPI_BYTE, navi->mpi_rank__1_0, 6 , navi->mpi_comm, &req_Facet_1_0_src__0_0_dest__2_1 );
 MPI_Isend( (void*) &Facet_1_0_src__0_0_dest__2_1_Send , sizeof(struct Facet_1_0_src__0_0_dest__2_1) , MPI_BYTE, navi->mpi_rank__m1_0, 6 , navi->mpi_comm, &req_Facet_1_0_src__0_0_dest__2_1 );

MPI_Irecv( (void*) &Facet_0_1_src__0_0_dest__0_2_Recv , sizeof(struct Facet_0_1_src__0_0_dest__0_2) , MPI_BYTE, navi->mpi_rank__0_1, 0 , navi->mpi_comm, &req_Facet_0_1_src__0_0_dest__0_2 );
 MPI_Isend( (void*) &Facet_0_1_src__0_0_dest__0_2_Send , sizeof(struct Facet_0_1_src__0_0_dest__0_2) , MPI_BYTE, navi->mpi_rank__0_m1, 0 , navi->mpi_comm, &req_Facet_0_1_src__0_0_dest__0_2 );

MPI_Irecv( (void*) &Facet_0_1_src__0_0_dest__1_2_Recv , sizeof(struct Facet_0_1_src__0_0_dest__1_2) , MPI_BYTE, navi->mpi_rank__0_1, 1 , navi->mpi_comm, &req_Facet_0_1_src__0_0_dest__1_2 );
 MPI_Isend( (void*) &Facet_0_1_src__0_0_dest__1_2_Send , sizeof(struct Facet_0_1_src__0_0_dest__1_2) , MPI_BYTE, navi->mpi_rank__0_m1, 1 , navi->mpi_comm, &req_Facet_0_1_src__0_0_dest__1_2 );

MPI_Irecv( (void*) &Facet_1_1_src__0_0_dest__2_2_Recv , sizeof(struct Facet_1_1_src__0_0_dest__2_2) , MPI_BYTE, navi->mpi_rank__1_1, 10 , navi->mpi_comm, &req_Facet_1_1_src__0_0_dest__2_2 );
 MPI_Isend( (void*) &Facet_1_1_src__0_0_dest__2_2_Send , sizeof(struct Facet_1_1_src__0_0_dest__2_2) , MPI_BYTE, navi->mpi_rank__m1_m1, 10 , navi->mpi_comm, &req_Facet_1_1_src__0_0_dest__2_2 );

Formura_internal_1();
MPI_Irecv( (void*) &Facet_0_1_src__1_0_dest__1_2_Recv , sizeof(struct Facet_0_1_src__1_0_dest__1_2) , MPI_BYTE, navi->mpi_rank__0_1, 2 , navi->mpi_comm, &req_Facet_0_1_src__1_0_dest__1_2 );
 MPI_Isend( (void*) &Facet_0_1_src__1_0_dest__1_2_Send , sizeof(struct Facet_0_1_src__1_0_dest__1_2) , MPI_BYTE, navi->mpi_rank__0_m1, 2 , navi->mpi_comm, &req_Facet_0_1_src__1_0_dest__1_2 );

MPI_Irecv( (void*) &Facet_0_1_src__1_0_dest__2_2_Recv , sizeof(struct Facet_0_1_src__1_0_dest__2_2) , MPI_BYTE, navi->mpi_rank__0_1, 3 , navi->mpi_comm, &req_Facet_0_1_src__1_0_dest__2_2 );
 MPI_Isend( (void*) &Facet_0_1_src__1_0_dest__2_2_Send , sizeof(struct Facet_0_1_src__1_0_dest__2_2) , MPI_BYTE, navi->mpi_rank__0_m1, 3 , navi->mpi_comm, &req_Facet_0_1_src__1_0_dest__2_2 );

MPI_Wait(&req_Facet_1_0_src__0_0_dest__2_0,MPI_STATUS_IGNORE);

Formura_internal_2();
MPI_Irecv( (void*) &Facet_0_1_src__2_0_dest__2_2_Recv , sizeof(struct Facet_0_1_src__2_0_dest__2_2) , MPI_BYTE, navi->mpi_rank__0_1, 4 , navi->mpi_comm, &req_Facet_0_1_src__2_0_dest__2_2 );
 MPI_Isend( (void*) &Facet_0_1_src__2_0_dest__2_2_Send , sizeof(struct Facet_0_1_src__2_0_dest__2_2) , MPI_BYTE, navi->mpi_rank__0_m1, 4 , navi->mpi_comm, &req_Facet_0_1_src__2_0_dest__2_2 );

Formura_internal_3();
MPI_Irecv( (void*) &Facet_1_0_src__0_1_dest__2_1_Recv , sizeof(struct Facet_1_0_src__0_1_dest__2_1) , MPI_BYTE, navi->mpi_rank__1_0, 7 , navi->mpi_comm, &req_Facet_1_0_src__0_1_dest__2_1 );
 MPI_Isend( (void*) &Facet_1_0_src__0_1_dest__2_1_Send , sizeof(struct Facet_1_0_src__0_1_dest__2_1) , MPI_BYTE, navi->mpi_rank__m1_0, 7 , navi->mpi_comm, &req_Facet_1_0_src__0_1_dest__2_1 );

MPI_Irecv( (void*) &Facet_1_0_src__0_1_dest__2_2_Recv , sizeof(struct Facet_1_0_src__0_1_dest__2_2) , MPI_BYTE, navi->mpi_rank__1_0, 8 , navi->mpi_comm, &req_Facet_1_0_src__0_1_dest__2_2 );
 MPI_Isend( (void*) &Facet_1_0_src__0_1_dest__2_2_Send , sizeof(struct Facet_1_0_src__0_1_dest__2_2) , MPI_BYTE, navi->mpi_rank__m1_0, 8 , navi->mpi_comm, &req_Facet_1_0_src__0_1_dest__2_2 );

Formura_internal_4();
MPI_Wait(&req_Facet_1_0_src__0_0_dest__2_1,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_1_0_src__0_1_dest__2_1,MPI_STATUS_IGNORE);

Formura_internal_5();
MPI_Wait(&req_Facet_0_1_src__0_0_dest__0_2,MPI_STATUS_IGNORE);

Formura_internal_6();
MPI_Irecv( (void*) &Facet_1_0_src__0_2_dest__2_2_Recv , sizeof(struct Facet_1_0_src__0_2_dest__2_2) , MPI_BYTE, navi->mpi_rank__1_0, 9 , navi->mpi_comm, &req_Facet_1_0_src__0_2_dest__2_2 );
 MPI_Isend( (void*) &Facet_1_0_src__0_2_dest__2_2_Send , sizeof(struct Facet_1_0_src__0_2_dest__2_2) , MPI_BYTE, navi->mpi_rank__m1_0, 9 , navi->mpi_comm, &req_Facet_1_0_src__0_2_dest__2_2 );

MPI_Wait(&req_Facet_0_1_src__0_0_dest__1_2,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_0_1_src__1_0_dest__1_2,MPI_STATUS_IGNORE);

Formura_internal_7();
MPI_Wait(&req_Facet_0_1_src__1_0_dest__2_2,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_0_1_src__2_0_dest__2_2,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_1_0_src__0_1_dest__2_2,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_1_0_src__0_2_dest__2_2,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_1_1_src__0_0_dest__2_2,MPI_STATUS_IGNORE);

Formura_internal_8();

/* HALFWAYS */
Formura_internal_9();
MPI_Irecv( (void*) &Facet_m1_0_src__2_2_dest__0_2_Recv , sizeof(struct Facet_m1_0_src__2_2_dest__0_2) , MPI_BYTE, navi->mpi_rank__m1_0, 5 , navi->mpi_comm, &req_Facet_m1_0_src__2_2_dest__0_2 );
 MPI_Isend( (void*) &Facet_m1_0_src__2_2_dest__0_2_Send , sizeof(struct Facet_m1_0_src__2_2_dest__0_2) , MPI_BYTE, navi->mpi_rank__1_0, 5 , navi->mpi_comm, &req_Facet_m1_0_src__2_2_dest__0_2 );

MPI_Irecv( (void*) &Facet_m1_0_src__2_2_dest__0_1_Recv , sizeof(struct Facet_m1_0_src__2_2_dest__0_1) , MPI_BYTE, navi->mpi_rank__m1_0, 4 , navi->mpi_comm, &req_Facet_m1_0_src__2_2_dest__0_1 );
 MPI_Isend( (void*) &Facet_m1_0_src__2_2_dest__0_1_Send , sizeof(struct Facet_m1_0_src__2_2_dest__0_1) , MPI_BYTE, navi->mpi_rank__1_0, 4 , navi->mpi_comm, &req_Facet_m1_0_src__2_2_dest__0_1 );

MPI_Irecv( (void*) &Facet_0_m1_src__2_2_dest__2_0_Recv , sizeof(struct Facet_0_m1_src__2_2_dest__2_0) , MPI_BYTE, navi->mpi_rank__0_m1, 10 , navi->mpi_comm, &req_Facet_0_m1_src__2_2_dest__2_0 );
 MPI_Isend( (void*) &Facet_0_m1_src__2_2_dest__2_0_Send , sizeof(struct Facet_0_m1_src__2_2_dest__2_0) , MPI_BYTE, navi->mpi_rank__0_1, 10 , navi->mpi_comm, &req_Facet_0_m1_src__2_2_dest__2_0 );

MPI_Irecv( (void*) &Facet_0_m1_src__2_2_dest__1_0_Recv , sizeof(struct Facet_0_m1_src__2_2_dest__1_0) , MPI_BYTE, navi->mpi_rank__0_m1, 9 , navi->mpi_comm, &req_Facet_0_m1_src__2_2_dest__1_0 );
 MPI_Isend( (void*) &Facet_0_m1_src__2_2_dest__1_0_Send , sizeof(struct Facet_0_m1_src__2_2_dest__1_0) , MPI_BYTE, navi->mpi_rank__0_1, 9 , navi->mpi_comm, &req_Facet_0_m1_src__2_2_dest__1_0 );

MPI_Irecv( (void*) &Facet_m1_m1_src__2_2_dest__0_0_Recv , sizeof(struct Facet_m1_m1_src__2_2_dest__0_0) , MPI_BYTE, navi->mpi_rank__m1_m1, 0 , navi->mpi_comm, &req_Facet_m1_m1_src__2_2_dest__0_0 );
 MPI_Isend( (void*) &Facet_m1_m1_src__2_2_dest__0_0_Send , sizeof(struct Facet_m1_m1_src__2_2_dest__0_0) , MPI_BYTE, navi->mpi_rank__1_1, 0 , navi->mpi_comm, &req_Facet_m1_m1_src__2_2_dest__0_0 );

Formura_internal_10();
MPI_Irecv( (void*) &Facet_0_m1_src__1_2_dest__1_0_Recv , sizeof(struct Facet_0_m1_src__1_2_dest__1_0) , MPI_BYTE, navi->mpi_rank__0_m1, 8 , navi->mpi_comm, &req_Facet_0_m1_src__1_2_dest__1_0 );
 MPI_Isend( (void*) &Facet_0_m1_src__1_2_dest__1_0_Send , sizeof(struct Facet_0_m1_src__1_2_dest__1_0) , MPI_BYTE, navi->mpi_rank__0_1, 8 , navi->mpi_comm, &req_Facet_0_m1_src__1_2_dest__1_0 );

MPI_Irecv( (void*) &Facet_0_m1_src__1_2_dest__0_0_Recv , sizeof(struct Facet_0_m1_src__1_2_dest__0_0) , MPI_BYTE, navi->mpi_rank__0_m1, 7 , navi->mpi_comm, &req_Facet_0_m1_src__1_2_dest__0_0 );
 MPI_Isend( (void*) &Facet_0_m1_src__1_2_dest__0_0_Send , sizeof(struct Facet_0_m1_src__1_2_dest__0_0) , MPI_BYTE, navi->mpi_rank__0_1, 7 , navi->mpi_comm, &req_Facet_0_m1_src__1_2_dest__0_0 );

MPI_Wait(&req_Facet_m1_0_src__2_2_dest__0_2,MPI_STATUS_IGNORE);

Formura_internal_11();
MPI_Irecv( (void*) &Facet_0_m1_src__0_2_dest__0_0_Recv , sizeof(struct Facet_0_m1_src__0_2_dest__0_0) , MPI_BYTE, navi->mpi_rank__0_m1, 6 , navi->mpi_comm, &req_Facet_0_m1_src__0_2_dest__0_0 );
 MPI_Isend( (void*) &Facet_0_m1_src__0_2_dest__0_0_Send , sizeof(struct Facet_0_m1_src__0_2_dest__0_0) , MPI_BYTE, navi->mpi_rank__0_1, 6 , navi->mpi_comm, &req_Facet_0_m1_src__0_2_dest__0_0 );

Formura_internal_12();
MPI_Irecv( (void*) &Facet_m1_0_src__2_1_dest__0_1_Recv , sizeof(struct Facet_m1_0_src__2_1_dest__0_1) , MPI_BYTE, navi->mpi_rank__m1_0, 3 , navi->mpi_comm, &req_Facet_m1_0_src__2_1_dest__0_1 );
 MPI_Isend( (void*) &Facet_m1_0_src__2_1_dest__0_1_Send , sizeof(struct Facet_m1_0_src__2_1_dest__0_1) , MPI_BYTE, navi->mpi_rank__1_0, 3 , navi->mpi_comm, &req_Facet_m1_0_src__2_1_dest__0_1 );

MPI_Irecv( (void*) &Facet_m1_0_src__2_1_dest__0_0_Recv , sizeof(struct Facet_m1_0_src__2_1_dest__0_0) , MPI_BYTE, navi->mpi_rank__m1_0, 2 , navi->mpi_comm, &req_Facet_m1_0_src__2_1_dest__0_0 );
 MPI_Isend( (void*) &Facet_m1_0_src__2_1_dest__0_0_Send , sizeof(struct Facet_m1_0_src__2_1_dest__0_0) , MPI_BYTE, navi->mpi_rank__1_0, 2 , navi->mpi_comm, &req_Facet_m1_0_src__2_1_dest__0_0 );

Formura_internal_13();
MPI_Wait(&req_Facet_m1_0_src__2_1_dest__0_1,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_m1_0_src__2_2_dest__0_1,MPI_STATUS_IGNORE);

Formura_internal_14();
MPI_Wait(&req_Facet_0_m1_src__2_2_dest__2_0,MPI_STATUS_IGNORE);

Formura_internal_15();
MPI_Irecv( (void*) &Facet_m1_0_src__2_0_dest__0_0_Recv , sizeof(struct Facet_m1_0_src__2_0_dest__0_0) , MPI_BYTE, navi->mpi_rank__m1_0, 1 , navi->mpi_comm, &req_Facet_m1_0_src__2_0_dest__0_0 );
 MPI_Isend( (void*) &Facet_m1_0_src__2_0_dest__0_0_Send , sizeof(struct Facet_m1_0_src__2_0_dest__0_0) , MPI_BYTE, navi->mpi_rank__1_0, 1 , navi->mpi_comm, &req_Facet_m1_0_src__2_0_dest__0_0 );

MPI_Wait(&req_Facet_0_m1_src__1_2_dest__1_0,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_0_m1_src__2_2_dest__1_0,MPI_STATUS_IGNORE);

Formura_internal_16();
MPI_Wait(&req_Facet_m1_m1_src__2_2_dest__0_0,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_m1_0_src__2_0_dest__0_0,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_m1_0_src__2_1_dest__0_0,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_0_m1_src__0_2_dest__0_0,MPI_STATUS_IGNORE);

MPI_Wait(&req_Facet_0_m1_src__1_2_dest__0_0,MPI_STATUS_IGNORE);

Formura_internal_17();


}
navi->time_step += 4;
return 0;}
