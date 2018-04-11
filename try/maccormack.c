
#include <mpi.h>
#include <math.h>
#include <stdbool.h>

#include "maccormack.h"



double u[306];
double Rsc0[160];
double Rsc1[160];
struct Facet_1_src__0_dest__1 Facet_1_src__0_dest__1_Send;struct Facet_1_src__0_dest__1 Facet_1_src__0_dest__1_Recv;MPI_Request req_Facet_1_src__0_dest__1;

double Ridge_0_Om_23_r__0_r__1[2];
double Ridge_0_Om_24_r__0_r__1[2];
double Ridge_0_Om_25_r__0_r__1[2];
struct Facet_m1_src__1_dest__0 Facet_m1_src__1_dest__0_Send;struct Facet_m1_src__1_dest__0 Facet_m1_src__1_dest__0_Recv;MPI_Request req_Facet_m1_src__1_dest__0;

double Ridge_0_Om_23_r__1_r__0[2];
double Ridge_0_Om_24_r__1_r__0[2];
double Ridge_0_Om_25_r__1_r__0[2];

struct Formura_Navigator navi;

void Formura_decode_mpi_rank (int r, int *ix){
int s=r;
*ix=s%1;
s=s/1;
}
int Formura_encode_mpi_rank ( int ix){
int s = 0;
s *= 1;
s += (ix%1+1)%1;
return s;}
int Formura_Init (struct Formura_Navigator *navi, MPI_Comm comm){
int ix;
navi->mpi_comm = comm;
{int r; MPI_Comm_rank(comm,&r);navi->mpi_my_rank = r;}
Formura_decode_mpi_rank( navi->mpi_my_rank, &ix);
navi->mpi_rank__m1=Formura_encode_mpi_rank( ix+(-1));
navi->mpi_rank__0=Formura_encode_mpi_rank( ix+(0));
navi->mpi_rank__1=Formura_encode_mpi_rank( ix+(1));
navi->time_step=0;
navi->offset_x=ix*300-2;
navi->lower_x=2;
navi->upper_x=2+300;
return 0;}


int Formura_Forward (struct Formura_Navigator *navi){
for(int timestep=0;timestep<1;++timestep){
Formura_internal();
MPI_Irecv( (void*) &Facet_1_src__0_dest__1_Recv , sizeof(struct Facet_1_src__0_dest__1) , MPI_BYTE, navi->mpi_rank__1, 0 , navi->mpi_comm, &req_Facet_1_src__0_dest__1 );
 MPI_Isend( (void*) &Facet_1_src__0_dest__1_Send , sizeof(struct Facet_1_src__0_dest__1) , MPI_BYTE, navi->mpi_rank__m1, 0 , navi->mpi_comm, &req_Facet_1_src__0_dest__1 );

MPI_Wait(&req_Facet_1_src__0_dest__1,MPI_STATUS_IGNORE);

Formura_internal_3();

/* HALFWAYS */
Formura_internal_4();
MPI_Irecv( (void*) &Facet_m1_src__1_dest__0_Recv , sizeof(struct Facet_m1_src__1_dest__0) , MPI_BYTE, navi->mpi_rank__m1, 0 , navi->mpi_comm, &req_Facet_m1_src__1_dest__0 );
 MPI_Isend( (void*) &Facet_m1_src__1_dest__0_Send , sizeof(struct Facet_m1_src__1_dest__0) , MPI_BYTE, navi->mpi_rank__1, 0 , navi->mpi_comm, &req_Facet_m1_src__1_dest__0 );

MPI_Wait(&req_Facet_m1_src__1_dest__0,MPI_STATUS_IGNORE);

Formura_internal_5();


}
navi->time_step += 8;
return 0;}
