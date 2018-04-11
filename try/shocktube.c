
#include <mpi.h>
#include <math.h>
#include <stdbool.h>

#include "shocktube.h"



double e[306];
double m[306];
double r[306];
double Rsc0[168];
double Rsc1[168];
double Rsc2[168];
double Rsc3[168];
double Rsc4[168];
double Rsc6[168];
double Rsc5[168];
struct Facet_1_src__0_dest__1 Facet_1_src__0_dest__1_Send;struct Facet_1_src__0_dest__1 Facet_1_src__0_dest__1_Recv;MPI_Request req_Facet_1_src__0_dest__1;

double Ridge_0_Om_70_r__0_r__1[2];
double Ridge_0_Om_71_r__0_r__1[2];
double Ridge_0_Om_72_r__0_r__1[2];
double Ridge_0_Om_147_r__0_r__1[2];
double Ridge_0_Om_148_r__0_r__1[2];
double Ridge_0_Om_149_r__0_r__1[2];
double Ridge_0_Om_150_r__0_r__1[2];
double Ridge_0_Om_151_r__0_r__1[2];
double Ridge_0_Om_152_r__0_r__1[2];
double Ridge_0_Om_227_r__0_r__1[2];
double Ridge_0_Om_228_r__0_r__1[2];
double Ridge_0_Om_229_r__0_r__1[2];
double Ridge_0_Om_230_r__0_r__1[2];
double Ridge_0_Om_231_r__0_r__1[2];
double Ridge_0_Om_232_r__0_r__1[2];
double Ridge_0_Om_307_r__0_r__1[2];
double Ridge_0_Om_308_r__0_r__1[2];
double Ridge_0_Om_309_r__0_r__1[2];
double Ridge_0_Om_310_r__0_r__1[2];
double Ridge_0_Om_311_r__0_r__1[2];
double Ridge_0_Om_312_r__0_r__1[2];
struct Facet_m1_src__1_dest__0 Facet_m1_src__1_dest__0_Send;struct Facet_m1_src__1_dest__0 Facet_m1_src__1_dest__0_Recv;MPI_Request req_Facet_m1_src__1_dest__0;

double Ridge_0_Om_70_r__1_r__0[2];
double Ridge_0_Om_71_r__1_r__0[2];
double Ridge_0_Om_72_r__1_r__0[2];
double Ridge_0_Om_147_r__1_r__0[2];
double Ridge_0_Om_148_r__1_r__0[2];
double Ridge_0_Om_149_r__1_r__0[2];
double Ridge_0_Om_150_r__1_r__0[2];
double Ridge_0_Om_151_r__1_r__0[2];
double Ridge_0_Om_152_r__1_r__0[2];
double Ridge_0_Om_227_r__1_r__0[2];
double Ridge_0_Om_228_r__1_r__0[2];
double Ridge_0_Om_229_r__1_r__0[2];
double Ridge_0_Om_230_r__1_r__0[2];
double Ridge_0_Om_231_r__1_r__0[2];
double Ridge_0_Om_232_r__1_r__0[2];
double Ridge_0_Om_307_r__1_r__0[2];
double Ridge_0_Om_308_r__1_r__0[2];
double Ridge_0_Om_309_r__1_r__0[2];
double Ridge_0_Om_310_r__1_r__0[2];
double Ridge_0_Om_311_r__1_r__0[2];
double Ridge_0_Om_312_r__1_r__0[2];

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

Formura_internal_10();

/* HALFWAYS */
Formura_internal_11();
MPI_Irecv( (void*) &Facet_m1_src__1_dest__0_Recv , sizeof(struct Facet_m1_src__1_dest__0) , MPI_BYTE, navi->mpi_rank__m1, 0 , navi->mpi_comm, &req_Facet_m1_src__1_dest__0 );
 MPI_Isend( (void*) &Facet_m1_src__1_dest__0_Send , sizeof(struct Facet_m1_src__1_dest__0) , MPI_BYTE, navi->mpi_rank__1, 0 , navi->mpi_comm, &req_Facet_m1_src__1_dest__0 );

MPI_Wait(&req_Facet_m1_src__1_dest__0,MPI_STATUS_IGNORE);

Formura_internal_12();


}
navi->time_step += 8;
return 0;}
