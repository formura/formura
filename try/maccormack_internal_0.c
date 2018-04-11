
#include <mpi.h>
#include <math.h>
#include <stdbool.h>

#include "maccormack.h"

void Formura_internal(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_St_u[ix]=u[ix+(2)];}

Formura_subroutine_2(0,148,2,3,4,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_23_r__0_r__1[ix]=Rsc0[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_23_r__0_r__1[ix]=Rsc0[ix+(0)];}

Formura_subroutine_0(0,146,Rsc0,0,Rsc0,1,Rsc0,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_24_r__0_r__1[ix]=Rsc1[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_24_r__0_r__1[ix]=Rsc1[ix+(0)];}

Formura_subroutine_0(0,144,Rsc1,0,Rsc1,1,Rsc1,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_25_r__0_r__1[ix]=Rsc0[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_25_r__0_r__1[ix]=Rsc0[ix+(0)];}

Formura_subroutine_1(10,152,Rsc0,-10,Rsc0,-9,Rsc0,-8,-8,0);

}
void Formura_internal_3(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
u[ix+(302)]=Facet_1_src__0_dest__1_Recv.Ridge_1_St_u[ix];}

Formura_subroutine_2(2,154,148,149,150,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Ridge_0_Om_23_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(154)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_23_r__0_r__1[ix];}

Formura_subroutine_0(2,156,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Ridge_0_Om_24_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(156)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_24_r__0_r__1[ix];}

Formura_subroutine_0(2,158,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Ridge_0_Om_25_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(158)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_25_r__0_r__1[ix];}

Formura_subroutine_1(152,310,Rsc0,-152,Rsc0,-151,Rsc0,-150,-8,0);

}
void Formura_internal_4(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_St_u[ix]=u[ix+(300)];}

Formura_subroutine_2(0,148,152,153,154,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_23_r__1_r__0[ix]=Rsc0[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_23_r__1_r__0[ix]=Rsc0[ix+(0)];}

Formura_subroutine_0(0,146,Rsc0,0,Rsc0,1,Rsc0,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_24_r__1_r__0[ix]=Rsc1[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_24_r__1_r__0[ix]=Rsc1[ix+(0)];}

Formura_subroutine_0(0,144,Rsc1,0,Rsc1,1,Rsc1,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_25_r__1_r__0[ix]=Rsc0[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_25_r__1_r__0[ix]=Rsc0[ix+(0)];}

Formura_subroutine_1(160,302,Rsc0,-160,Rsc0,-159,Rsc0,-158,0,0);

}
void Formura_internal_5(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
u[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_St_u[ix];}

Formura_subroutine_2(2,154,-2,-1,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_23_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(154)]=Ridge_0_Om_23_r__1_r__0[ix];}

Formura_subroutine_0(2,156,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_24_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(156)]=Ridge_0_Om_24_r__1_r__0[ix];}

Formura_subroutine_0(2,158,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_25_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(158)]=Ridge_0_Om_25_r__1_r__0[ix];}

Formura_subroutine_1(2,160,Rsc0,-2,Rsc0,-1,Rsc0,0,0,0);

}
void Formura_subroutine_0(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
    bool  a=(1.0==2.0)/*True*/;
 bool  a_0=(1.0==2.0)/*True*/;
     double  a_1=(a?0.1:0.2)/*True*/;
 double  a_2=(a_0?0.1:0.2)/*True*/;
 double  a_3=argx2[ix+(argx3)]/*True*/;
 double  a_4=argx4[ix+(argx5)]/*True*/;
 double  a_5=argx6[ix+(argx7)]/*True*/;
 double  a_6=(fabs(a_3))/*True*/;
 double  a_7=(fabs(a_4))/*True*/;
 double  a_8=(a_4-a_3)/*True*/;
 double  a_9=(a_5-a_4)/*True*/;
 double  a_10=(a_1*a_8)/*True*/;
 double  a_11=(a_2*a_9)/*True*/;
 double  a_12=(a_6-a_10)/*True*/;
 double  a_13=(a_7-a_11)/*True*/;
 double  a_14=(a_4+a_13)/*True*/;
 double  a_15=(a_13-a_12)/*True*/;
 double  a_16=(a_2*a_15)/*True*/;
 double  a_17=(a_14-a_16)/*True*/;
  double  a_18=(a_17/2.0)/*True*/;
 
argx8[ix+(argx9)]=(+a_18);

}
}void Formura_subroutine_1(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,int argx8,int argx9){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
    bool  a=(1.0==2.0)/*True*/;
 bool  a_0=(1.0==2.0)/*True*/;
     double  a_1=(a?0.1:0.2)/*True*/;
 double  a_2=(a_0?0.1:0.2)/*True*/;
 double  a_3=argx2[ix+(argx3)]/*True*/;
 double  a_4=argx4[ix+(argx5)]/*True*/;
 double  a_5=argx6[ix+(argx7)]/*True*/;
 double  a_6=(fabs(a_3))/*True*/;
 double  a_7=(fabs(a_4))/*True*/;
 double  a_8=(a_4-a_3)/*True*/;
 double  a_9=(a_5-a_4)/*True*/;
 double  a_10=(a_1*a_8)/*True*/;
 double  a_11=(a_2*a_9)/*True*/;
 double  a_12=(a_6-a_10)/*True*/;
 double  a_13=(a_7-a_11)/*True*/;
 double  a_14=(a_4+a_13)/*True*/;
 double  a_15=(a_13-a_12)/*True*/;
 double  a_16=(a_2*a_15)/*True*/;
 double  a_17=(a_14-a_16)/*True*/;
  double  a_18=(a_17/2.0)/*True*/;
 
u[ix+(argx8)+(argx9)]=a_18;

}
}void Formura_subroutine_2(int argx0,int argx1,int argx2,int argx3,int argx4,rsc_surface * __restrict  argx5,int argx6){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
    bool  a=(1.0==2.0)/*True*/;
 bool  a_0=(1.0==2.0)/*True*/;
     double  a_1=(a?0.1:0.2)/*True*/;
 double  a_2=(a_0?0.1:0.2)/*True*/;
 double  a_3=u[ix+(argx2)]/*True*/;
 double  a_4=u[ix+(argx3)]/*True*/;
 double  a_5=u[ix+(argx4)]/*True*/;
 double  a_6=(fabs(a_3))/*True*/;
 double  a_7=(fabs(a_4))/*True*/;
 double  a_8=(a_4-a_3)/*True*/;
 double  a_9=(a_5-a_4)/*True*/;
 double  a_10=(a_1*a_8)/*True*/;
 double  a_11=(a_2*a_9)/*True*/;
 double  a_12=(a_6-a_10)/*True*/;
 double  a_13=(a_7-a_11)/*True*/;
 double  a_14=(a_4+a_13)/*True*/;
 double  a_15=(a_13-a_12)/*True*/;
 double  a_16=(a_2*a_15)/*True*/;
 double  a_17=(a_14-a_16)/*True*/;
  double  a_18=(a_17/2.0)/*True*/;
 
argx5[ix+(argx6)]=(+a_18);

}
}