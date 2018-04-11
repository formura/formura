
#include <mpi.h>
#include <math.h>
#include <stdbool.h>

#include "shocktube.h"

void Formura_internal(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_St_e[ix]=e[ix+(2)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_St_m[ix]=m[ix+(2)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_St_r[ix]=r[ix+(2)];}

Formura_subroutine_14(0,148,2,3,4,3,4,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_70_r__0_r__1[ix]=Rsc0[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_70_r__0_r__1[ix]=Rsc0[ix+(0)];}

Formura_subroutine_9(0,148,3,4,2,3,4,3,4,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_71_r__0_r__1[ix]=Rsc1[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_71_r__0_r__1[ix]=Rsc1[ix+(0)];}

Formura_subroutine_8(0,148,3,4,3,4,2,3,4,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_72_r__0_r__1[ix]=Rsc2[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_72_r__0_r__1[ix]=Rsc2[ix+(0)];}

Formura_subroutine_13(0,146,4,Rsc0,0,Rsc0,1,Rsc0,2,Rsc1,0,Rsc1,1,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_147_r__0_r__1[ix]=Rsc3[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_147_r__0_r__1[ix]=Rsc3[ix+(0)];}

Formura_subroutine_7(0,146,4,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc1,2,Rsc2,0,Rsc2,1,Rsc4,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_148_r__0_r__1[ix]=Rsc4[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_148_r__0_r__1[ix]=Rsc4[ix+(0)];}

Formura_subroutine_6(0,146,4,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc2,0,Rsc2,1,Rsc2,2,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_149_r__0_r__1[ix]=Rsc5[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_149_r__0_r__1[ix]=Rsc5[ix+(0)];}

Formura_subroutine_12(0,144,Rsc3,0,Rsc3,1,Rsc3,2,Rsc4,1,Rsc4,2,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_150_r__0_r__1[ix]=Rsc2[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_150_r__0_r__1[ix]=Rsc2[ix+(0)];}

Formura_subroutine_0(0,144,Rsc3,1,Rsc3,2,Rsc4,0,Rsc4,1,Rsc4,2,Rsc5,1,Rsc5,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_151_r__0_r__1[ix]=Rsc1[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_151_r__0_r__1[ix]=Rsc1[ix+(0)];}

Formura_subroutine_5(0,144,Rsc3,1,Rsc3,2,Rsc4,1,Rsc4,2,Rsc5,0,Rsc5,1,Rsc5,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_152_r__0_r__1[ix]=Rsc0[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_152_r__0_r__1[ix]=Rsc0[ix+(0)];}

Formura_subroutine_10(0,142,Rsc3,2,Rsc2,0,Rsc2,1,Rsc2,2,Rsc1,0,Rsc1,1,Rsc6,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_227_r__0_r__1[ix]=Rsc6[ix+(140)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_227_r__0_r__1[ix]=Rsc6[ix+(0)];}

Formura_subroutine_1(0,142,Rsc4,2,Rsc2,0,Rsc2,1,Rsc1,0,Rsc1,1,Rsc1,2,Rsc0,0,Rsc0,1,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_228_r__0_r__1[ix]=Rsc3[ix+(140)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_228_r__0_r__1[ix]=Rsc3[ix+(0)];}

Formura_subroutine_3(0,142,Rsc5,2,Rsc2,0,Rsc2,1,Rsc1,0,Rsc1,1,Rsc0,0,Rsc0,1,Rsc0,2,Rsc4,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_229_r__0_r__1[ix]=Rsc4[ix+(140)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_229_r__0_r__1[ix]=Rsc4[ix+(0)];}

Formura_subroutine_12(0,140,Rsc6,0,Rsc6,1,Rsc6,2,Rsc3,1,Rsc3,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_230_r__0_r__1[ix]=Rsc0[ix+(138)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_230_r__0_r__1[ix]=Rsc0[ix+(0)];}

Formura_subroutine_0(0,140,Rsc6,1,Rsc6,2,Rsc3,0,Rsc3,1,Rsc3,2,Rsc4,1,Rsc4,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_231_r__0_r__1[ix]=Rsc1[ix+(138)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_231_r__0_r__1[ix]=Rsc1[ix+(0)];}

Formura_subroutine_5(0,140,Rsc6,1,Rsc6,2,Rsc3,1,Rsc3,2,Rsc4,0,Rsc4,1,Rsc4,2,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_232_r__0_r__1[ix]=Rsc2[ix+(138)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_232_r__0_r__1[ix]=Rsc2[ix+(0)];}

Formura_subroutine_10(0,138,Rsc6,2,Rsc0,0,Rsc0,1,Rsc0,2,Rsc1,0,Rsc1,1,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_307_r__0_r__1[ix]=Rsc5[ix+(136)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_307_r__0_r__1[ix]=Rsc5[ix+(0)];}

Formura_subroutine_1(0,138,Rsc3,2,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc1,2,Rsc2,0,Rsc2,1,Rsc6,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_308_r__0_r__1[ix]=Rsc6[ix+(136)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_308_r__0_r__1[ix]=Rsc6[ix+(0)];}

Formura_subroutine_3(0,138,Rsc4,2,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc2,0,Rsc2,1,Rsc2,2,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_309_r__0_r__1[ix]=Rsc3[ix+(136)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_309_r__0_r__1[ix]=Rsc3[ix+(0)];}

Formura_subroutine_12(0,136,Rsc5,0,Rsc5,1,Rsc5,2,Rsc6,1,Rsc6,2,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_310_r__0_r__1[ix]=Rsc2[ix+(134)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_310_r__0_r__1[ix]=Rsc2[ix+(0)];}

Formura_subroutine_0(0,136,Rsc5,1,Rsc5,2,Rsc6,0,Rsc6,1,Rsc6,2,Rsc3,1,Rsc3,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_311_r__0_r__1[ix]=Rsc1[ix+(134)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_311_r__0_r__1[ix]=Rsc1[ix+(0)];}

Formura_subroutine_5(0,136,Rsc5,1,Rsc5,2,Rsc6,1,Rsc6,2,Rsc3,0,Rsc3,1,Rsc3,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_312_r__0_r__1[ix]=Rsc0[ix+(134)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_1_src__0_dest__1_Send.Ridge_1_Om_312_r__0_r__1[ix]=Rsc0[ix+(0)];}

Formura_subroutine_11(10,144,Rsc5,-8,Rsc2,-10,Rsc2,-9,Rsc2,-8,Rsc1,-10,Rsc1,-9,-8,0);

Formura_subroutine_2(10,144,Rsc6,-8,Rsc2,-10,Rsc2,-9,Rsc1,-10,Rsc1,-9,Rsc1,-8,Rsc0,-10,Rsc0,-9,-8,0);

Formura_subroutine_4(10,144,Rsc3,-8,Rsc2,-10,Rsc2,-9,Rsc1,-10,Rsc1,-9,Rsc0,-10,Rsc0,-9,Rsc0,-8,-8,0);

}
void Formura_internal_10(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
m[ix+(302)]=Facet_1_src__0_dest__1_Recv.Ridge_1_St_m[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
r[ix+(302)]=Facet_1_src__0_dest__1_Recv.Ridge_1_St_r[ix];}

Formura_subroutine_14(2,154,148,149,150,149,150,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
e[ix+(302)]=Facet_1_src__0_dest__1_Recv.Ridge_1_St_e[ix];}

Formura_subroutine_9(2,154,149,150,148,149,150,149,150,Rsc1,0);

Formura_subroutine_8(2,154,149,150,149,150,148,149,150,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Ridge_0_Om_70_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(154)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_70_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Ridge_0_Om_71_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(154)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_71_r__0_r__1[ix];}

Formura_subroutine_13(2,156,148,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc1,-2,Rsc1,-1,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Ridge_0_Om_72_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(154)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_72_r__0_r__1[ix];}

Formura_subroutine_7(2,156,148,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc2,-2,Rsc2,-1,Rsc6,0);

Formura_subroutine_6(2,156,148,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc2,-2,Rsc2,-1,Rsc2,0,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(0)]=Ridge_0_Om_147_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(156)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_147_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(0)]=Ridge_0_Om_148_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(156)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_148_r__0_r__1[ix];}

Formura_subroutine_12(2,158,Rsc3,-2,Rsc3,-1,Rsc3,0,Rsc6,-1,Rsc6,0,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(0)]=Ridge_0_Om_149_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(156)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_149_r__0_r__1[ix];}

Formura_subroutine_0(2,158,Rsc3,-1,Rsc3,0,Rsc6,-2,Rsc6,-1,Rsc6,0,Rsc5,-1,Rsc5,0,Rsc1,0);

Formura_subroutine_5(2,158,Rsc3,-1,Rsc3,0,Rsc6,-1,Rsc6,0,Rsc5,-2,Rsc5,-1,Rsc5,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Ridge_0_Om_150_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(158)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_150_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Ridge_0_Om_151_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(158)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_151_r__0_r__1[ix];}

Formura_subroutine_10(2,160,Rsc3,-2,Rsc2,-2,Rsc2,-1,Rsc2,0,Rsc1,-2,Rsc1,-1,Rsc4,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Ridge_0_Om_152_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(158)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_152_r__0_r__1[ix];}

Formura_subroutine_1(2,160,Rsc6,-2,Rsc2,-2,Rsc2,-1,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc0,-2,Rsc0,-1,Rsc3,0);

Formura_subroutine_3(2,160,Rsc5,-2,Rsc2,-2,Rsc2,-1,Rsc1,-2,Rsc1,-1,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc6,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(0)]=Ridge_0_Om_227_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(160)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_227_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(0)]=Ridge_0_Om_228_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(160)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_228_r__0_r__1[ix];}

Formura_subroutine_12(2,162,Rsc4,-2,Rsc4,-1,Rsc4,0,Rsc3,-1,Rsc3,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(0)]=Ridge_0_Om_229_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(160)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_229_r__0_r__1[ix];}

Formura_subroutine_0(2,162,Rsc4,-1,Rsc4,0,Rsc3,-2,Rsc3,-1,Rsc3,0,Rsc6,-1,Rsc6,0,Rsc1,0);

Formura_subroutine_5(2,162,Rsc4,-1,Rsc4,0,Rsc3,-1,Rsc3,0,Rsc6,-2,Rsc6,-1,Rsc6,0,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Ridge_0_Om_230_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(162)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_230_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Ridge_0_Om_231_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(162)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_231_r__0_r__1[ix];}

Formura_subroutine_10(2,164,Rsc4,-2,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc1,-2,Rsc1,-1,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Ridge_0_Om_232_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(162)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_232_r__0_r__1[ix];}

Formura_subroutine_1(2,164,Rsc3,-2,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc2,-2,Rsc2,-1,Rsc4,0);

Formura_subroutine_3(2,164,Rsc6,-2,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc2,-2,Rsc2,-1,Rsc2,0,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(0)]=Ridge_0_Om_307_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(164)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_307_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(0)]=Ridge_0_Om_308_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(164)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_308_r__0_r__1[ix];}

Formura_subroutine_12(2,166,Rsc5,-2,Rsc5,-1,Rsc5,0,Rsc4,-1,Rsc4,0,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(0)]=Ridge_0_Om_309_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(164)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_309_r__0_r__1[ix];}

Formura_subroutine_0(2,166,Rsc5,-1,Rsc5,0,Rsc4,-2,Rsc4,-1,Rsc4,0,Rsc3,-1,Rsc3,0,Rsc1,0);

Formura_subroutine_5(2,166,Rsc5,-1,Rsc5,0,Rsc4,-1,Rsc4,0,Rsc3,-2,Rsc3,-1,Rsc3,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Ridge_0_Om_310_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(166)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_310_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Ridge_0_Om_311_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(166)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_311_r__0_r__1[ix];}

Formura_subroutine_11(144,310,Rsc5,-144,Rsc2,-144,Rsc2,-143,Rsc2,-142,Rsc1,-144,Rsc1,-143,-8,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Ridge_0_Om_312_r__0_r__1[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(166)]=Facet_1_src__0_dest__1_Recv.Ridge_1_Om_312_r__0_r__1[ix];}

Formura_subroutine_2(144,310,Rsc4,-144,Rsc2,-144,Rsc2,-143,Rsc1,-144,Rsc1,-143,Rsc1,-142,Rsc0,-144,Rsc0,-143,-8,0);

Formura_subroutine_4(144,310,Rsc3,-144,Rsc2,-144,Rsc2,-143,Rsc1,-144,Rsc1,-143,Rsc0,-144,Rsc0,-143,Rsc0,-142,-8,0);

}
void Formura_internal_11(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_St_e[ix]=e[ix+(300)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_St_m[ix]=m[ix+(300)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_St_r[ix]=r[ix+(300)];}

Formura_subroutine_14(0,148,152,153,154,153,154,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_70_r__1_r__0[ix]=Rsc0[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_70_r__1_r__0[ix]=Rsc0[ix+(0)];}

Formura_subroutine_9(0,148,153,154,152,153,154,153,154,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_71_r__1_r__0[ix]=Rsc1[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_71_r__1_r__0[ix]=Rsc1[ix+(0)];}

Formura_subroutine_8(0,148,153,154,153,154,152,153,154,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_72_r__1_r__0[ix]=Rsc2[ix+(146)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_72_r__1_r__0[ix]=Rsc2[ix+(0)];}

Formura_subroutine_13(0,146,154,Rsc0,0,Rsc0,1,Rsc0,2,Rsc1,0,Rsc1,1,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_147_r__1_r__0[ix]=Rsc3[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_147_r__1_r__0[ix]=Rsc3[ix+(0)];}

Formura_subroutine_7(0,146,154,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc1,2,Rsc2,0,Rsc2,1,Rsc4,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_148_r__1_r__0[ix]=Rsc4[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_148_r__1_r__0[ix]=Rsc4[ix+(0)];}

Formura_subroutine_6(0,146,154,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc2,0,Rsc2,1,Rsc2,2,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_149_r__1_r__0[ix]=Rsc5[ix+(144)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_149_r__1_r__0[ix]=Rsc5[ix+(0)];}

Formura_subroutine_12(0,144,Rsc3,0,Rsc3,1,Rsc3,2,Rsc4,1,Rsc4,2,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_150_r__1_r__0[ix]=Rsc2[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_150_r__1_r__0[ix]=Rsc2[ix+(0)];}

Formura_subroutine_0(0,144,Rsc3,1,Rsc3,2,Rsc4,0,Rsc4,1,Rsc4,2,Rsc5,1,Rsc5,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_151_r__1_r__0[ix]=Rsc1[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_151_r__1_r__0[ix]=Rsc1[ix+(0)];}

Formura_subroutine_5(0,144,Rsc3,1,Rsc3,2,Rsc4,1,Rsc4,2,Rsc5,0,Rsc5,1,Rsc5,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_152_r__1_r__0[ix]=Rsc0[ix+(142)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_152_r__1_r__0[ix]=Rsc0[ix+(0)];}

Formura_subroutine_10(0,142,Rsc3,2,Rsc2,0,Rsc2,1,Rsc2,2,Rsc1,0,Rsc1,1,Rsc6,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_227_r__1_r__0[ix]=Rsc6[ix+(140)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_227_r__1_r__0[ix]=Rsc6[ix+(0)];}

Formura_subroutine_1(0,142,Rsc4,2,Rsc2,0,Rsc2,1,Rsc1,0,Rsc1,1,Rsc1,2,Rsc0,0,Rsc0,1,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_228_r__1_r__0[ix]=Rsc3[ix+(140)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_228_r__1_r__0[ix]=Rsc3[ix+(0)];}

Formura_subroutine_3(0,142,Rsc5,2,Rsc2,0,Rsc2,1,Rsc1,0,Rsc1,1,Rsc0,0,Rsc0,1,Rsc0,2,Rsc4,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_229_r__1_r__0[ix]=Rsc4[ix+(140)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_229_r__1_r__0[ix]=Rsc4[ix+(0)];}

Formura_subroutine_12(0,140,Rsc6,0,Rsc6,1,Rsc6,2,Rsc3,1,Rsc3,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_230_r__1_r__0[ix]=Rsc0[ix+(138)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_230_r__1_r__0[ix]=Rsc0[ix+(0)];}

Formura_subroutine_0(0,140,Rsc6,1,Rsc6,2,Rsc3,0,Rsc3,1,Rsc3,2,Rsc4,1,Rsc4,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_231_r__1_r__0[ix]=Rsc1[ix+(138)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_231_r__1_r__0[ix]=Rsc1[ix+(0)];}

Formura_subroutine_5(0,140,Rsc6,1,Rsc6,2,Rsc3,1,Rsc3,2,Rsc4,0,Rsc4,1,Rsc4,2,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_232_r__1_r__0[ix]=Rsc2[ix+(138)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_232_r__1_r__0[ix]=Rsc2[ix+(0)];}

Formura_subroutine_10(0,138,Rsc6,2,Rsc0,0,Rsc0,1,Rsc0,2,Rsc1,0,Rsc1,1,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_307_r__1_r__0[ix]=Rsc5[ix+(136)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_307_r__1_r__0[ix]=Rsc5[ix+(0)];}

Formura_subroutine_1(0,138,Rsc3,2,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc1,2,Rsc2,0,Rsc2,1,Rsc6,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_308_r__1_r__0[ix]=Rsc6[ix+(136)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_308_r__1_r__0[ix]=Rsc6[ix+(0)];}

Formura_subroutine_3(0,138,Rsc4,2,Rsc0,0,Rsc0,1,Rsc1,0,Rsc1,1,Rsc2,0,Rsc2,1,Rsc2,2,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_309_r__1_r__0[ix]=Rsc3[ix+(136)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_309_r__1_r__0[ix]=Rsc3[ix+(0)];}

Formura_subroutine_12(0,136,Rsc5,0,Rsc5,1,Rsc5,2,Rsc6,1,Rsc6,2,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_310_r__1_r__0[ix]=Rsc2[ix+(134)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_310_r__1_r__0[ix]=Rsc2[ix+(0)];}

Formura_subroutine_0(0,136,Rsc5,1,Rsc5,2,Rsc6,0,Rsc6,1,Rsc6,2,Rsc3,1,Rsc3,2,Rsc1,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_311_r__1_r__0[ix]=Rsc1[ix+(134)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_311_r__1_r__0[ix]=Rsc1[ix+(0)];}

Formura_subroutine_5(0,136,Rsc5,1,Rsc5,2,Rsc6,1,Rsc6,2,Rsc3,0,Rsc3,1,Rsc3,2,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Facet_m1_src__1_dest__0_Send.Ridge_m1_Om_312_r__1_r__0[ix]=Rsc0[ix+(134)];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Ridge_0_Om_312_r__1_r__0[ix]=Rsc0[ix+(0)];}

Formura_subroutine_11(160,294,Rsc5,-158,Rsc2,-160,Rsc2,-159,Rsc2,-158,Rsc1,-160,Rsc1,-159,8,0);

Formura_subroutine_2(160,294,Rsc6,-158,Rsc2,-160,Rsc2,-159,Rsc1,-160,Rsc1,-159,Rsc1,-158,Rsc0,-160,Rsc0,-159,8,0);

Formura_subroutine_4(160,294,Rsc3,-158,Rsc2,-160,Rsc2,-159,Rsc1,-160,Rsc1,-159,Rsc0,-160,Rsc0,-159,Rsc0,-158,8,0);

}
void Formura_internal_12(){

for (int  ix = 0 ; ix < 2 ;++ ix ){
m[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_St_m[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
r[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_St_r[ix];}

Formura_subroutine_14(2,154,-2,-1,0,-1,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
e[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_St_e[ix];}

Formura_subroutine_9(2,154,-1,0,-2,-1,0,-1,0,Rsc1,0);

Formura_subroutine_8(2,154,-1,0,-1,0,-2,-1,0,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_70_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(154)]=Ridge_0_Om_70_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_71_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(154)]=Ridge_0_Om_71_r__1_r__0[ix];}

Formura_subroutine_13(2,156,-2,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc1,-2,Rsc1,-1,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_72_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(154)]=Ridge_0_Om_72_r__1_r__0[ix];}

Formura_subroutine_7(2,156,-2,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc2,-2,Rsc2,-1,Rsc6,0);

Formura_subroutine_6(2,156,-2,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc2,-2,Rsc2,-1,Rsc2,0,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_147_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(156)]=Ridge_0_Om_147_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_148_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(156)]=Ridge_0_Om_148_r__1_r__0[ix];}

Formura_subroutine_12(2,158,Rsc3,-2,Rsc3,-1,Rsc3,0,Rsc6,-1,Rsc6,0,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_149_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(156)]=Ridge_0_Om_149_r__1_r__0[ix];}

Formura_subroutine_0(2,158,Rsc3,-1,Rsc3,0,Rsc6,-2,Rsc6,-1,Rsc6,0,Rsc5,-1,Rsc5,0,Rsc1,0);

Formura_subroutine_5(2,158,Rsc3,-1,Rsc3,0,Rsc6,-1,Rsc6,0,Rsc5,-2,Rsc5,-1,Rsc5,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_150_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(158)]=Ridge_0_Om_150_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_151_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(158)]=Ridge_0_Om_151_r__1_r__0[ix];}

Formura_subroutine_10(2,160,Rsc3,-2,Rsc2,-2,Rsc2,-1,Rsc2,0,Rsc1,-2,Rsc1,-1,Rsc4,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_152_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(158)]=Ridge_0_Om_152_r__1_r__0[ix];}

Formura_subroutine_1(2,160,Rsc6,-2,Rsc2,-2,Rsc2,-1,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc0,-2,Rsc0,-1,Rsc3,0);

Formura_subroutine_3(2,160,Rsc5,-2,Rsc2,-2,Rsc2,-1,Rsc1,-2,Rsc1,-1,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc6,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_227_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(160)]=Ridge_0_Om_227_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_228_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(160)]=Ridge_0_Om_228_r__1_r__0[ix];}

Formura_subroutine_12(2,162,Rsc4,-2,Rsc4,-1,Rsc4,0,Rsc3,-1,Rsc3,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_229_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc6[ix+(160)]=Ridge_0_Om_229_r__1_r__0[ix];}

Formura_subroutine_0(2,162,Rsc4,-1,Rsc4,0,Rsc3,-2,Rsc3,-1,Rsc3,0,Rsc6,-1,Rsc6,0,Rsc1,0);

Formura_subroutine_5(2,162,Rsc4,-1,Rsc4,0,Rsc3,-1,Rsc3,0,Rsc6,-2,Rsc6,-1,Rsc6,0,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_230_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(162)]=Ridge_0_Om_230_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_231_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(162)]=Ridge_0_Om_231_r__1_r__0[ix];}

Formura_subroutine_10(2,164,Rsc4,-2,Rsc0,-2,Rsc0,-1,Rsc0,0,Rsc1,-2,Rsc1,-1,Rsc5,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_232_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(162)]=Ridge_0_Om_232_r__1_r__0[ix];}

Formura_subroutine_1(2,164,Rsc3,-2,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc1,0,Rsc2,-2,Rsc2,-1,Rsc4,0);

Formura_subroutine_3(2,164,Rsc6,-2,Rsc0,-2,Rsc0,-1,Rsc1,-2,Rsc1,-1,Rsc2,-2,Rsc2,-1,Rsc2,0,Rsc3,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_307_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc5[ix+(164)]=Ridge_0_Om_307_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_308_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc4[ix+(164)]=Ridge_0_Om_308_r__1_r__0[ix];}

Formura_subroutine_12(2,166,Rsc5,-2,Rsc5,-1,Rsc5,0,Rsc4,-1,Rsc4,0,Rsc2,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_309_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc3[ix+(164)]=Ridge_0_Om_309_r__1_r__0[ix];}

Formura_subroutine_0(2,166,Rsc5,-1,Rsc5,0,Rsc4,-2,Rsc4,-1,Rsc4,0,Rsc3,-1,Rsc3,0,Rsc1,0);

Formura_subroutine_5(2,166,Rsc5,-1,Rsc5,0,Rsc4,-1,Rsc4,0,Rsc3,-2,Rsc3,-1,Rsc3,0,Rsc0,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_310_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc2[ix+(166)]=Ridge_0_Om_310_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_311_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc1[ix+(166)]=Ridge_0_Om_311_r__1_r__0[ix];}

Formura_subroutine_11(-6,160,Rsc5,6,Rsc2,6,Rsc2,7,Rsc2,8,Rsc1,6,Rsc1,7,8,0);


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(0)]=Facet_m1_src__1_dest__0_Recv.Ridge_m1_Om_312_r__1_r__0[ix];}


for (int  ix = 0 ; ix < 2 ;++ ix ){
Rsc0[ix+(166)]=Ridge_0_Om_312_r__1_r__0[ix];}

Formura_subroutine_2(-6,160,Rsc4,6,Rsc2,6,Rsc2,7,Rsc1,6,Rsc1,7,Rsc1,8,Rsc0,6,Rsc0,7,8,0);

Formura_subroutine_4(-6,160,Rsc3,6,Rsc2,6,Rsc2,7,Rsc1,6,Rsc1,7,Rsc0,6,Rsc0,7,Rsc0,8,8,0);

}
void Formura_subroutine_0(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,rsc_surface * __restrict  argx14,int argx15,rsc_surface * __restrict  argx16,int argx17){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=argx14[ix+(argx15)]/*True*/;
   double  a_6=(1.4-1.0)/*True*/;
 double  a_7=(1.4-1.0)/*True*/;
 double  a_8=(a_6*a_4)/*True*/;
 double  a_9=(a_7*a_5)/*True*/;
   double  a_10=(3.0-1.4)/*True*/;
 double  a_11=(3.0-1.4)/*True*/;
   double  a_12=pow(a_2,2.0)/*True*/;
 double  a_13=pow(a_3,2.0)/*True*/;
 double  a_14=(a_10*a_12)/*True*/;
 double  a_15=(a_11*a_13)/*True*/;
   double  a_16=(2.0*a)/*True*/;
 double  a_17=(2.0*a_0)/*True*/;
 double  a_18=(a_14/a_16)/*True*/;
 double  a_19=(a_15/a_17)/*True*/;
 double  a_20=(a_8+a_18)/*True*/;
 double  a_21=(a_9+a_19)/*True*/;
 double  a_22=(0.3/1.0)/*True*/;
 double  a_23=(a_21-a_20)/*True*/;
 double  a_24=(0.2*1.0)/*True*/;
  double  a_25=(2.0*a_2)/*True*/;
 double  a_26=(a_3-a_25)/*True*/;
 double  a_27=(a_26+a_1)/*True*/;
 double  a_28=(a_24*a_27)/*True*/;
 double  a_29=(-a_28)/*True*/;
 double  a_30=(a_23+a_29)/*True*/;
 double  a_31=(a_22*a_30)/*True*/;
 
argx16[ix+(argx17)]=(a_2-a_31);

}
}void Formura_subroutine_1(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,rsc_surface * __restrict  argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,rsc_surface * __restrict  argx18,int argx19){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=argx14[ix+(argx15)]/*True*/;
 double  a_6=argx16[ix+(argx17)]/*True*/;
   double  a_7=(1.4-1.0)/*True*/;
 double  a_8=(1.4-1.0)/*True*/;
 double  a_9=(a_7*a_5)/*True*/;
 double  a_10=(a_8*a_6)/*True*/;
   double  a_11=(3.0-1.4)/*True*/;
 double  a_12=(3.0-1.4)/*True*/;
   double  a_13=pow(a_2,2.0)/*True*/;
 double  a_14=pow(a_3,2.0)/*True*/;
 double  a_15=(a_11*a_13)/*True*/;
 double  a_16=(a_12*a_14)/*True*/;
   double  a_17=(2.0*a_0)/*True*/;
 double  a_18=(2.0*a_1)/*True*/;
 double  a_19=(a_15/a_17)/*True*/;
 double  a_20=(a_16/a_18)/*True*/;
 double  a_21=(a_9+a_19)/*True*/;
 double  a_22=(a_10+a_20)/*True*/;
 double  a_23=(a+a_3)/*True*/;
  double  a_24=(a_23/2.0)/*True*/;
 double  a_25=(0.3/1.0)/*True*/;
  double  a_26=(a_25/2.0)/*True*/;
 double  a_27=(a_22-a_21)/*True*/;
 double  a_28=(0.2*1.0)/*True*/;
  double  a_29=(2.0*a_3)/*True*/;
 double  a_30=(a_4-a_29)/*True*/;
 double  a_31=(a_30+a_2)/*True*/;
 double  a_32=(a_28*a_31)/*True*/;
 double  a_33=(-a_32)/*True*/;
 double  a_34=(a_27+a_33)/*True*/;
 double  a_35=(a_26*a_34)/*True*/;
 double  a_36=(a_24-a_35)/*True*/;
 
argx18[ix+(argx19)]=(+a_36);

}
}void Formura_subroutine_10(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,rsc_surface * __restrict  argx14,int argx15){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
   double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=(a+a_1)/*True*/;
  double  a_6=(a_5/2.0)/*True*/;
 double  a_7=(0.3/1.0)/*True*/;
  double  a_8=(a_7/2.0)/*True*/;
 double  a_9=(a_4-a_3)/*True*/;
 double  a_10=(0.2*1.0)/*True*/;
  double  a_11=(2.0*a_1)/*True*/;
 double  a_12=(a_2-a_11)/*True*/;
 double  a_13=(a_12+a_0)/*True*/;
 double  a_14=(a_10*a_13)/*True*/;
 double  a_15=(-a_14)/*True*/;
 double  a_16=(a_9+a_15)/*True*/;
 double  a_17=(a_8*a_16)/*True*/;
 double  a_18=(a_6-a_17)/*True*/;
 
argx14[ix+(argx15)]=(+a_18);

}
}void Formura_subroutine_11(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,int argx14,int argx15){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
   double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=(a+a_1)/*True*/;
  double  a_6=(a_5/2.0)/*True*/;
 double  a_7=(0.3/1.0)/*True*/;
  double  a_8=(a_7/2.0)/*True*/;
 double  a_9=(a_4-a_3)/*True*/;
 double  a_10=(0.2*1.0)/*True*/;
  double  a_11=(2.0*a_1)/*True*/;
 double  a_12=(a_2-a_11)/*True*/;
 double  a_13=(a_12+a_0)/*True*/;
 double  a_14=(a_10*a_13)/*True*/;
 double  a_15=(-a_14)/*True*/;
 double  a_16=(a_9+a_15)/*True*/;
 double  a_17=(a_8*a_16)/*True*/;
 double  a_18=(a_6-a_17)/*True*/;
 
r[ix+(argx14)+(argx15)]=a_18;

}
}void Formura_subroutine_12(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
   double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=(0.3/1.0)/*True*/;
 double  a_5=(a_3-a_2)/*True*/;
 double  a_6=(0.2*1.0)/*True*/;
  double  a_7=(2.0*a_0)/*True*/;
 double  a_8=(a_1-a_7)/*True*/;
 double  a_9=(a_8+a)/*True*/;
 double  a_10=(a_6*a_9)/*True*/;
 double  a_11=(-a_10)/*True*/;
 double  a_12=(a_5+a_11)/*True*/;
 double  a_13=(a_4*a_12)/*True*/;
 
argx12[ix+(argx13)]=(a_0-a_13);

}
}void Formura_subroutine_13(int argx0,int argx1,int argx2,rsc_surface * __restrict  argx3,int argx4,rsc_surface * __restrict  argx5,int argx6,rsc_surface * __restrict  argx7,int argx8,rsc_surface * __restrict  argx9,int argx10,rsc_surface * __restrict  argx11,int argx12,rsc_surface * __restrict  argx13,int argx14){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
   double  a=r[ix+(argx2)]/*True*/;
 double  a_0=argx3[ix+(argx4)]/*True*/;
 double  a_1=argx5[ix+(argx6)]/*True*/;
 double  a_2=argx7[ix+(argx8)]/*True*/;
 double  a_3=argx9[ix+(argx10)]/*True*/;
 double  a_4=argx11[ix+(argx12)]/*True*/;
 double  a_5=(a+a_1)/*True*/;
  double  a_6=(a_5/2.0)/*True*/;
 double  a_7=(0.3/1.0)/*True*/;
  double  a_8=(a_7/2.0)/*True*/;
 double  a_9=(a_4-a_3)/*True*/;
 double  a_10=(0.2*1.0)/*True*/;
  double  a_11=(2.0*a_1)/*True*/;
 double  a_12=(a_2-a_11)/*True*/;
 double  a_13=(a_12+a_0)/*True*/;
 double  a_14=(a_10*a_13)/*True*/;
 double  a_15=(-a_14)/*True*/;
 double  a_16=(a_9+a_15)/*True*/;
 double  a_17=(a_8*a_16)/*True*/;
 double  a_18=(a_6-a_17)/*True*/;
 
argx13[ix+(argx14)]=(+a_18);

}
}void Formura_subroutine_14(int argx0,int argx1,int argx2,int argx3,int argx4,int argx5,int argx6,rsc_surface * __restrict  argx7,int argx8){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
   double  a=r[ix+(argx2)]/*True*/;
 double  a_0=r[ix+(argx3)]/*True*/;
 double  a_1=r[ix+(argx4)]/*True*/;
 double  a_2=m[ix+(argx5)]/*True*/;
 double  a_3=m[ix+(argx6)]/*True*/;
 double  a_4=(0.3/1.0)/*True*/;
 double  a_5=(a_3-a_2)/*True*/;
 double  a_6=(0.2*1.0)/*True*/;
  double  a_7=(2.0*a_0)/*True*/;
 double  a_8=(a_1-a_7)/*True*/;
 double  a_9=(a_8+a)/*True*/;
 double  a_10=(a_6*a_9)/*True*/;
 double  a_11=(-a_10)/*True*/;
 double  a_12=(a_5+a_11)/*True*/;
 double  a_13=(a_4*a_12)/*True*/;
 
argx7[ix+(argx8)]=(a_0-a_13);

}
}void Formura_subroutine_2(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,rsc_surface * __restrict  argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,int argx18,int argx19){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=argx14[ix+(argx15)]/*True*/;
 double  a_6=argx16[ix+(argx17)]/*True*/;
   double  a_7=(1.4-1.0)/*True*/;
 double  a_8=(1.4-1.0)/*True*/;
 double  a_9=(a_7*a_5)/*True*/;
 double  a_10=(a_8*a_6)/*True*/;
   double  a_11=(3.0-1.4)/*True*/;
 double  a_12=(3.0-1.4)/*True*/;
   double  a_13=pow(a_2,2.0)/*True*/;
 double  a_14=pow(a_3,2.0)/*True*/;
 double  a_15=(a_11*a_13)/*True*/;
 double  a_16=(a_12*a_14)/*True*/;
   double  a_17=(2.0*a_0)/*True*/;
 double  a_18=(2.0*a_1)/*True*/;
 double  a_19=(a_15/a_17)/*True*/;
 double  a_20=(a_16/a_18)/*True*/;
 double  a_21=(a_9+a_19)/*True*/;
 double  a_22=(a_10+a_20)/*True*/;
 double  a_23=(a+a_3)/*True*/;
  double  a_24=(a_23/2.0)/*True*/;
 double  a_25=(0.3/1.0)/*True*/;
  double  a_26=(a_25/2.0)/*True*/;
 double  a_27=(a_22-a_21)/*True*/;
 double  a_28=(0.2*1.0)/*True*/;
  double  a_29=(2.0*a_3)/*True*/;
 double  a_30=(a_4-a_29)/*True*/;
 double  a_31=(a_30+a_2)/*True*/;
 double  a_32=(a_28*a_31)/*True*/;
 double  a_33=(-a_32)/*True*/;
 double  a_34=(a_27+a_33)/*True*/;
 double  a_35=(a_26*a_34)/*True*/;
 double  a_36=(a_24-a_35)/*True*/;
 
m[ix+(argx18)+(argx19)]=a_36;

}
}void Formura_subroutine_3(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,rsc_surface * __restrict  argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,rsc_surface * __restrict  argx18,int argx19){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=argx14[ix+(argx15)]/*True*/;
 double  a_6=argx16[ix+(argx17)]/*True*/;
 double  a_7=(1.4*a_4)/*True*/;
 double  a_8=(1.4*a_5)/*True*/;
 double  a_9=(a_7*a_2)/*True*/;
 double  a_10=(a_8*a_3)/*True*/;
 double  a_11=(a_9/a_0)/*True*/;
 double  a_12=(a_10/a_1)/*True*/;
   double  a_13=(1.4-1.0)/*True*/;
 double  a_14=(1.4-1.0)/*True*/;
   double  a_15=pow(a_2,3.0)/*True*/;
 double  a_16=pow(a_3,3.0)/*True*/;
 double  a_17=(a_13*a_15)/*True*/;
 double  a_18=(a_14*a_16)/*True*/;
     double  a_19=pow(a_0,2.0)/*True*/;
 double  a_20=pow(a_1,2.0)/*True*/;
 double  a_21=(2.0*a_19)/*True*/;
 double  a_22=(2.0*a_20)/*True*/;
 double  a_23=(a_17/a_21)/*True*/;
 double  a_24=(a_18/a_22)/*True*/;
 double  a_25=(a_11-a_23)/*True*/;
 double  a_26=(a_12-a_24)/*True*/;
 double  a_27=(a+a_5)/*True*/;
  double  a_28=(a_27/2.0)/*True*/;
 double  a_29=(0.3/1.0)/*True*/;
  double  a_30=(a_29/2.0)/*True*/;
 double  a_31=(a_26-a_25)/*True*/;
 double  a_32=(0.2*1.0)/*True*/;
  double  a_33=(2.0*a_5)/*True*/;
 double  a_34=(a_6-a_33)/*True*/;
 double  a_35=(a_34+a_4)/*True*/;
 double  a_36=(a_32*a_35)/*True*/;
 double  a_37=(-a_36)/*True*/;
 double  a_38=(a_31+a_37)/*True*/;
 double  a_39=(a_30*a_38)/*True*/;
 double  a_40=(a_28-a_39)/*True*/;
 
argx18[ix+(argx19)]=(+a_40);

}
}void Formura_subroutine_4(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,rsc_surface * __restrict  argx14,int argx15,rsc_surface * __restrict  argx16,int argx17,int argx18,int argx19){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=argx14[ix+(argx15)]/*True*/;
 double  a_6=argx16[ix+(argx17)]/*True*/;
 double  a_7=(1.4*a_4)/*True*/;
 double  a_8=(1.4*a_5)/*True*/;
 double  a_9=(a_7*a_2)/*True*/;
 double  a_10=(a_8*a_3)/*True*/;
 double  a_11=(a_9/a_0)/*True*/;
 double  a_12=(a_10/a_1)/*True*/;
   double  a_13=(1.4-1.0)/*True*/;
 double  a_14=(1.4-1.0)/*True*/;
   double  a_15=pow(a_2,3.0)/*True*/;
 double  a_16=pow(a_3,3.0)/*True*/;
 double  a_17=(a_13*a_15)/*True*/;
 double  a_18=(a_14*a_16)/*True*/;
     double  a_19=pow(a_0,2.0)/*True*/;
 double  a_20=pow(a_1,2.0)/*True*/;
 double  a_21=(2.0*a_19)/*True*/;
 double  a_22=(2.0*a_20)/*True*/;
 double  a_23=(a_17/a_21)/*True*/;
 double  a_24=(a_18/a_22)/*True*/;
 double  a_25=(a_11-a_23)/*True*/;
 double  a_26=(a_12-a_24)/*True*/;
 double  a_27=(a+a_5)/*True*/;
  double  a_28=(a_27/2.0)/*True*/;
 double  a_29=(0.3/1.0)/*True*/;
  double  a_30=(a_29/2.0)/*True*/;
 double  a_31=(a_26-a_25)/*True*/;
 double  a_32=(0.2*1.0)/*True*/;
  double  a_33=(2.0*a_5)/*True*/;
 double  a_34=(a_6-a_33)/*True*/;
 double  a_35=(a_34+a_4)/*True*/;
 double  a_36=(a_32*a_35)/*True*/;
 double  a_37=(-a_36)/*True*/;
 double  a_38=(a_31+a_37)/*True*/;
 double  a_39=(a_30*a_38)/*True*/;
 double  a_40=(a_28-a_39)/*True*/;
 
e[ix+(argx18)+(argx19)]=a_40;

}
}void Formura_subroutine_5(int argx0,int argx1,rsc_surface * __restrict  argx2,int argx3,rsc_surface * __restrict  argx4,int argx5,rsc_surface * __restrict  argx6,int argx7,rsc_surface * __restrict  argx8,int argx9,rsc_surface * __restrict  argx10,int argx11,rsc_surface * __restrict  argx12,int argx13,rsc_surface * __restrict  argx14,int argx15,rsc_surface * __restrict  argx16,int argx17){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=argx2[ix+(argx3)]/*True*/;
 double  a_0=argx4[ix+(argx5)]/*True*/;
 double  a_1=argx6[ix+(argx7)]/*True*/;
 double  a_2=argx8[ix+(argx9)]/*True*/;
 double  a_3=argx10[ix+(argx11)]/*True*/;
 double  a_4=argx12[ix+(argx13)]/*True*/;
 double  a_5=argx14[ix+(argx15)]/*True*/;
 double  a_6=(1.4*a_4)/*True*/;
 double  a_7=(1.4*a_5)/*True*/;
 double  a_8=(a_6*a_1)/*True*/;
 double  a_9=(a_7*a_2)/*True*/;
 double  a_10=(a_8/a)/*True*/;
 double  a_11=(a_9/a_0)/*True*/;
   double  a_12=(1.4-1.0)/*True*/;
 double  a_13=(1.4-1.0)/*True*/;
   double  a_14=pow(a_1,3.0)/*True*/;
 double  a_15=pow(a_2,3.0)/*True*/;
 double  a_16=(a_12*a_14)/*True*/;
 double  a_17=(a_13*a_15)/*True*/;
     double  a_18=pow(a,2.0)/*True*/;
 double  a_19=pow(a_0,2.0)/*True*/;
 double  a_20=(2.0*a_18)/*True*/;
 double  a_21=(2.0*a_19)/*True*/;
 double  a_22=(a_16/a_20)/*True*/;
 double  a_23=(a_17/a_21)/*True*/;
 double  a_24=(a_10-a_22)/*True*/;
 double  a_25=(a_11-a_23)/*True*/;
 double  a_26=(0.3/1.0)/*True*/;
 double  a_27=(a_25-a_24)/*True*/;
 double  a_28=(0.2*1.0)/*True*/;
  double  a_29=(2.0*a_4)/*True*/;
 double  a_30=(a_5-a_29)/*True*/;
 double  a_31=(a_30+a_3)/*True*/;
 double  a_32=(a_28*a_31)/*True*/;
 double  a_33=(-a_32)/*True*/;
 double  a_34=(a_27+a_33)/*True*/;
 double  a_35=(a_26*a_34)/*True*/;
 
argx16[ix+(argx17)]=(a_4-a_35);

}
}void Formura_subroutine_6(int argx0,int argx1,int argx2,rsc_surface * __restrict  argx3,int argx4,rsc_surface * __restrict  argx5,int argx6,rsc_surface * __restrict  argx7,int argx8,rsc_surface * __restrict  argx9,int argx10,rsc_surface * __restrict  argx11,int argx12,rsc_surface * __restrict  argx13,int argx14,rsc_surface * __restrict  argx15,int argx16,rsc_surface * __restrict  argx17,int argx18){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=e[ix+(argx2)]/*True*/;
 double  a_0=argx3[ix+(argx4)]/*True*/;
 double  a_1=argx5[ix+(argx6)]/*True*/;
 double  a_2=argx7[ix+(argx8)]/*True*/;
 double  a_3=argx9[ix+(argx10)]/*True*/;
 double  a_4=argx11[ix+(argx12)]/*True*/;
 double  a_5=argx13[ix+(argx14)]/*True*/;
 double  a_6=argx15[ix+(argx16)]/*True*/;
 double  a_7=(1.4*a_4)/*True*/;
 double  a_8=(1.4*a_5)/*True*/;
 double  a_9=(a_7*a_2)/*True*/;
 double  a_10=(a_8*a_3)/*True*/;
 double  a_11=(a_9/a_0)/*True*/;
 double  a_12=(a_10/a_1)/*True*/;
   double  a_13=(1.4-1.0)/*True*/;
 double  a_14=(1.4-1.0)/*True*/;
   double  a_15=pow(a_2,3.0)/*True*/;
 double  a_16=pow(a_3,3.0)/*True*/;
 double  a_17=(a_13*a_15)/*True*/;
 double  a_18=(a_14*a_16)/*True*/;
     double  a_19=pow(a_0,2.0)/*True*/;
 double  a_20=pow(a_1,2.0)/*True*/;
 double  a_21=(2.0*a_19)/*True*/;
 double  a_22=(2.0*a_20)/*True*/;
 double  a_23=(a_17/a_21)/*True*/;
 double  a_24=(a_18/a_22)/*True*/;
 double  a_25=(a_11-a_23)/*True*/;
 double  a_26=(a_12-a_24)/*True*/;
 double  a_27=(a+a_5)/*True*/;
  double  a_28=(a_27/2.0)/*True*/;
 double  a_29=(0.3/1.0)/*True*/;
  double  a_30=(a_29/2.0)/*True*/;
 double  a_31=(a_26-a_25)/*True*/;
 double  a_32=(0.2*1.0)/*True*/;
  double  a_33=(2.0*a_5)/*True*/;
 double  a_34=(a_6-a_33)/*True*/;
 double  a_35=(a_34+a_4)/*True*/;
 double  a_36=(a_32*a_35)/*True*/;
 double  a_37=(-a_36)/*True*/;
 double  a_38=(a_31+a_37)/*True*/;
 double  a_39=(a_30*a_38)/*True*/;
 double  a_40=(a_28-a_39)/*True*/;
 
argx17[ix+(argx18)]=(+a_40);

}
}void Formura_subroutine_7(int argx0,int argx1,int argx2,rsc_surface * __restrict  argx3,int argx4,rsc_surface * __restrict  argx5,int argx6,rsc_surface * __restrict  argx7,int argx8,rsc_surface * __restrict  argx9,int argx10,rsc_surface * __restrict  argx11,int argx12,rsc_surface * __restrict  argx13,int argx14,rsc_surface * __restrict  argx15,int argx16,rsc_surface * __restrict  argx17,int argx18){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=m[ix+(argx2)]/*True*/;
 double  a_0=argx3[ix+(argx4)]/*True*/;
 double  a_1=argx5[ix+(argx6)]/*True*/;
 double  a_2=argx7[ix+(argx8)]/*True*/;
 double  a_3=argx9[ix+(argx10)]/*True*/;
 double  a_4=argx11[ix+(argx12)]/*True*/;
 double  a_5=argx13[ix+(argx14)]/*True*/;
 double  a_6=argx15[ix+(argx16)]/*True*/;
   double  a_7=(1.4-1.0)/*True*/;
 double  a_8=(1.4-1.0)/*True*/;
 double  a_9=(a_7*a_5)/*True*/;
 double  a_10=(a_8*a_6)/*True*/;
   double  a_11=(3.0-1.4)/*True*/;
 double  a_12=(3.0-1.4)/*True*/;
   double  a_13=pow(a_2,2.0)/*True*/;
 double  a_14=pow(a_3,2.0)/*True*/;
 double  a_15=(a_11*a_13)/*True*/;
 double  a_16=(a_12*a_14)/*True*/;
   double  a_17=(2.0*a_0)/*True*/;
 double  a_18=(2.0*a_1)/*True*/;
 double  a_19=(a_15/a_17)/*True*/;
 double  a_20=(a_16/a_18)/*True*/;
 double  a_21=(a_9+a_19)/*True*/;
 double  a_22=(a_10+a_20)/*True*/;
 double  a_23=(a+a_3)/*True*/;
  double  a_24=(a_23/2.0)/*True*/;
 double  a_25=(0.3/1.0)/*True*/;
  double  a_26=(a_25/2.0)/*True*/;
 double  a_27=(a_22-a_21)/*True*/;
 double  a_28=(0.2*1.0)/*True*/;
  double  a_29=(2.0*a_3)/*True*/;
 double  a_30=(a_4-a_29)/*True*/;
 double  a_31=(a_30+a_2)/*True*/;
 double  a_32=(a_28*a_31)/*True*/;
 double  a_33=(-a_32)/*True*/;
 double  a_34=(a_27+a_33)/*True*/;
 double  a_35=(a_26*a_34)/*True*/;
 double  a_36=(a_24-a_35)/*True*/;
 
argx17[ix+(argx18)]=(+a_36);

}
}void Formura_subroutine_8(int argx0,int argx1,int argx2,int argx3,int argx4,int argx5,int argx6,int argx7,int argx8,rsc_surface * __restrict  argx9,int argx10){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=r[ix+(argx2)]/*True*/;
 double  a_0=r[ix+(argx3)]/*True*/;
 double  a_1=m[ix+(argx4)]/*True*/;
 double  a_2=m[ix+(argx5)]/*True*/;
 double  a_3=e[ix+(argx6)]/*True*/;
 double  a_4=e[ix+(argx7)]/*True*/;
 double  a_5=e[ix+(argx8)]/*True*/;
 double  a_6=(1.4*a_4)/*True*/;
 double  a_7=(1.4*a_5)/*True*/;
 double  a_8=(a_6*a_1)/*True*/;
 double  a_9=(a_7*a_2)/*True*/;
 double  a_10=(a_8/a)/*True*/;
 double  a_11=(a_9/a_0)/*True*/;
   double  a_12=(1.4-1.0)/*True*/;
 double  a_13=(1.4-1.0)/*True*/;
   double  a_14=pow(a_1,3.0)/*True*/;
 double  a_15=pow(a_2,3.0)/*True*/;
 double  a_16=(a_12*a_14)/*True*/;
 double  a_17=(a_13*a_15)/*True*/;
     double  a_18=pow(a,2.0)/*True*/;
 double  a_19=pow(a_0,2.0)/*True*/;
 double  a_20=(2.0*a_18)/*True*/;
 double  a_21=(2.0*a_19)/*True*/;
 double  a_22=(a_16/a_20)/*True*/;
 double  a_23=(a_17/a_21)/*True*/;
 double  a_24=(a_10-a_22)/*True*/;
 double  a_25=(a_11-a_23)/*True*/;
 double  a_26=(0.3/1.0)/*True*/;
 double  a_27=(a_25-a_24)/*True*/;
 double  a_28=(0.2*1.0)/*True*/;
  double  a_29=(2.0*a_4)/*True*/;
 double  a_30=(a_5-a_29)/*True*/;
 double  a_31=(a_30+a_3)/*True*/;
 double  a_32=(a_28*a_31)/*True*/;
 double  a_33=(-a_32)/*True*/;
 double  a_34=(a_27+a_33)/*True*/;
 double  a_35=(a_26*a_34)/*True*/;
 
argx9[ix+(argx10)]=(a_4-a_35);

}
}void Formura_subroutine_9(int argx0,int argx1,int argx2,int argx3,int argx4,int argx5,int argx6,int argx7,int argx8,rsc_surface * __restrict  argx9,int argx10){
for (int  ix = argx0 ; ix < argx1 ; ix += 1 ){
     double  a=r[ix+(argx2)]/*True*/;
 double  a_0=r[ix+(argx3)]/*True*/;
 double  a_1=m[ix+(argx4)]/*True*/;
 double  a_2=m[ix+(argx5)]/*True*/;
 double  a_3=m[ix+(argx6)]/*True*/;
 double  a_4=e[ix+(argx7)]/*True*/;
 double  a_5=e[ix+(argx8)]/*True*/;
   double  a_6=(1.4-1.0)/*True*/;
 double  a_7=(1.4-1.0)/*True*/;
 double  a_8=(a_6*a_4)/*True*/;
 double  a_9=(a_7*a_5)/*True*/;
   double  a_10=(3.0-1.4)/*True*/;
 double  a_11=(3.0-1.4)/*True*/;
   double  a_12=pow(a_2,2.0)/*True*/;
 double  a_13=pow(a_3,2.0)/*True*/;
 double  a_14=(a_10*a_12)/*True*/;
 double  a_15=(a_11*a_13)/*True*/;
   double  a_16=(2.0*a)/*True*/;
 double  a_17=(2.0*a_0)/*True*/;
 double  a_18=(a_14/a_16)/*True*/;
 double  a_19=(a_15/a_17)/*True*/;
 double  a_20=(a_8+a_18)/*True*/;
 double  a_21=(a_9+a_19)/*True*/;
 double  a_22=(0.3/1.0)/*True*/;
 double  a_23=(a_21-a_20)/*True*/;
 double  a_24=(0.2*1.0)/*True*/;
  double  a_25=(2.0*a_2)/*True*/;
 double  a_26=(a_3-a_25)/*True*/;
 double  a_27=(a_26+a_1)/*True*/;
 double  a_28=(a_24*a_27)/*True*/;
 double  a_29=(-a_28)/*True*/;
 double  a_30=(a_23+a_29)/*True*/;
 double  a_31=(a_22*a_30)/*True*/;
 
argx9[ix+(argx10)]=(a_2-a_31);

}
}