#include <stdio.h>
#include <math.h>
#include <mpi.h>
#include "diffusion1.h"

double dx = SPACE_INTERVAL_X;

// グローバル配列の宣言
double q[NX*MX];

void Formura_Decode_rank(int p, int *px) {
  *px = (int)p%PX;
}

int Formura_Encode_rank(int px) {
  int x = (px + PX)%PX;
  return x;
}

void Formura_Init(Formura_Navi *n, MPI_Comm comm) {
  int size, rank;
  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  if (size != P0) {
    printf("WARNING: inconsistent mpi size\n");
  }

  int ix;
  Formura_Decode_rank(rank, &ix);

  n->my_rank = Formura_Encode_rank(ix);
  n->rank_p1 = Formura_Encode_rank(ix+1);
  n->rank_m1 = Formura_Encode_rank(ix-1);
  n->mpi_world = comm;

  n->time_step = 0;

  n->lower_x = 2*DX;
  n->upper_x = NX*MX;
  n->offset_x = ix*LX - 2*DX;
  n->length_x = dx*LX*PX;


  printf("Formura_Init\n");
  printf("  rank = %d/%d\n", rank, size);
  printf("  p0 = %d\n", p0);
  printf("  ix = %d\n", ix);
  printf("  my_rank = %d\n", n->my_rank);
  printf("  rank_p1 = %d\n", n->rank_p1);
  printf("  rank_m1 = %d\n", n->rank_m1);
}

double to_pos_x(int ix, Formura_Navi n) {
  int WX = PX*LX;
  int d = (Ns*n.time_step)%WX;
  return dx*((ix+n.offset_x-d+WX)%WX);
}

// 1タイムステップ更新
typedef struct {
  double q[NX+2*Ns];
} Formura_Buff;

typedef struct {
  double q[NX];
} Formura_Rslt;

void Formura_Step(Formura_Buff *buff, Formura_Rslt *rslt) {
for(int ix = 0; ix < NX + 2*(Ns - 1); ix++) {
double a0 = buff->q[ix+1];
double a1 = 0.1;
double a2 = 1.0e-2;
double a3 = a1*a2;
double a4 = a3*a2;
double a5 = buff->q[ix+2];
double a6 = buff->q[ix+0];
double a7 = a5+a6;
double a8 = 2.0;
double a9 = a8*a0;
double a10 = a7-a9;
double a11 = 2.0;
double a12 = pow(a2,a11);
double a13 = a10/a12;
double a14 = a4*a13;
double a15 = a0+a14;
rslt->q[ix+0] = a15;
}


}

typedef struct {
  int x_origin;

  double q_wall_x[NT][2*Ns];
  double q_res[NX];
} Formura_Param;

// NTタイムステップ更新
void Formura_Update(Formura_Param *p) {
  Formura_Buff buff;
  Formura_Rslt rslt;

  // 床を読む
  int ix0 = p->x_origin;
  for(int ix = 0; ix < NX; ix++) {
    rslt.q[ix] = q[ix0+ix];
  }

  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < NX; ix++) {
      buff.q[ix] = rslt.q[ix];
    }
    // x壁を読む
    for(int ix = 0; ix < 2*Ns; ix++) {
      buff.q[ix+NX] = p->q_wall_x[it][ix];
    }

    Formura_Step(&buff, &rslt);

    // x壁を書く
    for(int ix = 0; ix < 2*Ns; ix++) {
      p->q_wall_x[it][ix] = buff.q[ix];
    }
  }

  // 床を書く
  for(int ix = 0; ix < NX; ix++) {
    p->q_res[ix] = rslt.q[ix];
  }
}

double q_tmp[MX*NX];
void Formura_Forward(Formura_Navi *n) {
  Formura_Param p;
  
  // 床の準備
  double q_send_buf_x[2*DX][1];
  double q_recv_buf_x[2*DX][1];

  for(int ix = 0; ix < 2*DX; ix++) {
    q_send_buf_x[ix][0] = q[ix+LX];
  }

  MPI_Request send_req_x, recv_req_x;
  MPI_Isend(q_send_buf_x, sizeof(q_send_buf_x), MPI_BYTE, n->rank_p1, 0, n->mpi_world, &send_req_x);
  MPI_Irecv(q_recv_buf_x, sizeof(q_recv_buf_x), MPI_BYTE, n->rank_m1, 0, n->mpi_world, &recv_req_x);

  // x壁の初期化
  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < 2*Ns; ix++) {
        p.q_wall_x[it][ix] = 1.0;
    }
  }

  // 床の更新
  for(int jx = MX - 1; jx > 2*DX/NX; jx--) {

    p.x_origin = jx*NX;
    Formura_Update(&p);

    // 床の受けとり
    for(int ix = 0; ix < NX; ix++) {
      q_tmp[ix+jx*NX] = p.q_res[ix];
    }
  }

  // 通信待ち
  MPI_Wait(&send_req_x, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_x, MPI_STATUS_IGNORE);

  for(int ix = 0; ix < 2*DX; ix++) {
    q[ix] = q_recv_buf_x[ix][0];
  }

  // 残りの床を更新
  for(int jx = 2*DX/NX; jx >= 0; jx--) {

    p.x_origin = jx*NX;
    Formura_Update(&p);

    // 床の受けとり
    for(int ix = 0; ix < NX; ix++) {
      q_tmp[ix+jx*NX] = p.q_res[ix];
    }
  }

  // 床の書き出し
  for(int ix = 0; ix < LX; ix++) {
    q[ix+2*DX] = q_tmp[ix];
  }

  n->time_step += NT;
}
