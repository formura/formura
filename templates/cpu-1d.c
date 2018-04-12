#include <stdio.h>
#include <math.h>
#include <mpi.h>
%{ if with_simd }
#include <x86intrin.h>
%{ endif }
#include "#{target}.h"

double dx = SPACE_INTERVAL;

// グローバル配列の宣言
%{ forall (t,q) <- qs }
#{t} #{q}[NX*MX];
%{ endforall}

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

  int ix,iy;
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
%{ forall (t,q) <- qs }
  #{t} #{q}[NX+2*Ns];
%{ endforall }
} Formura_Buff;

typedef struct {
%{ forall (t,q) <- qs }
  #{t} #{q}[NX];
%{ endforall }
} Formura_Rslt;

void Formura_Step(Formura_Buff *buff, Formura_Rslt *rslt) {
#{step}
}

typedef struct {
  int x_origin;

%{ forall (t,q) <- qs }
  #{t} #{q}_wall_x[NT][2*Ns];
  #{t} #{q}_res[NX];
%{ endforall }
} Formura_Param;

// NTタイムステップ更新
void Formura_Update(Formura_Param *p) {
  Formura_Buff buff;
  Formura_Rslt rslt;

  // 床を読む
  int ix0 = p->x_origin;
  for(int ix = 0; ix < NX; ix++) {
%{ forall (_,q) <- qs }
    rslt.#{q}[ix] = #{q}[ix0+ix];
%{ endforall }
  }

  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < NX; ix++) {
%{ forall (_,q) <- qs }
      buff.#{q}[ix][iy] = rslt.#{q}[ix][iy];
%{ endforall }
    }
    // x壁を読む
    for(int ix = 0; ix < 2*Ns; ix++) {
%{ forall (_,q) <- qs }
      buff.#{q}[ix+NX] = p->#{q}_wall_x[it][ix];
%{ endforall }
    }

    Formura_Step(&buff, &rslt);

    // x壁を書く
    for(int ix = 0; ix < 2*Ns; ix++) {
%{ forall (_,q) <- qs }
      p->#{q}_wall_x[it][ix] = buff.#{q}[ix];
%{ endforall }
    }
  }

  // 床を書く
  for(int ix = 0; ix < NX; ix++) {
%{ forall (_,q) <- qs }
    p->#{q}_res[ix] = rslt.#{q}[ix];
%{ endforall }
  }
}

%{ forall (_,q) <- qs }
#{t} #{q}_tmp[MX*NX];
%{ endforall }
void Formura_Forward(Formura_Navi *n) {
  Formura_Param p;
  
  // 床の準備
  #{fst (head qs)} q_send_buf_x[2*DX][#{length qs}];
  #{fst (head qs)} q_recv_buf_x[2*DX][#{length qs}];

  for(int ix = 0; ix < 2*DX; ix++) {
%{ forall (i,(_,q)) <- zip [0..] qs }
    q_send_buf_x[ix] = #{q}[ix+LX][#{i}];
%{ endforall }
  }

  MPI_Request send_req_x, recv_req_x;
  MPI_Isend(q_send_buf_x, sizeof(q_send_buf_x), MPI_BYTE, n->rank_p1, 0, n->mpi_world, &send_req_x);
  MPI_Irecv(q_recv_buf_x, sizeof(q_recv_buf_x), MPI_BYTE, n->rank_m1, 0, n->mpi_world, &recv_req_x);

  // x壁の初期化
  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < 2*Ns; ix++) {
%{ forall (_,q) <- qs }
        p.#{q}_wall_x[it][ix] = 1.0;
%{ endforall }
    }
  }

  // 床の更新
  for(int jx = MX - 1; jx > 2*DX/NX; jx--) {

    p.x_origin = jx*NX;
    Formura_Update(&p);

    // 床の受けとり
    for(int ix = 0; ix < NX; ix++) {
%{ forall (_,q) <- qs }
      #{q}_tmp[ix+jx*NX] = p.#{q}_res[ix];
%{ endforall }
    }
  }

  // 通信待ち
  MPI_Wait(&send_req_x, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_x, MPI_STATUS_IGNORE);

  for(int ix = 0; ix < 2*DX; ix++) {
%{ forall (i,(_,q)) <- zip [0..] qs }
    #{q}[ix] = q_recv_buf_x[ix][#{i}];
%{ endforall }
  }

  // 残りの床を更新
  for(int jx = 2*DX/NX; jx >= 0; jx--) {

    p.x_origin = jx*NX;
    Formura_Update(&p);

    // 床の受けとり
    for(int ix = 0; ix < NX; ix++) {
%{ forall (_,q) <- qs }
      #{q}_tmp[ix+jx*NX] = p.#{q}_res[ix];
%{ endforall }
    }
  }

  // 床の書き出し
  for(int ix = 0; ix < LX; ix++) {
%{ forall (_,q) <- qs }
    #{q}[ix+2*DX] = #{q}_tmp[ix];
%{ endforall }
  }

  n->time_step += NT;
}
