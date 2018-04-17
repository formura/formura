#include <stdio.h>
#include <math.h>
#include <mpi.h>
%{ if with_simd }
#include <x86intrin.h>
%{ endif }
#include "#{hxxFileName}"

double d#{map toLower a1} = SPACE_INTERVAL_#{a1};

// グローバル配列の宣言
%{ forall (q,t) <- qs }
#{t} #{q}#{globalSize};
%{ endforall}

void Formura_Decode_rank(int p, int *px) {
  *px = (int)p%P#{a1};
}

int Formura_Encode_rank(int px) {
  int x = (px + P#{a1})%P#{a1};
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

  n->lower_x = 2*D#{a1};
  n->upper_x = N#{a1}*M#{a1};
  n->offset_x = ix*L#{a1} - 2*D#{a1};
  n->length_x = d#{map toLower a1}*L#{a1}*P#{a1};


  printf("Formura_Init\n");
  printf("  rank = %d/%d\n", rank, size);
  printf("  p0 = %d\n", p0);
  printf("  ix = %d\n", ix);
  printf("  my_rank = %d\n", n->my_rank);
  printf("  rank_p1 = %d\n", n->rank_p1);
  printf("  rank_m1 = %d\n", n->rank_m1);
}

double to_pos_#{map toLower a1}(int ix, Formura_Navi n) {
  int W#{a1} = P#{a1}*L#{a1};
  int d = (Ns*n.time_step)%W#{a1};
  return d#{map toLower a1}*((ix+n.offset_x-d+W#{a1})%W#{a1});
}

// 1タイムステップ更新
typedef struct {
%{ forall (q,t) <- qs }
  #{t} #{q}[N#{a1}+2*Ns];
%{ endforall }
} Formura_Buff;

typedef struct {
%{ forall (q,t) <- qs }
  #{t} #{q}[N#{a1}];
%{ endforall }
} Formura_Rslt;

void Formura_Step(Formura_Buff *buff, Formura_Rslt *rslt) {
#{step}
}

typedef struct {
  int x_origin;

%{ forall (q,t) <- qs }
  #{t} #{q}_wall_x[NT][2*Ns];
  #{t} #{q}_res[N#{a1}];
%{ endforall }
} Formura_Param;

// NTタイムステップ更新
void Formura_Update(Formura_Param *p) {
  Formura_Buff buff;
  Formura_Rslt rslt;

  // 床を読む
  int ix0 = p->x_origin;
  for(int ix = 0; ix < N#{a1}; ix++) {
%{ forall (q,_) <- qs }
    rslt.#{q}[ix] = #{q}[ix0+ix];
%{ endforall }
  }

  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < N#{a1}; ix++) {
%{ forall (q,_) <- qs }
      buff.#{q}[ix] = rslt.#{q}[ix];
%{ endforall }
    }
    // x壁を読む
    for(int ix = 0; ix < 2*Ns; ix++) {
%{ forall (q,_) <- qs }
      buff.#{q}[ix+N#{a1}] = p->#{q}_wall_x[it][ix];
%{ endforall }
    }

    Formura_Step(&buff, &rslt);

    // x壁を書く
    for(int ix = 0; ix < 2*Ns; ix++) {
%{ forall (q,_) <- qs }
      p->#{q}_wall_x[it][ix] = buff.#{q}[ix];
%{ endforall }
    }
  }

  // 床を書く
  for(int ix = 0; ix < N#{a1}; ix++) {
%{ forall (q,_) <- qs }
    p->#{q}_res[ix] = rslt.#{q}[ix];
%{ endforall }
  }
}

%{ forall (q,t) <- qs }
#{t} #{q}_tmp[M#{a1}*N#{a1}];
%{ endforall }
void Formura_Forward(Formura_Navi *n) {
  Formura_Param p;
  
  // 床の準備
  #{snd (head qs)} q_send_buf_x[2*D#{a1}][#{length qs}];
  #{snd (head qs)} q_recv_buf_x[2*D#{a1}][#{length qs}];

  for(int ix = 0; ix < 2*D#{a1}; ix++) {
%{ forall (i,q) <- nqs }
    q_send_buf_x[ix][#{i}] = #{q}[ix+L#{a1}];
%{ endforall }
  }

  MPI_Request send_req_x, recv_req_x;
  MPI_Isend(q_send_buf_x, sizeof(q_send_buf_x), MPI_BYTE, n->rank_p1, 0, n->mpi_world, &send_req_x);
  MPI_Irecv(q_recv_buf_x, sizeof(q_recv_buf_x), MPI_BYTE, n->rank_m1, 0, n->mpi_world, &recv_req_x);

  // x壁の初期化
  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < 2*Ns; ix++) {
%{ forall (q,_) <- qs }
        p.#{q}_wall_x[it][ix] = 1.0;
%{ endforall }
    }
  }

  // 床の更新
  for(int jx = M#{a1} - 1; jx > 2*D#{a1}/N#{a1}; jx--) {

    p.x_origin = jx*N#{a1};
    Formura_Update(&p);

    // 床の受けとり
    for(int ix = 0; ix < N#{a1}; ix++) {
%{ forall (q,_) <- qs }
      #{q}_tmp[ix+jx*N#{a1}] = p.#{q}_res[ix];
%{ endforall }
    }
  }

  // 通信待ち
  MPI_Wait(&send_req_x, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_x, MPI_STATUS_IGNORE);

  for(int ix = 0; ix < 2*D#{a1}; ix++) {
%{ forall (i,q) <- nqs }
    #{q}[ix] = q_recv_buf_x[ix][#{i}];
%{ endforall }
  }

  // 残りの床を更新
  for(int jx = 2*D#{a1}/N#{a1}; jx >= 0; jx--) {

    p.x_origin = jx*N#{a1};
    Formura_Update(&p);

    // 床の受けとり
    for(int ix = 0; ix < N#{a1}; ix++) {
%{ forall (q,_) <- qs }
      #{q}_tmp[ix+jx*N#{a1}] = p.#{q}_res[ix];
%{ endforall }
    }
  }

  // 床の書き出し
  for(int ix = 0; ix < L#{a1}; ix++) {
%{ forall (q,_) <- qs }
    #{q}[ix+2*D#{a1}] = #{q}_tmp[ix];
%{ endforall }
  }

  n->time_step += NT;
}
