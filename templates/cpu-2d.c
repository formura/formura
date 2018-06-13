#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <mpi.h>
%{ if with_simd }
#include <x86intrin.h>
%{ endif }
#include "#{hxxFileName}"

double d#{map toLower a1} = SPACE_INTERVAL_#{a1};
double d#{map toLower a2} = SPACE_INTERVAL_#{a2};

// グローバル配列の宣言
%{ forall (q,t) <- qs }
#{t} #{q}#{globalSize};
%{ endforall}

void Formura_Decode_rank(int p, int *px, int *py) {
  *px = (int)p%P#{a1};
  *py = (int)p/P#{a1};
}

int Formura_Encode_rank(int px, int py) {
  int x = (px + P#{a1})%P#{a1};
  int y = (py + P#{a2})%P#{a2};
  return (x + y*P#{a1});
}


void Formura_Init(Formura_Navi *n, MPI_Comm comm) {
  int size, rank;
  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  if (size != P0) {
    printf("WARNING: inconsistent mpi size\n");
  }

  int ix, iy;
  Formura_Decode_rank(rank, &ix, &iy);

  n->my_rank = Formura_Encode_rank(ix,iy);
  n->rank_p1_0 = Formura_Encode_rank(ix+1,iy);
  n->rank_m1_0 = Formura_Encode_rank(ix-1,iy);
  n->rank_0_p1 = Formura_Encode_rank(ix,iy+1);
  n->rank_0_m1 = Formura_Encode_rank(ix,iy-1);
  n->rank_p1_p1 = Formura_Encode_rank(ix+1,iy+1);
  n->rank_m1_m1 = Formura_Encode_rank(ix-1,iy-1);
  n->mpi_world = comm;

  n->time_step = 0;

  n->lower_#{map toLower a1} = 2*D#{a1};
  n->upper_#{map toLower a1} = N#{a1}*M#{a1};
  n->offset_#{map toLower a1} = ix*L#{a1} - 2*D#{a1};
  n->length_#{map toLower a1} = d#{map toLower a1}*L#{a1}*P#{a1};
  n->lower_#{map toLower a2} = 2*D#{a2};
  n->upper_#{map toLower a2} = N#{a2}*M#{a2};
  n->offset_#{map toLower a2} = iy*L#{a2} - 2*D#{a2};
  n->length_#{map toLower a2} = d#{map toLower a2}*L#{a2}*P#{a2};

  printf("Formura_Init\n");
  printf("  rank = %d/%d\n", rank, size);
  printf("  ix = %d\n", ix);
  printf("  iy = %d\n", iy);
  printf("  my_rank = %d\n", n->my_rank);
  printf("  rank_p1_0 = %d\n", n->rank_p1_0);
  printf("  rank_m1_0 = %d\n", n->rank_m1_0);
  printf("  rank_0_p1 = %d\n", n->rank_0_p1);
  printf("  rank_0_m1 = %d\n", n->rank_0_m1);
  printf("  rank_p1_p1 = %d\n", n->rank_p1_p1);
  printf("  rank_m1_m1 = %d\n", n->rank_m1_m1);
}

double to_pos_#{map toLower a1}(int ix, Formura_Navi n) {
  int W#{a1} = P#{a1}*L#{a1};
  int d = (Ns*n.time_step)%W#{a1};
  return d#{map toLower a1}*((ix+n.offset_#{map toLower a1}-d+W#{a1})%W#{a1});
}

double to_pos_#{map toLower a2}(int iy, Formura_Navi n) {
  int W#{a2} = P#{a2}*L#{a2};
  int d = (Ns*n.time_step)%W#{a2};
  return d#{map toLower a2}*((iy+n.offset_#{map toLower a2}-d+W#{a2})%W#{a2});
}

// 1タイムステップ更新
typedef struct {
%{ forall (q,t) <- qs }
  #{t} #{q}[N#{a1}+2*Ns][N#{a2}+2*Ns];
%{ endforall }
} Formura_Buff;

typedef struct {
%{ forall (q,t) <- qs }
  #{t} #{q}[N#{a1}][N#{a2}];
%{ endforall }
} Formura_Rslt;

void Formura_Step(Formura_Buff *buff, Formura_Rslt *rslt) {
#{step}
}

typedef struct {
  int x_origin;
  int y_origin;

%{ forall (q,t) <- qs }
  #{t} #{q}_wall_x[NT][2*Ns][N#{a2}+2*Ns];
  #{t} #{q}_wall_y[NT][N#{a1}+2*Ns][2*Ns];
  #{t} #{q}_res[N#{a1}][N#{a2}];
%{ endforall }
} Formura_Param;

// NTタイムステップ更新
void Formura_Update(Formura_Param *p) {
  static Formura_Buff buff;
  static Formura_Rslt rslt;

  // 床を読む
  int ix0 = p->x_origin;
  int iy0 = p->y_origin;
  for(int ix = 0; ix < N#{a1}; ix++) {
    for(int iy = 0; iy < N#{a2}; iy++) {
%{ forall (q,_) <- qs }
      rslt.#{q}[ix][iy] = #{q}[ix0+ix][iy0+iy];
%{ endforall }
    }
  }

  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < N#{a1}; ix++) {
      for(int iy = 0; iy < N#{a2}; iy++) {
%{ forall (q,_) <- qs }
        buff.#{q}[ix][iy] = rslt.#{q}[ix][iy];
%{ endforall }
      }
    }
    // x壁を読む
    for(int ix = 0; ix < 2*Ns; ix++) {
      for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
        buff.#{q}[ix+N#{a1}][iy] = p->#{q}_wall_x[it][ix][iy];
%{ endforall }
      }
    }
    // y壁を読む
    for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
      for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
        buff.#{q}[ix][iy+N#{a2}] = p->#{q}_wall_y[it][ix][iy];
%{ endforall }
      }
    }

    Formura_Step(&buff, &rslt);

    // x壁を書く
    for(int ix = 0; ix < 2*Ns; ix++) {
      for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
        p->#{q}_wall_x[it][ix][iy] = buff.#{q}[ix][iy];
%{ endforall }
      }
    }
    // y壁を書く
    for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
      for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
        p->#{q}_wall_y[it][ix][iy] = buff.#{q}[ix][iy];
%{ endforall }
      }
    }
  }

  // 床を書く
  for(int ix = 0; ix < N#{a1}; ix++) {
    for(int iy = 0; iy < N#{a2}; iy++) {
%{ forall (q,_) <- qs }
      p->#{q}_res[ix][iy] = rslt.#{q}[ix][iy];
%{ endforall }
    }
  }
}

void Formura_Forward(Formura_Navi *n) {
%{ forall (q,t) <- qs }
  static #{t} #{q}_tmp[M#{a1}*N#{a1}][M#{a2}*N#{a2}];
  static #{t} #{q}_tmp_wall_x[M#{a2}][NT][2*Ns][N#{a2}+2*Ns];
  static #{t} #{q}_tmp_wall_y[M#{a1}][NT][N#{a1}+2*Ns][2*Ns];
%{ endforall }
  
  // 床の準備
  static #{snd (head qs)} q_send_buf_x[2*D#{a1}][L#{a2}][#{length qs}];
  static #{snd (head qs)} q_send_buf_y[L#{a1}][2*D#{a2}][#{length qs}];
  static #{snd (head qs)} q_send_buf_xy[2*D#{a1}][2*D#{a2}][#{length qs}];
  static #{snd (head qs)} q_recv_buf_x[2*D#{a1}][L#{a2}][#{length qs}];
  static #{snd (head qs)} q_recv_buf_y[L#{a1}][2*D#{a2}][#{length qs}];
  static #{snd (head qs)} q_recv_buf_xy[2*D#{a1}][2*D#{a2}][#{length qs}];

  static Formura_Param p;

  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
%{ forall (i,q) <- nqs }
      q_send_buf_xy[ix][iy][#{i}] = #{q}[ix+L#{a1}][iy+L#{a2}];
%{ endforall }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
%{ forall (i,q) <- nqs }
      q_send_buf_x[ix][iy][#{i}] = #{q}[ix+L#{a1}][iy+2*D#{a2}];
%{ endforall }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
%{ forall (i,q) <- nqs }
      q_send_buf_y[ix][iy][#{i}] = #{q}[ix+2*D#{a1}][iy+L#{a2}];
%{ endforall }
    }
  }

  MPI_Request send_req_x, send_req_y, send_req_xy, recv_req_x, recv_req_y, recv_req_xy;
  MPI_Isend(q_send_buf_x, sizeof(q_send_buf_x), MPI_BYTE, n->rank_p1_0, 0, n->mpi_world, &send_req_x);
  MPI_Isend(q_send_buf_y, sizeof(q_send_buf_y), MPI_BYTE, n->rank_0_p1, 0, n->mpi_world, &send_req_y);
  MPI_Isend(q_send_buf_xy, sizeof(q_send_buf_xy), MPI_BYTE, n->rank_p1_p1, 0, n->mpi_world, &send_req_xy);
  MPI_Irecv(q_recv_buf_x, sizeof(q_recv_buf_x), MPI_BYTE, n->rank_m1_0, 0, n->mpi_world, &recv_req_x);
  MPI_Irecv(q_recv_buf_y, sizeof(q_recv_buf_y), MPI_BYTE, n->rank_0_m1, 0, n->mpi_world, &recv_req_y);
  MPI_Irecv(q_recv_buf_xy, sizeof(q_recv_buf_xy), MPI_BYTE, n->rank_m1_m1, 0, n->mpi_world, &recv_req_xy);

  // x壁の初期化
  for(int jy = 0; jy < M#{a2}; jy++) {
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < 2*Ns; ix++) {
        for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
          #{q}_tmp_wall_x[jy][it][ix][iy] = 1.0;
%{ endforall }
        }
      }
    }
  }
  // y壁の初期化
  for(int jx = 0; jx < M#{a1}; jx++) {
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
        for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
          #{q}_tmp_wall_y[jx][it][ix][iy] = 1.0;
%{ endforall }
        }
      }
    }
  }

  // 床の更新
  for(int jx = M#{a1} - 1; jx > 2*D#{a1}/N#{a1}; jx--) {
    // y壁の読み込み
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
        for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
          p.#{q}_wall_y[it][ix][iy] = #{q}_tmp_wall_y[jx][it][ix][iy];
%{ endforall }
        }
      }
    }

    for(int jy = M#{a2} - 1; jy > 2*D#{a2}/N#{a2}; jy--) {
      // x壁の読み込み
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < 2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
            p.#{q}_wall_x[it][ix][iy] = #{q}_tmp_wall_x[jy][it][ix][iy];
%{ endforall }
          }
        }
      }

      p.x_origin = jx*N#{a1};
      p.y_origin = jy*N#{a2};
      Formura_Update(&p);

      // x壁の書き出し
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < 2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
#{q}_tmp_wall_x[jy][it][ix][iy] = p.#{q}_wall_x[it][ix][iy];
%{ endforall }
          }
        }
      }

      // 床の受けとり
      for(int ix = 0; ix < N#{a1}; ix++) {
        for(int iy = 0; iy < N#{a2}; iy++) {
%{ forall (q,_) <- qs }
#{q}_tmp[ix+jx*N#{a1}][iy+jy*N#{a2}] = p.#{q}_res[ix][iy];
%{ endforall }
        }
      }
    }

    // y壁の書き出し
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
        for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
#{q}_tmp_wall_y[jx][it][ix][iy] = p.#{q}_wall_y[it][ix][iy];
%{ endforall }
        }
      }
    }
  }

  // 通信待ち
  MPI_Wait(&send_req_x, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_x, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_y, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_y, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_xy, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_xy, MPI_STATUS_IGNORE);

  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
%{ forall (i,q) <- nqs }
#{q}[ix][iy] = q_recv_buf_xy[ix][iy][#{i}];
%{ endforall }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
%{ forall (i,q) <- nqs }
#{q}[ix][iy+2*D#{a2}] = q_recv_buf_x[ix][iy][#{i}];
%{ endforall }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
%{ forall (i,q) <- nqs }
#{q}[ix+2*D#{a1}][iy] = q_recv_buf_y[ix][iy][#{i}];
%{ endforall }
    }
  }

  // 残りの床を更新
  for(int jx = M#{a1} - 1; jx > 2*D#{a1}/N#{a1}; jx--) {
    // y壁の読み込み
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
        for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
          p.#{q}_wall_y[it][ix][iy] = #{q}_tmp_wall_y[jx][it][ix][iy];
%{ endforall }
        }
      }
    }

    for(int jy = 2*D#{a2}/N#{a2}; jy >= 0; jy--) {
      // x壁の読み込み
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < 2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
            p.#{q}_wall_x[it][ix][iy] = #{q}_tmp_wall_x[jy][it][ix][iy];
%{ endforall }
          }
        }
      }

      p.x_origin = jx*N#{a1};
      p.y_origin = jy*N#{a2};
      Formura_Update(&p);

      // x壁の書き出し
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < 2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
#{q}_tmp_wall_x[jy][it][ix][iy] = p.#{q}_wall_x[it][ix][iy];
%{ endforall }
          }
        }
      }

      // 床の受けとり
      for(int ix = 0; ix < N#{a1}; ix++) {
        for(int iy = 0; iy < N#{a2}; iy++) {
%{ forall (q,_) <- qs }
#{q}_tmp[ix+jx*N#{a1}][iy+jy*N#{a2}] = p.#{q}_res[ix][iy];
%{ endforall }
        }
      }
    }

    // y壁の書き出し
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
        for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
#{q}_tmp_wall_y[jx][it][ix][iy] = p.#{q}_wall_y[it][ix][iy];
%{ endforall }
        }
      }
    }
  }
  for(int jx = 2*D#{a1}/N#{a1}; jx >= 0; jx--) {
    // y壁の読み込み
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
        for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
          p.#{q}_wall_y[it][ix][iy] = #{q}_tmp_wall_y[jx][it][ix][iy];
%{ endforall }
        }
      }
    }

    for(int jy = M#{a2} - 1; jy >= 0; jy--) {
      // x壁の読み込み
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < 2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
            p.#{q}_wall_x[it][ix][iy] = #{q}_tmp_wall_x[jy][it][ix][iy];
%{ endforall }
          }
        }
      }

      p.x_origin = jx*N#{a1};
      p.y_origin = jy*N#{a2};
      Formura_Update(&p);

      // x壁の書き出し
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < 2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
%{ forall (q,_) <- qs }
#{q}_tmp_wall_x[jy][it][ix][iy] = p.#{q}_wall_x[it][ix][iy];
%{ endforall }
          }
        }
      }

      // 床の受けとり
      for(int ix = 0; ix < N#{a1}; ix++) {
        for(int iy = 0; iy < N#{a2}; iy++) {
%{ forall (q,_) <- qs }
          #{q}_tmp[ix+jx*N#{a1}][iy+jy*N#{a2}] = p.#{q}_res[ix][iy];
%{ endforall }
        }
      }
    }

    // y壁の書き出し
    for(int it = 0; it < NT; it++) {
      for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
        for(int iy = 0; iy < 2*Ns; iy++) {
%{ forall (q,_) <- qs }
          #{q}_tmp_wall_y[jx][it][ix][iy] = p.#{q}_wall_y[it][ix][iy];
%{ endforall }
        }
      }
    }
  }

  // 床の書き出し
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
%{ forall (q,_) <- qs }
      #{q}[ix+2*D#{a1}][iy+2*D#{a2}] = #{q}_tmp[ix][iy];
%{ endforall }
    }
  }

  n->time_step += NT;
}
