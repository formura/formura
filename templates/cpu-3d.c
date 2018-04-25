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
double d#{map toLower a3} = SPACE_INTERVAL_#{a3};

// グローバル配列の宣言
%{ forall (q,t) <- qs }
#{t} #{q}#{globalSize};
%{ endforall}

void Formura_Decode_rank(int p, int *px, int *py, int *pz) {
  int pw = (int)p%(P#{a1}*P#{a2});
  *px = (int)pw%P#{a1};
  *py = (int)pw/P#{a1};
  *pz = (int)p/(P#{a1}*P#{a2});
}

int Formura_Encode_rank(int px, int py, int pz) {
  int x = (px + P#{a1})%P#{a1};
  int y = (py + P#{a2})%P#{a2};
  int z = (pz + P#{a3})%P#{a3};
  return (x + y*P#{a1} + z*P#{a1}*P#{a2});
}

void Formura_Init(Formura_Navi *n, MPI_Comm comm) {
  int size, rank;
  MPI_Comm_size(comm, &size);
  MPI_Comm_rank(comm, &rank);

  int ix,iy,iz;
  Formura_Decode_rank(rank, &ix, &iy, &iz);

  n->time_step = 0;
  n->my_rank = Formura_Encode_rank(ix, iy, iz);
  n->rank_p1_0_0 = Formura_Encode_rank(ix+1, iy, iz);
  n->rank_m1_0_0 = Formura_Encode_rank(ix-1, iy, iz);
  n->rank_0_p1_0 = Formura_Encode_rank(ix, iy+1, iz);
  n->rank_0_m1_0 = Formura_Encode_rank(ix, iy-1, iz);
  n->rank_0_0_p1 = Formura_Encode_rank(ix, iy, iz+1);
  n->rank_0_0_m1 = Formura_Encode_rank(ix, iy, iz-1);
  n->rank_p1_p1_0 = Formura_Encode_rank(ix+1, iy+1, iz);
  n->rank_m1_m1_0 = Formura_Encode_rank(ix-1, iy-1, iz);
  n->rank_p1_0_p1 = Formura_Encode_rank(ix+1, iy, iz+1);
  n->rank_m1_0_m1 = Formura_Encode_rank(ix-1, iy, iz-1);
  n->rank_0_p1_p1 = Formura_Encode_rank(ix, iy+1, iz+1);
  n->rank_0_m1_m1 = Formura_Encode_rank(ix, iy-1, iz-1);
  n->rank_p1_p1_p1 = Formura_Encode_rank(ix+1, iy+1, iz+1);
  n->rank_m1_m1_m1 = Formura_Encode_rank(ix-1, iy-1, iz-1);
  n->mpi_world = comm;

  n->lower_#{map toLower a1} = 2*D#{a1};
  n->upper_#{map toLower a1} = N#{a1}*M#{a1};
  n->offset_#{map toLower a1} = ix*L#{a1} - 2*D#{a1};
  n->length_#{map toLower a1} = d#{map toLower a1}*L#{a1}*P#{a1};
  n->lower_#{map toLower a2} = 2*D#{a2};
  n->upper_#{map toLower a2} = N#{a2}*M#{a2};
  n->offset_#{map toLower a2} = iy*L#{a2} - 2*D#{a2};
  n->length_#{map toLower a2} = d#{map toLower a2}*L#{a2}*P#{a2};
  n->lower_#{map toLower a3} = 2*D#{a3};
  n->upper_#{map toLower a3} = N#{a3}*M#{a3};
  n->offset_#{map toLower a3} = iz*L#{a3} - 2*D#{a3};
  n->length_#{map toLower a3} = d#{map toLower a3}*L#{a3}*P#{a3};

  printf("Formura_Init\n");
  printf("  rank = %d/%d\n", rank, size);
  printf("  ix = %d\n", ix);
  printf("  iy = %d\n", iy);
  printf("  iz = %d\n", iz);
  printf("  my_rank = %d\n", n->my_rank);
  printf("  rank_p1_0_0 = %d\n", n->rank_p1_0_0);
  printf("  rank_m1_0_0 = %d\n", n->rank_m1_0_0);
  printf("  rank_0_p1_0 = %d\n", n->rank_0_p1_0);
  printf("  rank_0_m1_0 = %d\n", n->rank_0_m1_0);
  printf("  rank_0_0_p1 = %d\n", n->rank_0_0_p1);
  printf("  rank_0_0_m1 = %d\n", n->rank_0_0_m1);
  printf("  rank_p1_p1_0 = %d\n", n->rank_p1_p1_0);
  printf("  rank_m1_m1_0 = %d\n", n->rank_m1_m1_0);
  printf("  rank_p1_0_p1 = %d\n", n->rank_p1_0_p1);
  printf("  rank_m1_0_m1 = %d\n", n->rank_m1_0_m1);
  printf("  rank_0_p1_p1 = %d\n", n->rank_0_p1_p1);
  printf("  rank_0_m1_m1 = %d\n", n->rank_0_m1_m1);
  printf("  rank_p1_p1_p1 = %d\n", n->rank_p1_p1_p1);
  printf("  rank_m1_m1_m1 = %d\n", n->rank_m1_m1_m1);
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

double to_pos_#{map toLower a3}(int iz, Formura_Navi n) {
  int W#{a3} = P#{a3}*L#{a3};
  int d = (Ns*n.time_step)%W#{a3};
  return d#{map toLower a3}*((iz+n.offset_#{map toLower a3}-d+W#{a3})%W#{a3});
}

// 1タイムステップ更新
typedef struct {
%{ forall (q,t) <- qs }
  #{t} #{q}[N#{a1}+2*Ns][N#{a2}+2*Ns][N#{a3}+2*Ns];
%{ endforall }
} Formura_Buff;

typedef struct {
%{ forall (q,t) <- qs }
  #{t} #{q}[N#{a1}][N#{a2}][N#{a3}];
%{ endforall }
} Formura_Rslt;

void Formura_Step(Formura_Buff *buff, Formura_Rslt *rslt) {
#{step}
}

typedef struct {
  int x_origin;
  int y_origin;
  int z_origin;

%{ forall (q,t) <- qs }
  #{t} #{q}_wall_x[NT][2*Ns][N#{a2}+2*Ns][N#{a3}+2*Ns];
  #{t} #{q}_wall_y[NT][N#{a1}+2*Ns][2*Ns][N#{a3}+2*Ns];
  #{t} #{q}_wall_z[NT][N#{a1}+2*Ns][N#{a2}+2*Ns][2*Ns];
  #{t} #{q}_res[N#{a1}][N#{a2}][N#{a3}];
%{ endforall }
} Formura_Param;

// NTタイムステップ更新
void Formura_Update(Formura_Param *p) {
  Formura_Buff buff;
  Formura_Rslt rslt;
    // 床を読む
  int ix0 = p->x_origin;
  int iy0 = p->y_origin;
  int iz0 = p->z_origin;
  for(int ix = 0; ix < N#{a1}; ix++) {
    for(int iy = 0; iy < N#{a2}; iy++) {
      for(int iz = 0; iz < N#{a3}; iz++) {
%{ forall (q,_) <- qs }
        rslt.#{q}[ix][iy][iz] = #{q}[ix0+ix][iy0+iy][iz0+iz];
%{ endforall }
      }
    }
  }

  for(int it = 0; it < NT; it++) {
    for(int ix = 0; ix < N#{a1}; ix++) {
      for(int iy = 0; iy < N#{a2}; iy++) {
        for(int iz = 0; iz < N#{a3}; iz++) {
%{ forall (q,_) <- qs }
          buff.#{q}[ix][iy][iz] = rslt.#{q}[ix][iy][iz];
%{ endforall }
        }
      }
    }
    // x壁を読む
    for(int ix = 0; ix < 2*Ns; ix++) {
      for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
        for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
          buff.#{q}[ix+N#{a1}][iy][iz] = p->#{q}_wall_x[it][ix][iy][iz];
%{ endforall }
        }
      }
    }
    // y壁を読む
    for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
      for(int iy = 0; iy < 2*Ns; iy++) {
        for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
          buff.#{q}[ix][iy+N#{a2}][iz] = p->#{q}_wall_y[it][ix][iy][iz];
%{ endforall }
        }
      }
    }
    // z壁を読む
    for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
      for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
        for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
          buff.#{q}[ix][iy][iz+N#{a3}] = p->#{q}_wall_z[it][ix][iy][iz];
%{ endforall }
        }
      }
    }

    Formura_Step(&buff, &rslt);

    // x壁を書く
    for(int ix = 0; ix < 2*Ns; ix++) {
      for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
        for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
          p->#{q}_wall_x[it][ix][iy][iz] = buff.#{q}[ix][iy][iz];
%{ endforall }
        }
      }
    }
    // y壁を書く
    for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
      for(int iy = 0; iy < 2*Ns; iy++) {
        for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
          p->#{q}_wall_y[it][ix][iy][iz] = buff.#{q}[ix][iy][iz];
%{ endforall }
        }
      }
    }
    // z壁を書く
    for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
      for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
        for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
          p->#{q}_wall_z[it][ix][iy][iz] = buff.#{q}[ix][iy][iz];
%{ endforall }
        }
      }
    }
  }

  // 床を書く
  for(int ix = 0; ix < N#{a1}; ix++) {
    for(int iy = 0; iy < N#{a2}; iy++) {
      for(int iz = 0; iz < N#{a3}; iz++) {
%{ forall (q,_) <- qs }
        p->#{q}_res[ix][iy][iz] = rslt.#{q}[ix][iy][iz];
%{ endforall }
      }
    }
  }
}

%{ forall (q,t) <- qs }
#{t} #{q}_tmp[M#{a1}*N#{a1}][M#{a2}*N#{a2}][M#{a3}*N#{a3}];
#{t} #{q}_tmp_wall_x[M#{a2}][M#{a3}][NT][2*Ns][N#{a2}+2*Ns][N#{a3}+2*Ns];
#{t} #{q}_tmp_wall_y[M#{a3}][M#{a1}][NT][N#{a1}+2*Ns][2*Ns][N#{a3}+2*Ns];
#{t} #{q}_tmp_wall_z[M#{a1}][M#{a2}][NT][N#{a1}+2*Ns][N#{a2}+2*Ns][2*Ns];
%{ endforall }

#{snd (head qs)} q_send_buf_x[2*D#{a1}][L#{a2}][L#{a3}][#{length qs}];
#{snd (head qs)} q_send_buf_y[L#{a1}][2*D#{a2}][L#{a3}][#{length qs}];
#{snd (head qs)} q_send_buf_z[L#{a1}][L#{a2}][2*D#{a3}][#{length qs}];
#{snd (head qs)} q_send_buf_xy[2*D#{a1}][2*D#{a2}][L#{a3}][#{length qs}];
#{snd (head qs)} q_send_buf_yz[L#{a1}][2*D#{a2}][2*D#{a3}][#{length qs}];
#{snd (head qs)} q_send_buf_zx[2*D#{a1}][L#{a2}][2*D#{a3}][#{length qs}];
#{snd (head qs)} q_send_buf_xyz[2*D#{a1}][2*D#{a2}][2*D#{a3}][#{length qs}];
#{snd (head qs)} q_recv_buf_x[2*D#{a1}][L#{a2}][L#{a3}][#{length qs}];
#{snd (head qs)} q_recv_buf_y[L#{a1}][2*D#{a2}][L#{a3}][#{length qs}];
#{snd (head qs)} q_recv_buf_z[L#{a1}][L#{a2}][2*D#{a3}][#{length qs}];
#{snd (head qs)} q_recv_buf_xy[2*D#{a1}][2*D#{a2}][L#{a3}][#{length qs}];
#{snd (head qs)} q_recv_buf_yz[L#{a1}][2*D#{a2}][2*D#{a3}][#{length qs}];
#{snd (head qs)} q_recv_buf_zx[2*D#{a1}][L#{a2}][2*D#{a3}][#{length qs}];
#{snd (head qs)} q_recv_buf_xyz[2*D#{a1}][2*D#{a2}][2*D#{a3}][#{length qs}];

void Formura_Forward(Formura_Navi *n) {
  Formura_Param p;
  
  // 床の準備

  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        q_send_buf_xyz[ix][iy][iz][#{i}] = #{q}[ix+L#{a1}][iy+L#{a2}][iz+L#{a3}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < L#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        q_send_buf_xy[ix][iy][iz][#{i}] = #{q}[ix+L#{a1}][iy+L#{a2}][iz+2*D#{a3}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        q_send_buf_yz[ix][iy][iz][#{i}] = #{q}[ix+2*D#{a1}][iy+L#{a2}][iz+L#{a3}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        q_send_buf_zx[ix][iy][iz][#{i}] = #{q}[ix+L#{a1}][iy+2*D#{a2}][iz+L#{a3}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
      for(int iz = 0; iz < L#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        q_send_buf_x[ix][iy][iz][#{i}] = #{q}[ix+L#{a1}][iy+2*D#{a2}][iz+2*D#{a3}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < L#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        q_send_buf_y[ix][iy][iz][#{i}] = #{q}[ix+2*D#{a1}][iy+L#{a2}][iz+2*D#{a3}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        q_send_buf_z[ix][iy][iz][#{i}] = #{q}[ix+2*D#{a1}][iy+2*D#{a2}][iz+L#{a3}];
%{ endforall }
      }
    }
  }

  MPI_Request send_req_x, send_req_y, send_req_z, send_req_xy, send_req_yz, send_req_zx, send_req_xyz;
  MPI_Request recv_req_x, recv_req_y, recv_req_z, recv_req_xy, recv_req_yz, recv_req_zx, recv_req_xyz;
  MPI_Isend(q_send_buf_x, sizeof(q_send_buf_x), MPI_BYTE, n->rank_p1_0_0, 0, n->mpi_world, &send_req_x);
  MPI_Isend(q_send_buf_y, sizeof(q_send_buf_y), MPI_BYTE, n->rank_0_p1_0, 0, n->mpi_world, &send_req_y);
  MPI_Isend(q_send_buf_z, sizeof(q_send_buf_z), MPI_BYTE, n->rank_0_0_p1, 0, n->mpi_world, &send_req_z);
  MPI_Isend(q_send_buf_xy, sizeof(q_send_buf_xy), MPI_BYTE, n->rank_p1_p1_0, 0, n->mpi_world, &send_req_xy);
  MPI_Isend(q_send_buf_yz, sizeof(q_send_buf_yz), MPI_BYTE, n->rank_0_p1_p1, 0, n->mpi_world, &send_req_yz);
  MPI_Isend(q_send_buf_zx, sizeof(q_send_buf_zx), MPI_BYTE, n->rank_p1_0_p1, 0, n->mpi_world, &send_req_zx);
  MPI_Isend(q_send_buf_xyz, sizeof(q_send_buf_xyz), MPI_BYTE, n->rank_p1_p1_p1, 0, n->mpi_world, &send_req_xyz);
  MPI_Irecv(q_recv_buf_x, sizeof(q_recv_buf_x), MPI_BYTE, n->rank_m1_0_0, 0, n->mpi_world, &recv_req_x);
  MPI_Irecv(q_recv_buf_y, sizeof(q_recv_buf_y), MPI_BYTE, n->rank_0_m1_0, 0, n->mpi_world, &recv_req_y);
  MPI_Irecv(q_recv_buf_z, sizeof(q_recv_buf_z), MPI_BYTE, n->rank_0_0_m1, 0, n->mpi_world, &recv_req_z);
  MPI_Irecv(q_recv_buf_xy, sizeof(q_recv_buf_xy), MPI_BYTE, n->rank_m1_m1_0, 0, n->mpi_world, &recv_req_xy);
  MPI_Irecv(q_recv_buf_yz, sizeof(q_recv_buf_yz), MPI_BYTE, n->rank_0_m1_m1, 0, n->mpi_world, &recv_req_yz);
  MPI_Irecv(q_recv_buf_zx, sizeof(q_recv_buf_zx), MPI_BYTE, n->rank_m1_0_m1, 0, n->mpi_world, &recv_req_zx);
  MPI_Irecv(q_recv_buf_xyz, sizeof(q_recv_buf_xyz), MPI_BYTE, n->rank_m1_m1_m1, 0, n->mpi_world, &recv_req_xyz);

  // x壁の初期化
  for(int jy = 0; jy < M#{a2}; jy++) {
    for(int jz = 0; jz < M#{a3}; jz++) {
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < 2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz] = 1.0;
%{ endforall }
            }
          }
        }
      }
    }
  }
  // y壁の初期化
  for(int jz = 0; jz < M#{a3}; jz++) {
    for(int jx = 0; jx < M#{a1}; jx++) {
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < 2*Ns; iy++) {
            for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz] = 1.0;
%{ endforall }
            }
          }
        }
      }
    }
  }
  // z壁の初期化
  for(int jx = 0; jx < M#{a1}; jx++) {
    for(int jy = 0; jy < M#{a2}; jy++) {
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz] = 1.0;
%{ endforall }
            }
          }
        }
      }
    }
  }

  // 床の更新
  for(int jx = M#{a1} - 1; jx > 2*D#{a1}/N#{a1}; jx--) {
    for(int jy = M#{a2} - 1; jy > 2*D#{a2}/N#{a2}; jy--) {
      // z壁の読み込み
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              p.#{q}_wall_z[it][ix][iy][iz] = #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      for(int jz = M#{a3} - 1; jz > 2*D#{a3}/N#{a3}; jz--) {
        // x壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_x[it][ix][iy][iz] = #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_y[it][ix][iy][iz] = #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        p.x_origin = jx*N#{a1};
        p.y_origin = jy*N#{a2};
        p.z_origin = jz*N#{a3};
        Formura_Update(&p);

        // x壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz] = p.#{q}_wall_x[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz] = p.#{q}_wall_y[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        // 床の受けとり
        for(int ix = 0; ix < N#{a1}; ix++) {
          for(int iy = 0; iy < N#{a2}; iy++) {
            for(int iz = 0; iz < N#{a3}; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp[ix+jx*N#{a1}][iy+jy*N#{a2}][iz+jz*N#{a3}] = p.#{q}_res[ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      // z壁の書き出し
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz] = p.#{q}_wall_z[it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
    }
  }

  // 通信待ち
  MPI_Wait(&send_req_x, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_x, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_y, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_y, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_z, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_z, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_xy, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_xy, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_yz, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_yz, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_zx, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_zx, MPI_STATUS_IGNORE);
  MPI_Wait(&send_req_xyz, MPI_STATUS_IGNORE);
  MPI_Wait(&recv_req_xyz, MPI_STATUS_IGNORE);

  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        #{q}[ix][iy][iz] = q_recv_buf_xyz[ix][iy][iz][#{i}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < L#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        #{q}[ix][iy][iz+2*D#{a3}] = q_recv_buf_xy[ix][iy][iz][#{i}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        #{q}[ix+2*D#{a1}][iy][iz] = q_recv_buf_yz[ix][iy][iz][#{i}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        #{q}[ix][iy+2*D#{a2}][iz] = q_recv_buf_zx[ix][iy][iz][#{i}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < 2*D#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
      for(int iz = 0; iz < L#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        #{q}[ix][iy+2*D#{a2}][iz+2*D#{a3}] = q_recv_buf_x[ix][iy][iz][#{i}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < 2*D#{a2}; iy++) {
      for(int iz = 0; iz < L#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        #{q}[ix+2*D#{a1}][iy][iz+2*D#{a3}] = q_recv_buf_y[ix][iy][iz][#{i}];
%{ endforall }
      }
    }
  }
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
      for(int iz = 0; iz < 2*D#{a3}; iz++) {
%{ forall (i,q) <- nqs }
        #{q}[ix+2*D#{a1}][iy+2*D#{a2}][iz] = q_recv_buf_z[ix][iy][iz][#{i}];
%{ endforall }
      }
    }
  }

  // 残りの床を更新 (1/3)
  for(int jx = 2*D#{a1}/N#{a1}; jx >= 0; jx--) {
    for(int jy = M#{a2} - 1; jy > 2*D#{a2}/N#{a2}; jy--) {
      // z壁の読み込み
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              p.#{q}_wall_z[it][ix][iy][iz] = #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      for(int jz = M#{a3} - 1; jz > 2*D#{a3}/N#{a3}; jz--) {
        // x壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_x[it][ix][iy][iz] = #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_y[it][ix][iy][iz] = #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        p.x_origin = jx*N#{a1};
        p.y_origin = jy*N#{a2};
        p.z_origin = jz*N#{a3};
        Formura_Update(&p);

        // x壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz] = p.#{q}_wall_x[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz] = p.#{q}_wall_y[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        // 床の受けとり
        for(int ix = 0; ix < N#{a1}; ix++) {
          for(int iy = 0; iy < N#{a2}; iy++) {
            for(int iz = 0; iz < N#{a3}; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp[ix+jx*N#{a1}][iy+jy*N#{a2}][iz+jz*N#{a3}] = p.#{q}_res[ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      // z壁の書き出し
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz] = p.#{q}_wall_z[it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
    }
  }
  // 残りの床を更新 (2/3)
  for(int jx = M#{a1} - 1; jx >= 0; jx--) {
    for(int jy = 2*D#{a2}/N#{a2}; jy >= 0; jy--) {
      // z壁の読み込み
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              p.#{q}_wall_z[it][ix][iy][iz] = #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      for(int jz = M#{a3} - 1; jz > 2*D#{a3}/N#{a3}; jz--) {
        // x壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_x[it][ix][iy][iz] = #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_y[it][ix][iy][iz] = #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        p.x_origin = jx*N#{a1};
        p.y_origin = jy*N#{a2};
        p.z_origin = jz*N#{a3};
        Formura_Update(&p);

        // x壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz] = p.#{q}_wall_x[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz] = p.#{q}_wall_y[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        // 床の受けとり
        for(int ix = 0; ix < N#{a1}; ix++) {
          for(int iy = 0; iy < N#{a2}; iy++) {
            for(int iz = 0; iz < N#{a3}; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp[ix+jx*N#{a1}][iy+jy*N#{a2}][iz+jz*N#{a3}] = p.#{q}_res[ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      // z壁の書き出し
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz] = p.#{q}_wall_z[it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
    }
  }
  // 残りの床を更新 (3/3)
  for(int jx = M#{a1} - 1; jx >= 0; jx--) {
    for(int jy = M#{a2} - 1; jy >= 0; jy--) {
      // z壁の読み込み
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
              p.#{q}_wall_z[it][ix][iy][iz] = #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      for(int jz = 2*D#{a3}/N#{a3}; jz >= 0; jz--) {
        // x壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_x[it][ix][iy][iz] = #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の読み込み
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                p.#{q}_wall_y[it][ix][iy][iz] = #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        p.x_origin = jx*N#{a1};
        p.y_origin = jy*N#{a2};
        p.z_origin = jz*N#{a3};
        Formura_Update(&p);

        // x壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < 2*Ns; ix++) {
            for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_x[jy][jz][it][ix][iy][iz] = p.#{q}_wall_x[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }
        // y壁の書き出し
        for(int it = 0; it < NT; it++) {
          for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
            for(int iy = 0; iy < 2*Ns; iy++) {
              for(int iz = 0; iz < N#{a3}+2*Ns; iz++) {
%{ forall (q,_) <- qs }
                #{q}_tmp_wall_y[jz][jx][it][ix][iy][iz] = p.#{q}_wall_y[it][ix][iy][iz];
%{ endforall }
              }
            }
          }
        }

        // 床の受けとり
        for(int ix = 0; ix < N#{a1}; ix++) {
          for(int iy = 0; iy < N#{a2}; iy++) {
            for(int iz = 0; iz < N#{a3}; iz++) {
%{ forall (q,_) <- qs }
              #{q}_tmp[ix+jx*N#{a1}][iy+jy*N#{a2}][iz+jz*N#{a3}] = p.#{q}_res[ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
      // z壁の書き出し
      for(int it = 0; it < NT; it++) {
        for(int ix = 0; ix < N#{a1}+2*Ns; ix++) {
          for(int iy = 0; iy < N#{a2}+2*Ns; iy++) {
            for(int iz = 0; iz < 2*Ns; iz++) {
%{ forall (q,_) <- qs }
             #{q}_tmp_wall_z[jx][jy][it][ix][iy][iz] = p.#{q}_wall_z[it][ix][iy][iz];
%{ endforall }
            }
          }
        }
      }
    }
  }

  // 床の書き出し
  for(int ix = 0; ix < L#{a1}; ix++) {
    for(int iy = 0; iy < L#{a2}; iy++) {
      for(int iz = 0; iz < L#{a3}; iz++) {
%{ forall (q,_) <- qs }
        #{q}[ix+2*D#{a1}][iy+2*D#{a2}][iz+2*D#{a3}] = #{q}_tmp[ix][iy][iz];
%{ endforall }
      }
    }
  }

  n->time_step += NT;
}
