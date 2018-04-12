#include <mpi.h>
#define PX #{node_x}
#define P0 PX

#define NT #{temporal_blocking_interval}

#define NX #{grid_per_block_x}
#define MX #{block_per_node_x}

#define Ns #{sleeve}

#define DX (Ns*NT)
#define LX #{grid_per_node_x}

#define LENGTH_PER_NODE_X #{length_per_node_x}
#define SPACE_INTERVAL (LENGTH_PER_NODE_X/LX)

extern double dx;

// グローバル配列
%{ forall (t,q) <- qs }
extern #{t} #{q}[NX*MX];
%{ endforall}

typedef struct {
  int time_step;
  int lower_x;
  int upper_x;
  int offset_x;
  double length_x;
  int my_rank;
  int rank_p1;
  int rank_m1;
  MPI_Comm mpi_world;
} Formura_Navi;

double to_pos_x(int, Formura_Navi);
void Formura_Init(Formura_Navi *, MPI_Comm);
void Formura_Forward(Formura_Navi *);
