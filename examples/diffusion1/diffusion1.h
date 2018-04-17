#include <mpi.h>
#define PX 1
#define P0 1

#define NT 5

#define NX 10
#define MX 11

#define Ns 1

#define LX 100

#define LENGTH_PER_NODE_X 1.0

#define DX (Ns*NT)
#define SPACE_INTERVAL_X (LENGTH_PER_NODE_X/LX)
extern double dx;

// グローバル配列
extern q double[NX*MX];

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
