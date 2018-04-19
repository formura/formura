#include <mpi.h>
%{ forall (a,p) <- mpishape }
#define P#{a} #{p}
%{ endforall }
#define P0 #{totalMPI}

#define NT #{ic ^. icTemporalBlockingInterval}

%{ forall (a,n) <- gridPerBlock }
#define N#{a} #{n}
%{ endforall }
%{ forall (a,m) <- blockPerNode }
#define M#{a} #{m}
%{ endforall }

#define Ns #{ic ^. icSleeve}

%{ forall (a,l) <- gridPerNode }
#define L#{a} #{l}
%{ endforall }

%{ forall (a,l) <- lengthPerNode }
#define LENGTH_PER_NODE_#{a} #{l}
%{ endforall }

%{ forall a <- axes }
#define D#{a} (Ns*NT)
#define SPACE_INTERVAL_#{a} (LENGTH_PER_NODE_#{a}/L#{a})
extern double d#{map toLower a};
%{ endforall }

// グローバル配列
%{ forall (q,t) <- qs }
extern #{t} #{q}#{globalSize};
%{ endforall}

typedef struct {
  int time_step;
%{ forall a <- axes }
  int lower_#{map toLower a};
  int upper_#{map toLower a};
  int offset_#{map toLower a};
  double length_#{map toLower a};
%{ endforall }
  int my_rank;
%{ forall r <- ranks }
  int rank_#{r};
%{ endforall }
  MPI_Comm mpi_world;
} Formura_Navi;

%{ forall a <- axes }
double to_pos_#{map toLower a}(int, Formura_Navi);
%{ endforall }
void Formura_Init(Formura_Navi *, MPI_Comm);
void Formura_Forward(Formura_Navi *);
