CC = mpicc
CCFLAG = -Wall -std=c99 -lm -O3 -ffast-math -mavx2 -mfma
N = 1
.SUFFIXES:
.PRECIOUS: %.s

%.s: %.c
	$(CC) $(CCFLAG) -S $< -o $@

%.o: %.s
	$(CC) $(CCFLAG) -c $< -o $@

main: main.o diffusion2.o
	$(CC) $(CCFLAG) $^ -o $@

run: main
	mkdir -p data
	mpirun -n $(N) ./main

clean:
	rm -rf *.o *.s main
