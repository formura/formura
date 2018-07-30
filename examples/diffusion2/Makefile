CC = mpicc
CCFLAG = -Wall -std=c99 -O3 -ffast-math -mavx2 -mfma
CCLIBS = -lm
.SUFFIXES:
.PRECIOUS: %.s

diffusion2.c diffusion2.h: diffusion2.fmr diffusion2.yaml
	formura diffusion2.fmr

%.s: %.c diffusion2.h
	$(CC) $(CCFLAG) -S $< -o $@ $(CCLIBS)

%.o: %.s
	$(CC) $(CCFLAG) -c $< -o $@ $(CCLIBS)

main: main.o diffusion2.o
	$(CC) $(CCFLAG) $^ -o $@ $(CCLIBS)

run: main
	mkdir -p data
	./run ./main

clean:
	rm -rf *.o *.s main
