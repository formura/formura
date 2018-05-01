CC = mpicc
CCFLAG = -Wall -std=c99 -lm -O3 -ffast-math -mavx2 -mfma
.SUFFIXES:
.PRECIOUS: %.s

diffusion2.c diffusion2.h: diffusion2.fmr diffusion2.yaml
	formura diffusion1.fmr

%.s: %.c diffusion2.h
	$(CC) $(CCFLAG) -S $< -o $@

%.o: %.s
	$(CC) $(CCFLAG) -c $< -o $@

main: main.o diffusion2.o
	$(CC) $(CCFLAG) $^ -o $@

run: main
	mkdir -p data
	./run ./main

clean:
	rm -rf *.o *.s main
