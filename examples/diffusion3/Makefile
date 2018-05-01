CC = mpicc
CCFLAG = -Wall -std=c99 -lm -O3 -ffast-math -mavx2 -mfma
.SUFFIXES:
.PRECIOUS: %.s

diffusion3.c diffusion3.h: diffusion3.fmr diffusion3.yaml
	formura diffusion1.fmr

%.s: %.c diffusion3.h
	$(CC) $(CCFLAG) -S $< -o $@

%.o: %.s
	$(CC) $(CCFLAG) -c $< -o $@

main: main.o diffusion3.o
	$(CC) $(CCFLAG) $^ -o $@

run: main
	mkdir -p data
	./run ./main

clean:
	rm -rf *.o *.s main
