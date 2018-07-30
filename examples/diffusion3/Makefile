CC = mpicc
CCFLAG = -Wall -std=c99 -O3 -ffast-math -mavx2 -mfma
CCLIBS = -lm
.SUFFIXES:
.PRECIOUS: %.s

diffusion3.c diffusion3.h: diffusion3.fmr diffusion3.yaml
	formura diffusion3.fmr

%.s: %.c diffusion3.h
	$(CC) $(CCFLAG) -S $< -o $@ $(CCLIBS)

%.o: %.s
	$(CC) $(CCFLAG) -c $< -o $@ $(CCLIBS)

main: main.o diffusion3.o
	$(CC) $(CCFLAG) $^ -o $@ $(CCLIBS)

run: main
	mkdir -p data
	./run ./main

clean:
	rm -rf *.o *.s main
