CC = mpicc
CCFLAG = -Wall -std=c11 -O3 -ffast-math -march=native
CCLIBS = -lm
FMRNAME = 3d_hllc
OBJ = main.o $(FMRNAME).o
HEADER = $(FMRNAME).h
NUMERICAL_CONFIG = config.yaml
.PHONY: clean plot run

$(FMRNAME).c $(FMRNAME).h: $(FMRNAME).fmr $(NUMERICAL_CONFIG)
	formura --nc $(NUMERICAL_CONFIG) $<

%.o: %.c $(HEADER)
	$(CC) $(CCFLAG) -c $<

main: $(OBJ)
	$(CC) $(CCFLAG) $(OBJ) -o $@ $(CCLIBS)

run: main
	mkdir -p data
	./run ./main

plot:
	python3 plot.py

clean:
	rm -rf main *.o data/ run $(FMRNAME).c $(FMRNAME).h *.png

