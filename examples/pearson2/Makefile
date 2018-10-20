CC = mpicc
CCFLAG = -Wall -std=c11 -O3 -ffast-math -mavx2 -mfma -fopenmp
CCLIBS = -lm
TARGET = pearson2
.SUFFIXES:
.PRECIOUS: %.s

$(TARGET).c $(TARGET).h: $(TARGET).fmr $(TARGET).yaml
	formura $(TARGET).fmr

%.s: %.c $(TARGET).h
	$(CC) $(CCFLAG) -S $< -o $@ $(CCLIBS)

%.o: %.s
	$(CC) $(CCFLAG) -c $< -o $@ $(CCLIBS)

main: main.o $(TARGET).o
	$(CC) $(CCFLAG) $^ -o $@ $(CCLIBS)

run: main
	mkdir -p data
	./run ./main

clean:
	rm -rf *.o *.s main
