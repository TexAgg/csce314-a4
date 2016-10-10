SOURCE=Main
FLAGS = -outputdir bin

all: $(SOURCE)

$(SOURCE): $(SOURCE).hs
	ghc $(FLAGS) $(SOURCE).hs

.PHONY: clean
clean:
	rm -rf *.hi *.o

.PHONY: run
run: $(SOURCE)
	./$(SOURCE)