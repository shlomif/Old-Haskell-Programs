OBJECTS = Contents_Abs.o Contents.o Url.o QuadPres.o main.o

all: qp

%.o : %.hs
	ghc -c -o $@ $<

qp: $(OBJECTS)
	ghc -o $@ $(OBJECTS)

clean:
	rm -f qp $(OBJECTS)

