.POSIX:
.SUFFIXES:
CWARN = -Wall -Wextra
CFLAGS = -Og -g

all: chanter

chanter: parser.o lexer.o test.o
	gcc -o $@ $(CFLAGS) $(CWARN) $^

parser.o: parser.c parser.h
lexer.o: lexer.c lexer.h
test.o: test.c

clean:
	rm -- chanter *.o

check-syntax:
	GCC_COLORS= gcc $(CWARN) ${CHK_SOURCES} || true

.SUFFIXES: .c .o
.c.o:
	gcc ${CFLAGS} -c $<
