EXE := chanter
SRC := $(wildcard *.c)
OBJ := $(patsubst %.c, %.o, $(SRC))
CC_OPTS = -Wall -Wextra
CC_FLAGS = -Og

$(EXE): $(OBJ)
	gcc -o $@ $(CC_OPTS) $^

%.o: %.c
	gcc -c $(CC_FLAGS) -o $@ $^

check-syntax:
	GCC_COLORS= gcc $(CC_OPTS) ${CHK_SOURCES} || true
