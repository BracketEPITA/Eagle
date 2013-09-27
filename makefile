rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

NAME := Eagle
TARGET_DIR := target/
VERSION := 0.0.1-SNAPSHOT

ARTIFACT := $(TARGET_DIR)$(NAME)-$(VERSION)

OCAML := ocamlopt

SRC  := $(filter %.ml,$(call rwildcard,src/main/ocaml/,*))

.PHONY:
all: $(SRC)
	mkdir -p $(TARGET_DIR) && \
	$(OCAML) -o $(ARTIFACT) $^

clean:
	rm -Rf $(TARGET_DIR)