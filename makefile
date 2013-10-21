NAME=Eagle
TARGET_DIR=target/
VERSION=0.0.1-SNAPSHOT

ARTIFACT=$(TARGET_DIR)$(NAME)-$(VERSION)

DIR=src/main/ocaml/org/bracket/eagle/

OCAML=ocamlbuild
OCOPT=-use-ocamlfind -r -quiet -build-dir $(TARGET_DIR)build -install-bin-dir $(TARGET_DIR)

.PHONY:
all: eagle

eagle:: clean
	$(OCAML) $(OCOPT) $(DIR)main.native
	cp -f $(TARGET_DIR)build/$(DIR)main.native $(TARGET_DIR)

clean:
	$(OCAML) $(OCOPT) -clean
