# Makefile

MD=$(wildcard ??_*.md)
MAIN=mainpage.markdown

all: $(MAIN)

$(MAIN): $(MD) Makefile
	for F in $(MD); do \
    echo sed -f markdown2doxygen.sed $F > ${F%%.md}.markdown; \
	  sed -f markdown2doxygen.sed $F > ${F%%.md}.markdown; \
	done
#	cat $(MD) > $(MAIN) ; cp $(MAIN) mainpage.md
#	sed -i .bak 's#`\([$$/\.0-z_][$$/\.0-z_]*\)`#@c \1#g' $(MAIN)
#	sed -i .bak 's#`\([$$/\.0-z_ ][$$/\.0-z_ ]*\)`#@code \1 @endcode#g' $(MAIN)
	
clean:
	@- rm -f $(MAIN) *.bak

