.PHONY: all
all:
	jbuilder build --dev

.PHONY: clean
clean:
	jbuilder clean

.PHONY: utop
utop:
	jbuilder utop
