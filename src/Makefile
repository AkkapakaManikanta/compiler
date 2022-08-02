COMMON=tigerAst.sml mips.sml temp.sml ir.sml basicblock.sml treeir.sml graph.sml


%.lex.sml: %.lex
	mllex $<

%.grm.sml: %.grm
	mlyacc $<


all: ec out.mips
#all: ec

.PHONY: all clean test

clean:
	rm -f *.lex.sml
	rm -f *.grm.sml *.grm.desc *.grm.sig ec
	rm -f out.mips

ec: ec.sml ec.mlb tigerExpr.grm.sml tigerExpr.lex.sml ${COMMON} translate.sml
	mlton ec.mlb
	
out.mips: ec test.tiger
	./ec < test.tiger > out.mips

test: all
	${CURDIR}/ec test.tiger
