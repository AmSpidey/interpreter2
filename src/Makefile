all:
	happy -gca ParOstaczGr.y
	alex -g LexOstaczGr.x
	ghc --make TestOstaczGr.hs -o TestOstaczGr

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocOstaczGr.* LexOstaczGr.* ParOstaczGr.* LayoutOstaczGr.* SkelOstaczGr.* PrintOstaczGr.* TestOstaczGr.* AbsOstaczGr.* TestOstaczGr ErrM.* SharedString.* ComposOp.* ostacz_gr.dtd XMLOstaczGr.* Makefile*
	

