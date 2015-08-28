all:
	ghc FlowchartGenerator.hs

clean:
	rm -f *.hi *.o *.png *.gv FlowchartGenerator

run_bioinput:	all
	./FlowchartGenerator < bioinput.txt

run_cseinput:	all
	./FlowchartGenerator < cseinput.txt

run_mathinput:	all
	./FlowchartGenerator < mathinput.txt
