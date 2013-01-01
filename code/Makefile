all: step2tests output
run: run_step2_tests run_output

STEPTWOFILES = types.ml print.ml graphmatrix.ml step1.ml step2.ml step2tests.ml

step2tests: $(STEPTWOFILES)
	ocamlc -g -o step2tests $(STEPTWOFILES)

FILES = types.ml print.ml graphmatrix.ml step1.ml step2.ml step3.ml step2tests.ml output.ml


STEPTHREEFILES = types.ml print.ml graphmatrix.ml step3.ml step3tests.ml

step3tests: $(STEPTHREEFILES)
	ocamlc -g -o step3tests $(STEPTHREEFILES)

output: $(FILES)
	ocamlc -g -o output $(FILES)
    
run_step2_tests:
	./step2tests

run_step3_tests:
	./step3tests

run_output:
	./output

clean:
	rm -f step2tests *.cmi *.cmo

