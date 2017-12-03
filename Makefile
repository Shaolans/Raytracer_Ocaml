
all:
	ocamlopt -o raytracer-demo str.cmxa sceneLoader.ml graphics.cmxa raytracer.ml

clean:
	rm -rf raytracer-demo *.cm* *.o
