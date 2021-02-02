EMACS = emacs

all: docs style
	${EMACS} --batch --script ./org-publish.el

docs: texidocs/carbslinux.texi texidocs/carbslinux.txt
	mkdir -p docs/docs
	makeinfo --html --css-ref=https://www.gnu.org/software/gnulib/manual.css -o docs/docs/carbslinux texidocs/carbslinux.texi
	makeinfo --html --css-ref=https://www.gnu.org/software/gnulib/manual.css --no-split -o docs/docs/carbslinux.html texidocs/carbslinux.texi
	cp texidocs/carbslinux.txt docs/docs/carbslinux.txt

style: style/carbslinux.css
	mkdir -p docs
	cp style/carbslinux.css docs/style.css

update-submodules:
	git submodule update --remote --init --recursive -f

publish:
	${EMACS} --batch --script ./org-publish.el

rebuild:
	rm -rf .cache/var/timestamps
	${MAKE} all

clean:
	rm -rf ./docs ./.cache/var/timestamps

.PHONY: all docs style update-submodules publish rebuild clean
