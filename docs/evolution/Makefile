default: figures
	pdflatex main.tex
	pdflatex main.tex

# The following depends on graphviz.
# To install graphviz, download the graphviz-rhel.repo file and save it
# (as root) in /etc/yum.repos.d/. Then do "yum install graphviz".

figures:
	dot -Tpdf evolution.dot -o evolution.pdf

