all: parlbias_prq_final.docx

parlbias_prq_final.docx: parlbias_prq_final.tex
	pandoc \
	-S \
	$< -o $@ --bibliography=../../thesis/bibliography/library.bib \
	-V geometry='margin=1in' \
	-V fontsize=12pt \
	-V linestretch=2 \
	-V fontfamily=mathptmx \
	-V lang=english \
	-V papersize=a4paper
