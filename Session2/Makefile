all: S2_master.html S2_script.R S2_script_full.R

clean:
	rm S2_master.html S2_student.Rmd S2_script.R S2_script_full.R

S2_master.html:
	Rscript -e "library(rmarkdown); render('S2_master.Rmd')"

S2_student.Rmd:
	python remove_exercises.py S2_master.Rmd > $@

S2_script.R: S2_student.Rmd
	Rscript -e "library(knitr); purl('S2_student.Rmd', output = '$@', documentation = 2)"

S2_script_full.R:
	Rscript -e "library(knitr); purl('S2_master.Rmd', output = '$@', documentation = 2)"
