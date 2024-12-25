for file in ./dot/*
do
	echo "$file"
	dot -Tpdf $file -o ./pdf/$(basename $file .dot).pdf
done
