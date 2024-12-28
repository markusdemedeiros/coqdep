for file in ./pdf/*
do
	echo "$file"
	pdf2svg $file ./svg/$(basename $file .pdf).svg
done
