# Data

All data was taken from the website of Felix Just, S.J., Ph.D. who compiled this information.

- [Biblical Book Names & Abbreviations](http://catholic-resources.org/Bible/Abbreviations-Abreviaciones.htm)
- [Old Testament Reference](http://catholic-resources.org/Bible/OT-Statistics-Compared.htm)
- [New Testament Reference](http://catholic-resources.org/Bible/NT-Statistics-Greek.htm)
- [Scripture Index of Lectionary Readings Used for Weekday Masses](http://catholic-resources.org/Lectionary/Index-Weekdays.htm)
- [Scripture Index of Lectionary Readings Used for Sundays and Major Feasts](http://catholic-resources.org/Lectionary/Index-Sundays.htm)

## Data Quality

There were some tricky things I had to fix to get the data read in properly. One was the differences between '—' vs '-' for string processing. I replaced all of the former with the latter in the files to make processing easier.

I also changed some readings that didn't parse well in my setup as described below:

"Eccl 1:2, 2:21-23" should be "Eccl 1:2; 2:21-23"
"Isa 49: 3, 5-6" should be "Isa 49:3, 5-6"
"Bar 3:9-15, 32-4:4" is easier parsed as "Bar 3:9-15, 3:32-4:4"
"Jon 3:1-5, 10" should be "Jonah 3:1-5, 10"
"Habb 1:2-3; 2:2-4" should be "Hab 1:2-3; 2:2-4"

There were also many readings from the Psalms that were written with "+" symbols as in "Ps 30:2+4, 5-6, 11-12a+13b" which I didn't know what to do with so these were not included in the analysis.
