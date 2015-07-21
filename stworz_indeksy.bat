@echo off

rm syf -rf
mkdir syf
cd syf

R CMD Sweave --pdf ..\songs.Rnw
RScript --encoding=UTF-8 ..\indeksy.R .. 2> blabla.out
R CMD Sweave --pdf autorzy.Rnw
R CMD Sweave --pdf piosenki.Rnw

cd ..

mv syf/songs.pdf .
mv syf/autorzy.pdf .
mv syf/piosenki.pdf .

:rm syf -rf
rm indeksy.Rout -rf