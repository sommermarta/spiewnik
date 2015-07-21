@echo off

R CMD BATCH pomoc.R

rm pomoc.Rout -f
rm .RData -f