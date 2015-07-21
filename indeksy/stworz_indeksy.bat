@echo off
R CMD BATCH --encoding=UTF-8 C:\Users\Marta\Desktop\Marta\GitHub\spiewnik\indeksy\indeksy.R
R CMD Sweave --pdf C:\Users\Marta\Desktop\Marta\GitHub\spiewnik\indeksy\autorzy.Rnw
R CMD Sweave --pdf C:\Users\Marta\Desktop\Marta\GitHub\spiewnik\indeksy\piosenki.Rnw
