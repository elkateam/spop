module Przypomnienia where

import System.IO
import Data.Time
import Data.List
import Data.Char
import System.Locale
import Control.Exception
import System.IO.Error


{-
SPOP projekt - Przypomnienia
Dawid Góralczyk
£ukasz WoŸniak

-}




-- uruchomienie programu
main = do
	menuLoop

-- Menu glowne - pokazuje ogolne opcje programu.
menuLoop :: IO()
menuLoop = do 
	putStrLn "***** P R Z Y P O M N I E N I A *****"
	putStrLn "Menu glowne"
	putStrLn "1  Utworz zadanie"
	putStrLn "2  Przegladaj zadania"
	putStrLn "0  Wyjscie"
	cmd <- getLine
	case cmd of
		"1" -> do 
			utworzZadanie
			menuLoop
		"2" -> do przegladajZadania
		"0" -> do putStrLn "Koniec."
		_ -> do
			putStrLn "Nieprawidlowy wybor"
			menuLoop

-- Dodawanie zadania
utworzZadanie = do
	putStrLn "Dodawanie zadania"

-- Przegl¹danie zadañ
przegladajZadania = do
	putStrLn "Przegladanie zadan"
	putStrLn "1  Wszystkie zadania"
	putStrLn "2  Zadania do zrealizowania w dniu dzisiejszym"
	putStrLn "3  Zrealizowane zadania"
	putStrLn "0  Menu glowne"
	cmd <- getLine
	case cmd of
		"1" -> do 
			wszystkieZadania
			przegladajZadania
		"2" -> do 
			zadaniaDzis
			przegladajZadania
		"3" -> do 
			zrealizowaneZadania
			przegladajZadania
		"0" -> do menuLoop
		_ -> do
			putStrLn "Nieprawidlowy wybor"
			przegladajZadania
			
-- Wyœwietlanie wszystkich zadañ
wszystkieZadania = do
	putStrLn "Wszystkie zadania"

-- Wyœwietlanie zadañ do zrealizowania dzisiaj
zadaniaDzis = do
	putStrLn "Zadania do zrealizowania w dniu dzisiejszym"
		
-- Wyœwietlanie zadañ zrealizowanych
zrealizowaneZadania = do
	putStrLn "Zadania zrealizowane"
	
	



