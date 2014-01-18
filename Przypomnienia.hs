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
plikZwydarzeniami = "wydarzenia.txt"
-- dane o wydarzeniu
data Wydarzenie = Wydarzenie {
	wydarzenieId            :: Int, 	-- id wydarzenia
	nazwa               	:: String,	-- nazwa wydarzenia
	dataWydarzenia          :: Day, -- data wydarzenia
	godzinaWydarzenia		:: String,	-- godzina wydarzenia
	cykl                    :: Int,	-- cykl rezerwacji	1-jednorazowy, 2-codziennie, 3-tydzien, 4-miesiac, 5-rok
	zrealizowane			:: Bool  -- 1-zadanie zrealizowane, 0-niezrealizowane
} deriving (Show, Read, Eq)

-- Zwraca id wydarzenia
getWydarzenieID :: Wydarzenie -> Int
getWydarzenieID (Wydarzenie {wydarzenieId=id}) = id

-- Zwraca wydarzenie o podanym ID
getWydarzenie :: [Wydarzenie] -> Int -> [Wydarzenie]
getWydarzenie [] id = []
getWydarzenie (x:xs) id =
	if (getWydarzenieID x == id) then
		[x]
	else
		(getWydarzenie xs id)

-- Zwraca nowe, wolne ID wydarzenia
nastepneWydarzenieID :: [Wydarzenie] -> Int -> Int
nastepneWydarzenieID [] newID = newID
nastepneWydarzenieID (x:xs) newID =
	if (getWydarzenieID x >= newID) then
		nastepneWydarzenieID xs ((getWydarzenieID x)+1)
	else
		nastepneWydarzenieID xs newID

-- zapisz wydarzenia do pliku
zapiszWydarzenia wydarzenia = do
	writeFile plikZwydarzeniami (show wydarzenia)

-- wczytaj zadania z pliku
wczytajPlik = do
	hFile <- openFile plikZwydarzeniami ReadMode
	fileStr <- hGetContents hFile
	let wydarzenia = (read fileStr) :: [Wydarzenie]
	putStrLn ("Wczytano zadan: " ++ (show (length wydarzenia)) ++ "\n")
	hClose hFile
	return wydarzenia

--zamien cykl na napis
cyklNapis :: Int -> String
cyklNapis x 
	| x==1 = "wydarzenie jednorazowe"
	| x==2 = "kazdego dnia"
	| x==3 = "co tydzien"
	| x==4 = "co miesiac"
	| x==5 = "co rok"
	| otherwise = ""
	
-- zamien liste zadañ na napis
zadaniaNapis :: [Wydarzenie] -> String
zadaniaNapis [] = ""
zadaniaNapis (x:xs) = (zadanieNapis x) ++ zadaniaNapis xs

-- zamien zadanie na napis
zadanieNapis :: Wydarzenie -> String
zadanieNapis (Wydarzenie {
	wydarzenieId=wydarzenieId, 
	nazwa=nazwa,
	dataWydarzenia=dataWydarzenia, 
	godzinaWydarzenia=godzinaWydarzenia,
	cykl=cykl}) = 
	"Wydarzenie " ++ show wydarzenieId ++ ": " ++ (show nazwa) ++ "\n" 
		++  "Dzien: " ++ (show dataWydarzenia) ++ " Godzina: " ++ (show godzinaWydarzenia) ++ "\n"
		++ "Cykl: " ++ (cyklNapis cykl) ++ "\n"


-- sprawdza, czy data jest w formacie YYYY-MM-DD
czyData :: String -> Bool
czyData "" = False
czyData date = 
	if ((length date) /= 10) then
	False
	else
	sprawdzDateString date (length date)

-- funkcja pomocnicza do sprawdzenia daty
sprawdzDateString :: String -> Int -> Bool
sprawdzDateString [x] 1
	| isDigit x == True = True
	| otherwise = False
sprawdzDateString (x:xs) ind
	| (ind == 3 || ind == 6) && (x == '-') = sprawdzDateString xs (ind-1)
	| (ind == 3 || ind == 6) && (x /= '-') = False
	| isDigit x == True = sprawdzDateString xs (ind-1)
	| otherwise = False

czyGodzina :: String -> Bool
czyGodzina "" = False
czyGodzina time =
	if ((length time) /= 5) then
	False
	else
	sprawdzGodzineString time (length time)

-- sprawdzanie godziny
sprawdzGodzineString :: String -> Int -> Bool
sprawdzGodzineString [x] 1
	| isDigit x == True = True
	| otherwise = False
sprawdzGodzineString (x:xs) ind
	| (ind == 3) && (x == ':') = sprawdzGodzineString xs (ind-1)
	| (ind == 3) && (x /= ':') = False
	| isDigit x == True = sprawdzGodzineString xs (ind-1)
	| otherwise = False

-- sprawdzanie, czy napis jest liczba
czyLiczba :: String -> Bool
czyLiczba "" = False
czyLiczba [x] =
	if isDigit x == True then
	True
	else
	False
czyLiczba (x:xs) = 
	if (isDigit x == True) then
	czyLiczba xs
	else
	False

-- sprawdzanie, czy napis jest poprawnym cyklem
czyCykl :: String -> Bool
czyCykl "" = False
czyCykl cykl
	| cykl=="1" || cykl=="2" || cykl=="3" || cykl=="4" || cykl=="5" = True
	| otherwise = False

--usuwanie zadania:
usunZadanie :: [Wydarzenie] -> Int -> [Wydarzenie]
usunZadanie [] id = []
usunZadanie [zadanie] id =
	if (getWydarzenieID zadanie) == id then
		[]
	else
		[zadanie]
usunZadanie (s:reszta) id = (usunZadanie [s] id) ++ (usunZadanie reszta id)

	
utworzPlikWydarzen = do
	catch   (do 
		putStrLn ("Sprawdzanie " ++ plikZwydarzeniami)
		plik <- readFile plikZwydarzeniami
		return ()
		) errorHandler
	where errorHandler e = 
		if isDoesNotExistError e then do
			putStrLn ("Tworzenie pliku: " ++ plikZwydarzeniami)
			writeFile plikZwydarzeniami (show ([] :: [Wydarzenie]))
			else 
			putStrLn ("Blad przy otwieraniu pliku: " ++ plikZwydarzeniami)	
	
-- uruchomienie programu
main = do
	utworzPlikWydarzen 
	let a = "test";
	menuLoop

-- Menu glowne - pokazuje ogolne opcje programu.
menuLoop :: IO()
menuLoop  = do 
	putStrLn "***** P R Z Y P O M N I E N I A *****"
	putStrLn "Menu glowne"
	putStrLn "1  Utworz zadanie"
	putStrLn "2  Zarzadzanie zadaniami"
	putStrLn "3  Wprowadz dzisiejsza date"
	putStrLn "0  Wyjscie"
	cmd <- getLine
	case cmd of
		"1" -> do 
			utworzZadanie
			menuLoop
		"2" -> do przegladajZadania
		--"3" -> do wprowadzDate ""
		"0" -> do putStrLn "Koniec."
		_ -> do
			putStrLn "Nieprawidlowy wybor"
			menuLoop

-- Dodawanie zadania
utworzZadanie = do
	putStrLn "Dodaj wydarzenie"
	putStr "Podaj nazwe wydarzenia: "
	nazwaWyd <- getLine
	putStr "Podaj date wydarzenia (YYYY-MM-DD): "
	dataWydarzeniaStr <- getLine
	if czyData dataWydarzeniaStr then do
		let dataWyd = (readTime defaultTimeLocale "%F" dataWydarzeniaStr) :: Day
		putStr "Podaj godzine wydarzenia (hh:mm): "
		godzinaWydarzeniaStr <- getLine
		if czyGodzina godzinaWydarzeniaStr then do
			let godzinaWyd = godzinaWydarzeniaStr
			putStrLn "Wybierz cykl wydarzenia: "
			putStrLn "1  Wydarzenie jednorazowe"
			putStrLn "2  Cykl dzienny"
			putStrLn "3  Cykl tygodniowy"
			putStrLn "4  Cykl miesieczny"
			putStrLn "5  Cykl roczny"
			cyklStr <- getLine
			if czyCykl cyklStr then do
				let cyklWyd = (read cyklStr ) :: Int
				wydarzenia <- wczytajPlik
				let noweWydarzenie = Wydarzenie{
					wydarzenieId=nastepneWydarzenieID wydarzenia 1,
					nazwa=nazwaWyd,
					dataWydarzenia = dataWyd,
					godzinaWydarzenia = godzinaWyd,
					cykl = cyklWyd,
					zrealizowane = False
				}
				zapiszWydarzenia (wydarzenia ++ [noweWydarzenie])
				putStrLn "\nRezerwacja zapisana.\n"
			else
				putStr "\nNiepoprawny cykl\n"
		else
			putStr "\nNiepoprawna godzina wydarzenia!!!!!!!!!!!!!!!!!!!!!!\n"
	else 
		putStrLn "\nData jest nieprawidlowa !!!!!!!!!!!!!!\n"
	

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
	putStrLn "Wszystkie zadania: "
	zadania <- wczytajPlik
	putStrLn (zadaniaNapis zadania)
	putStrLn "1  Usun zadanie"
	putStrLn "2  Oznacz zadanie jako zrealizowane"
	putStrLn "0  Powrot"
	cmd <- getLine
	case cmd of
		"1" -> do 
			usunWydarzenie
			przegladajZadania
		"2" -> do 
			zadaniaDzis
			przegladajZadania
		"0" -> do
			przegladajZadania
		_ -> do
			putStrLn "Nieprawidlowy wybor"
			wszystkieZadania
	
usunWydarzenie = do
	zadania <- wczytajPlik
	putStrLn "Podaj numer zadania do usuniecia"
	id_zadania <- getLine
	if czyLiczba id_zadania then do
		let zadanieID = (read id_zadania) :: Int
		zapiszWydarzenia(usunZadanie zadania zadanieID)
	else
		putStrLn "Niepoprawny numer stolika"
-- Wyœwietlanie zadañ do zrealizowania dzisiaj
zadaniaDzis = do
	putStrLn "Zadania do zrealizowania w dniu dzisiejszym"
		
-- Wyœwietlanie zadañ zrealizowanych
zrealizowaneZadania = do
	putStrLn "Zadania zrealizowane"
	
	



