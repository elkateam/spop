module Przypomnienia where

import System.IO
import Data.Time
import Data.List
import Data.Char
import Data.Time.Calendar
import System.Locale
import Control.Exception
import System.IO.Error
import System.IO.Unsafe


{-
SPOP projekt - Przypomnienia
Dawid Góralczyk
£ukasz WoŸniak

-}
plikZwydarzeniami = "wydarzenia.txt"
dzisiaj = getCurrentDay :: Day

-- dane o wydarzeniu
data Wydarzenie = Wydarzenie {
	wydarzenieId            :: Int, 	-- id wydarzenia
	nazwa               	:: String,	-- nazwa wydarzenia
	dataWydarzenia          :: Day, -- data wydarzenia
	godzinaWydarzenia		:: String,	-- godzina wydarzenia
	cykl                    :: Int,	-- cykl zadania	1-jednorazowy, 2-codziennie, 3-tydzien, 4-miesiac, 5-rok
	zrealizowane			:: Bool  -- 1-zadanie zrealizowane, 0-niezrealizowane
} deriving (Show, Read, Eq)

-- Zwraca id wydarzenia
getWydarzenieID :: Wydarzenie -> Int
getWydarzenieID (Wydarzenie {wydarzenieId=id}) = id

--zwraca zrealizowanie zadania
getZrealizowane :: Wydarzenie -> Bool
getZrealizowane (Wydarzenie {zrealizowane=zr}) = zr

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

--zwraca nastepny termin wydarzenia
nastepnyTermin :: Day -> Int -> Day
nastepnyTermin dzien cykl
	| cykl==2 = addDays 1 dzien
	| cykl==3 = addDays 7 dzien
	| cykl==4 = addGregorianMonthsClip 1 dzien
	| cykl==5 = addGregorianYearsClip 1 dzien

-- zapisz wydarzenia do pliku
zapiszWydarzenia wydarzenia = do
	writeFile plikZwydarzeniami (show wydarzenia)

-- wczytaj zadania z pliku
wczytajPlik = do
	hFile <- openFile plikZwydarzeniami ReadMode
	fileStr <- hGetContents hFile
	let wydarzenia = (read fileStr) :: [Wydarzenie]
	putStrLn ("Wczytano zadan: " ++ (show (length wydarzenia)))
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

--wypisuje, czy zadanie jest zrealizowane
zrealizowaneNapis :: Bool -> String
zrealizowaneNapis x 
	| x==False = "NIE"
	| x==True = "TAK"
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
	cykl=cykl,
	zrealizowane=zrealizowane}) = 
	"\nWydarzenie " ++ show wydarzenieId ++ ": " ++ (show nazwa) 
		++ "\n    Dzien: " ++ (show dataWydarzenia) ++ " Godzina: " ++ (show godzinaWydarzenia) ++ "\n"
		++ "    Cykl: " ++ (cyklNapis cykl) ++ "\n" ++ "    Zrealizowane: " ++ (zrealizowaneNapis zrealizowane) ++ "\n"


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

--oznaczanie zadania jako zrealizowanego
realizujZadanie :: [Wydarzenie] -> [Wydarzenie] -> [Wydarzenie]
realizujZadanie [] zadanie = []
realizujZadanie (x:xs) [Wydarzenie {
	wydarzenieId=wydarzenieId, 
	nazwa=nazwa,
	dataWydarzenia=dataWydarzenia, 
	godzinaWydarzenia=godzinaWydarzenia,
	cykl=cykl,
	zrealizowane=zrealizowane}] = 
		if (getWydarzenieID x == wydarzenieId) then do
			let noweWydarzenie = Wydarzenie {
				wydarzenieId=wydarzenieId, 
				nazwa=nazwa,
				dataWydarzenia=dataWydarzenia, 
				godzinaWydarzenia=godzinaWydarzenia,
				cykl=cykl,
				zrealizowane=True}
			if (cykl==1) then
				xs ++ [noweWydarzenie]
			else do
				let stareWydarzenie = Wydarzenie {
					wydarzenieId=nastepneWydarzenieID (x:xs) 1,  
					nazwa=nazwa,
					dataWydarzenia=nastepnyTermin dataWydarzenia cykl, 
					godzinaWydarzenia=godzinaWydarzenia,
					cykl=cykl,
					zrealizowane=False}
				xs ++ [noweWydarzenie] ++ [stareWydarzenie]
			
		else
			[x] ++ realizujZadanie xs (getWydarzenie xs wydarzenieId)

	
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

getCurrentDay :: Day
getCurrentDay = utctDay (unsafePerformIO getCurrentTime)

getWydarzenieDataWydarzenia :: Wydarzenie -> String
getWydarzenieDataWydarzenia (Wydarzenie{
							wydarzenieId = wydId,
							dataWydarzenia=dataWydarz}) = "Data wydarzenia " ++ show wydId ++ ": " ++ show dataWydarz ++ "\n"

getDatyWydarzen :: [Wydarzenie] -> String
getDatyWydarzen [] = []
getDatyWydarzen (x:xs) = (getWydarzenieDataWydarzenia x) ++ getDatyWydarzen xs

getCurrentWydarzenia :: [Wydarzenie] -> Day -> String
getCurrentWydarzenia [] dzis = []
getCurrentWydarzenia (x:xs) dzis
	| getDataWydarzenie x == dzis = (zadanieNapis x) ++ getCurrentWydarzenia xs dzis
	| getCyklWydarzenie x == 2 = (zadanieNapis x) ++ getCurrentWydarzenia xs dzis
	| getCyklWydarzenie x == 3 && (diffDays dzis (getDataWydarzenie x)) `mod` 7 == 0 = (zadanieNapis x) ++ getCurrentWydarzenia xs dzis
	| getCyklWydarzenie x == 4 && (diffDays dzis (getDataWydarzenie x)) `mod` 30 == 0 = (zadanieNapis x) ++ getCurrentWydarzenia xs dzis
	| getCyklWydarzenie x == 5 && (diffDays dzis (getDataWydarzenie x)) `mod` 365 == 0 = (zadanieNapis x) ++ getCurrentWydarzenia xs dzis
	| otherwise = getCurrentWydarzenia xs dzis

getCyklWydarzenie :: Wydarzenie -> Int
getCyklWydarzenie (Wydarzenie{cykl = cyklWyd}) = cyklWyd

getDataWydarzenie :: Wydarzenie -> Day
getDataWydarzenie (Wydarzenie{dataWydarzenia = dataWydarz}) = dataWydarz

getZrealizowaneFromWydarzenie :: Wydarzenie -> Bool
getZrealizowaneFromWydarzenie (Wydarzenie{zrealizowane = zreal}) = zreal

getZrealizowaneWydarzenia :: [Wydarzenie] -> String
getZrealizowaneWydarzenia [] = []
getZrealizowaneWydarzenia (x:xs)
	| getZrealizowaneFromWydarzenie x == True = (zadanieNapis x) ++ getZrealizowaneWydarzenia xs
	| otherwise = getZrealizowaneWydarzenia xs
	
-- uruchomienie programu
main = do
	utworzPlikWydarzen 
	let a = "test";
	menuLoop

-- Menu glowne - pokazuje ogolne opcje programu.
menuLoop :: IO()
menuLoop = do 
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
		"3" -> do 
			setDzisiaj
			menuLoop
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
				putStrLn "\nZadanie zapisane.\n"
			else
				putStr "\nNiepoprawny cykl wydarzenia.\n"
		else
			putStr "\nNiepoprawna godzina wydarzenia!\n"
	else 
		putStrLn "\nData jest nieprawidlowa!\n"
	

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
			realizujWydarzenie
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
		putStrLn "Niepoprawny numer zadania"
		
realizujWydarzenie = do
	zadania <- wczytajPlik
	putStrLn "Podaj numer zadania ktore chcesz oznaczyc jako zrealizowane"
	id_zadania <- getLine
	if czyLiczba id_zadania then do
		let zadanieID = (read id_zadania) :: Int
		let zadanie = getWydarzenie zadania zadanieID 
		if (zadanie /= [] && not(getZrealizowane (head zadanie))) then do
			zapiszWydarzenia(realizujZadanie zadania zadanie)
			putStrLn "test"
		else
			putStrLn "Brak zadania lub zostalo juz zrealizowane"
	else
		putStrLn "Niepoprawny numer zadania"	
	
-- Wyœwietlanie zadañ do zrealizowania dzisiaj
zadaniaDzis = do
	putStrLn "Zadania do zrealizowania w dniu dzisiejszym"
	--let dzisiaj = getCurrentDay :: Day
	putStrLn ("Dzisiejsza data: " ++ show (dzisiaj) ++ "\n")
	wydarzenia <- wczytajPlik
	putStrLn (getCurrentWydarzenia wydarzenia dzisiaj)

-- Wyœwietlanie zadañ zrealizowanych
zrealizowaneZadania = do
	putStrLn "Zadania zrealizowane\n"
	wydarzenia <- wczytajPlik
	putStrLn (getZrealizowaneWydarzenia wydarzenia)
	
setDzisiaj = do
	putStr "Podaj date dzisiejsza (TEST) (YYYY-MM-DD): "
	dataDzisStr <- getLine
	if czyData dataDzisStr then do
		let dzisiaj = (read dataDzisStr) :: Day
		putStrLn "Poprawnie ustalono date dzisiejsza"
	else do
		putStrLn "Wprowadzona data jest w zlym formacie"


