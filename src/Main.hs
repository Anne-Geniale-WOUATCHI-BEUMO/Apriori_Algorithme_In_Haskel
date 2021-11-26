module Main where

import System.IO
import Data.List.Split
import qualified Data.List as List
import qualified Data.Set as Set
import Control.Monad




--POUR CE PROGRAMME NOUS AVONS UTILISE LE GESTIONNAIRE DE PROJET STACK
--POUR EXECUTER CE PROGRAMME IL FAUT AU PREALABLE INSTALLE LE MODULE Split et Configurer
--LES FONCTIONS CI DESSOUS PERMET DE :
--        *Générer un tableau de transaction dans un fichier  transact.txt
--        *Générer la liste des 1-Candidat dans un fichier cand1.txt
--        *Générer la liste des 2-candidat (blocké dessus)
--        *Générer la liste des 3-candidat et plus (blocké)
--        *Générer les itemsets Frequents (En cours)
--  NB * La génération des candidats a été effectué sur 03 axes parcequ'a partir du candidat 3 , on peut généralisé
--     * 1 et 2 sont des cas particulier , à partir de 3 les K-2 items doivent etre identiques



maFonction::(Show a) => [a] -> [a]
maFonction  [x,_] = [x]
sum' :: (Num a) => [a] -> a
sum' = sum
compar :: (Show a,Eq a)=> [a]->[a]->Bool
compar elt1 elt2 = elt1 == elt2

mafonct :: [String] -> String
mafonct [] = ""
mafonct [x] = x
mafonct (x:xs) = x


--SEPARE INVOICE ET DESCRIPTION !!IMPORTANT POUR POUVOIR GENERER LES TRANSACTIONS

fonction:: Handle -> IO ()
fonction nomF  = do
    t <- hIsEOF nomF
    if t
    then return ()
    else do
      fic <- hGetLine nomF
      let  gin =  take 1 ( splitOn "\t" fic)
      appendFile "./file/invoice.txt" (mafonct gin)
      appendFile "./file/invoice.txt" "\n"
      appendFile "./file/Candidat1.txt"  (mafonct (tail $ splitOn "\t" fic))
      appendFile "./file/Candidat1.txt" "\n"
      fonction nomF
      hClose nomF
      fich2 <- openFile "./file/invoice.txt" ReadMode
      filtrerFile fich2  "" True  ""
    



--FILTRER LES FICHIERS POUR OBTENIR LES INVOICES SANS DOUBLON !! IMPORTANT POUR LA GENERATION DES TRANSACTION

filtrerFile:: Handle ->String->Bool ->String -> IO ()
filtrerFile fich1  contain diff cont = do
     t<- hIsEOF fich1  
     if t 
     then do
       hClose fich1
       fich1 <- openFile "./file/dataset.txt" ReadMode
       fich2 <- openFile "./file/tempo.txt" ReadMode
       transactFichier fich1 fich2 "" False
       return()
       else do
          if diff 
          then do 
             contain <- hGetLine fich1
             appendFile "./file/tempo.txt" contain
             appendFile "./file/tempo.txt" "\n" 
             filtrerFile fich1  contain False cont
          else do
              cont <- hGetLine fich1
              if cont == contain
              then do
             --contain <- hGetLine fich1 
                filtrerFile fich1 contain False cont
              else do
             --appendFile "tempo.txt" cont
               let contain = cont
               appendFile "./file/tempo.txt" cont
               appendFile "./file/tempo.txt" "\n" 
               filtrerFile fich1  contain False cont



--GENERER LES TRANSACTIONS 

transactFichier :: Handle -> Handle -> String ->Bool -> IO()
transactFichier fichier  fichTempo linefichTempo  gin  = do
    t <- hIsEOF fichier
    if t
      then do
        return ()
      else do
        linefichier <- hGetLine fichier
        let line1 =  take 1 $ splitOn "\t" linefichier
        let line2 = tail $ splitOn "\t" linefichier
        if  gin == False 
        then do
          linefichTempo <- hGetLine fichTempo
          if linefichTempo == mafonct line1 
          then do
             appendFile "./file/transact.txt" linefichTempo 
             appendFile "./file/transact.txt" ":" 
             appendFile "./file/transact.txt" "[\""
             appendFile "./file/transact.txt" (mafonct line2)
             appendFile "./file/transact.txt" "\""
             transactFichier fichier  fichTempo linefichTempo True 
          else do
                  --appendFile "transact.txt" "]\n"
                  transactFichier fichier fichTempo linefichTempo  True
        else do  
              if linefichTempo == mafonct line1 
              then do 
                   appendFile "./file/transact.txt" ";" 
                   appendFile "./file/transact.txt" "\""
                   appendFile "./file/transact.txt" (mafonct line2)
                   appendFile "./file/transact.txt" "\""
                   transactFichier fichier  fichTempo  linefichTempo  True    
              else do
                  appendFile "./file/transact.txt" "]\n"
                  linefichTempo <- hGetLine fichTempo
                  --transactFichier fichier fichTempo linefichTempo  False 
                  appendFile "./file/transact.txt" linefichTempo 
                  appendFile "./file/transact.txt" ":" 
                  appendFile "./file/transact.txt" "[\""
                  appendFile "./file/transact.txt" (mafonct line2)
                  appendFile "./file/transact.txt" "\""
                  transactFichier fichier  fichTempo linefichTempo True 




----- *GENERATION DES CANDIDATS 1 DANS UN FICHIER Cand1.txt

genCand1 :: Handle ->[String] -> IO ()
genCand1 file tab = do
     t<- hIsEOF file
     if t
       then do
         let tabl = List.nub tab
         cand1 tabl
         return ()
       else do
          line <- hGetLine file
          --let tab = [""]
          let tab1 = List.insert line tab
          genCand1 file tab1        
cand1 :: [String] -> IO() 
cand1 [""] = print "liste vide!"
cand1 [x] = appendFile "./file/Cand1.txt" x
cand1 [] = print "Liste vide"
cand1 (x:xs) = do
     appendFile "./file/Cand1.txt" x
     appendFile "./file/Cand1.txt" "\n"
     cand1 xs
            

add :: Integer -> Integer
add a = a+1


--    *PETITE FONCTION QUI DEVRAIT ME PERMETTRE DE STOCKER LES ELEMENTS DU FICHIER DANS UNE LISTE !!
--       AFIN DE POUVOIR LES MANIPULE PLUS AISEMENT !!

--maFonc :: Handle  -> [String ]
--maFonc fil = do
   --line <- hGetLine fil
   --let tab = List.insert line tab
   --maFonc fil
            

--    * GENERATION DES CANDIDATS   2
--    * cette fonction ne marche pas encore!

--candidat2::Handle->String ->IO()
--candidat2 fil1 fil2 = do
       --elt <- hGetLine fil1
       --appendFile fil2 elt
      -- t <- HisEOF fil1
       --if t 
        -- then do return()
         --else do
           -- elt2 <- hGetLine fil1
            --appendFile fil2 ","
            --appendFile fil2  elt2

cand2 :: [String ]->Int->Int -> [[String]]
cand2 gin nb nbr= do
    let tab =List.insert (gin !! nb) tab
    if nbr<1
      then do
        tab <- List.insert (gin !! (nb+1)) tab
        cand2 gin (nb+1) (nbr+1)
      else do
        let tabl = List.insert tab tabl
        tab <- take 1 tab
        cand2 gin (nb+1) 0
       



main:: IO()
main = do
    
    --POUR GENERER TRANSACTION IL FAUT :
    -- ° Extraire les Invoices du dataset d'ou le fichier "invoice.txt"
    -- ° Supprimé les doublons des invoices à partir de invoice.txt d'ou le fichier "tempo.txt"
    -- ° Utilisé les fichiers dataset.txt et tempo.txt pour générer les transactions d'ou le fichier "transact.txt"
   
       -- ---------°°°°°°°°°°°°°___Execution__°°°°°°°°°°---------------
    --PREMIERE EXECUTION
    fich1 <- openFile "./file/dataset.txt" ReadMode
    fonction fich1
    hClose fich1
    --Deuxieme EXECUTION : décommentez ce qui suit
    --fich2 <- openFile "./file/Candidat1.txt" ReadWriteMode
    --genCand1 fich2 [""]
    --hClose fich2
   
  

   