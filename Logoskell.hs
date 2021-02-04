module Main where

import Prelude hiding (Left, Right)

-- Crayon : gere les coordonnees et l'angle
data Crayon = TaillerCrayon Float Float Float deriving(Show)

-- ListeInstructions : explicite
type ListeInstructions = [TypeInstruction]

-- TypeInstruction : convertit la liste de donnees en instructions
data TypeInstruction = Forward Float
                     | Repeat Int [TypeInstruction]
                     | Left Float
                     | Right Float
                     deriving (Show, Read)

-- variables
crayon = TaillerCrayon 100.0 100.0 0.0
svgList = "<?xml version=\"1.0\" encoding=\"utf-8\"?><svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"400\" height=\"400\"><title>Exemple</title>"

-- definition des fonctions
parse string = read (string) :: ListeInstructions -- fonction avec argument "string"

addElemInList x c lst
    | c <= 0 = lst
    | c > 0 = addElemInList x (c-1) (x ++ lst)  

fonctionnement [] _ listesvg = listesvg ++ "</svg>" -- execute lorsque la liste d instruction est vide = fin du programme

fonctionnement (instruction:listeInstructionSuite) (TaillerCrayon x y angle) listesvg = case instruction of
                (Forward oui) -> (fonctionnement listeInstructionSuite crayon nouvellelistesvg)
                    where crayon = TaillerCrayon (x + oui*cos(angle*pi/180)) (y + oui*(-sin(angle*pi/180))) (angle)
                          nouvellelistesvg = listesvg ++ "<line x1=\""++(show x)++"\" y1=\""++(show y)++"\" x2=\""++(show (x+oui*cos(angle * pi/180)))++"\" y2=\""++(show(y+oui*(-sin(angle * pi/180))))++"\" stroke=\"red\"/>"
                (Left oui) -> (fonctionnement listeInstructionSuite crayon listesvg)
                    where crayon = TaillerCrayon x y (angle-oui)
                (Right oui) -> (fonctionnement listeInstructionSuite crayon listesvg)
                    where crayon = TaillerCrayon x y (angle+oui)
                (Repeat nombreDeFois liste) -> (fonctionnement resteprogrammeavecrepeat crayon listesvg)
                    where crayon = (TaillerCrayon x y angle)
                          resteprogrammeavecrepeat = (addElemInList liste nombreDeFois listeInstructionSuite)

main :: IO ()
main = do
    putStrLn "Mot clefs : entre crochets, avec les mots-clés Forward, Left, Right ou repeat []\n Exemple : [Forward 2, Left 4]"
    line <- getLine
    let prog = parse line
    let svg = fonctionnement prog crayon svgList
    putStrLn svg
    writeFile "s.svg" svg
