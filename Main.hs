import System.Console.ANSI
import Data.Maybe
import Control.Monad
import Control.Concurrent
import System.IO
import System.Directory
import Control.Applicative
import System.FilePath

type Size = (Int, Int)


-- Config --
data Config = Config {cSize :: Size}

mkConfig :: IO Config
mkConfig = do
    -- Get terminal size for tRow & tCol --
    size <- getTerminalSize
    return $ Config { cSize = fromJust size}
    

-- Display Functions --
displayEntireScreen :: Config -> Char -> IO ()
displayEntireScreen config char = do
    let (tRow, tCol) = cSize config
    -- Repeates the same Char across the entire screen --
    putStrLn $ replicate tCol char

displayCentered :: String -> Int -> IO ()
displayCentered text width = do
    -- Puts text centered between "|" --
    putStrLn $ "|" ++ (replicate (div (width - (length text) - 2) 2) ' ') ++ text ++ (replicate (div (width - (length text) - 1) 2) ' ') ++ "|"

-- TITLE --
displayTitle :: Config -> String -> IO ()
displayTitle config text = do
    setCursorPosition 0 0
    let (tRow, tCol) = cSize config
    
    -- Builds box and displays centered title text --
    displayEntireScreen config '-'
    displayCentered text tCol
    displayEntireScreen config '-'

-- CONTAINERS --
setContainerPosition :: Config -> Char -> Int -> IO ()
setContainerPosition config side line = do
    let (tRow, tCol) = cSize config
    
    -- Sets container position based on Left or Right desination --
    if   side == 'L' 
    then setCursorPosition line 0
    else setCursorPosition line (div tCol 2)


displayContainer :: Config -> Char -> String -> [String] -> Int -> IO ()
displayContainer config side title items selIndex = do
    let (tRow, tCol) = cSize config

    -- Top of container title box
    setContainerPosition config side 4
    putStr $ " " ++ replicate ((div tCol 2) - 2) '-'
        
    -- Container title text --
    setContainerPosition config side 5
    
    let adjust = if odd (length title) then 3 else 2
    putStr $ " |" ++ replicate (((div tCol 4) - 2) - (div (length title) 2)) ' ' ++ title ++ replicate (((div tCol 4) - adjust) - (div (length title) 2)) ' ' ++ "|"

    -- Bottome of container title box
    setContainerPosition config side 6
    putStr $ " " ++ replicate ((div tCol 2) - 2) '-'
    
    -- Dislay Container Contents --
    let n = tRow - 13
    displayItems items 7 n 0 selIndex tCol 7
    
    -- Bottom of container contents box --
    setContainerPosition config side (tRow - 6)
    putStr $ " " ++ replicate ((div tCol 2) - 2) '-'
    
    where
        displayItems :: [String] -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
        displayItems [] _ 0 _ _ _ _ = return () 
        -- After contents section is filled --
        displayItems [] position n _ _ tCol tRow  = do 
        -- Display black space after all items are displayed --
            setContainerPosition config side position
            
            putStr $ " |" ++ replicate ((div tCol 2) - 4) ' ' ++ "|"
            
            displayItems [] (position + 1) (n - 1) 0 selIndex tCol (tRow + 1)
        
        displayItems (item:xs) position n index selIndex tCol tRow = do 
        -- Loop through contents and display on screen --

            setContainerPosition config side position
            
            -- Set adjust if odd # of char and color if selected --
            let adjust    = if odd (length item) then 3    else 2
            let selected  = if index == selIndex then True else False
            
            placeItem item selected tCol adjust
            
            displayItems xs (position + 1) (n - 1) (index + 1) selIndex tCol (tRow + 1)
            
            where
                placeItem :: String -> Bool -> Int -> Int -> IO ()
                placeItem item selected tCol adjust = do

                    let color = if selected then "\ESC[32m" else "\ESC[0m"

                    putStr $ 
                        "\ESC[0m |" ++ replicate (((div tCol 4) - 2) - (div (length item) 2)) ' ' ++
                        color ++ item ++ 
                        replicate (((div tCol 4) - adjust) - (div (length item) 2)) ' ' ++ "\ESC[0m|"

-- FOOTER --
setFooterPosition :: Int -> IO ()
setFooterPosition tRow = do
    setCursorPosition (tRow - 4) 0
        

displayFooter :: Config -> String -> IO ()
displayFooter config text = do
    let (tRow, tCol) = cSize config
    
    setFooterPosition tRow
    
    -- Builds box and displays centered footer text --
    displayEntireScreen config '-'
    displayCentered text tCol
    displayEntireScreen config '-'

getDirectories :: FilePath -> IO [FilePath]
getDirectories filePath = do
  allFiles <- listDirectory filePath
  filterM (doesDirectoryExist . (filePath </>)) allFiles

prevDirectory :: String -> Int -> String
prevDirectory x 0 = x
prevDirectory x y = do
    -- Reverses and chops string down to "/"
    let countDirectory = length x

    if   take 1 (reverse x) == "/" 
    then prevDirectory (take (countDirectory - 1) x) 0
    else prevDirectory (take (countDirectory - 1) x) 1

-- MAIN --
main :: IO ()
main = do
    clearScreen

    setupTerminal
    
    config <- mkConfig
    let currPath     = "/home"
    itemsL <- getDirectories currPath
    let selectedItemPath = currPath ++ "/" ++ (itemsL !! 0)
    itemsR <- getDirectories selectedItemPath
    
    renderScreen config 0 currPath itemsL (itemsL !! 0) itemsR currPath 

    runProgram config 0 currPath

    restoreTerminal

runProgram :: Config -> Int -> FilePath -> IO ()
runProgram config position currPath = do
    let (tRow, tCol) = cSize config

    threadDelay (10 ^ 4)

    -- Get count of Left items to determine if user can move positions --
    oldItemsL <- getDirectories currPath
    let itemCountL = length oldItemsL

    -- Get selected / highlighted item based on position --
    let selectedItem     = oldItemsL !! position
    let selectedItemPath = currPath ++ "/" ++ selectedItem
    
    -- Get count of number of folders in Right container --
    let itemCountR = length selectedItemPath

    -- Get User Input --
    c <- getChar

    -- Based on user input && Right count set new Path --
    let newCurrPath = case c of
                        'd' -> if itemCountR == 0 then currPath else selectedItemPath
                        'a' -> prevDirectory currPath 1
                        -- Custom Options --
                        'x' -> "/"
                        _   -> currPath

    -- Based on user input && Right count set new Position --
    let newPosition = case c of
                        'w' -> if position == 0                then position else position - 1
                        'a' -> 0
                        's' -> if position == (itemCountL - 1) then position else position + 1
                        'd' -> if itemCountR == 0              then position else 0
                        -- Custom Options --
                        'x' -> 0
                        _   -> position

    -- Set Left container contents --
    let newItemsLPath = newCurrPath
    itemsL <- getDirectories newItemsLPath

    -- Set Right container contents --
    let newSelectedItem     = itemsL !! newPosition
    let newSelectedItemPath = newCurrPath ++ "/" ++ newSelectedItem
    itemsR <- getDirectories newSelectedItemPath

    -- Makes sure we don't display too many items on the terminal --
    let itemsLV = if length itemsL <= (tRow - 13) then itemsL else ["ERROR: TOO MANY FOLDERS!"]
    let itemsRV = if length itemsR <= (tRow - 13) then itemsR else ["ERROR: TOO MANY FOLDERS!"]

    renderScreen config newPosition newCurrPath itemsLV newSelectedItem itemsRV newCurrPath


renderScreen :: Config -> Int -> String -> [String] -> String -> [String] -> FilePath -> IO ()
renderScreen config position titleL itemsL titleR itemsR currPath = do

    -- Header (Title) --
    displayTitle config "Testing title screen!"

    -- Containers (Main Content) --
    -- Left --
    displayContainer config 'L' titleL itemsL position

    -- Right --
    displayContainer config 'R' titleR itemsR position

    -- Footer --
    displayFooter config "W - Move Up | S - Move Down | D - Move Into Directory | A - Move to Prev. Directory | X - Move to /" --
    
    runProgram config position currPath
    
setupTerminal :: IO ()
setupTerminal = do
    hideCursor
    hSetEcho stdout False
    forM_ [stdin, stdout] $ \h -> hSetBuffering h NoBuffering

restoreTerminal :: IO ()
restoreTerminal = do
    showCursor
    hSetEcho stdin  True
    hSetEcho stdout True