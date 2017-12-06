getInput :: [String] -> IO ()
getInput tasks = do
    putStrLn ""
    putStrLn "Current Tasks:"
    mapM_ putTask (zip [0..] tasks)
    userInput <- getLine
    process userInput tasks

process :: String -> [String] -> IO ()
process ('+':' ':task)  tasks = getInput (task:tasks)
process ('-':' ':num )  tasks =
    case remove (read num) tasks of
        Nothing -> do
            putStrLn "No Task with this number found"
            getInput tasks
        Just tasks' -> getInput tasks'
process "q"             tasks = return ()
process "?"             tasks = do
    showAvailableCommands
    getInput tasks
process userInput       tasks = do
    putStrLn ("Invalid userInput: `" ++ userInput ++ "`")
    getInput tasks

putTask :: (Int, String) -> IO ()
putTask (n, task) = putStrLn (show n ++ ": " ++ task)

remove :: Int -> [a] -> Maybe [a]
remove 0 (_:as) = Just as
remove n (a:as) = do
    as' <- remove (n - 1) as
    return (a:as')
remove _  []    = Nothing

showAvailableCommands = do
    putStrLn "Commands:"
    putStrLn "+ <Title>    - Add a Task"
    putStrLn "- <TaskNr>   - Delete Task"
    putStrLn "q            - Quit"
    putStrLn "?            - Show this help"

main = do
    showAvailableCommands
    getInput []

