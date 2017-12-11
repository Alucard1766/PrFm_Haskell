-- (ID,Name)
type Task = (Int,String)

putTask :: Task -> IO ()
putTask (id,task) = putStrLn (show id ++ ": " ++ task)

getInput :: [Task] -> IO ()
getInput tasks = do
    putStrLn ""
    putStrLn "Current task list:"
    mapM_ putTask tasks
    command <- getLine
    process command tasks

process :: String -> [Task] -> IO ()
process ('+':' ':task) tasks = getInput $ convert task tasks
process ('-':' ':num ) tasks = getInput $ remove (read num) tasks
process  "q"           tasks = return ()
process "?"            tasks = do
    showAvailableCommands
    getInput tasks
process  command       tasks = do
    putStrLn ("Invalid command: `" ++ command ++ "`")
    getInput tasks

convert:: String -> [Task] -> [Task]
convert task [] = [] ++ [(0,task)]
convert task tasks = tasks ++ [(fst(last tasks)+1,task)]

remove:: Int -> [Task] -> [Task]
remove y xs = filter (not . ((==) y) . fst) xs

showAvailableCommands = do
    putStrLn "Commands:"
    putStrLn "+ <Title>    - Add a Task"
    putStrLn "- <TaskNr>   - Delete Task"
    putStrLn "q            - Quit"
    putStrLn "?            - Show this help"

main = do
    showAvailableCommands
    getInput []