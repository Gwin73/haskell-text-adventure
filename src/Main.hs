{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import qualified Data.Map as M
import Control.Monad.State
import Control.Lens
import Data.List
import Data.Maybe
import Control.Applicative

type Id = String

data Item = Item 
    { _itemId :: Id
    , _itemDescription :: String
    , _weight :: Int
    , _onTake :: Id -> Id -> State World String
    , _onDrop :: Id -> Id -> State World String }

onTakeId _ _ = return "" 
onDropId _ _ = return ""  

class Container a where
    getItem :: Id -> a -> Maybe Item
    addItem :: Item -> a -> a
    removeItem :: Id -> a -> a

data Location = Location
    { _locationId :: Id
    , _description :: String
    , _items :: M.Map Id Item
    , _connectedLocations :: [Id]
    , _onGo :: Id -> State World String }

onGoId _ = return "" 

data Character = Character 
    { _characterId :: Id
    , _inventory :: M.Map Id Item
    , _location :: Id }

data World = World
    { _locations :: M.Map Id Location
    , _characters :: M.Map Id Character
    , _rounds :: Int
    , _winCondition :: World -> Bool
    , _looseCondition :: World -> Bool }

makeLenses ''Item
makeLenses ''Location  
makeLenses ''Character 
makeLenses ''World

instance Container Location where
    getItem itemId location = location ^. items ^. at itemId
    addItem item = items . at (item ^. itemId) ?~ item
    removeItem itemId = items . at itemId .~ Nothing

instance Container Character where
    getItem itemId character = character ^. inventory ^. at itemId
    addItem item = inventory . at (item ^. itemId) ?~ item
    removeItem itemId = inventory . at itemId .~ Nothing    

----------------------------------------------------------------
world = World (M.fromList locations) (M.fromList [("player", Character "player" (M.fromList []) "locA")]) 0 winFunc looseFunc
    where
        locations = [("locA", Location "locA" "description" (M.fromList [("itemA", itemA)]) ["locB"] onGoLocA), ("locB", Location "locB" "description" (M.fromList [("itemB", itemB)]) ["locA"] onGoId)]
        itemA = Item "itemA" "desc" 1 onTakeItemA onDropItemA
        itemB = Item "itemB" "desc" 10 onTakeId onDropId

onTakeItemA "player" "locA" = do
    (locations . at "locA" . traversed) %= (addItem $ Item "onTakeItem" "desc" 1 onTakeId onDropId)
    return "onTakeLocAMessage\n"
onTakeItemA _ _ = return ""

onDropItemA "player" "locB" = do
    (locations . at "locB" . traversed) %= (addItem $ Item "onDropItem" "desc" 1 onTakeId onDropId)
    return "onDropLocBMessage\n"
onDropItemA _ _ = return ""

onGoLocA "player" = do
    (locations . at "locA" . traversed) %= (addItem $ Item "onGoLocBItem" "desc" 1 onTakeId onDropId)
    return "onGoLocBMessage\n"
onGoLocA _ = return ""

winFunc world = isJust (world ^. locations ^? at "locB" . _Just . items . at "onGoLocBItem" . _Just)

looseFunc world = (world ^. rounds) >= 10
---------------------------------
main :: IO ()
main = do
    putStrLn "GameName by Someone"
    putStrLn "Introduction text..."
    gameloop world
    putStrLn "Thanks for playing!"

gameloop :: World -> IO ()
gameloop world = do
    when (not $ (world ^. winCondition) world || (world ^. looseCondition) world) $ do
        putChar '>'
        line <- getLine
        when (line /= "quit") $ 
            case parse $ words line of 
                Just f -> do 
                    let (res, world') = runState f world
                    putStrLn $ res ++ "\n"
                    let world'' = (rounds %~ (+1)) world'
                    gameloop world''
                Nothing -> do 
                    putStrLn $ line ++ " is not a valid command\n"
                    gameloop world
    if ((world ^. winCondition) world)
        then putStrLn "You won"
        else when ((world ^. looseCondition) world) (putStrLn "You lost")

parse :: (MonadState World m) => [String] -> Maybe (m String)
parse ["look"] = return $ look "player"
parse ["inventory"] = return $ showInventory "player"
parse ["go", locationId] = return $ go locationId "player" 
parse ["take", itemId] = return $ Main.take itemId "player" 
parse ["drop", itemId] = return $ Main.drop itemId "player" 
parse _ = Nothing

-------------------------------------------------------  
go :: (MonadState World m) => Id -> Id -> m String
go locationId characterId = do
    location <- liftM (getLocByCharId characterId) get
    w <- get
    if hasExit locationId location
        then do
            characters . at characterId . traversed %= (teleport locationId w)
            (message, w') <- liftM (runState $ (location ^. onGo) characterId) get
            put w'
            return $ intercalate " " [message ++ characterId, "successfully moved to", locationId]
        else return $ locationId ++ " is not a nearby location"

hasExit :: Id -> Location -> Bool
hasExit locationId location = elem locationId $ location ^. connectedLocations

teleport :: Id -> World -> Character -> Character 
teleport locationId world = 
    if M.member locationId (world ^. locations)
        then location .~ locationId
        else error "Invalid location"

------------------------------------------------------------------------------------
look :: (MonadState World m) => Id -> m String
look characterId = do
    location <- liftM (getLocByCharId characterId) get
    let 
        name = location ^. locationId
        descript = location ^. description
        itemsDescription = "Items: " ++ (unwords $ map (^. itemId) $ M.elems $ (location ^. items))
        exitsDescription = "Exits: " ++ (unwords $ location ^. connectedLocations)
    return $ intercalate "\n" [underlined name, descript, itemsDescription, exitsDescription]

showInventory :: (MonadState World m) => Id -> m String
showInventory characterId = do    
    character <- liftM (getCharByCharId characterId) get
    let 
        itemsDescription = map (\i -> intercalate " " [i ^. itemId , i ^. itemDescription , show $ i ^. weight]) $ M.elems $ (character ^. inventory)
    return $ intercalate "\n" ([underlined $ "inventory of " ++ characterId] ++ itemsDescription)

underlined :: String -> String
underlined s = intercalate "\n" [s, map (const '-') s]    

------------------------------------------------------------------------------------
take :: (MonadState World m) => Id -> Id -> m String
take itemId characterId = do
    character <- liftM (getCharByCharId characterId) get 
    location <- liftM (getLocByCharId characterId) get
    case getItem itemId location of
        Just item -> do
            let totalWeight = sum $ map (^. weight) (M.elems $ character ^. inventory)
            if totalWeight + (item ^. weight) <= 10 
                then do
                    characters . at characterId . traversed %= addItem item
                    locations . at (location ^. locationId) . traversed %= removeItem itemId
                    (message, w') <- liftM (runState $ (item ^. onTake) characterId (location ^. locationId)) get
                    put w'
                    return $ unwords [message ++ characterId, "successfully picked up", itemId]
                else return $ unwords [characterId, "is carrying to much"]
        Nothing -> return $ unwords ["There is no", itemId, "here"]

drop :: (MonadState World m) => Id -> Id -> m String
drop itemId characterId = do
    character <- liftM (getCharByCharId characterId) get 
    location <- liftM (getLocByCharId characterId) get
    case getItem itemId character of
        Just item -> do
            characters . at characterId . traversed %= removeItem itemId 
            locations . at (location ^. locationId) . traversed %= addItem item 
            (message, w') <- liftM (runState $ (item ^. onDrop) characterId (location ^. locationId)) get
            put w'
            return $ unwords [message ++ characterId, "successfully dropped", itemId]
        Nothing -> return $ unwords ["There is no", itemId, "in", characterId ++ "s", "inventory"]    

-------------------------------------------------------------------------------------
getLocByCharId :: Id -> World -> Location
getLocByCharId characterId world = 
    case M.lookup characterId (world ^. characters) of
        Just character -> case M.lookup (character ^. location) (world ^. locations) of
            Just location -> location
            Nothing -> error "character doesnt have a valid location" 
        Nothing -> error $ "No character with id " ++ characterId  

getCharByCharId :: Id -> World -> Character
getCharByCharId characterId world = 
    case M.lookup characterId (world ^. characters) of
        Just character -> character
        Nothing -> error $ "No character with id " ++ characterId
