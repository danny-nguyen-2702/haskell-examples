------------------------------------- Zippers -------------------------------------

-------------- Taking a walk --------------
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

tree :: Tree Char
tree = 
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )  


data Direction = L | R deriving (Show)
type Directions = [Direction]

-- change an element of the tree based on the path from the root of the tree to the element that we want to change
changeToP :: Directions -> Tree Char -> Tree Char
changeToP _ Empty = Empty
changeToP [] (Node _ l r) = Node 'P' l r
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)

-- To avoid printing out the whole tree, let's make a function that takes a list of directions and tells us what the element at the destination is
elemAt :: Directions -> Tree a -> Maybe a
elemAt _ Empty = Nothing
elemAt [] (Node x l r) = Just x
elemAt (L:ds) (Node x l r) = elemAt ds l
elemAt (R:ds) (Node x l r) = elemAt ds r



-------------- A trail of breadcrumbs --------------

type Breadcrumbs = [Direction]

-- Here's a function that takes a tree and some breadcrumbs and moves to the left sub-tree while adding L to the head of the list that represents our breadcrumbs

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Empty, bs) = (Empty, bs)
goLeft (Node x l r, bs) = (l, L:bs)

-- Here's a function to go right:
goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Empty, bs) = (Empty, bs)
goRight (Node x l r, bs) = (r, R:bs)

-- ghci> goLeft . goRight $ (tree, [])
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])


-- To make walking along our tree clearer, we can use the -: function that we defined like below. This function allows us to apply functions to values by first writing the value, then writing a -: and then the function
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x 

-- ghci> (tree, []) -: goRight -: goLeft
-- (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])


-------- Going back up

-- What if we now want to go back up in our tree? From our breadcrumbs we know that the current tree is the left sub-tree of its parent and that it is the right sub-tree of its parent, but that's it. They don't tell us enough about the parent of the current sub-tree for us to be able to go up in the tree. It would seem that apart from the direction that we took, a single breadcrumb should also contain all other data that we need to go back up.

-- Let's modify our breadcrumbs so that they also contain information about everything that we previously ignored when moving left and right. Instead of Direction, we'll make a new data type:

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

-- Now, instead of just L, we have a LeftCrumb that also contains the element in the node that we moved from and the right tree that we didn't visit. Instead of R, we have RightCrumb, which contains the element in the node that we moved from and the left tree that we didn't visit.

type Breadcrumbs' a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs' a)  

-- Next up, we have to modify the goLeft and goRight functions to store information about the paths that we didn't take in our breadcrumbs, instead of ignoring that information like they did before.

goLeft' :: Zipper a -> Maybe (Zipper a)
goLeft' (Empty, bs) = Nothing
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r : bs)


goRight' :: Zipper a -> Maybe (Zipper a)
goRight' (Empty, bs) = Nothing
goRight' (Node x l r, bs) = Just (r, RightCrumb x l : bs)


-- We were previously able to go left and right. What we've gotten now is the ability to actualy go back up by remembering stuff about the parent nodes and the paths that we didn't visit. Here's the goUp function:

goUp :: Zipper a -> Maybe (Zipper a)
goUp (Node x l r, []) = Nothing
goUp (Empty, bs) = Nothing
goUp (subNode, LeftCrumb x r : bs) = Just (Node x subNode r, bs)
goUp (subNode, RightCrumb x l : bs) = Just (Node x l subNode, bs)


-- With a pair of Tree a and Breadcrumbs a, we have all the information to rebuild the whole tree and we also have a focus on a sub-tree. This scheme also enables us to easily move up, left and right. Such a pair that contains a focused part of a data structure and its surroundings is called a zipper, because moving our focus up and down the data structure resembles the operation of a zipper on a regular pair of pants.

-------- Manipulating trees under focus
modify :: (a -> a) -> Zipper a -> Maybe (Zipper a)
modify f (Node x l r, bs) = Just (Node (f x) l r, bs)
modify f (Empty, bs) = Nothing

-- We go left, then right and then modify the root element by replacing it with a 'P'. This reads even better if we use -:
-- ghci> let newFocus = return (tree,[]) >>= goLeft' >>= goRight' >>= modify (\_ -> 'P')  

-- We can then move up if we want and replace an element with a mysterious 'X':
-- ghci> let newFocus2 = newFocus >>= goUp >>= modify (\_ -> 'X')  


-- Each node has two sub-trees, even if those sub-trees are empty trees. So if we're focusing on an empty sub-tree, one thing we can do is to replace it with a non-empty subtree, thus attaching a tree to a leaf node. The code for this is simple:

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- ghci> let farLeft = return (tree,[]) >>= goLeft' >>= goLeft' >>= goLeft' >>= goLeft'
-- ghci> let newFocus = attach (Node 'Z' Empty Empty) <$> farLeft



-- Making a function that walks all the way to the top of the tree, regardless of what we're focusing on, is really easy.

topMost :: Zipper a -> Maybe (Zipper a)
topMost (t, []) = Just (t, [])
topMost (t, bs) = topMost =<< goUp (t, bs)




-------------- Focusing on lists --------------

-- Zippers can be used with pretty much any data structure, so it's no surprise that they can be used to focus on sub-lists of lists. After all, lists are pretty much like trees, only where a node in a tree has an element (or not) and several sub-trees, a node in a list has an element and only a single sub-list.

data List a = Nil | Cons a (List a) deriving (Show, Eq, Ord, Read)

-- Let's make a zipper for lists. To change the focus on sub-lists of a list, we move either forward or back (whereas with trees we moved either up or left or right).

-- The first list represents the list that we're focusing on and the second list is the list of breadcrumbs. 
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward ([], bs) = ([], bs)
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, []) = (xs, [])
goBack (xs, b:bs) = (b:xs, bs)

-- ghci> xs = [1..10] 
-- ghci> xs -: goForward -: goForward -: goForward
-- ([4,5,6,7,8,9,10],[3,2,1])
-- ghci> xs -: goBack
-- ([3,4,5,6,7,8,9,10],[2,1])



-------------- A very simple file system --------------

-- Now that we know how zippers work, let's use trees to represent a very simple file system and then make a zipper for that file system, which will allow us to move between folders, just like we usually do when jumping around our file system.

type Name = String
type Content = String
data FSItem = File Name Content | Folder Name [FSItem] deriving (Show)

-- A file comes with two strings, which represent its name and the data it holds. A folder comes with a string that is its name and a list of items. If that list is empty, then we have an empty folder.

-- Here's a folder with some files and sub-folders:
myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ] 


-------- A zipper for our file system

-- Like with binary trees and lists, we're going to be leaving breadcrumbs that contain info about all the stuff that we chose not to visit. Like we said, a single breadcrumb should be kind of like a node, only it should contain everything except the sub-tree that we're currently focusing on. It should also note where the hole is so that once we move back up, we can plug our previous focus into the hole.

-- In this case, a breadcrumb should be like a folder, only it should be missing the folder that we currently chose. Why not like a file, you ask? Well, because once we're focusing on a file, we can't move deeper into the file system, so it doesn't make sense to leave a breadcrumb that says that we came from a file. A file is sort of like an empty tree.

-- If we're focusing on the folder "root" and we then focus on the file "dijon_poupon.doc", what should the breadcrumb that we leave look like? Well, it should contain the name of its parent folder along with the items that come before the file that we're focusing on and the items that come after it. So all we need is a Name and two lists of items. By keeping separate lists for the items that come before the item that we're focusing and for the items that come after it, we know exactly where to place it once we move back up. So this way, we know where the hole is.

-- Here's our breadcrumb type for the file system:

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

-- Going back up in the hierarchy is very simple. We just take the latest breadcrumb and assemble a new focus from the current focus and breadcrumb. Like so:

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs): bs) = (Folder name (ls ++ [item] ++ rs), bs)

-- How about going deeper into the file system? If we're in the "root" and we want to focus on "dijon_poupon.doc", the breadcrumb that we leave is going to include the name "root" along with the items that precede "dijon_poupon.doc" and the ones that come after it.

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) = (item, (FSCrumb folderName ls rs): bs)
    where   (ls, item:rs) = break (nameIs name) items

nameIs :: Name -> FSItem -> Bool
nameIs name (File fileName _) = name == fileName
nameIs name (Folder folderName _) = name == folderName

-- ghci> let newFocus = (myDisk,[]) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
-- ghci> fst newFocus  
-- File "skull_man(scary).bmp" "Yikes!"  
-- ghci> let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"  
-- ghci> fst newFocus2  
-- File "watermelon_smash.gif" "smash!!" 


---------- Manipulating our file system

-- Now that we know how to navigate our file system, manipulating it is easy. Here's a function that renames the currently focused file or folder:
fsRename :: Name -> FSZipper -> FSZipper 
fsRename newName (File _ content, bs) = (File newName content, bs)
fsRename newName (Folder _ items, bs) = (Folder newName items, bs)

-- ghci> let newFocus = (myDisk,[]) -: fsTo "pics" -: fsRename "cspi" -: fsUp  


-- How about a function that makes a new item in the current folder? Behold:
fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder name items, bs) = (Folder name (item:items), bs)

-- ghci> let newFocus = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp  


