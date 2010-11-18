---- Didn't get how to define the Data type.
data NestedList a = Elem a | List [NestedList a] deriving Show

flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x
