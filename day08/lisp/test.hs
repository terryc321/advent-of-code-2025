import GHC.Data.Graph.Inductive.Graph
import GHC.Data.Graph.Inductive.PatriciaTree
import GHC.Data.Graph.Inductive.Query.DFS (components)

let g = mkGraph [(1,()), (2,()), (3,()), (4,()), (5,())]
                [(1,2,()), (2,3,())] :: Gr () ()
components g

