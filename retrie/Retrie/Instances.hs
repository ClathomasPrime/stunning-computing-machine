


newtype Var = Var { unVar :: String }
  deriving(Show, Eq, Ord)
-- type Var = String

type Liter = Int

data Stmt 
  = Bind Var Expr
  | Run Expr
  | Return Expr
  deriving(Show)

data Expr 
  = Apply Expr Expr
  | List [Expr]
  | Do [Stmt]
  | Lit Liter
  deriving(Show)

instance Trieable Var where
  data TrieMap Var a = VarTrieMap { varMap :: Map Var a }

  mEmpty = VarTrieMap . Map $ Map.empty
  mUnion (VarTrieMap u) (VarTrieMap v)
    = VarTrieMap . Map $ Map.unionWith (++) (unMap u) (unMap v)

instance Trieable Liter where
  data TrieMap Liter a = LiterTrieMap { literMap :: Map Liter a }

instance Trieable Stmt where
  data TrieMap Stmt a = StmtTrieMap 
    { stmtMapBind :: TrieMap Var (TrieMap Expr a)
    , stmtMapRun :: TrieMap Expr a
    , stmtMapReturn :: TrieMap Expr a
    }

  mEmpty = StmtTrieMap mEmpty mEmpty mEmpty
  mUnion (StmtTrieMap a b c) (StmtTrieMap a' b' c') 
    = StmtTrieMap (mUnion a a') (mUnion b b') (mUnion c c')

instance Trieable Expr where
  data TrieMap Expr a = ExprTrieMap 
    { exprMapApply :: TrieMap Expr (TrieMap Expr a)
    , exprMapList :: TrieMap [Expr] a
    , exprMapDo :: TrieMap [Stmt] a
    , exprMapLit :: TrieMap Liter a
    }

  mEmpty = ExprTrieMap mEmpty mEmpty mEmpty mEmpty
  mUnion (ExprTrieMap a b c d) (ExprTrieMap a' b' c' d') 
    = ExprTrieMap (mUnion a a') (mUnion b b') (mUnion c c') (mUnion d d')
 
