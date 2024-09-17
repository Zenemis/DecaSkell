module Parser.AST (
    AST
) where


-- Progam final constant keywords
data SEMICOLON = SEMICOLON
data COMMA     = COMMA

data NEWARRAY  = NEWARRAY
data ARRAY     = ARRAY 

data THIS      = THIS
data NEW       = NEW
data DOT       = DOT

data CLASS     = CLASS
data INTERFACE = INTERFACE
data IMPLEMENTS= IMPLEMENTS
data EXTENDS   = EXTENDS

data VOID      = VOID
data INT       = INT
data DOUBLE    = DOUBLE
data BOOL      = BOOL
data STRING    = STRING

data LPAR      = LPAR
data RPAR      = RPAR
data RBRACE    = RBRACE
data LBRACE    = LBRACE
data RBRAK    = RBRAK
data LBRAK    = LBRAK

data IF        = IF
data ELSE      = ELSE
data ELIF      = ELIF
data WHILE     = WHILE
data FOR       = FOR
data RETURN    = RETURN
data BREAK     = BREAK

data PRINT     = PRINT
data READINTEGER = READINTEGER
data READLINE    = READLINE

data ASSIGN     = ASSIGN
data PLUS       = PLUS
data MINUS      = MINUS
data TIMES      = TIMES
data DIV        = DIV
data MODULO     = MODULO
data LT         = LT
data LEQ        = LEQ
data GT         = GT
data GEQ        = GEQ
data EQUAL      = EQUAL
data NEQ        = NEQ
data AND        = AND
data OR         = OR
data NOT        = NOT

-- Program final variables
type ID = String

data TConst
    = TRUE
    | FALSE
    | INTLITT String
    | DOUBLELITT String
    | STRLITT String
    | NULL
    deriving (Show, Eq)

data TProgram 
    = Epsilon
    | Program Decl

data TDecl 
    = VariableDecl TVariableDecl
    | FunctionDecl TFunctionDecl
    | ClassDecl TClassDecl
    | InterfaceDecl TInterfaceDecl

data TVariableDecls
    = Epsilon
    | VariableDecl TVariableDecl TVariableDecls

data TVariableDecl 
    = VariableDecl TVariable SEMICOLON

data TVariable 
    = Variable TType TIdentifier

data TType s
    = VOID 
    | INT
    | DOUBLE
    | BOOL
    | STRING 
    | TID s
    | RecType TType s ARRAY

data TFunctionDecl 
    = FunctionDecl TType ID LPAR TFormals RPAR TStmtBlock

data TFormals
    = Epsilon
    | Formals TFormalNN

data TFormalNN
    = Formal TVariable
    | Formals TVariable COMMA TFormals

data TClassDecl 
    = ClassDecl CLASS ID TExtendDecl TImplementsDecl LBRACE TField RBRACE

data TExtendDecl
    = Epsilon 
    | ExtendDecl EXTENDS ID

data TImplementsDecl
    = Epsilon 
    | ImplementsDecl TImplementsNN

data TImplementsNN
    = ImplementsDecl IMPLEMENTS ID
    | ImplementsDecls IMPLEMENTS ID COMMA

data TField
    = TVariableDecl
    | FunctionDecl

data InterfaceDecl
    = InterfaceDecl INTERFACE ID LBRACE TPrototypes RBRACE

data TPrototypes
    = Epsilon
    | Prototype TPrototype TPrototypes

data TPrototype
    = Prototype TType ID LPAR TFormals RPAR SEMICOLON 

data TStmtBlock
    = StmtBlock LBRACE TVariableDecls TStmts RBRACE

data TStmts
    = Epsilon
    | Stmt TStmt

data TStmt
    = StmtEps SEMICOLON
    | StmtExpr TExpr SEMICOLON
    | StmtIf TIfStmt
    | StmtWhile TWhileStmt
    | StmtFor TForStmt
    | StmtBreak TBreakStmt
    | StmtReturn TReturnStmt
    | StmtPrint TPrintStmt
    | StmtBlock TStmtBlock

data TIfStmt
    = IfStmt IF LPAR TExpr RPAR TStmt
    | IfElseStmt IF LPAR TExpr RPAR TStmt ELSE TStmt

data TWhileStmt
    = While WHILE LPAR TExpr RPAR

data TForStmt
    = ForC FOR LPAR SEMICOLON TExpr SEMICOLON RPAR TStmt
    | ForIC FOR LPAR TExpr SEMICOLON TExpr SEMICOLON RPAR TStmt
    | ForCA FOR LPAR SEMICOLON TExpr SEMICOLON TExpr RPAR TStmt
    | ForCA FOR LPAR TExpr SEMICOLON TEWxpr SEMICOLON TExpr RPAR TStmt

data TReturnStmt
    = RetVoid RETURN SEMICOLON
    | Return RETURN TExpr SEMICOLON

data TBreakStmt
    = Break SEMICOLON

data TPrintStmt
    = PrintStmt PRINT LPAR TExprs RPAR SEMICOLON

data TExprs
    = Expr TExpr
    | Exprs TExpr COMMA TExprs

data TExpr
    = ExprAssign TLValue ASSIGN TExpr
    | ExprConst TConst
    | ExprLVal TLValue
    | ExprThis THIS
    | ExprCall TCall
    | ExprPar LPAR TExpr RPAR
    | ExprPlus TExpr PLUS TExpr
    | ExprMinus TExpr MINUS TExpr
    | ExprTimes TExpr TIMES TExpr
    | ExprDiv TExpr DIV TExpr
    | ExprMod TExpr MODULO TExpr
    | ExprNeg MINUS TExpr
    | ExprLT TExpr LT TExpr
    | ExprLEQ TExpr LEQ TExpr
    | ExprGT TExpr GT TExpr
    | ExprGEQ TExpr GEQ TExpr
    | ExprEQ TExpr EQUAL TExpr
    | ExprNEQ TExpr NEQ TExpr
    | ExprAnd TExpr AND TExpr
    | ExprOr TExpr OR TExpr
    | ExprNot NOT TExpr
    | ExprRdInt READINTEGER LPAR RPAR
    | ExprRdLine READLINE LPAR RPAR
    | ExprNew NEW LPAR ID RPAR
    | ExprNewArr NEWARRAY LPAR TExpr COMMA TType RPAR

data TLValue
    = LVariable ID
    | LField TExpr DOT ID
    | LItem TExpr LBRAK TExpr RBRAK

data TCall
    = CallFunc ID LPAR TActuals RPAR
    | CallMeth TExpr DOT ID LPAR TActuals RPAR

data TActuals
    = Epsilon
    | ActualExprs TExprs
