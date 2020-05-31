with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Lex; use Lex;
with Ast_Tree; use Ast_Tree;

package Debug is
    
    -- Prints a lexical token
    procedure Print_Token(TT : Token; Buf : Unbounded_String);
    
    -- Prints an AST
    procedure Print_Tree(Ast : in Ast_Tree.Tree);
    
end Debug;
