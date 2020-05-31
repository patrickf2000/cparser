with Ast; use Ast;
with Ast_Tree; use Ast_Tree;

package Parser is

    procedure Build_Tree(Ast : in out Ast_Tree.Tree; Path : String);
    procedure Run_Pass2(Ast : in out Ast_Tree.Tree);

end Parser;
