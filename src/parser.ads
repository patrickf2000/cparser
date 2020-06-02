with Ast; use Ast;
with Ast_Tree; use Ast_Tree;
with Var_Vector;

package Parser is

    Vars : Var_Vector.Vector;

    procedure Build_Tree(Ast : in out Ast_Tree.Tree; Path : String);
    procedure Run_Pass2(Ast : in out Ast_Tree.Tree);

end Parser;
