with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ast_Tree; use Ast_Tree;
with Parser; use Parser;
with Debug; use Debug;

procedure Main is
    Ast : Tree;
begin
    Build_Tree(Ast, "first.c");
    Run_Pass2(Ast);
    Print_Tree(Ast);
end Main;
