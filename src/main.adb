with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;

with Ast_Tree; use Ast_Tree;
with Parser; use Parser;
with Debug; use Debug;

procedure Main is
    -- Rename clunky package
    package ACL renames Ada.Command_Line;
    
    -- Local variables
    Ast : Tree;
    File_Name : Unbounded_String := To_Unbounded_String("first.c");
    
    -- Parse command line arguments
    procedure Parse_Args is
        Index : Integer := 0;
    begin
        if ACL.Argument_Count > 0 then
            File_Name := To_Unbounded_String(ACL.Argument(1));
        end if;
    end Parse_Args;
    
begin
    Parse_Args;
    
    Build_Tree(Ast, To_String(File_Name));
    Run_Pass2(Ast);
    Print_Tree(Ast);
end Main;
