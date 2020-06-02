with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;

with Ast_Tree; use Ast_Tree;
with Parser; use Parser;
with Debug; use Debug;
with Analyzer; use Analyzer;
with Unwriter; use Unwriter;

procedure Main is
    -- Rename clunky package
    package ACL renames Ada.Command_Line;
    
    -- Local variables
    Ast : Tree;
    File_Name : Unbounded_String := To_Unbounded_String("first.c");
    Silent : Boolean := False;
    
    -- Parse command line arguments
    procedure Parse_Args is
        Index : Integer := 0;
    begin
        loop
            Index := Index + 1;
            exit when Index > ACL.Argument_Count;
            
            if ACL.Argument(Index) = "--silent" then
                Silent := True;
            else
                File_Name := To_Unbounded_String(ACL.Argument(Index));
            end if;
        end loop;
    end Parse_Args;
    
begin
    Parse_Args;
    
    Build_Tree(Ast, To_String(File_Name));
    Run_Pass2(Ast);
    
    if not Silent then
        Print_Tree(Ast);
        Print_Vars(Vars);
    end if;
    
    Unwrite("output.c", Ast);
end Main;
