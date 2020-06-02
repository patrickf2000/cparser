with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;

with Ast; use Ast;
with Ast_Tree; use Ast_Tree;

package body Analyzer is
    
    -- Scope:
    -- If you want the global scope, Scope := "GLOBAL"
    -- Otherwise, simply specify the function name
    --
    procedure Get_Vars(Ast : Ast_Tree.Tree; Scope : String) is
        
        -- Parses variables from a function
        procedure Parse_Function(Position : in out Cursor) is
            Current : Ast_Node;
        begin
            while Has_Element(Position) loop
                Current := Element(Position);
                
                if Current.Node_Type = VarDec then
                    Put_Line("Var: " & To_String(Current.Name));
                end if;
                
                Position := Next_Sibling(Position);
            end loop;
        end Parse_Function;
        
        -- Local variables
        Position : Cursor := Ast.Root;
        Current : Ast_Node;
    begin
        New_Line;
        Put_Line("Variables of scope: " & Scope);
        
        Position := First_Child(Position);
        Current := Element(Position);
        
        if Scope = "GLOBAL" then
            Put_Line("Parsing Global");
        else
            Position := First_Child(Position);
            
            while Has_Element(Position) loop
                Current := Element(Position);
                
                if Current.Node_Type = Func and Current.Name = Scope then
                    Position := First_Child(Position);
                    Parse_Function(Position);
                    exit;
                end if;
                
                Current := Element(Position);
            end loop;
        end if;
    end Get_Vars;

end Analyzer;
