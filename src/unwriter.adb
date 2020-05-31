with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;

with Ast_Tree; use Ast_Tree;
with Ast; use Ast;

package body Unwriter is

    -- The Unwrite function takes an abstract syntax tree, and translates
    -- it back to C. This will be useful because we can create a test script
    -- to verify that it is being parsed correctly.
    --
    -- It also could form the base of a source-to-source translator.
    --
    procedure Unwrite(Out_File : String; Ast : Ast_Tree.Tree) is
        -- Global variables
        File : File_Type;
        Current : Ast_Node;
        Space : Integer := 0;
        
        procedure Write_Data_Type is
        begin
            case Current.D_Type is
                when Int => Put(File, "int");
                when others => Put(File, "");
            end case;
        end Write_Data_Type;
        
        -- Write a function declaration
        procedure Write_Func is
            Name : String := To_String(Current.Name);
        begin
            Write_Data_Type;
            Put_Line(File, " " & Name & "() {");
        end Write_Func;
        
        -- The main walk function
        procedure Walk(Position : in out Cursor) is
            Position2 : Cursor;
            Is_Func : Boolean := False;
        begin
            if Has_Element(Position) then
                Current := Element(Position);
                
                case Current.Node_Type is
                    when Scope | Func =>
                        if Current.Node_Type = Func then
                            Write_Func;
                            Is_Func := True;
                        end if;
                        
                        if Child_Count(Position) > 0 then
                            Space := Space + 4;
                            Position2 := First_Child(Position);
                            Walk(Position2);
                            Space := Space - 4;
                        end if;
                        
                        if Is_Func then
                            Put_Line(File, "}");
                            Put_Line(File, "");
                            Is_Func := False;
                        end if;
                    
                    when VarAssign => null;
                        
                    when Func_Call => null;
                        
                    when others => null;
                end case;
                
                Position := Next_Sibling(Position);
                Walk(Position);
            end if;
        end Walk;
        
        --Variables specific to the main procedure
        Position : Cursor := Ast.Root;
    begin
        Create(File, Ada.Text_IO.Out_File, Out_File);
        
        Position := First_Child(Position);
        Walk(Position);
        
        Close(File);
    end Unwrite;

end Unwriter;
