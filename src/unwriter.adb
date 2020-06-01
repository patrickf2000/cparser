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
        
        -- Forward declaration
        procedure Write_Func_Call(Position : in out Cursor);
        
        -- Writes out the spacing
        procedure Write_Space is
        begin
            for I in 5 .. Space loop
                Put(File, " ");
            end loop;
        end Write_Space;
        
        -- Writes a data type
        procedure Write_Data_Type is
        begin
            case Current.D_Type is
                when Int => Put(File, "int");
                when others => Put(File, "");
            end case;
        end Write_Data_Type;
        
        -- Writes children of a node
        procedure Write_Children(Position : in out Cursor) is
        begin
            while Has_Element(Position) loop
                Current := Element(Position);
                
                case Current.Node_Type is
                    when Id =>
                        declare
                            Name2 : String := To_String(Current.Name);
                        begin
                            Put(File, Name2);
                        end;
                        
                    when Int => Put(File, Current.Int_Field1, 0);
                        
                    when Str => Put(File, '"' & To_String(Current.Name) & '"');
                        
                    when Add => Put(File, " + ");
                    when Sub => Put(File, " - ");
                    when Mul => Put(File, " * ");
                    when Div => Put(File, " / ");
                        
                    when Func_Call =>
                        declare
                            Position2 : Cursor := First_Child(Position);
                        begin
                            Write_Func_Call(Position2);
                        end;
                        
                    when others => null;
                end case;
                
                Position := Next_Sibling(Position);
            end loop; 
        end Write_Children;
        
        -- Write a function declaration
        procedure Write_Func is
            Name : String := To_String(Current.Name);
        begin
            Write_Data_Type;
            Put_Line(File, " " & Name & "() {");
        end Write_Func;
        
        -- Write a function call
        procedure Write_Func_Call(Position : in out Cursor) is
            Name : String := To_String(Current.Name);
            Position2 : Cursor := Position;
            First_Param : Boolean := True;
        begin
            Put(File, Name & "(");
            Current := Element(Position2);
            
            while Has_Element(Position2) loop
                if First_Param then
                    First_Param := False;
                else
                    Put(File, ", ");
                end if;
                
                declare
                    Position3 : Cursor := First_Child(Position2);
                begin
                    Write_Children(Position3);
                end;
                
                Position2 := Next_Sibling(Position2);
            end loop;
            
            Put(File, ")");
        exception
            when others => Put(File, ")");
        end Write_Func_Call;
        
        -- Write a variable assignment
        procedure Write_Var_Assign(Position : in out Cursor) is
            Name : String := To_String(Current.Name);
        begin
            Write_Space;
            Put(File, Name & " = ");
            
            Current := Element(Position);
            if Current.Node_Type = Math then
                Position := First_Child(Position);
            end if;
            
            Write_Children(Position);            
            Put_Line(File, ";");
        end Write_Var_Assign;
        
        -- Write return statement
        procedure Write_Ret(Position : in out Cursor) is
        begin
            Write_Space;
            Put(File, "return");
            
            if Has_Element(Position) then
                Put(File, " ");
                Write_Children(Position);
            end if;
            
            Put_Line(File, ";");
        end Write_Ret;
        
        -- The main walk function
        procedure Walk(Position : in out Cursor) is
            Position2 : Cursor;
            Is_Func : Boolean := False;
        begin
            if Has_Element(Position) then
                Current := Element(Position);
                
                case Current.Node_Type is
                    -- Scopes and function declarations
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
                    
                    -- Variable declarations
                    when VarDec =>
                        declare
                            Name : String := To_String(Current.Name);
                        begin
                            Write_Space;
                            Write_Data_Type;
                            Put_Line(File, " " & Name & ";");
                        end;
                       
                    -- Variable assignments    
                    when VarAssign =>
                        Position2 := First_Child(Position);
                        Write_Var_Assign(Position2);
                       
                    -- Function calls
                    when Func_Call =>
                        Write_Space;
                        Position2 := First_Child(Position);
                        Write_Func_Call(Position2);
                        Put_Line(File, ";");
                        
                    -- Return statements
                    when Ret =>
                        Position2 := First_Child(Position);
                        Write_Ret(Position2);
                        
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
        
        Put_Line(File, "#include <stdio.h>");
        New_Line(File);
        
        Position := First_Child(Position);
        Walk(Position);
        
        Close(File);
    end Unwrite;

end Unwriter;
