with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;

with Ast_Tree; use Ast_Tree;
with Ast; use Ast;
with Types; use Types;

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
        procedure Walk(Position : in out Cursor);
        
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
                when Void => Put(File, "void");
                    
                when Char => Put(File, "char");
                when UChar => Put(File, "unsigned char");
                    
                when Short => Put(File, "short");
                when UShort => Put(File, "unsigned short");
                    
                when Int => Put(File, "int");
                when UInt => Put(File, "unsigned int");
                    
                when Long => Put(File, "long");
                when ULong => Put(File, "unsigned long");
                when LL => Put(File, "long long");
                    
                when FloatT => Put(File, "float");
                when Double => Put(File, "double");
                
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
                    when FloatL => Put(File, Current.Float_Field1, 0);
                        
                    when Str => Put(File, '"' & To_String(Current.Name) & '"');
                    when CharL => Put(File, ''' & To_String(Current.Name) & ''');
                        
                    when Add => Put(File, " + ");
                    when Sub => Put(File, " - ");
                    when Mul => Put(File, " * ");
                    when Div => Put(File, " / ");
                        
                    when Equal => Put(File, " == ");
                    when Greater => Put(File, " > ");
                    when Less => Put(File, " < ");
                        
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
        procedure Write_Func(Position : in out Cursor) is
            Name : String := To_String(Current.Name);
            Position2 : Cursor := First_Child(Position);
            First_Param : Boolean := True;
        begin
            Write_Data_Type;
            Put(File, " " & Name & "(");
            
            if Child_Count(Position2) > 0 then
                Position2 := First_Child(Position2);
                
                while Has_Element(Position2) loop
                    if First_Param then
                        First_Param := False;
                    else
                        Put(File, ", ");
                    end if;
                    
                    Current := Element(Position2);
                    Write_Data_Type;
                    Put(File, " " & To_String(Current.Name));
                    Position2 := Next_Sibling(Position2);
                end loop;
            end if;
            
            Put_Line(File, ") {");
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
        
        -- Write conditional statement
        procedure Write_Cond(Position : in out Cursor; Is_Elif : Boolean) is
            Position2 : Cursor;
        begin
            Write_Space;
            
            if Is_Elif then
                Put(File, "} else if (");
            else
                Put(File, "if (");
            end if;
            
            Position2 := First_Child(Position);
            
            Write_Children(Position2);
            Put_Line(File, ") {");
            
            Position := Next_Sibling(Position);
            Space := Space + 4;
            Walk(Position);
            Space := Space - 4;
        end Write_Cond;
        
        -- Write an else statement
        procedure Write_Else(Position : in out Cursor) is
        begin
            Write_Space;
            Put_Line(File, "} else {");
            
            Space := Space + 4;
            Walk(Position);
            Space := Space - 4;
        end Write_Else;
        
        -- The main walk function
        procedure Walk(Position : in out Cursor) is
            Position2 : Cursor;
        begin
            if Has_Element(Position) then
                Current := Element(Position);
                
                case Current.Node_Type is
                    -- Scopes and function declarations
                    when Scope | Func =>
                        if Current.Node_Type = Func then
                            Write_Func(Position);
                        end if;
                        
                        if Child_Count(Position) > 0 then
                            Space := Space + 4;
                            Position2 := First_Child(Position);
                            Walk(Position2);
                            Space := Space - 4;
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
                        
                    -- Conditional statements
                    when Cond_If | Cond_Elif =>
                        Position2 := First_Child(Position);
                        
                        if Current.Node_Type = Cond_Elif then
                            Write_Cond(Position2, True);
                        else
                            Write_Cond(Position2, False);
                        end if;
                        
                    when Cond_Else =>
                        Position2 := First_Child(Position);
                        Write_Else(Position2);
                        
                    when End_Block =>
                        Write_Space;
                        Put_Line(File, "}");
                        
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
