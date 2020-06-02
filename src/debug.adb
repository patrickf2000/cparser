with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;

with Lex; use Lex;
with Ast; use Ast;
with Types; use Types;
with Ast_Tree;
with Var_Vector;

package body Debug is
    
    -- Prints a token
    procedure Print_Token(TT : Token; Buf : Unbounded_String) is
    begin
        case TT is
            when Int => Put_Line("Int");
            when Ret => Put_Line("Return");
            when Syscall => Put_Line("Syscall");
            when Id => Put_Line("ID -> " & To_String(Buf));
            when Num => Put_Line("Number -> " & To_String(Buf));
            when Eof => Put_Line("EOF");
            when LParen => Put_Line("(");
            when RParen => Put_Line(")");
            when LCBrace => Put_Line("{");
            when RCBrace => Put_Line("}");
            when SemiColon => Put_Line(";");
            when Assign => Put_Line("=");
            
            when others => Put_Line("IDK!");
        end case;
    end Print_Token;
    
    -- Converts a data type to a string
    procedure Print_Data_Type(D_Type : Data_Type) is
    begin
        case D_Type is
            when Void => Put("Void");
                
            when Char => Put("Char");
            when UChar => Put("UChar");
                
            when Short => Put("Short");
            when UShort => Put("UShort");
                
            when Int => Put("Int");
            when UInt => Put("UInt");
                
            when Long => Put("Long");
            when ULong => Put("ULong");
            when LL => Put("LongLong");
                
            when FloatT => Put("Float");
            when Double => Put("Double");
                
            when others => Put("??");
        end case;
    end Print_Data_Type;
    
    -- Debug our tree
    procedure Print_Tree(Ast : in Ast_Tree.Tree) is
        Current : Ast_Node;
        Space : Integer := 0;
        
        -- Prints a function
        procedure Print_Func is
            Name : String := To_String(Current.Name);
        begin
            Put("Func " & Name & " (");
            Print_Data_Type(Current.D_Type);
            Put_Line(")");
        end Print_Func;
        
        -- Prints a function call
        procedure Print_Func_Call is
            Name : String := To_String(Current.Name);
        begin
            Put_Line("FuncCall " & Name);
        end Print_Func_Call;
        
        -- Prints a variable declaration
        procedure Print_Var(Is_Dec : Boolean) is
            Name : String := To_String(Current.Name);
        begin
            if Is_Dec then
                Put("VarDec " & Name & " (");
                Print_Data_Type(Current.D_Type);
            Put_Line(")");
            else
                Put_Line("VarAssign " & Name);
            end if;
        end Print_Var;
        
        -- Prints an element
        procedure Print_Element is
        begin
            for I in 0 .. Space loop
                Put(" ");
            end loop;
            
            case Current.Node_Type is
                -- Keywords
                when Scope => Put_Line("Scope");
                when Func => Print_Func;
                when Func_Call => Print_Func_Call;
                when Param => Put_Line("Param");
                when Args => Put_Line("Args");
                when VarDec => Print_Var(True);
                when VarAssign => Print_Var(False);
                when Ret => Put_Line("Ret");
                    
                when Cond_If => Put_Line("If");
                when Cond_Elif => Put_Line("Elif");
                when Cond_Else => Put_Line("Else");
                    
                -- Identifiers and literals
                when Int => Put("No: "); Put(Current.Int_Field1, 0); New_Line;
                when FloatL => Put("Flt: "); Put(Current.Float_Field1, 0); New_Line;
                when Id => Put_Line("ID: " & To_String(Current.Name));
                when Str => Put_Line("Str: " & To_String(Current.Name));
                when CharL => Put_Line("ChL: " & To_String(Current.Name));
                when Math => Put_Line("Math");
                    
                -- Comma operator
                when Comma => Put_Line("Sy: ,");
                    
                -- Operators
                when Add => Put_Line("Op: +");
                when Sub => Put_Line("Op: -");
                when Mul => Put_Line("Op: *");
                when Div => Put_Line("Op: /");
                    
                -- All others
                when others => Put_Line("??");
            end case;
        end;
        
        --Iterates through tree
        procedure Walk(Position : in out Cursor) is
            Position2 : Cursor;
        begin
            if Has_Element(Position) then
                Current := Element(Position);
                Print_Element;
                
                if Child_Count(Position) > 0 then
                    Position2 := First_Child(Position);
                    Space := Space + 2;
                    Walk(Position2);
                    Space := Space - 2;
                end if;
                
                Position := Next_Sibling(Position);
                Walk(Position);
            end if;
        end Walk;
        
        -- The root
        Position : Cursor := Ast.Root;
    begin
        Position := First_Child(Position);
        Walk(Position);
    end Print_Tree;
    
    -- Prints out the variable declared in our unit
    --
    procedure Print_Vars(Vars : in Var_Vector.Vector) is
    begin
        New_Line;
        Put_Line("All variables:");
        
        for V of Vars loop
            Put("[" & To_String(V.Scope) & "] ");
            Print_Data_Type(V.D_Type);
            Put_Line(" " & To_String(V.Name));
        end loop;
    end Print_Vars;
    
end Debug;
