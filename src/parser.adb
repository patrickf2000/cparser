with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;

with Lex; use Lex;
with Ast_Tree; use Ast_Tree;
with Ast; use Ast;

-- The main parser area
package body Parser is

    -- Build thre tree
    procedure Build_Tree(Ast : in out Ast_Tree.Tree; Path : String) is
        File : File_Type;
        CurrentToken : Token := None;
        Buf : Unbounded_String;
        Position : Cursor := Ast.Root;
        Root : Ast_Node := Ast_Global;
        
        -- Builds AST node until the end of an expression
        procedure Build_Children(Parent_Node : Ast_Node) is
            
            -- Local variables
            Node : Ast_Node;
            
            -- Builds an AST node based on the current position
            procedure Build_Node is
            begin
                case CurrentToken is
                when Num => 
                    Node := Ast_Int;
                    Node.Int_Field1 := Integer'Value(To_String(Buf));
                
                when others => null;
                end case;
            end Build_Node;
            
        begin
            Position := Find(Ast, Parent_Node);
            
            while CurrentToken /= SemiColon loop
                Build_Node;
                if Node.Node_Type /= None then
                    Append_Child(Ast, Position, Node);
                end if;
                
                CurrentToken := Get_Token(File, Buf);
            end loop;
            
            Position := Parent(Position);
        end Build_Children;
        
        -- Builds function declarations
        procedure Build_Func(Data_Type : Token; Name : Unbounded_String) is
            Func : Ast_Node := Ast_Func(Name);
        begin
            --TODO: We will need arguments
            while CurrentToken /= LCBrace loop
                CurrentToken := Get_Token(File, Buf);
            end loop;
            
            Append_Child(Ast, Position, Func);
            Position := Find(Ast, Func);
        end Build_Func;
        
        -- Builds variable assignments
        procedure Build_Var_Assign(Name : Unbounded_String) is
            Var_Assign : Ast_Node := Ast_Var_Assign(Name);
        begin
            Append_Child(Ast, Position, Var_Assign);
            Build_Children(Var_Assign);
        end Build_Var_Assign;
        
        -- Builds variable declarations
        procedure Build_Var_Dec(Data_Type : Token; Name : Unbounded_String) is
            Var_Dec : Ast_Node := Ast_Var_Dec(Name);
        begin
            Append_Child(Ast, Position, Var_Dec);
            Build_Var_Assign(Name);
        end Build_Var_Dec;
        
        procedure Build_Return is
            Ret_Node : Ast_Node;
        begin
            Ret_Node.Node_Type := Ret;
            Append_Child(Ast, Position, Ret_Node);
            Build_Children(Ret_Node);
        end Build_Return;
        
    -- The main parsing area
    begin
        -- Add the root node
        Append_Child(Ast, Position, Root);
        Position := First_Child(Position);
    
        -- Start passing
        Open(File, In_File, Path);
        
        while CurrentToken /= Eof loop
            CurrentToken := Get_Token(File, Buf);
            
            case CurrentToken is
                -- Could be variable declaration or function declaration
                when Int =>
                    declare
                        Data_Type : Token := CurrentToken;
                        NameToken : Token := Get_Token(File, Buf);
                        Name : Unbounded_String := Buf;
                    begin
                        CurrentToken := Get_Token(File, Buf);

                        if CurrentToken = LParen then
                            Build_Func(Data_Type, Name);
                        elsif CurrentToken = Assign then
                            Build_Var_Dec(Data_Type, Name);
                        else
                            -- TODO: SYNTAX ERROR
                            Put_Line("SYNTAX ERROR");
                        end if;
                    end;
                    
                -- End blck
                when RCBrace => Position := Parent(Position);
                
                -- Return statements
                when Ret => Build_Return;
                
                -- TODO: We need a syntax error here
                when others => null;
            end case;
        end loop;
        
        Close(File);
    end Build_Tree;
    
    -- Debug our tree
    procedure Print_Tree(Ast : in Ast_Tree.Tree) is
        Current : Ast_Node;
        Space : Integer := 0;
        
        -- Prints a function
        procedure Print_Func is
            Name : String := To_String(Current.Name);
        begin
            Put_Line("Func " & Name);
        end Print_Func;
        
        -- Prints a variable declaration
        procedure Print_Var(Is_Dec : Boolean) is
            Name : String := To_String(Current.Name);
        begin
            if Is_Dec then
                Put_Line("VarDec " & Name);
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
                when Scope => Put_Line("Scope");
                when Func => Print_Func;
                when VarDec => Print_Var(True);
                when VarAssign => Print_Var(False);
                when Ret => Put_Line("Ret");
                when Int => Put(Current.Int_Field1, 0); New_Line;
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

end Parser;

