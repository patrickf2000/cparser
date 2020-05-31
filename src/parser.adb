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
        
        -- Forward declarations
        procedure Build_Func_Call(Name : Unbounded_String);
        
        -- Builds AST node until the end of an expression
        procedure Build_Children(Parent_Node : Ast_Node; 
                                 Stop_Token : Token := SemiColon) is
            
            -- Local variables
            Node : Ast_Node;
            
            -- Builds an AST node based on the current position
            procedure Build_Node is
            begin
                case CurrentToken is
                when Num => 
                    Node := Ast_Int;
                    Node.Int_Field1 := Integer'Value(To_String(Buf));
                    
                when Id =>
                    declare
                        Name : Unbounded_String := Buf;
                    begin
                        CurrentToken := Get_Token(File, Buf);
                        
                        if CurrentToken = LParen then
                            Build_Func_Call(Name);
                        else
                            Node := Ast_Id(Name);
                            Unget_Token(CurrentToken);
                        end if;
                    end;
                  
                when Comma => Node := Ast_Comma;
                    
                when Plus => Node.Node_Type := Add;
                when Minus => Node.Node_Type := Sub;
                when Mul => Node.Node_Type := Mul;
                when Div => Node.Node_Type := Div;
                
                when others => null;
                end case;
            end Build_Node;
            
        begin
            Position := Find_In_Subtree(Position, Parent_Node);
            
            while CurrentToken /= Stop_Token loop
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
        
        -- Build function calls
        procedure Build_Func_Call(Name : Unbounded_String) is
            Func : Ast_Node := Ast_Func_Call(Name);
        begin
            Append_Child(Ast, Position, Func);
            Build_Children(Func, RParen);
        end Build_Func_Call;
        
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
            Ret_Node : Ast_Node := Ast_Ret;
        begin
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
                    
                    -- Could be variable assignment or function call
                when Id =>
                    declare
                        Name : Unbounded_String := Buf;
                    begin
                        CurrentToken := Get_Token(File, Buf);
                        
                        if CurrentToken = LParen then
                            Build_Func_Call(Name);
                        elsif CurrentToken = Assign then
                            Build_Var_Assign(Name);
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
                -- Keywords
                when Scope => Put_Line("Scope");
                when Func => Print_Func;
                when Func_Call => Print_Func_Call;
                when VarDec => Print_Var(True);
                when VarAssign => Print_Var(False);
                when Ret => Put_Line("Ret");
                    
                -- Identifiers and literals
                when Int => Put("No: "); Put(Current.Int_Field1, 0); New_Line;
                when Id => Put_Line("ID: " & To_String(Current.Name));
                    
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

end Parser;

