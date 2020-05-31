with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Lex; use Lex;
with Ast_Tree; use Ast_Tree;
with Ast_Vector;
with Ast_IMap;
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
    
    -- This runs through the tree and consolidates it into more meaningful parts
    -- For instance, we will perform these steps
    -- 1) Variable assignments
    --    -> If there are multiple children, we add a math node as a parent
    -- 2) Function parameters
    --    -> All arguments must go under a "Param" node. These are separated by commas
    --
    -- Some basic syntax errors are caught here
    --
    procedure Run_Pass2(Ast : in out Ast_Tree.Tree) is
        Current : Ast_Node;
        
        -- Syntax error
        procedure Syntax_Error(Msg : String) is
        begin
            Put_Line("[Syntax Error] " & Msg);
            OS_Exit(1);
        end Syntax_Error;
        
        -- Handle function call
        -- Algorithm:
        -- 1) Gather all children
        -- 2) Clear the subtree
        -- 3) Iterate through the children and create parameters
        --    -> When we hit a comma, add the param as a child, and clear things out
        --
        procedure Handle_Func_Call(Position : in out Cursor) is
            Current_Param : Ast_Node := Ast_Param;
            Position2 : Cursor := First_Child(Position);
            Children : Ast_Vector.Vector;
            Node : Ast_Node;
        begin
            while Has_Element(Position2) loop
                Node := Element(Position2);
                Children.Append(Node);
                Position2 := Next_Sibling(Position2);
            end loop;
            
            Delete_Children(Ast, Position);
            Append_Child(Ast, Position, Current_Param);
            Position2 := Find_In_Subtree(Position, Current_Param);
            
            for N of Children loop
                if N.Node_Type = Comma then
                    Current_Param := Ast_Param;
                    Append_Child(Ast, Position, Current_Param);
                    Position2 := Find_In_Subtree(Position, Current_Param);
                else
                    Append_Child(Ast, Position2, N);
                end if;
            end loop;
            
        end Handle_Func_Call;
        
        -- Handle variable assignment
        procedure Handle_Var_Assign(Position : in out Cursor) is
            Children_No : Count_Type := Child_Count(Position);
        begin
            if Children_No = 0 then
                Syntax_Error("Expected values in variable assignment.");
            elsif Children_No = 1 then
                null;
            else
                declare
                    Children : Ast_Vector.Vector;
                    Func_Map : Ast_IMap.Map;
                    Position2 : Cursor := First_Child(Position);
                    Node : Ast_Node;
                    Math : Ast_Node := Ast_Math;
                    Func_No : Integer := 1;
                begin
                    while Has_Element(Position2) loop
                        Node := Element(Position2);
                        Children.Append(Node);
                        
                        -- If we have a function call, we need to get all the parameters
                        if Node.Node_Type = Func_Call and Child_Count(Position2) > 0 then
                            declare
                                Position3 : Cursor := First_Child(Position2);
                                Node3 : Ast_Node;
                                F_Children : Ast_Vector.Vector;
                            begin
                                while Has_Element(Position3) loop
                                    Node3 := Element(Position3);
                                    F_Children.Append(Node3);
                                    Position3 := Next_Sibling(Position3);
                                end loop;
                                
                                Func_Map.Include(Func_No, F_Children);
                                Func_No := Func_No + 1;
                            end;
                        end if;
                        
                        Position2 := Next_Sibling(Position2);
                    end loop;
                    
                    Func_No := 1;
                    
                    Delete_Children(Ast, Position);
                    Append_Child(Ast, Position, Math);
                    Position2 := Find_In_Subtree(Position, Math);
                    
                    for N of Children loop
                        Append_Child(Ast, Position2, N);
                        
                        -- If we have a function call, we need to pass the parameters
                        -- back into the tree
                        if N.Node_Type = Func_Call then
                            declare
                                Position3 : Cursor := Find_In_Subtree(Position, N);
                                F_Children : Ast_Vector.Vector := Func_Map.Element(Func_No);
                            begin
                                for N2 of F_Children loop
                                    Append_Child(Ast, Position3, N2);
                                end loop;
                                
                                Handle_Func_Call(Position3);
                            end;
                            
                            Func_No := Func_No + 1;
                        end if;
                    end loop;
                end;
            end if;
        end Handle_Var_Assign;
        
        --Iterates through tree
        procedure Walk(Position : in out Cursor) is
            Position2 : Cursor;
        begin
            if Has_Element(Position) then
                Current := Element(Position);
                
                case Current.Node_Type is
                    when Scope | Func =>
                        if Child_Count(Position) > 0 then
                            Position2 := First_Child(Position);
                            Walk(Position2);
                        end if;
                    
                    when VarAssign => Handle_Var_Assign(Position);
                        
                    when Func_Call => Handle_Func_Call(Position);
                        
                    when others => null;
                end case;
                
                Position := Next_Sibling(Position);
                Walk(Position);
            end if;
        end Walk;
        
        -- Start at the root node
        Position : Cursor := Ast.Root;
    begin
        Position := First_Child(Position);
        Walk(Position);
    end Run_Pass2;
    
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
                when Param => Put_Line("Param");
                when VarDec => Print_Var(True);
                when VarAssign => Print_Var(False);
                when Ret => Put_Line("Ret");
                    
                -- Identifiers and literals
                when Int => Put("No: "); Put(Current.Int_Field1, 0); New_Line;
                when Id => Put_Line("ID: " & To_String(Current.Name));
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

end Parser;

