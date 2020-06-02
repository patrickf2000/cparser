with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Lex; use Lex;
with Ast_Tree; use Ast_Tree;
with Ast_Vector;
with Ast_IMap;
with Ast; use Ast;
with Types; use Types;
with Debug; use Debug;

-- The main parser area
package body Parser is
    
    -- Syntax error
    procedure Syntax_Error(Msg : String; Ln_No : String := "") is
    begin
        Put_Line("At: " & Ln_No);
        Put_Line("[Syntax Error] " & Msg);
        OS_Exit(1);
    end Syntax_Error;

    -- Build thre tree
    procedure Build_Tree(Ast : in out Ast_Tree.Tree; Path : String) is
        File : File_Type;
        Current_Token : Token;
        Position : Cursor := Ast.Root;
        Root : Ast_Node := Ast_Global;
        Current_Scope : Unbounded_String;
        
        -- Forward declarations
        procedure Build_Func_Call(Name : Unbounded_String);
        procedure Build_Var_Dec(D_Type : Token_Type; Name : Unbounded_String;
                                Var_Assign : Boolean := True;
                                Is_Unsigned : Boolean := False);
        
        -- Builds AST node until the end of an expression
        procedure Build_Children(Parent_Node : Ast_Node; 
                                 Stop_Token : Token_Type := SemiColon) is
            
            -- Local variables
            Node : Ast_Node;
            
            -- Builds an AST node based on the current position
            procedure Build_Node is
                Buf : Unbounded_String := Current_Token.Buf;
            begin
                case Current_Token.T_Type is
                    when Num => 
                        Node := Ast_Int;
                        Node.Int_Field1 := Integer'Value(To_String(Buf));
                        
                    when FloatL =>
                        Node := Ast_Float;
                        Node.Float_Field1 := Float'Value(To_String(Buf));
                    
                    when StringL =>
                        Node := Ast_String;
                        Node.Name := Buf;
                        
                    when CharL =>
                        Node := Ast_Char;
                        Node.Name := Buf;
                    
                    when Id =>
                        declare
                            Name : Unbounded_String := Buf;
                        begin
                            Current_Token := Get_Token(File);
                        
                            if Current_Token.T_Type = LParen then
                                Build_Func_Call(Name);
                            else
                                Node := Ast_Id(Name);
                                Unget_Token(Current_Token);
                            end if;
                        end;
                  
                    when Comma => Node := Ast_Comma;
                    
                    when Plus => Node.Node_Type := Add;
                    when Minus => Node.Node_Type := Sub;
                    when Mul => Node.Node_Type := Mul;
                    when Div => Node.Node_Type := Div;
                        
                    when Assign =>
                        Current_Token := Get_Token(File);
                        
                        if Current_Token.T_Type = Assign then
                            Node.Node_Type := Equal;
                        else
                            Unget_Token(Current_Token);
                        end if;
                        
                    when Greater => Node.Node_Type := Greater;
                
                    when others => Node.Node_Type := None;
                end case;
            end Build_Node;
            
        begin
            Position := Find_In_Subtree(Position, Parent_Node);
            
            while Current_Token.T_Type /= Stop_Token loop
                Build_Node;
                if Node.Node_Type /= None then
                    Append_Child(Ast, Position, Node);
                end if;
                
                Current_Token := Get_Token(File);
            end loop;
            
            Position := Parent(Position);
        end Build_Children;
        
        -- Builds function declarations
        procedure Build_Func(Data_Type : Token_Type; Name : Unbounded_String) is
            Func : Ast_Node := Ast_Func(Name);
            Args : Ast_Node := Ast_Args;
            
            Type_Token, Name_Token : Token;
        begin
            Current_Scope := Name;
            Func.D_Type := Token_To_Data(Data_Type);
            
            Append_Child(Ast, Position, Func);
            Position := Find_In_Subtree(Position, Func);
            
            Append_Child(Ast, Position, Args);
            Position := Find_In_Subtree(Position, Args);
            
            Current_Token := Get_Token(File);
            
            -- Function arguments
            while Current_Token.T_Type /= RParen loop
                if Current_Token.T_Type = Comma then
                    null;
                else
                    Type_Token := Current_Token;
                    Name_Token := Get_Token(File);
                
                    Build_Var_Dec(Type_Token.T_Type, Name_Token.Buf, False);
                end if;
                
                Current_Token := Get_Token(File);
            end loop;
            
            while Current_Token.T_Type /= LCBrace loop
                Current_Token := Get_Token(File);
            end loop;
            
            Position := Parent(Position);
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
            -- Drop the assignment token
            Current_Token := Get_Token(File);
            
            -- Build the assignment
            Append_Child(Ast, Position, Var_Assign);
            Build_Children(Var_Assign);
        end Build_Var_Assign;
        
        -- Builds variable declarations
        procedure Build_Var_Dec(D_Type : Token_Type; Name : Unbounded_String;
                                Var_Assign : Boolean := True;
                                Is_Unsigned : Boolean := False) is
            Var_Dec : Ast_Node := Ast_Var_Dec(Name);
            DT : Data_Type := Token_To_Data(D_Type, Is_Unsigned);
            V : Var := (Name => Name, D_Type => DT, Scope => Current_Scope);
        begin
            Vars.Append(V);
            Var_Dec.D_Type := DT;
            Append_Child(Ast, Position, Var_Dec);
            
            if Var_Assign then
                Build_Var_Assign(Name);
            end if;
        end Build_Var_Dec;
        
        procedure Build_Return is
            Ret_Node : Ast_Node := Ast_Ret;
        begin
            Append_Child(Ast, Position, Ret_Node);
            Build_Children(Ret_Node);
        end Build_Return;
        
        -- Build a conditional statement
        procedure Build_Cond(Is_Elif : Boolean := False) is
            Cond_Node : Ast_Node;
            Cmp : Ast_Node := Ast_Cond;
        begin
            if Is_Elif then
                Cond_Node := Ast_Elif;
            else
                Cond_Node := Ast_If;
            end if;
            
            Append_Child(Ast, Position, Cond_Node);
            Position := Find_In_Subtree(Position, Cond_Node);
            
            Append_Child(Ast, Position, Cmp);
            Build_Children(Cmp, RParen);
            
            while Current_Token.T_Type /= LCBrace loop
                Current_Token := Get_Token(File);
            end loop;
        end Build_Cond;
        
        -- Builds an else statement
        procedure Build_Else is
            Else_Node : Ast_Node := Ast_Else;
        begin
            Append_Child(Ast, Position, Else_Node);
            Position := Find_In_Subtree(Position, Else_Node);
        end Build_Else;
        
    -- The main parsing area
    begin
        -- Add the root node
        Append_Child(Ast, Position, Root);
        Position := First_Child(Position);
    
        -- Start passing
        Open(File, In_File, Path);
        
        while Current_Token.T_Type /= Eof loop
            Current_Token := Get_Token(File);
            
            case Current_Token.T_Type is
                -- Could be an unsigned variable or float declaration
                when Unsigned =>
                    declare
                        Data_Token : Token := Get_Token(File);
                        Data_Type : Token_Type := Data_Token.T_Type;
                        Name_Token : Token := Get_Token(File);
                        Name : Unbounded_String := Name_Token.Buf;
                    begin
                        Current_Token := Get_Token(File);
                        
                        case Data_Type is
                            when Char | Short | Int | Long =>
                                if Current_Token.T_Type = LParen then
                                    Build_Func(Data_Type, Name);
                                elsif Current_Token.T_Type = Assign then
                                    Build_Var_Dec(Data_Type, Name, Is_Unsigned => True);
                                elsif Current_Token.T_Type = SemiColon then
                                    Build_Var_Dec(Data_Type, Name, False, True);
                                else
                                    Syntax_Error("Invalid modifier unsigned.");
                                end if;
                                
                            when others => Syntax_Error("Invalid modifier unsigned.");
                        end case;
                    end;
                
                -- Could be variable declaration or function declaration
                when Signed | Void | Char | Short | Int | Long | FloatT | Double =>
                    if Current_Token.T_Type = Signed then
                        Current_Token := Get_Token(File);
                        
                        case Current_Token.T_Type is
                            when Void | FloatT | Double =>
                                Syntax_Error("The signed keyword cannot be used with void, float, or double.");
                                
                            when others => null;
                        end case;
                    end if;
                    
                    declare
                        Data_Token : Token := Current_Token;
                        Data_Type : Token_Type := Data_Token.T_Type;
                        Name_Token : Token := Get_Token(File);
                        Name : Unbounded_String := Name_Token.Buf;
                    begin
                        Current_Token := Get_Token(File);

                        if Current_Token.T_Type = LParen then
                            Build_Func(Data_Type, Name);
                        elsif Current_Token.T_Type = Assign then
                            Build_Var_Dec(Data_Type, Name);
                        elsif Current_Token.T_Type = SemiColon then
                            Build_Var_Dec(Data_Type, Name, False);
                        else
                            Syntax_Error("Expected variable or function declaration.");
                        end if;
                    end;
                    
                    -- Could be variable assignment or function call
                when Id =>
                    declare
                        Name : Unbounded_String := Current_Token.Buf;
                        Ln_No : Positive_Count := Line(File);
                    begin
                        if To_String(Name) = "#include" then
                            while Current_Token.T_Type /= NewLn loop
                                Current_Token := Get_Token(File);
                            end loop;
                        else
                            Current_Token := Get_Token(File);
                        
                            if Current_Token.T_Type = LParen then
                                Build_Func_Call(Name);
                            elsif Current_Token.T_Type = Assign then
                                Build_Var_Assign(Name);
                            else
                                Syntax_Error("Expected variable declaration or function call.",
                                            Ln_No'Image);
                            end if;
                        end if;
                    end;
                    
                -- Conditional statement
                when If_T => Build_Cond;    
                when Else_T =>
                    Current_Token := Get_Token(File);
                    
                    if Current_Token.T_Type = If_T then
                        Build_Cond(True);
                    else
                        Build_Else;
                    end if;
                    
                -- End block
                when RCBrace => 
                    Position := Parent(Position);
                    
                    declare
                        Next_Token : Token := Get_Token(File);
                        End_B : Ast_Node;
                    begin
                        if Next_Token.T_Type = Else_T then
                            Unget_Token(Next_Token);
                        else
                            End_B.Node_Type := End_Block;
                            Append_Child(Ast, Position, End_B);
                        end if;
                    end;
                
                -- Return statements
                when Ret => Build_Return;
                
                -- TODO: We need a syntax error here
                when others => null;
            end case;
        end loop;
        
        Close(File);
        
    exception
        when E : Constraint_Error =>
            declare
                Ln_No : Positive_Count := Line(File);
            begin
                Put_Line(Exception_Message(E));
                Syntax_Error("Encountered conversion error.", Ln_No'Image);
            end;
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
            Position2 : Cursor := First_Child(Position);
            Node : Ast_Node;
        begin
            if Children_No = 0 then
                Syntax_Error("Expected values in variable assignment.");
            elsif Children_No = 1 then
                Node := Element(Position2);
                
                if Node.Node_Type = Func_Call and Child_Count(Position2) > 0 then
                    Handle_Func_Call(Position2);
                end if;
            else
                declare
                    Children : Ast_Vector.Vector;
                    Func_Map : Ast_IMap.Map;
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

end Parser;

