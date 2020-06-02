with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Lex; use Lex;

package body Ast is
    
    -- Convert a Lex token type to a data type
    function Token_To_Data(T : Token; Is_Unsigned : Boolean := False) return Data_Type is
    begin
        if Is_Unsigned then
            case T is
            when Char => return UChar;
            when Short => return UShort;
            when Int => return UInt;
            when Long => return ULong;
           
            when others => return None;
            end case; 
        else
            case T is
            when Void => return Void;
            when Char => return Char;
            when Short => return Short;
            when Int => return Int;
            when Long => return Long;
            when FloatT => return FloatT;
            when Double => return Double;
           
            when others => return None;
            end case;
        end if;
    end Token_To_Data;
    
    -- Creates a scope node
    function Ast_Global return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Scope;
        return Node;
    end Ast_Global;
    
    -- Creates a function node
    function Ast_Func(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Func;
        Node.Name := Name;
        return Node;
    end Ast_Func;
    
    -- Creates a function call node
    function Ast_Func_Call(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Func_Call;
        Node.Name := Name;
        return Node;
    end Ast_Func_Call;
    
    -- Creates a variable assignment node
    function Ast_Var_Assign(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.Node_Type := VarAssign;
        Node.Name := Name;
        Node.UID := UID;
        return Node;
    end Ast_Var_Assign;
    
    -- Creates a variable declaration node
    function Ast_Var_Dec(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.Node_Type := VarDec;
        Node.Name := Name;
        Node.UID := UID;
        return Node;
    end Ast_Var_Dec;
    
    -- Return an integer
    function Ast_Ret return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Ret;
        return Node;
    end Ast_Ret;
    
    -- Represents an integer
    function Ast_Int return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Int;
        return Node;
    end Ast_Int;
    
    -- Represents a float
    function Ast_Float return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := FloatL;
        return Node;
    end Ast_Float;
    
    -- Represents a string
    function Ast_String return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Str;
        return Node;
    end Ast_String;
    
    -- Represents a character
    function Ast_Char return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := CharL;
        return Node;
    end Ast_Char;
    
    -- Represents an ID
    function Ast_Id(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Id;
        Node.Name := Name;
        return Node;
    end Ast_Id;
    
    -- Represents a comma
    -- A comma is only used in the first part of the AST
    function Ast_Comma return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Comma;
        return Node;
    end Ast_Comma;
    
    -- Returns a parameter node
    function Ast_Param return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Param;
        return Node;
    end Ast_Param;
    
    -- Returns an arguments node
    function Ast_Args return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Args;
        return Node;
    end Ast_Args;
    
    -- Returns math node
    function Ast_Math return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Math;
        return Node;
    end Ast_Math;
    
    -- Returns an if statement
    function Ast_If return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Cond_If;
        return Node;
    end Ast_If;
    
    -- Returns an else-if statement
    function Ast_Elif return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Cond_Elif;
        return Node;
    end Ast_Elif;
    
    -- Returns an else statement
    function Ast_Else return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Cond_Else;
        return Node;
    end Ast_Else;
    
end Ast;
