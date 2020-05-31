with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Ast is
    
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
    
    -- Returns math node
    function Ast_Math return Ast_Node is
        Node : Ast_Node;
    begin
        UID := UID + 1;
        Node.UID := UID;
        Node.Node_Type := Math;
        return Node;
    end Ast_Math;
    
end Ast;
