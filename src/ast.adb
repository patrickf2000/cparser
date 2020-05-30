with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Ast is
    
    -- Creates a scope node
    function Ast_Global return Ast_Node is
        Node : Ast_Node;
    begin
        Node.Node_Type := Scope;
        return Node;
    end Ast_Global;
    
    -- Creates a function node
    function Ast_Func(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        Node.Node_Type := Func;
        Node.Name := Name;
        return Node;
    end Ast_Func;
    
    -- Creates a variable assignment node
    function Ast_Var_Assign(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        Node.Node_Type := VarAssign;
        Node.Name := Name;
        return Node;
    end Ast_Var_Assign;
    
    -- Creates a variable declaration node
    function Ast_Var_Dec(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        Node.Node_Type := VarDec;
        Node.Name := Name;
        return Node;
    end Ast_Var_Dec;
    
    -- Return an integer
    function Ast_Ret return Ast_Node is
        Node : Ast_Node;
    begin
        Node.Node_Type := Ret;
        return Node;
    end Ast_Ret;
    
    -- Represents an integer
    function Ast_Int return Ast_Node is
        Node : Ast_Node;
    begin
        Node.Node_Type := Int;
        return Node;
    end Ast_Int;
    
    -- Represents an ID
    function Ast_Id(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node;
    begin
        Node.Node_Type := Id;
        Node.Name := Name;
        return Node;
    end Ast_Id;

end Ast;
