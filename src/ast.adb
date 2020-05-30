with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Ast is

    -- Creates a scope node
    function Ast_Global return Ast_Node is
        Node : Ast_Node := (Node_Type => Scope, Name => To_Unbounded_String(""));
    begin
        return Node;
    end Ast_Global;
    
    -- Creates a function node
    function Ast_Func(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node := (Node_Type => Func, Name => Name);
    begin
        return Node;
    end Ast_Func;
    
    -- Creates a variable assignment node
    function Ast_Var_Assign(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node := (Node_Type => VarAssign, Name => Name);
    begin
        return Node;
    end Ast_Var_Assign;
    
    -- Creates a variable declaration node
    function Ast_Var_Dec(Name : in Unbounded_String) return Ast_Node is
        Node : Ast_Node := (Node_Type => VarDec, Name => Name);
    begin
        return Node;
    end Ast_Var_Dec;
    
    -- Represents an integer
    function Ast_Int return Ast_Node is
        Node : Ast_Node := (Node_Type => Int, Name => To_Unbounded_String(""));
    begin
        return Node;
    end Ast_Int;

end Ast;
