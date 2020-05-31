with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;

-- Ast package declaration
package Ast is

    -- Represents node types
    type Ast_Type is (None, Scope, Func, Func_Call, Ret,
        Comma,
        VarDec, VarAssign,
        Add, Sub, Mul, Div,
        Id, Int);
    
    -- Unique identifier
    UID : Integer := 1;

    -- The base AST node
    type Ast_Node is record
        Node_Type : Ast_Type := None;
        UID : Integer := 1;
        Name : Unbounded_String;
        Int_Field1 : Integer := 0;
    end record;
    
    -- Helper functions
    function Ast_Global return Ast_Node;
    function Ast_Func(Name : in Unbounded_String) return Ast_Node;
    function Ast_Func_Call(Name : in Unbounded_String) return Ast_Node;
    function Ast_Var_Assign(Name : in Unbounded_String) return Ast_Node;
    function Ast_Var_Dec(Name : in Unbounded_String) return Ast_Node;
    function Ast_Ret return Ast_Node;
    function Ast_Int return Ast_Node;
    function Ast_Id(Name : in Unbounded_String) return Ast_Node;
    function Ast_Comma return Ast_Node;
end Ast;
