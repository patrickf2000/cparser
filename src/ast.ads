with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;

with Lex; use Lex;
with Types; use Types;

-- Ast package declaration
package Ast is

    -- Represents node types
    type Ast_Type is (None, Scope, Func, Func_Call, Ret,
                      Comma, Param, Args,
                      VarDec, VarAssign,
                      Cond_If, Cond_Elif, Cond_Else,
                      Add, Sub, Mul, Div,
                      Id, CharL, Int, FloatL, Math, Str);
    
    -- Unique identifier
    UID : Integer := 1;

    -- The base AST node
    type Ast_Node is record
        Node_Type : Ast_Type := None;
        UID : Integer := 1;
        Name : Unbounded_String;
        Int_Field1 : Integer := 0;
        Float_Field1 : Float := 0.0;
        D_Type : Data_Type := None;
    end record;
    
    -- Conversion functions
    function Token_To_Data(T : Token; Is_Unsigned : Boolean := False) return Data_Type;
    
    -- Helper functions
    function Ast_Global return Ast_Node;
    function Ast_Func(Name : in Unbounded_String) return Ast_Node;
    function Ast_Func_Call(Name : in Unbounded_String) return Ast_Node;
    function Ast_Var_Assign(Name : in Unbounded_String) return Ast_Node;
    function Ast_Var_Dec(Name : in Unbounded_String) return Ast_Node;
    function Ast_Ret return Ast_Node;
    function Ast_Int return Ast_Node;
    function Ast_Float return Ast_Node;
    function Ast_String return Ast_Node;
    function Ast_Char return Ast_Node;
    function Ast_Id(Name : in Unbounded_String) return Ast_Node;
    function Ast_Comma return Ast_Node;
    function Ast_Param return Ast_Node;
    function Ast_Args return Ast_Node;
    function Ast_Math return Ast_Node;
    function Ast_If return Ast_Node;
    function Ast_Elif return Ast_Node;
    function Ast_Else return Ast_Node;
end Ast;
