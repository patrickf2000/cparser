with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

with Ast; use Ast;

package Ast_Vector is new Ada.Containers.Vectors
  (Index_Type => Natural,
   Element_Type => Ast_Node);
