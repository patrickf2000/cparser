with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers; use Ada.Containers;

with Ast_Vector; use Ast_Vector;

package Ast_IMap is new Ada.Containers.Indefinite_Ordered_Maps
  (Key_Type => Integer,
   Element_Type => Vector);
