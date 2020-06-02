with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

with Types; use Types;

package Var_Vector is new Ada.Containers.Vectors
  (Index_Type => Natural,
   Element_Type => Var);
