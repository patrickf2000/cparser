with Ada.Containers.Multiway_Trees;
with Ada.Containers; use Ada.Containers;

with Ast; use Ast;

package Ast_Tree is new Ada.Containers.Multiway_Trees
        (Element_Type => Ast_Node);
