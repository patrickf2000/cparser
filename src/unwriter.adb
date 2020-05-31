with Ada.Text_IO; use Ada.Text_IO;

with Ast_Tree;

package body Unwriter is

    -- The Unwrite function takes an abstract syntax tree, and translates
    -- it back to C. This will be useful because we can create a test script
    -- to verify that it is being parsed correctly.
    --
    -- It also could form the base of a source-to-source translator.
    --
    procedure Unwrite(Out_File : String; Ast : Ast_Tree.Tree) is
        File : File_Type;
    begin
        Create(File, Ada.Text_IO.Out_File, Out_File);
        Close(File);
    end Unwrite;

end Unwriter;
