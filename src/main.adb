with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Lex; use Lex;
with Ast_Tree; use Ast_Tree;
with Parser; use Parser;

procedure Main is
    -- Local types
    --File : File_Type;
    Buf : Unbounded_String;
    --CurrentToken : Token := None;
    Ast : Tree;

    -- Prints a token
    procedure Print_Token(TT : Token) is
    begin
        case TT is
            when Int => Put_Line("Int");
            when Ret => Put_Line("Return");
            when Syscall => Put_Line("Syscall");
            when Id => Put_Line("ID -> " & To_String(Buf));
            when Num => Put_Line("Number -> " & To_String(Buf));
            when Eof => Put_Line("EOF");
            when LParen => Put_Line("(");
            when RParen => Put_Line(")");
            when LCBrace => Put_Line("{");
            when RCBrace => Put_Line("}");
            when SemiColon => Put_Line(";");
            when Assign => Put_Line("=");
            
            when others => Put_Line("IDK!");
        end case;
    end Print_Token;

begin
    --Open(File, In_File, "first.c");
    --
    --while CurrentToken /= Eof loop
    --    CurrentToken := Get_Token(File, Buf);
    --    Print_Token(CurrentToken);
    --end loop;
    --
    --Close(File);
    
    Build_Tree(Ast, "first.c");
    Print_Tree(Ast);
end Main;
