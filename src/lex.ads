with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--Lex package declaration
package Lex is
    type Token is (None, Eof, NewLn, Id, Num, StringL, CharL, FloatL,
                   LParen, RParen, LCBrace, RCBrace, SemiColon, Assign, Comma,
                   Plus, Minus, Mul, Div,          
                   Unsigned,
                   Void, Char, Short, Int, Long, FloatT, Double, 
                   Ret, Syscall);
        
    -- Global control variables needed by the lexer
    NextToken : Token := None;
    UndoToken : Token := None;
    Cls : Boolean := False;
    In_Quote : Boolean := False;
        
    procedure Unget_Token(T : Token);
    function Get_Token(File : File_Type; Buf : out Unbounded_String) return Token;
end Lex;
