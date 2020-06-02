with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--Lex package declaration
package Lex is
    type Token_Type is (None, Eof, NewLn, Id, Num, StringL, CharL, FloatL,
                   LParen, RParen, LCBrace, RCBrace, SemiColon, Assign, Comma,
                   Plus, Minus, Mul, Div,
                   Greater, Less, Not_T,
                   Unsigned, Signed,
                   Void, Char, Short, Int, Long, FloatT, Double,
                   If_T, Else_T,
                        Ret, Syscall);
    
    type Token is record
        T_Type : Token_Type := None;
        Buf : Unbounded_String;
    end record;
        
    -- Global control variables needed by the lexer
    NextToken : Token;
    UndoToken : Token;
    Cls : Boolean := False;
    In_Quote : Boolean := False;
    
    Buf : Unbounded_String;
        
    procedure Unget_Token(T : Token);
    function Get_Token(File : File_Type) return Token;
end Lex;
