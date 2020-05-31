with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Lex is
    
    -- Pushes back a lexical token
    procedure Unget_Token(T : Token) is
    begin
        UndoToken := T;
    end Unget_Token;

    --Gets a lexical token
    function Get_Token(File : File_Type; Buf : out Unbounded_String) return Token is
        -- Rename packages
        package UIO renames Ada.Text_IO.Unbounded_IO;
        
        -- Local variables
        C : Character;
        TT : Token;
        
        -- Converts a string to a token
        function To_Token(Input : String) return Token is
        
            -- Check to see if we have an integer
            function Is_Int(Item: in String) return Boolean is
                Dummy : Integer;
            begin
                Dummy := Integer'Value(Item);
                return True;
            exception
                when others => return False;
            end Is_Int;
            
        -- The lexical function
        begin
            if Input = "int" then
                return Int;
            elsif Input = "return" then
                return Ret;
            elsif Input = "syscall" then
                return Syscall;
            elsif Is_Int(Input) then
                return Num;
            else
                return Id;
            end if;
        end To_Token;
        
        -- Converts a character to a token
        function To_Token(Input : Character) return Token is
        begin
            case Input is
                when '(' => return LParen;
                when ')' => return RParen;
                when '{' => return LCBrace;
                when '}' => return RCBrace;
                when ';' => return SemiColon;
                when '=' => return Assign;
                when ',' => return Comma;
                when '+' => return Plus;
                when '-' => return Minus;
                when '*' => return Mul;
                when '/' => return Div;
                when others => return None;
            end case;
        end To_Token;
        
    -- The main part of the procedure
    begin
        if Cls then
            Set_Unbounded_String(Buf, "");
            Cls := False;
        end if;
        
        if UndoToken /= None then
            TT := UndoToken;
            UndoToken := None;
            return TT;
        end if;
        
        if End_Of_File(File) then
            return Eof;
        end if;
        
        if NextToken /= None then
            TT := NextToken;
            NextToken := None;
            return TT;
        end if;
    
        while not End_Of_File(File) loop
            Get(File, C);
            
            case C is
                when ' ' | Character'Val(9) =>
                    if Length(Buf) > 0 then
                        TT := To_Token(To_String(Buf));
                        Cls := True;
                        return TT;
                    end if;
                    
                when '(' | ')' | '{' | '}' | '=' | ';' | ',' |
                    '+' | '-' | '*' | '/' =>
                    TT := To_Token(C);
                    if Length(Buf) > 0 then
                        NextToken := TT;
                        TT := To_Token(To_String(Buf));
                        Cls := True;
                        return TT;
                    end if;
                    return TT;
                    
                when others => Append(Buf, C);
            end case;
        end loop;
        
        return TT;
    end Get_Token;
end Lex;

