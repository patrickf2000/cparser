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
    function Get_Token(File : File_Type) return Token is
        -- Rename packages
        package UIO renames Ada.Text_IO.Unbounded_IO;
        
        -- Local variables
        C : Character;
        TT : Token;
        
        -- Converts a string to a token
        function To_Token(Input : String) return Token is
            
            -- Check to see if we have a float
            function Is_Float(Item : in String) return Boolean is
                Dummy : Float;
            begin
                Dummy := Float'Value(Item);
                return True;
            exception
                when others => return False;
            end Is_Float;
        
            -- Check to see if we have an integer
            function Is_Int(Item: in String) return Boolean is
                Dummy : Integer;
            begin
                Dummy := Integer'Value(Item);
                return True;
            exception
                when others => return False;
            end Is_Int;
            
            -- Local value
            T : Token;
            
        -- The lexical function
        begin
            -- Data types
            if Input = "void" then
                T.T_Type := Void;
            elsif Input = "char" then
                T.T_Type := Char;
            elsif Input = "short" then
                T.T_Type := Short;
            elsif Input = "int" then
                T.T_Type := Int;
            elsif Input = "long" then
                T.T_Type := Long;
            elsif Input = "float" then
                T.T_Type := FloatT;
            elsif Input = "double" then
                T.T_Type := Double;
                
            -- Other keywords
            elsif Input = "unsigned" then
                T.T_Type := Unsigned;
            elsif Input = "signed" then
                T.T_Type := Signed;
                
            -- Conditional keywords
            elsif Input = "if" then
                T.T_Type := If_T;
            elsif Input = "else" then
                T.T_Type := Else_T;
                
            elsif Input = "return" then
                T.T_Type := Ret;
            elsif Input = "syscall" then
                T.T_Type := Syscall;
            elsif Is_Int(Input) then
                T.T_Type := Num;
                T.Buf := Buf;
            elsif Is_Float(Input) then
                T.T_Type := FloatL;
                T.Buf := Buf;
            else
                T.T_Type := Id;
                T.Buf := Buf;
            end if;
            
            return T;
        end To_Token;
        
        -- Converts a character to a token
        function To_Token(Input : Character) return Token is
            T : Token;
        begin
            case Input is
                when '(' => T.T_Type := LParen;
                when ')' => T.T_Type := RParen;
                when '{' => T.T_Type := LCBrace;
                when '}' => T.T_Type := RCBrace;
                when ';' => T.T_Type := SemiColon;
                when '=' => T.T_Type := Assign;
                when ',' => T.T_Type := Comma;
                when '+' => T.T_Type := Plus;
                when '-' => T.T_Type := Minus;
                when '*' => T.T_Type := Mul;
                when '/' => T.T_Type := Div;
                when '>' => T.T_Type := Greater;
                when '<' => T.T_Type := Less;
                when '!' => T.T_Type := Not_T;
                    
                when Character'Val(10) => T.T_Type := NewLn;
                    
                when others => T.T_Type := None;
            end case;
            
            return T;
        end To_Token;
        
    -- The main part of the procedure
    begin
        if Cls then
            Set_Unbounded_String(Buf, "");
            Cls := False;
        end if;
        
        if UndoToken.T_Type /= None then
            TT := UndoToken;
            UndoToken.T_Type := None;
            return TT;
        end if;
        
        if End_Of_File(File) then
            TT.T_Type := Eof;
            return TT;
        end if;
        
        if NextToken.T_Type /= None then
            TT := NextToken;
            NextToken.T_Type := None;
            return TT;
        end if;
    
        while not End_Of_File(File) loop
            Get_Immediate(File, C);
            
            if In_Quote then
                if C = '"' or C = ''' then
                    In_Quote := False;
                    TT.T_Type := StringL;
                    TT.Buf := Buf;
                    Cls := True;
                    
                    if C = ''' then
                        TT.T_Type := CharL;
                    end if;
                    
                    return TT;
                else
                    Append(Buf, C);
                end if;
            else
                case C is
                    when '"' | ''' => In_Quote := True;
                        
                    when ' ' | Character'Val(9) =>
                        if Length(Buf) > 0 then
                            TT := To_Token(To_String(Buf));
                            Cls := True;
                            return TT;
                        end if;
                    
                    when '(' | ')' | '{' | '}' | '=' | ';' | ',' |
                        '+' | '-' | '*' | '/' |
                        '>' | '<' | '!' |
                        Character'Val(10) =>
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
            end if;
        end loop;
        
        return TT;
    end Get_Token;
end Lex;

