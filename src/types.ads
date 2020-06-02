with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Types is

    type Data_Type is (None, Void, Char, Short, Int, Long, FloatT, Double);
    
    -- Represents a variable
    type Var is record
        Name : Unbounded_String;
        D_Type : Data_Type;
        Scope : Unbounded_String;
    end record;
    
    -- Represents a function
    type Func_Entry is record
        Name : Unbounded_String;
        Ret_Type : Data_Type;
    end record;

end Types;
