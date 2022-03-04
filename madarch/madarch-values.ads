with GL.Types;

with GPU_Types;

package Madarch.Values is
   package GLT renames GL.Types;

   type Value_Kind is (Vector3_Kind, Float_Kind, Int_Kind);

   function To_GLSL (K : Value_Kind) return String;

   type Value (Kind : Value_Kind := Float_Kind) is record
      case Kind is
         when Vector3_Kind =>
            Vector3_Value : GLT.Singles.Vector3;
         when Float_Kind =>
            Float_Value : GLT.Single;
         when Int_Kind =>
            Int_Value : GLT.Int;
      end case;
   end record;

   type Value_Array is array (Positive range <>) of Value;

   function Vector3 (X : GLT.Singles.Vector3) return Value;
   function Float (X : GLT.Single) return Value;
   function Int (X : GLT.Int) return Value;

   function "+" (L, R : Value) return Value;
   function "-" (L, R : Value) return Value;
   function "*" (L, R : Value) return Value;
   function "/" (L, R : Value) return Value;

   function "<"  (L, R : Value) return Value;
   function ">"  (L, R : Value) return Value;
   function "<=" (L, R : Value) return Value;
   function ">=" (L, R : Value) return Value;

   function Dot (L, R : Value) return Value;
   function Min (L, R : Value) return Value;
   function Max (L, R : Value) return Value;

   function "-" (V : Value) return Value;
   function Length (V : Value) return Value;
   function Normalize (V : Value) return Value;
   function Abs_Value (V : Value) return Value;

   function Sin (V : Value) return Value;
   function Cos (V : Value) return Value;
   function Tan (V : Value) return Value;
   function Asin (V : Value) return Value;
   function Acos (V : Value) return Value;
   function Atan (V : Value) return Value;

   function GPU_Type (K : Value_Kind) return GPU_Types.GPU_Type;
end Madarch.Values;
