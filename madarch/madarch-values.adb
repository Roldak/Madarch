with Math_Utils; use Math_Utils;

with GPU_Types.Base;

package body Madarch.Values is
   use type GLT.Single, GLT.Int, GLT.Singles.Vector3;

   function Vector3 (X : GLT.Singles.Vector3) return Value is
     (Vector3_Kind, X);

   function Float (X : GLT.Single) return Value is
     (Float_Kind, X);

   function Int (X : GLT.Int) return Value is
     (Int_Kind, X);

   procedure Check_Kinds (L, R : Value) is
   begin
      if L.Kind /= R.Kind then
         raise Program_Error with
            "Incompatible kinds " & L.Kind'Image & " and " & R.Kind'Image;
      end if;
   end Check_Kinds;

   function To_GLSL (K : Value_Kind) return String is
   begin
      case K is
         when Values.Vector3_Kind =>
            return "vec3";
         when Values.Float_Kind =>
            return "float";
         when Values.Int_Kind =>
            return "int";
      end case;
   end To_GLSL;

   function "+" (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, L.Vector3_Value + R.Vector3_Value);
         when Float_Kind =>
            return (Float_Kind, L.Float_Value + R.Float_Value);
         when Int_Kind =>
            return (Int_Kind, L.Int_Value + R.Int_Value);
      end case;
   end "+";

   function "-" (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, L.Vector3_Value - R.Vector3_Value);
         when Float_Kind =>
            return (Float_Kind, L.Float_Value - R.Float_Value);
         when Int_Kind =>
            return (Int_Kind, L.Int_Value - R.Int_Value);
      end case;
   end "-";

   function "*" (L, R : Value) return Value is
      use GL.Types;
   begin
      case L.Kind is
         when Vector3_Kind =>
            case R.Kind is
               when Vector3_Kind =>
                  return (Vector3_Kind,
                          Mul (L.Vector3_Value, R.Vector3_Value));
               when Float_Kind =>
                  return (Vector3_Kind,
                          L.Vector3_Value * R.Float_Value);
               when Int_Kind =>
                  return (Vector3_Kind,
                          L.Vector3_Value * Single (R.Int_Value));
            end case;
         when Float_Kind =>
            case R.Kind is
               when Vector3_Kind =>
                  return (Vector3_Kind,
                          L.Float_Value * R.Vector3_Value);
               when Float_Kind =>
                  return (Float_Kind,
                          L.Float_Value * R.Float_Value);
               when Int_Kind =>
                  return (Float_Kind,
                          L.Float_Value * Single (R.Int_Value));
            end case;
         when Int_Kind =>
            case R.Kind is
               when Vector3_Kind =>
                  return (Vector3_Kind,
                          Single (L.Int_Value) * R.Vector3_Value);
               when Float_Kind =>
                  return (Float_Kind,
                          Single (L.Int_Value) * R.Float_Value);
               when Int_Kind =>
                  return (Int_Kind, L.Int_Value * R.Int_Value);
            end case;
      end case;
   end "*";

   function "/" (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, Div (L.Vector3_Value, R.Vector3_Value));
         when Float_Kind =>
            return (Float_Kind, L.Float_Value + R.Float_Value);
         when Int_Kind =>
            return (Int_Kind, L.Int_Value + R.Int_Value);
      end case;
   end "/";

   function "**" (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            raise Program_Error with "'**' not applicable to vector3.";
         when Float_Kind =>
            return (Float_Kind, L.Float_Value ** R.Float_Value);
         when Int_Kind =>
            return (Int_Kind, L.Int_Value ** Natural (R.Int_Value));
      end case;
   end "**";

   function To_Int (B : Boolean) return GL.Types.Int is
     (if B then 1 else 0);

   function "<"  (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Float_Kind =>
            return (Int_Kind, To_Int (L.Float_Value < R.Float_Value));
         when Int_Kind =>
            return (Int_Kind, To_Int (L.Int_Value < R.Int_Value));
         when others =>
            raise Program_Error with "'<' not applicable to vector3.";
      end case;
   end "<";

   function ">"  (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Float_Kind =>
            return (Int_Kind, To_Int (L.Float_Value > R.Float_Value));
         when Int_Kind =>
            return (Int_Kind, To_Int (L.Int_Value > R.Int_Value));
         when others =>
            raise Program_Error with "'>' not applicable to vector3.";
      end case;
   end ">";

   function "<=" (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Float_Kind =>
            return (Int_Kind, To_Int (L.Float_Value <= R.Float_Value));
         when Int_Kind =>
            return (Int_Kind, To_Int (L.Int_Value <= R.Int_Value));
         when others =>
            raise Program_Error with "'<=' not applicable to vector3.";
      end case;
   end "<=";

   function ">=" (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Float_Kind =>
            return (Int_Kind, To_Int (L.Float_Value >= R.Float_Value));
         when Int_Kind =>
            return (Int_Kind, To_Int (L.Int_Value >= R.Int_Value));
         when others =>
            raise Program_Error with "'>=' not applicable to vector3.";
      end case;
   end ">=";

   function Dot (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Float_Kind, Dot (L.Vector3_Value, R.Vector3_Value));
         when others =>
            raise Program_Error with "Dot is only allowed on vectors";
      end case;
   end Dot;

   function Cross (L, R : Value) return Value is
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, Cross (L.Vector3_Value, R.Vector3_Value));
         when others =>
            raise Program_Error with "Cross is only allowed on vectors";
      end case;
   end Cross;

   function Min (L, R : Value) return Value is
      use GL.Types;
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, Min (L.Vector3_Value, R.Vector3_Value));
         when Float_Kind =>
            return (Float_Kind, Single'Min (L.Float_Value, R.Float_Value));
         when Int_Kind =>
            return (Int_Kind, GL.Types.Int'Min (L.Int_Value, R.Int_Value));
      end case;
   end Min;

   function Max (L, R : Value) return Value is
      use GL.Types;
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, Max (L.Vector3_Value, R.Vector3_Value));
         when Float_Kind =>
            return (Float_Kind, Single'Max (L.Float_Value, R.Float_Value));
         when Int_Kind =>
            return (Int_Kind, GL.Types.Int'Max (L.Int_Value, R.Int_Value));
      end case;
   end Max;

   function Clamp (V, LB, UB : Value) return Value is
      use GL.Types;
   begin
      Check_Kinds (V, LB);
      Check_Kinds (LB, UB);
      return Min (Max (V, LB), UB);
   end Clamp;

   function "-" (V : Value) return Value is
   begin
      case V.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, -1.0 * V.Vector3_Value);
         when Float_Kind =>
            return (Float_Kind, -V.Float_Value);
         when Int_Kind =>
            return (Int_Kind, -V.Int_Value);
      end case;
   end "-";

   function Length (V : Value) return Value is
   begin
      case V.Kind is
         when Vector3_Kind =>
            return (Float_Kind, Length (V.Vector3_Value));
         when Float_Kind =>
            raise Program_Error with "Cannot take length of float.";
         when Int_Kind =>
            raise Program_Error with "Cannot take length of int.";
      end case;
   end Length;

   function Normalize (V : Value) return Value is
   begin
      case V.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, Normalize (V.Vector3_Value));
         when Float_Kind =>
            raise Program_Error with "Cannot normalize float.";
         when Int_Kind =>
            raise Program_Error with "Cannot normalize int.";
      end case;
   end Normalize;

   function Abs_Value (V : Value) return Value is
   begin
      case V.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, Vec_Abs (V.Vector3_Value));
         when Float_Kind =>
            return (Float_Kind, abs V.Float_Value);
         when Int_Kind =>
            return (Int_Kind, abs V.Int_Value);
      end case;
   end Abs_Value;

   generic
      Name : String;
      with function Trig (X : GLT.Single) return GLT.Single;
   function Eval_Elementary (V : Value) return Value;

   function Eval_Elementary (V : Value) return Value is
   begin
      case V.Kind is
         when Float_Kind =>
            return (Float_Kind, Trig (V.Float_Value));
         when Int_Kind =>
            return (Float_Kind, Trig (GLT.Single (V.Int_Value)));
         when Vector3_Kind =>
            raise Program_Error with "Cannot apply " & Name & " to vector3.";
      end case;
   end Eval_Elementary;

   function Eval_Sin  is new Eval_Elementary ("Sin", Sin);
   function Eval_Cos  is new Eval_Elementary ("Cos", Cos);
   function Eval_Tan  is new Eval_Elementary ("Tan", Tan);
   function Eval_Asin is new Eval_Elementary ("Asin", ASin);
   function Eval_Acos is new Eval_Elementary ("Acos", Acos);
   function Eval_Atan is new Eval_Elementary ("Atan", Atan);
   function Eval_Sqrt is new Eval_Elementary ("Sqrt", Sqrt);

   function Sin (V : Value) return Value renames Eval_Sin;
   function Cos (V : Value) return Value renames Eval_Cos;
   function Tan (V : Value) return Value renames Eval_Tan;
   function Asin (V : Value) return Value renames Eval_Asin;
   function Acos (V : Value) return Value renames Eval_Acos;
   function Atan (V : Value) return Value renames Eval_Atan;
   function Sqrt (V : Value) return Value renames Eval_Sqrt;

   function Dot2 (V : Value) return Value is
   begin
      case V.Kind is
         when Vector3_Kind =>
            return (Float_Kind, Dot2 (V.Vector3_Value));
         when Float_Kind =>
            raise Program_Error with "cannot apply dot2 to float.";
         when Int_Kind =>
            raise Program_Error with "Cannot apply dot2 to int.";
      end case;
   end Dot2;

   function GPU_Type (K : Value_Kind) return GPU_Types.GPU_Type is
   begin
      case K is
         when Vector3_Kind =>
            return GPU_Types.Base.Vec_3;
         when Float_Kind =>
            return GPU_Types.Base.Float;
         when Int_Kind =>
            return GPU_Types.Base.Int;
      end case;
   end GPU_Type;
end Madarch.Values;
