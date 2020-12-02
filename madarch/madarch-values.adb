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
   begin
      Check_Kinds (L, R);
      case L.Kind is
         when Vector3_Kind =>
            return (Vector3_Kind, Mul (L.Vector3_Value, R.Vector3_Value));
         when Float_Kind =>
            return (Float_Kind, L.Float_Value * R.Float_Value);
         when Int_Kind =>
            return (Int_Kind, L.Int_Value * R.Int_Value);
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
