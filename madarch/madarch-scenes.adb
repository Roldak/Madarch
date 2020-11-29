with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

with Madarch.Components;
with Madarch.Exprs;
with Madarch.Values;

package body Madarch.Scenes is
   LF : Character renames Ada.Characters.Latin_1.LF;

   function Type_Reference (Kind : Values.Value_Kind) return String is
   begin
      case Kind is
         when Values.Vector3_Kind =>
            return "vec3";
         when Values.Float_Kind =>
            return "float";
         when Values.Int_Kind =>
            return "int";
      end case;
   end Type_Reference;

   function Dist_Function_Reference
     (Prim : Primitives.Primitive) return String
   is ("dist_to_" & Primitives.Get_Name (Prim));

   function Normal_Function_Reference
     (Prim : Primitives.Primitive) return String
   is (Primitives.Get_Name (Prim) & "_normal");

   function Component_Declaration
     (Comp : Components.Component) return String
   is
   begin
      return Type_Reference (Components.Get_Kind (Comp))
             & " "
             & Components.Get_Name (Comp);
   end Component_Declaration;

   function Primitive_Struct_Declaration
     (Prim : Primitives.Primitive) return Unbounded_String
   is
      Res  : Unbounded_String;
   begin
      Append (Res, "struct " & Primitives.Get_Name (Prim));
      Append (Res, " { ");
      for Comp of Primitives.Get_Components (Prim) loop
         Append (Res, Component_Declaration (Comp));
         Append (Res, "; ");
      end loop;
      Append (Res, "int material_id; };");
      return Res;
   end Primitive_Struct_Declaration;

   type Param is record
      Type_Name  : Unbounded_String;
      Param_Name : Unbounded_String;
   end record;

   function Create (Type_Name : String; Param_Name : String) return Param is
   begin
      return Param'
        (To_Unbounded_String (Type_Name),
         To_Unbounded_String (Param_Name));
   end Create;

   type Param_Array is array (Positive range <>) of Param;

   function Function_Declaration
     (Ret_Type : String;
      Fun_Name : String;
      Params   : Param_Array;
      Expr     : String) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      Append (Res, Ret_Type);
      Append (Res, " ");
      Append (Res, Fun_Name);
      Append (Res, "(");

      for I in Params'Range loop
         declare
            P : Param := Params (I);
            Is_Last : Boolean := I = Params'Last;
         begin
            Append (Res, P.Type_Name);
            Append (Res, " ");
            Append (Res, P.Param_Name);
            if not Is_Last then
               Append (Res, ", ");
            end if;
         end;
      end loop;

      Append (Res, ") {");
      Append (Res, LF);
      Append (Res, "return ");
      Append (Res, Expr);
      Append (Res, ";");
      Append (Res, LF);
      Append (Res, "}");
      return Res;
   end Function_Declaration;

   function Primitive_Dist_Function
     (Prim : Primitives.Primitive) return Unbounded_String
   is
      Prim_Param_Name  : String := "prim";
      Point_Param_Name : String := "x";

      Prim_Param_Expr  : Exprs.Struct_Expr :=
         Exprs.Struct_Identifier (Prim_Param_Name);

      Point_Param_Expr : Exprs.Expr :=
         Exprs.Value_Identifier (Point_Param_Name);

      Dist_Expr : Exprs.Expr'Class :=
         Primitives.Get_Dist_Expr (Prim, Prim_param_Expr, Point_Param_Expr);
   begin
      return Function_Declaration
        ("float",
         Dist_Function_Reference (Prim),
         (1 => Create (Primitives.Get_Name (Prim), Prim_Param_Name),
          2 => Create ("vec3", Point_Param_Name)),
         Dist_Expr.To_GLSL);
   end Primitive_Dist_Function;

   function Primitive_Normal_Function
     (Prim : Primitives.Primitive) return Unbounded_String
   is
      Prim_Param_Name  : String := "prim";
      Point_Param_Name : String := "x";

      Prim_Param_Expr  : Exprs.Struct_Expr :=
         Exprs.Struct_Identifier (Prim_Param_Name);

      Point_Param_Expr : Exprs.Expr :=
         Exprs.Value_Identifier (Point_Param_Name);

      Dist_Expr : Exprs.Expr'Class :=
         Primitives.Get_Normal_Expr (Prim, Prim_param_Expr, Point_Param_Expr);
   begin
      return Function_Declaration
        ("vec3",
         Normal_Function_Reference (Prim),
         (1 => Create (Primitives.Get_Name (Prim), Prim_Param_Name),
          2 => Create ("vec3", Point_Param_Name)),
         Dist_Expr.To_GLSL);
   end Primitive_Normal_Function;

   function Generate_Code
     (Prims_Count : Primitive_Count_Array) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      for Prim_Count of Prims_Count loop
         Append (Res, Primitive_Struct_Declaration (Prim_Count.Prim));
         Append (Res, LF);
         Append (Res, Primitive_Dist_Function (Prim_Count.Prim));
         Append (Res, LF);
         Append (Res, Primitive_Normal_Function (Prim_Count.Prim));
      end loop;
      return Res;
   end Generate_Code;

   function Compile
     (Prims_Count : Primitive_Count_Array) return Scene
   is
      Prims : Primitives.Primitive_Array_Access :=
         new Primitives.Primitive_Array'(1 .. Prims_Count'Length => <>);
   begin
      for I in 1 .. Prims'Length loop
         Prims (I) := Prims_Count (I).Prim;
      end loop;
      return new Scene_Internal'
        (Prims => Prims,
         GLSL  => Generate_Code (Prims_Count));
   end Compile;

   procedure Print_GLSL (S : Scene) is
   begin
      Put_Line (To_String (S.GLSL));
   end Print_GLSL;
end Madarch.Scenes;
