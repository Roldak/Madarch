with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

with Madarch.Components;
with Madarch.Exprs;
with Madarch.Values;

package body Madarch.Scenes is
   LF  : Character renames Ada.Characters.Latin_1.LF;
   DLF : constant String := LF & LF;

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

   function Prim_Count_Reference
     (Prim : Primitives.Primitive) return String
   is ("prim_" & Primitives.Get_Name (Prim) & "_count");

   function Prim_Array_Reference
     (Prim : Primitives.Primitive) return String
   is ("prim_" & Primitives.Get_Name (Prim) & "s");

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
      Append (Res, " {");
      Append (Res, LF);
      for Comp of Primitives.Get_Components (Prim) loop
         Append (Res, Component_Declaration (Comp));
         Append (Res, ";");
         Append (Res, LF);
      end loop;
      Append (Res, "int material_id;");
      Append (Res, LF);
      Append (Res, "};");
      return Res;
   end Primitive_Struct_Declaration;

   type Param is record
      Type_Name  : Unbounded_String;
      Param_Name : Unbounded_String;
      Mode       : Unbounded_String;
   end record;

   function Create
     (Type_Name : String; Param_Name : String; Mode : String := "") return Param is
   begin
      return Param'
        (To_Unbounded_String (Type_Name),
         To_Unbounded_String (Param_Name),
         To_unbounded_String (Mode));
   end Create;

   type Param_Array is array (Positive range <>) of Param;

   function Function_Declaration
     (Ret_Type : String;
      Fun_Name : String;
      Params   : Param_Array;
      Expr     : String;
      Stmts    : String := "") return Unbounded_String
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
            Append (Res, P.Mode);
            Append (Res, " ");
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

      if Stmts /= "" then
         Append (Res, Stmts);
         Append (Res, LF);
      end if;

      Append (Res, "return ");
      Append (Res, Expr);
      Append (Res, ";");
      Append (Res, LF);
      Append (Res, "}");
      return Res;
   end Function_Declaration;

   function Procedure_Declaration
     (Fun_Name : String;
      Params   : Param_Array;
      Stmts    : String := "") return Unbounded_String
   is (Function_Declaration ("void", Fun_Name, Params, "", Stmts));

   function Variable_Declaration
     (Var_Type : String;
      Var_Name : String;
      Value    : String;
      Const    : Boolean := False) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      if Const then
         Append (Res, "const ");
      end if;
      Append (Res, Var_Type);
      Append (Res, " ");
      Append (Res, Var_Name);
      Append (Res, " = ");
      Append (Res, Value);
      Append (Res, ";");
      return Res;
   end Variable_Declaration;

   function For_Loop
     (Var_Name  : String;
      Bound     : String;
      Loop_Body : String) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      Append (Res, "for (");
      Append (Res, Variable_Declaration ("int", Var_Name, "0"));
      Append (Res, Var_Name);
      Append (Res, " < ");
      Append (Res, Bound);
      Append (Res, "; ++");
      Append (Res, Var_Name);
      Append (Res, ") {");
      Append (Res, LF);
      Append (Res, Loop_Body);
      Append (Res, LF);
      Append (Res, "}");
      return Res;
   end For_Loop;

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

   function Scene_Description
     (Prims_Count : Primitive_Count_Array) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      Append (Res, "layout (std140, binding=1) uniform scene_description {");
      Append (Res, LF);

      for Prim_Count of Prims_Count loop
         Append (Res, "int ");
         Append (Res, Prim_Count_Reference (Prim_Count.Prim));
         Append (Res, ";");
         Append (Res, LF);

         Append (Res, Primitives.Get_Name (Prim_Count.Prim));
         Append (Res, " ");
         Append (Res, Prim_Array_Reference (Prim_Count.Prim));
         Append (Res, "[");
         Append (Res, Prim_Count.Count'Image);
         Append (Res, "];");
         Append (Res, LF);
      end loop;

      Append (Res, "};");

      return Res;
   end Scene_Description;

   function Closest_Primitive
     (Prims : Primitives.Primitive_Array_Access) return Unbounded_String
   is
      Stmts : Unbounded_String;
   begin
      Append (Stmts, Variable_Declaration ("float", "closest", "max_dist"));
      Append (Stmts, LF);

      for Prim of Prims.all loop
         declare
            Loop_Body : Unbounded_String;
         begin
            Append (Loop_Body, "closest = min(closest, ");
            Append (Loop_Body, Dist_Function_Reference (Prim));
            Append (Loop_Body, "(x, ");
            Append (Loop_Body, Prim_Array_Reference (Prim));
            Append (Loop_Body, "[i])");
            Append (Loop_Body, ");");
            Append (Stmts, For_Loop
              ("i", Prim_Count_Reference (Prim), To_String (Loop_Body)));
            Append (Stmts, LF);
         end;
      end loop;

      return Function_Declaration
        ("float", "closest_primitive",
         (1 => Create ("vec3", "x")),
         "closest",
         To_String (Stmts));
   end Closest_Primitive;

   function Closest_Primitive_Info
     (Prims_Count : Primitive_Count_Array) return Unbounded_String
   is
      Stmts : Unbounded_String;
      Total : Natural := 0;
   begin
      Append (Stmts, Variable_Declaration ("float", "closest", "max_dist"));
      Append (Stmts, LF);

      for Prim_Count of Prims_Count loop
         declare
            Prim : Primitives.Primitive renames Prim_Count.Prim;
            Loop_Body : Unbounded_String;
            Variable_Expr : Unbounded_String;
         begin
            Append (Variable_Expr, Dist_Function_Reference (Prim));
            Append (Variable_Expr, "(x, ");
            Append (Variable_Expr, Prim_Array_Reference (Prim));
            Append (Variable_Expr, "[i])");
            Append (Variable_Expr, ")");

            Append (Loop_Body, Variable_Declaration
              ("float", "dist", To_String (Variable_Expr)));
            Append (Loop_Body, LF);
            Append (Loop_Body, "if (dist < closest) {");
            Append (Loop_Body, LF);
            Append (Loop_Body, "closest = dist;");
            Append (Loop_Body, LF);
            Append (Loop_Body, "index = ");
            Append (Loop_Body, Total'Image);
            Append (Loop_Body, " + i;");
            Append (Loop_Body, LF);
            Append (Loop_Body, "}");

            Append (Stmts, For_Loop
              ("i", Prim_Count_Reference (Prim), To_String (Loop_Body)));
            Append (Stmts, LF);
         end;
         Total := Total + Prim_Count.Count;
      end loop;

      return Function_Declaration
        ("float", "closest_primitive_info",
         (1 => Create ("vec3", "x"), 2 => Create ("int", "index", "out")),
         "closest",
         To_String (Stmts));
   end Closest_Primitive_Info;

   function Primitive_Info
     (Prims_Count : Primitive_Count_Array) return Unbounded_String
   is
      Stmts : Unbounded_String;
   begin
      for Prim_Count of Prims_Count loop
         Append (Stmts, "if (index < ");
         Append (Stmts, Prim_Count.Count'Image);
         Append (Stmts, ") {");
         Append (Stmts, LF);

         Append (Stmts, "normal = ");
         Append (Stmts, Normal_Function_Reference (Prim_Count.Prim));
         Append (Stmts, "(pos, ");
         Append (Stmts, Prim_Array_Reference (Prim_Count.Prim));
         Append (Stmts, "[index]);");
         Append (Stmts, LF);

         Append (Stmts, "material_id = ");
         Append (Stmts, Prim_Array_Reference (Prim_Count.Prim));
         Append (Stmts, "[index].material_id;");
         Append (Stmts, LF);

         Append (Stmts, "return;");
         Append (Stmts, LF);

         Append (Stmts, "}");
         Append (Stmts, LF);

         Append (Stmts, "index -= ");
         Append (Stmts, Prim_Count.Count'Image);
         Append (Stmts, ";");
         Append (Stmts, LF);
      end loop;
      return Procedure_Declaration
        ("primitive_info",
         (1 => Create ("int", "index"),
          2 => Create ("vec3", "pos"),
          3 => Create ("vec3", "normal", "out"),
          4 => Create ("int", "material_id", "out")),
         To_String (Stmts));
   end Primitive_Info;

   function Generate_Code
     (Prims_Count : Primitive_Count_Array;
      Prims       : Primitives.Primitive_Array_Access;
      Max_Dist    : GL.Types.Single) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      Append (Res, Variable_Declaration
        ("float", "max_dist", Max_Dist'Image, Const => True));
      Append (Res, DLF);

      for Prim of Prims.all loop
         Append (Res, Primitive_Struct_Declaration (Prim));
         Append (Res, DLF);
         Append (Res, Primitive_Dist_Function (Prim));
         Append (Res, DLF);
         Append (Res, Primitive_Normal_Function (Prim));
         Append (Res, DLF);
      end loop;

      Append (Res, Scene_Description (Prims_Count));
      Append (Res, DLF);

      Append (Res, Closest_Primitive (Prims));
      Append (Res, DLF);

      Append (Res, Closest_Primitive_Info (Prims_Count));
      Append (Res, DLF);

      Append (Res, Primitive_Info (Prims_Count));

      return Res;
   end Generate_Code;

   function Compile
     (Prims_Count : Primitive_Count_Array;
      Max_Dist    : GL.Types.Single := 20.0) return Scene
   is
      Prims : Primitives.Primitive_Array_Access :=
         new Primitives.Primitive_Array'(1 .. Prims_Count'Length => <>);
   begin
      for I in 1 .. Prims'Length loop
         Prims (I) := Prims_Count (I).Prim;
      end loop;
      return new Scene_Internal'
        (Prims => Prims,
         GLSL  => Generate_Code (Prims_Count, Prims, Max_Dist));
   end Compile;

   procedure Print_GLSL (S : Scene) is
   begin
      Put_Line (To_String (S.GLSL));
   end Print_GLSL;
end Madarch.Scenes;
