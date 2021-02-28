with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

with Madarch.Components;
with Madarch.Exprs;
with Madarch.Values;

with GPU_Types.Base;
with GPU_Types.Fixed_Arrays;
with GPU_Types.Structs;

package body Madarch.Scenes is
   LF  : Character renames Ada.Characters.Latin_1.LF;
   DLF : constant String := LF & LF;

   function Vector3_String (V : Singles.Vector3) return String is
     ("vec3(" & V (GL.X)'Image &
      ", "    & V (GL.Y)'Image &
      ", "    & V (GL.Z)'Image & ")");

   function Vector3_String (V : Ints.Vector3) return String is
     ("vec3(" & V (GL.X)'Image &
      ", "    & V (GL.Y)'Image &
      ", "    & V (GL.Z)'Image & ")");

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

   function Light_Sample_Reference
     (Lit : Lights.Light) return String
   is ("sample_" & Lights.Get_Name (Lit));

   function Light_Count_Reference
     (Lit : Lights.Light) return String
   is ("light_" & Lights.Get_Name (Lit) & "_count");

   function Light_Array_Reference
     (Lit : Lights.Light) return String
   is ("light_" & Lights.Get_Name (Lit) & "s");

   function Component_Declaration
     (Comp : Components.Component) return String
   is
   begin
      return Values.To_GLSL (Components.Get_Kind (Comp))
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
      Append (Res, "};");
      return Res;
   end Primitive_Struct_Declaration;

   function Light_Struct_Declaration
     (Lit : Lights.Light) return Unbounded_String
   is
      Res  : Unbounded_String;
   begin
      Append (Res, "struct " & Lights.Get_Name (Lit));
      Append (Res, " {");
      Append (Res, LF);

      for Comp of Lights.Get_Components (Lit) loop
         Append (Res, Component_Declaration (Comp));
         Append (Res, ";");
         Append (Res, LF);
      end loop;

      Append (Res, "};");
      return Res;
   end Light_Struct_Declaration;

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
      Append (Res, LF);
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
         Dist_Expr.To_GLSL,
         Dist_Expr.Pre_GLSL);
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
         Dist_Expr.To_GLSL,
         Dist_Expr.Pre_GLSL);
   end Primitive_Normal_Function;

   function Light_Sample_Function
     (Lit : Lights.Light) return Unbounded_String
   is
      Light_Param_Name  : String := "l";
      Pos_Param_Name    : String := "pos";
      Normal_Param_Name : String := "normal";

      Light_Param_Expr  : Exprs.Struct_Expr :=
         Exprs.Struct_Identifier (Light_Param_Name);

      Pos_Param_Expr : Exprs.Expr :=
         Exprs.Value_Identifier (Pos_Param_Name);

      Normal_Param_Expr : Exprs.Expr :=
         Exprs.Value_Identifier (Normal_Param_Name);

      Dir_Param_Expr  : Exprs.Expr := Exprs.Value_Identifier ("dir");
      Dist_Param_Expr : Exprs.Expr := Exprs.Value_Identifier ("dist");

      Sample_Expr : Exprs.Expr'Class := Lights.Get_Sample_Expr
        (Lit, Light_Param_Expr,
         Pos_Param_Expr, Normal_Param_Expr,
         Dir_Param_Expr, Dist_Param_Expr);

      Position_Expr : Exprs.Expr'Class :=
         Lights.Get_Position_Expr (Lit, Light_Param_Expr);

      Stmts : Unbounded_String;
   begin
      Append (Stmts, "dir = ");
      Append (Stmts, Position_Expr.Pre_GLSL);
      Append (Stmts, Position_Expr.To_GLSL);
      Append (Stmts, " - pos;");
      Append (Stmts, LF);
      Append (Stmts, "dist = length(dir);");
      Append (Stmts, LF);
      Append (Stmts, "dir /= dist;");
      Append (Stmts, LF);

      return Function_Declaration
        ("vec3",
         Light_Sample_Reference (Lit),
         (1 => Create (Lights.Get_Name (Lit), Light_Param_Name),
          2 => Create ("vec3", Pos_Param_Name),
          3 => Create ("vec3", Normal_Param_Name),
          4 => Create ("vec3", "dir", "out"),
          5 => Create ("float", "dist", "out")),
         Sample_Expr.To_GLSL,
         To_String (Stmts) & Sample_Expr.Pre_GLSL);
   end Light_Sample_Function;

   function Scene_Description
     (Prims_Count  : Primitive_Count_Array;
      Lights_Count : Light_Count_Array) return Unbounded_String
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

      Append (Res, LF);

      for Light_Count of Lights_Count loop
         Append (Res, "int ");
         Append (Res, Light_Count_Reference (Light_Count.Light));
         Append (Res, ";");
         Append (Res, LF);

         Append (Res, Lights.Get_Name (Light_Count.Light));
         Append (Res, " ");
         Append (Res, Light_Array_Reference (Light_Count.Light));
         Append (Res, "[");
         Append (Res, Light_Count.Count'Image);
         Append (Res, "];");
         Append (Res, LF);
      end loop;

      Append (Res, LF);

      Append (Res, "int total_light_count;");
      Append (Res, LF);

      Append (Res, "};");

      return Res;
   end Scene_Description;

   function Closest_Primitive
     (Prims : Primitives.Primitive_Array_Access) return Unbounded_String
   is
      Stmts : Unbounded_String;
   begin
      Append (Stmts, Variable_Declaration ("float", "closest", "max_dist"));

      for Prim of Prims.all loop
         declare
            Loop_Body : Unbounded_String;
         begin
            Append (Loop_Body, "closest = min(closest, ");
            Append (Loop_Body, Dist_Function_Reference (Prim));
            Append (Loop_Body, "(");
            Append (Loop_Body, Prim_Array_Reference (Prim));
            Append (Loop_Body, "[i], x));");
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

      for Prim_Count of Prims_Count loop
         declare
            Prim : Primitives.Primitive renames Prim_Count.Prim;
            Loop_Body : Unbounded_String;
            Variable_Expr : Unbounded_String;
         begin
            Append (Variable_Expr, Dist_Function_Reference (Prim));
            Append (Variable_Expr, "(");
            Append (Variable_Expr, Prim_Array_Reference (Prim));
            Append (Variable_Expr, "[i], x);");

            Append (Loop_Body, Variable_Declaration
              ("float", "dist", To_String (Variable_Expr)));
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
      function Get_Material
        (Prim : Primitives.Primitive) return String
      is
         Name : String := Prim_Array_Reference (Prim) & "[index]";
         Inst : Exprs.Struct_Expr := Exprs.Struct_Identifier (Name);
      begin
         return Primitives.Get_Material_Expr (Prim, Inst).To_GLSL;
      end Get_Material;

      Stmts : Unbounded_String;
   begin
      for Prim_Count of Prims_Count loop
         Append (Stmts, "if (index < ");
         Append (Stmts, Prim_Count.Count'Image);
         Append (Stmts, ") {");
         Append (Stmts, LF);

         Append (Stmts, "normal = ");
         Append (Stmts, Normal_Function_Reference (Prim_Count.Prim));
         Append (Stmts, "(");
         Append (Stmts, Prim_Array_Reference (Prim_Count.Prim));
         Append (Stmts, "[index], pos);");
         Append (Stmts, LF);

         Append (Stmts, "material_id = ");
         Append (Stmts, Get_Material (Prim_Count.Prim));
         Append (Stmts, ";");
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

   function Sample_Light
     (Lits : Lights.Light_Array_Access) return Unbounded_String
   is
      Stmts : Unbounded_String;
   begin
      for Lit of Lits.all loop
         Append (Stmts, "if (index < ");
         Append (Stmts, Light_Count_Reference (Lit));
         Append (Stmts, ") {");
         Append (Stmts, LF);
         Append (Stmts, "return ");
         Append (Stmts, Light_Sample_Reference (Lit));
         Append (Stmts, "(");
         Append (Stmts, Light_Array_Reference (Lit));
         Append (Stmts, "[index], pos, normal, dir, dist);");
         Append (Stmts, LF);
         Append (Stmts, "}");
         Append (Stmts, LF);
         Append (Stmts, "index -= ");
         Append (Stmts, Light_Count_Reference (Lit));
         Append (Stmts, ";");
         Append (Stmts, LF);
      end loop;

      return Function_Declaration
        ("vec3", "sample_light",
         (1 => Create ("int", "index"),
          2 => Create ("vec3", "pos"),
          3 => Create ("vec3", "normal"),
          4 => Create ("vec3", "dir", "out"),
          5 => Create ("float", "dist", "out")),
         "vec3(0)",
         To_String (Stmts));
   end Sample_Light;

   function Partitioning_Info_Struct_Declaration
     (Partitioning : Partitioning_Settings;
      Prims        : Primitives.Primitive_Array_Access) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      Append (Res, "struct partition_info {");
      Append (Res, LF);
      for Prim of Prims.all loop
         Append (Res, "int ");
         Append (Res, Prim_Count_Reference (Prim));
         Append (Res, ";");
         Append (Res, LF);

         Append (Res, "int ");
         Append (Res, Prim_Array_Reference (Prim));
         Append (Res, "[");
         Append (Res, Partitioning.Index_Count'Image);
         Append (Res, "];");
         Append (Res, DLF);
      end loop;
      Append (Res, "};");
      return Res;
   end Partitioning_Info_Struct_Declaration;

   function Partitioning_Data_Buffer return Unbounded_String is
      Res : Unbounded_String;
   begin
      Append (Res, "layout (std140, binding=0) buffer partition_data_buffer {");
      Append (Res, LF);
      Append (Res, "partition_info partition_data[];");
      Append (Res, LF);
      Append (Res, "};");
      return Res;
   end Partitioning_Data_Buffer;

   function Partitioning_Index
     (Partitioning   : Partitioning_Settings;
      Fallback_Stmts : String) return Unbounded_String
   is
      Res : Unbounded_String;

      Dims : Ints.Vector3 := Partitioning.Grid_Dimensions;
   begin
      Append (Res, "vec3 fx = floor((x - ");
      Append (Res, Vector3_String (Partitioning.Grid_Offset));
      Append (Res, ") / ");
      Append (Res, Vector3_String (Partitioning.Grid_Spacing));
      Append (Res, ");");
      Append (Res, LF);

      Append (Res, "vec3 cfx = clamp(fx, vec3(0), ");
      Append (Res, Vector3_String (Partitioning.Grid_Dimensions));
      Append (Res, ");");
      Append (Res, LF);

      if Partitioning.Border_Behavior = Clamp then
         Append (Res, "fx = cfx;");
      else
         Append (Res, "if (fx != cfx) {");
         Append (Res, LF);
         Append (Res, Fallback_Stmts);
         Append (Res, "}");
      end if;

      Append (Res, LF);

      Append (Res, "int data_index = int(fx.x * ");
      Append (Res, Int'Image (Dims (GL.Y) * Dims (GL.Z)));
      Append (Res, " + fx.y * ");
      Append (Res, Int'Image (Dims (GL.Z)));
      Append (Res, " + fx.z);");
      Append (Res, LF);
      return Res;
   end Partitioning_Index;

   function Partitioning_Closest_Primitive
     (Partitioning : Partitioning_Settings;
      Prims        : Primitives.Primitive_Array_Access) return Unbounded_String
   is
      Stmts : Unbounded_String;

      Partition_Info : String := "partition_data[data_index]";
   begin
      Append (Stmts, Partitioning_Index
        (Partitioning, "return closest_primitive (x);"));
      Append (Stmts, Variable_Declaration ("float", "closest", "max_dist"));

      for Prim of Prims.all loop
         declare
            Count_Name : String := Primitives.Get_Name (Prim) & "_count";
            Count_Var  : Unbounded_String := Variable_Declaration
              ("int",
               Count_Name,
               Partition_Info & "." & Prim_Count_Reference (Prim));

            Index_Array : String :=
               Partition_Info & "." & Prim_Array_Reference (Prim);

            Loop_Body : Unbounded_String;
         begin
            Append (Stmts, Count_Var);

            Append (Loop_Body, "closest = min(closest, ");
            Append (Loop_Body, Dist_Function_Reference (Prim));
            Append (Loop_Body, "(");
            Append (Loop_Body, Prim_Array_Reference (Prim));
            Append (Loop_Body, "[");
            Append (Loop_Body, Index_Array);
            Append (Loop_Body, "[i]], x));");
            Append (Stmts, For_Loop
              ("i", Count_Name, To_String (Loop_Body)));
            Append (Stmts, LF);
         end;
      end loop;

      return Function_Declaration
        ("float", "partitioning_closest",
         (1 => Create ("vec3", "x")),
         "closest",
         To_String (Stmts));
   end Partitioning_Closest_Primitive;

   function Partitioning_Closest_Primitive_Info
     (Partitioning : Partitioning_Settings;
      Prims_Count  : Primitive_Count_Array) return Unbounded_String
   is
      Stmts : Unbounded_String;
      Total : Natural := 0;

      Partition_Info : String := "partition_data[data_index]";
   begin
      Append (Stmts, Partitioning_Index
        (Partitioning, "return closest_primitive_info (x, index);"));
      Append (Stmts, Variable_Declaration ("float", "closest", "max_dist"));

      for Prim_Count of Prims_Count loop
         declare
            Prim : Primitives.Primitive renames Prim_Count.Prim;

            Count_Name : String := Primitives.Get_Name (Prim) & "_count";
            Count_Var  : Unbounded_String := Variable_Declaration
              ("int",
               Count_Name,
               Partition_Info & "." & Prim_Count_Reference (Prim));

            Index_Array : String :=
               Partition_Info & "." & Prim_Array_Reference (Prim);
            Loop_Body : Unbounded_String;

            Variable_Expr : Unbounded_String;
         begin
            Append (Stmts, Count_Var);

            Append (Variable_Expr, Dist_Function_Reference (Prim));
            Append (Variable_Expr, "(");
            Append (Variable_Expr, Prim_Array_Reference (Prim));
            Append (Variable_Expr, "[prim_index], x)");

            Append (Loop_Body, Variable_Declaration
              ("int", "prim_index", Index_Array & "[i]"));
            Append (Loop_Body, Variable_Declaration
              ("float", "dist", To_String (Variable_Expr)));
            Append (Loop_Body, "if (dist < closest) {");
            Append (Loop_Body, LF);
            Append (Loop_Body, "closest = dist;");
            Append (Loop_Body, LF);
            Append (Loop_Body, "index = ");
            Append (Loop_Body, Total'Image);
            Append (Loop_Body, " + prim_index;");
            Append (Loop_Body, LF);
            Append (Loop_Body, "}");

            Append (Stmts, For_Loop
              ("i", Count_Name, To_String (Loop_Body)));
            Append (Stmts, LF);
         end;
         Total := Total + Prim_Count.Count;
      end loop;

      return Function_Declaration
        ("float", "partitioning_closest_info",
         (1 => Create ("vec3", "x"), 2 => Create ("int", "index", "out")),
         "closest",
         To_String (Stmts));
   end Partitioning_Closest_Primitive_Info;

   function Generate_Code
     (Prims_Count  : Primitive_Count_Array;
      Lights_Count : Light_Count_Array;
      Prims        : Primitives.Primitive_Array_Access;
      Lits         : Lights.Light_Array_Access;
      Max_Dist     : GL.Types.Single;
      Partitioning : Partitioning_Settings) return Unbounded_String
   is
      Res : Unbounded_String;
   begin
      Append (Res, Variable_Declaration
        ("float", "max_dist", Max_Dist'Image, Const => True));
      Append (Res, LF);

      for Prim of Prims.all loop
         Append (Res, Primitive_Struct_Declaration (Prim));
         Append (Res, DLF);
         Append (Res, Primitive_Dist_Function (Prim));
         Append (Res, DLF);
         Append (Res, Primitive_Normal_Function (Prim));
         Append (Res, DLF);
      end loop;

      for Lit of Lits.all loop
         Append (Res, Light_Struct_Declaration (Lit));
         Append (Res, DLF);
         Append (Res, Light_Sample_Function (Lit));
         Append (Res, DLF);
      end loop;

      Append (Res, Scene_Description (Prims_Count, Lights_Count));
      Append (Res, DLF);

      Append (Res, Closest_Primitive (Prims));
      Append (Res, DLF);

      Append (Res, Closest_Primitive_Info (Prims_Count));
      Append (Res, DLF);

      Append (Res, Primitive_Info (Prims_Count));
      Append (Res, DLF);

      Append (Res, Sample_Light (Lits));
      Append (Res, DLF);

      Append (Res, Partitioning_Info_Struct_Declaration (Partitioning, Prims));
      Append (Res, DLF);

      Append (Res, Partitioning_Data_Buffer);
      Append (Res, DLF);

      Append (Res, Partitioning_Closest_Primitive (Partitioning, Prims));
      Append (Res, DLF);

      Append (Res, Partitioning_Closest_Primitive_Info
        (Partitioning, Prims_Count));
      Append (Res, DLF);

      return Res;
   end Generate_Code;

   function Compute_Scene_GPU_Type
     (Prims_Count  : Primitive_Count_Array;
      Lights_Count : Light_Count_Array) return GPU_Types.GPU_Type
   is
      function Compute_Prim_Struct_Type
        (Prim : Primitives.Primitive) return GPU_Types.GPU_Type
      is
         Prim_Comps : Components.Component_Array :=
            Primitives.Get_Components (Prim);

         Type_Comps : GPU_Types.Named_Component_Array :=
           (1 .. Prim_Comps'Length => <>);
      begin
         for I in Prim_Comps'Range loop
            Type_Comps (I) :=
               Values.GPU_Type (Components.Get_Kind (Prim_Comps (I))).Named
                 (Components.Get_Name (Prim_Comps (I)));
         end loop;

         return GPU_Types.Structs.Create (Type_Comps);
      end Compute_Prim_Struct_Type;

      function Compute_Light_Struct_Type
        (Light : Lights.Light) return GPU_Types.GPU_Type
      is
         Light_Comps : Components.Component_Array :=
            Lights.Get_Components (Light);

         Type_Comps : GPU_Types.Named_Component_Array :=
           (1 .. Light_Comps'Length => <>);
      begin
         for I in Light_Comps'Range loop
            Type_Comps (I) :=
               Values.GPU_Type (Components.Get_Kind (Light_Comps (I))).Named
                 (Components.Get_Name (Light_Comps (I)));
         end loop;

         return GPU_Types.Structs.Create (Type_Comps);
      end Compute_Light_Struct_Type;

      Component_Count : Positive :=
        Prims_Count'Length * 2 + Lights_Count'Length * 2 + 1;

      Comps : GPU_Types.Named_Component_Array :=
        (1 .. Component_Count => <>);

      I : Natural := 1;

      procedure Add (Comp : GPU_Types.Named_Component) is
      begin
         Comps (I) := Comp;
         I := I + 1;
      end Add;
   begin
      for Prim_Count of Prims_Count loop
         Add (GPU_Types.Base.Int.Named
           (Prim_Count_Reference (Prim_Count.Prim)));

         Add (GPU_Types.Fixed_Arrays.Create
           (GL.Types.Int (Prim_Count.Count),
            Compute_Prim_Struct_Type (Prim_Count.Prim)).Named
              (Prim_Array_Reference (Prim_Count.Prim)));
      end loop;

      for Light_Count of Lights_Count loop
         Add (GPU_Types.Base.Int.Named
           (Light_Count_Reference (Light_Count.Light)));

         Add (GPU_Types.Fixed_Arrays.Create
           (GL.Types.Int (Light_Count.Count),
            Compute_Light_Struct_Type (Light_Count.Light)).Named
              (Light_Array_Reference (Light_Count.Light)));
      end loop;

      Add (GPU_Types.Base.Int.Named ("total_light_count"));

      return GPU_Types.Structs.Create (Comps);
   end Compute_Scene_GPU_Type;

   function Compute_Partitioning_GPU_Type
     (Partitioning : Partitioning_Settings;
      Prims        : Primitives.Primitive_Array_Access)
      return GPU_Types.GPU_Type
   is
      Index_Array_Type : GPU_Types.GPU_Type :=
         GPU_Types.Fixed_Arrays.Create
           (Int (Partitioning.Index_Count), GPU_Types.Base.Int);

      Comps : GPU_Types.Named_Component_Array :=
        (1 .. 2 * Prims'Length => <>);

      I : Natural := 1;

      procedure Add (Comp : GPU_Types.Named_Component) is
      begin
         Comps (I) := Comp;
         I := I + 1;
      end Add;

      Dims : Ints.Vector3 := Partitioning.Grid_Dimensions;
   begin
      for Prim of Prims.all loop
         Add (GPU_Types.Base.Int.Named (Primitives.Get_Name (Prim) & "_count"));
         Add (Index_Array_Type.Named (Primitives.Get_Name (Prim) & "_indices"));
      end loop;
      return GPU_Types.Fixed_Arrays.Create
        (Dims (GL.X) * Dims (GL.Y) * Dims (GL.Z),
         GPU_Types.Structs.Create (Comps));
   end Compute_Partitioning_GPU_Type;

   function Compile
     (All_Primitives : Primitive_Count_Array;
      All_Lights     : Light_Count_Array;
      Partitioning   : Partitioning_Settings := Default_Partitioning_Settings;
      Max_Dist       : GL.Types.Single := 20.0) return Scene
   is
      Prims : Primitives.Primitive_Array_Access :=
         new Primitives.Primitive_Array'(1 .. All_Primitives'Length => <>);

      Lits  : Lights.Light_Array_Access :=
         new Lights.Light_Array'(1 .. All_Lights'Length => <>);
   begin
      for I in 1 .. Prims'Length loop
         Prims (I) := All_Primitives (I).Prim;
      end loop;

      for I in 1 .. Lits'Length loop
         Lits (I) := All_Lights (I).Light;
      end loop;

      return new Scene_Internal'
        (Prims    => Prims,
         Lits     => Lits,
         GLSL     => Generate_Code
           (All_Primitives, All_Lights, Prims, Lits, Max_Dist, Partitioning),
         GPU_Type => Compute_Scene_GPU_Type (All_Primitives, All_Lights),

         Partitioning_Config => Partitioning,
         Partitioning_GPU_Type =>
            Compute_Partitioning_GPU_Type (Partitioning, Prims));
   end Compile;

   function Get_GLSL (S : Scene) return String is
     (To_String (S.GLSL));

   function Get_GPU_Type (S : Scene) return GPU_Types.GPU_Type is
     (S.GPU_Type);

   function Get_Partitioning_Settings
     (S : Scene) return Partitioning_Settings is (S.Partitioning_Config);

   function Get_Partitioning_GPU_Type (S : Scene) return GPU_Types.GPU_Type is
     (S.Partitioning_GPU_Type);

   procedure Get_Primitives_Location
     (S    : Scene;
      Prim : Primitives.Primitive;
      Array_Location : out GPU_Types.Locations.Location;
      Count_Location : out GPU_Types.Locations.Location)
   is
   begin
      Array_Location :=
         S.GPU_Type.Address.Component (Prim_Array_Reference (Prim));
      Count_Location :=
         S.GPU_Type.Address.Component (Prim_Count_Reference (Prim));
   end Get_Primitives_Location;

   procedure Get_Lights_Location
     (S    : Scene;
      Lit  : Lights.Light;
      Array_Location : out GPU_Types.Locations.Location;
      Count_Location : out GPU_Types.Locations.Location;
      Total_Location : out GPU_Types.Locations.Location)
   is
   begin
      Array_Location :=
         S.GPU_Type.Address.Component (Light_Array_Reference (Lit));
      Count_Location :=
         S.GPU_Type.Address.Component (Light_Count_Reference (Lit));
      Total_Location :=
         S.GPU_Type.Address.Component ("total_light_count");
   end Get_Lights_Location;
end Madarch.Scenes;
