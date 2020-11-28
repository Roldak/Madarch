with GNATCOLL.Strings;

with GL.Objects.Programs;
with GL.Objects.Shaders.Lists;

package Shader_Loader is
   use GNATCOLL.Strings;

   type Macro_Definition is record
      Name  : XString;
      Value : XString;
   end record;

   type Macro_Definition_Array is
      array (Natural range <>) of Macro_Definition;

   No_Macro_Definition_Array : constant Macro_Definition_Array :=
      (1 .. 0 => <>);

   function Create_Macro_Definition
     (Name, Value : String) return Macro_Definition;

   procedure Load_Shader
     (Shader       : in out GL.Objects.Shaders.Shader;
      Source_File  : String;
      Macros       : Macro_Definition_Array := No_Macro_Definition_Array;
      GLSL_Version : String);
end Shader_Loader;
