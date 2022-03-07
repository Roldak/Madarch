with GL.Objects.Shaders;
with GL.Objects.Programs;
with GL.Types;

package Compute_Shaders is
   use GL.Types;

   type Compute_Shader is tagged private;

   function Create
     (Shader : GL.Objects.Shaders.Shader) return Compute_Shader;

   procedure Dispatch (Self : Compute_Shader; X, Y, Z : UInt);
private
   type Compute_Shader is tagged record
      Program : GL.Objects.Programs.Program;
   end record;
end Compute_Shaders;
