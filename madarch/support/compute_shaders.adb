with GL.Compute;
with GL.Memory;

package body Compute_Shaders is
   function Create
     (Shader : GL.Objects.Shaders.Shader) return Compute_Shader
   is
      Program : GL.Objects.Programs.Program;
   begin
      Program.Initialize_Id;
      Program.Attach (Shader);
      Program.Link;
      return (Program => Program);
   end Create;

   procedure Dispatch (Self : Compute_Shader; X, Y, Z : UInt) is
   begin
      Self.Program.Use_Program;
      GL.Compute.Dispatch_Compute (X, Y, Z);
      GL.Memory.Barrier ((Shader_Storage => True, others => False));
   end Dispatch;
end Compute_Shaders;
