with Ada.Text_IO;

with GL.Objects.Programs;
with GL.Objects.Shaders.Lists;

with GNATCOLL.VFS;
with GNATCOLL.Strings;

procedure Load_Shader
  (Shader : in out GL.Objects.Shaders.Shader;
   Source_File : String)
is
   use GNATCOLL;

   function Load_Source (Path : VFS.Virtual_File) return Strings.XString;

   procedure Resolve_Include_Directives
     (Source_Dir : VFS.Virtual_File;
      Content    : in out Strings.XString;
      From       : Natural := 1)
   is
      Next_Include        : Natural := Content.Find ("#include", From);
      Included_File_Start : Natural := Content.Find('"', Next_Include + 1);
      Included_File_End   : Natural := Content.Find('"', Included_File_Start + 1);
   begin
      if Next_Include /= 0 then
         declare
            File_Name : Strings.XString := Content.Slice
               (Included_File_Start + 1, Included_File_End - 1);

            File_Path : VFS.Virtual_File := VFS.Create_From_Dir
              (Source_Dir, VFS.Filesystem_String (File_Name.To_String));
         begin
            Content.Replace_Slice
              (Next_Include,
               Included_File_End,
               Load_Source (File_Path));

            Resolve_Include_Directives
              (Source_Dir, Content, Included_File_End);
         end;
      end if;
   end Resolve_Include_Directives;

   function Load_Source (Path : VFS.Virtual_File) return Strings.XString is
      Content : Strings.XString := VFS.Read_File (Path);
   begin
      Resolve_Include_Directives (Path.Dir, Content);
      return Content;
   end Load_Source;

   Source_Path : VFS.Virtual_File :=
      VFS.Create (VFS.Filesystem_String (Source_File));
begin

   Shader.Initialize_Id;
   GL.Objects.Shaders.Set_Source
     (Shader, Load_Source (Source_Path).To_String);
   Shader.Compile;

   if not Shader.Compile_Status then
      Ada.Text_IO.Put_Line ("Compilation of " & Source_File & " failed. log:");
      Ada.Text_IO.Put_Line (Shader.Info_Log);
   end if;
end Load_Shader;
