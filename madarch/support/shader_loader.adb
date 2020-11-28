with Ada.Characters.Latin_1;
with Ada.Text_IO;

with GNATCOLL.VFS;

package body Shader_Loader is
   function Create_Macro_Definition
     (Name, Value : String) return Macro_Definition
   is
      XName, XValue : XString;
   begin
      XName.Set (Name);
      XValue.Set (Value);
      return (Name => XName, Value => XValue);
   end Create_Macro_Definition;

   procedure Load_Shader
     (Shader       : in out GL.Objects.Shaders.Shader;
      Source_File  : String;
      Macros       : Macro_Definition_Array := No_Macro_Definition_Array;
      GLSL_Version : String)
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

      function Generate_Macro_Definitions return String is
         Res : Strings.XString;
      begin
         Res.Append ("#version " & GLSL_Version & Ada.Characters.Latin_1.LF);

         for M of Macros loop
            Res.Append ("#define ");
            Res.Append (M.Name);
            Res.Append (" ");
            Res.Append (M.Value);
            Res.Append (Ada.Characters.Latin_1.LF);
         end loop;

         return Res.To_String;
      end Generate_Macro_Definitions;

      Source_Path : VFS.Virtual_File :=
         VFS.Create (VFS.Filesystem_String (Source_File));
   begin

      Shader.Initialize_Id;
      GL.Objects.Shaders.Set_Source
        (Shader,
         Generate_Macro_Definitions & Load_Source (Source_Path).To_String);
      Shader.Compile;

      if not Shader.Compile_Status then
         Ada.Text_IO.Put_Line ("Compilation of " & Source_File & " failed. log:");
         Ada.Text_IO.Put_Line (Shader.Info_Log);
      end if;
   end Load_Shader;
end Shader_Loader;
