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

   function Create_File_Substitution
     (File_Name : String; File_Content : String) return File_Substitution
   is
      XName, XContent : XString;
   begin
      XName.Set (File_Name);
      XContent.Set (File_Content);
      return (File_Name => XName, File_Content => XContent);
   end Create_File_Substitution;

   procedure Load_Shader
     (Shader       : in out GL.Objects.Shaders.Shader;
      Source_File  : String;
      Macros       : Macro_Definition_Array := No_Macro_Definition_Array;
      File_Substs  : File_Substitution_Array := No_File_Substitution_Array;
      GLSL_Version : String)
   is
      use GNATCOLL;

      function Get_Source_From_File_Name
        (Source_Dir : VFS.Virtual_File;
         File_Name  : Strings.XString) return Strings.XString;

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
            begin
               Content.Replace_Slice
                 (Next_Include,
                  Included_File_End,
                  Get_Source_From_File_Name (Source_Dir, File_Name));

               Resolve_Include_Directives
                 (Source_Dir, Content, Included_File_End);
            end;
         end if;
      end Resolve_Include_Directives;

      function Prepare_Content
        (Source_Dir : VFS.Virtual_File;
         Content    : Strings.XString) return Strings.XString
      is
         Prepared : Strings.XString := Content;
      begin
         Resolve_Include_Directives (Source_Dir, Prepared);
         return Prepared;
      end Prepare_Content;

      function Load_Source (Path : VFS.Virtual_File) return Strings.XString is
        (Prepare_Content (Path.Dir, VFS.Read_File (Path)));

      function Get_Source_From_File_Name
        (Source_Dir : VFS.Virtual_File;
         File_Name  : Strings.XString) return Strings.XString
      is
      begin
         for Subst of File_Substs loop
            if Subst.File_Name = File_Name then
               return Prepare_Content (Source_Dir, Subst.File_Content);
            end if;
         end loop;

         return Load_Source
           (VFS.Create_From_Dir
              (Source_Dir, VFS.Filesystem_String (File_Name.To_String)));
      end Get_Source_From_File_Name;

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
