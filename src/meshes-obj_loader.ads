with GNATCOLL.VFS;
with GNATCOLL.Strings;

package Meshes.Obj_Loader is
   use GNATCOLL;

   function Load_Obj_File (File_Path : String) return Mesh;

   function Load_Obj_File (File_Path : VFS.Virtual_File) return Mesh;

   function Load_Obj (Content : Strings.XString) return Mesh;
end Meshes.Obj_Loader;
