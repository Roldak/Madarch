with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Meshes.Obj_Loader is
   function Load_Obj_File (File_Path : String) return Mesh is
      VFS_Path : VFS.Virtual_File :=
         VFS.Create (VFS.Filesystem_String (File_Path));
   begin
      return Load_Obj_File (VFS_Path);
   end Load_Obj_File;

   function Load_Obj_File (File_Path : VFS.Virtual_File) return Mesh is
      Content : Strings.XString := VFS.Read_File (File_Path);
   begin
      return Load_Obj (Content);
   end Load_Obj_File;

   Vertex_Kind_Marker : constant String := "v ";
   Normal_Kind_Marker : constant String := "vn ";
   Face_Kind_Marker   : constant String := "f ";

   function Load_Obj (Content : Strings.XString) return Mesh is
      Lines  : Strings.XString_Array := Content.Split
        (Sep => Ada.Characters.Latin_1.LF, Omit_Empty => True);

      Result : Mesh;
   begin
      for Line of Lines loop
         declare
            Cursor : Positive := 1;

            function Read_Float return Single is
               First_Space  : Natural := Line.Find (' ', Cursor);
               Second_Space : Natural := Line.Find (' ', First_Space + 1);
            begin
               if First_Space = 0 then
                  raise Program_Error with "Invalid OBJ format";
               end if;

               if Second_Space = 0 then
                  Second_Space := Line.Length;
               end if;

               Cursor := Second_Space;

               return Single'Value
                 (Line.Slice (First_Space + 1, Second_Space - 1).To_String);
            end Read_Float;

            procedure Read_Vec3 (Dest : in out Vec3_Vectors.Vector) is
               X : Single := Read_Float;
               Y : Single := Read_Float;
               Z : Single := Read_Float;
            begin
               Dest.Append ((X, Y, Z));
            end Read_Vec3;

            function Read_Single_Index
              (First_Sep  : Character;
               Second_Sep : Character;
               Jump       : Boolean) return Natural
            is
               First_Index  : Natural := Line.Find (First_Sep, Cursor);
               Second_Index : Natural := Line.Find (Second_Sep, First_Index + 1);
            begin
               if First_Index = 0
                  or else First_Index = Second_Index
                  or else (not Jump and then First_Index /= Cursor)
               then
                  return 0;
               end if;

               if Second_Index = 0 then
                  Second_Index := Line.Length;
               end if;

               Cursor := Second_Index;

               return Natural'Value
                 (Line.Slice (First_Index + 1, Second_Index - 1).To_String);
            end Read_Single_Index;

            function Read_Mesh_Index return Mesh_Index is
               V : Natural := Read_Single_Index (' ', '/', True);
               T : Natural := Read_Single_Index ('/', '/', False);
               N : Natural := Read_Single_Index ('/', ' ', False);
            begin
               return (V, N);
            end Read_Mesh_Index;

            procedure Read_Triangle (Dest : in out Triangle_Vectors.Vector)
            is
               A : Mesh_Index := Read_Mesh_Index;
               B : Mesh_Index := Read_Mesh_Index;
               C : Mesh_Index := Read_Mesh_Index;
            begin
               Dest.Append ((A, B, C));
            end Read_Triangle;
         begin
            if Line.Starts_With (Vertex_Kind_Marker) then
               Read_Vec3 (Result.Vertices);
            elsif Line.Starts_With (Normal_Kind_Marker) then
               Read_Vec3 (Result.Normals);
            elsif Line.Starts_With (Face_Kind_Marker) then
               Read_Triangle (Result.Triangles);
            end if;
         end;
      end loop;
      return Result;
   end Load_Obj;
end Meshes.Obj_Loader;
