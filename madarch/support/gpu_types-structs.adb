package body GPU_Types.Structs is
   function Create
     (Named_Components : Named_Component_Array) return GPU_Type
   is
   begin
      return (T => new Struct'
        (GPU_Type_Internal with
            Components => new Named_Component_Array'(Named_Components)));
   end Create;

   overriding function Size (X : Struct) return Types.Size is
      use type GL.Types.Size;

      Total : Types.Size := 0;
   begin
      for C of X.Components.all loop
         Pad (Total, C.Typ.Alignment);
         Total := Total + C.Typ.Size;
      end loop;
      return Total;
   end Size;

   overriding function Component_Location
     (Self : Struct; Name : String) return Locations.Location
   is
      use type GL.Types.Size;

      Offset : Types.Size := 0;
   begin
      for C of Self.Components.all loop
         Pad (Offset, C.Typ.Alignment);
         if C.Name = Name then
            return (Offset, C.Typ);
         end if;
         Offset := Offset + C.Typ.Size;
      end loop;
      raise Program_Error with "Index out of bounds";
   end Component_Location;
end GPU_Types.Structs;
