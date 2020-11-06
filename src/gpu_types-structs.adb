package body GPU_Types.Structs is
   overriding function Size (X : Struct) return Types.Size is
      use type GL.Types.Size;

      Total : Types.Size := 0;
   begin
      for C of X.Components.all loop
         Pad (Total, C.Alignment);
         Total := Total + C.Size;
      end loop;
      return Total;
   end Size;

   function Create
     (Component_Types : GPU_Type_Array) return Struct
   is
   begin
      return
        (GPU_Type with Components => new GPU_Type_Array'(Component_Types));
   end Create;

   overriding function Component_Location
     (Self : Struct; Index : Positive) return Locations.Location
   is
      use type GL.Types.Size;

      Offset : Types.Size := 0;
      Comp_Index : Positive := 1;
   begin
      for C of Self.Components.all loop
         Pad (Offset, C.Alignment);
         if Comp_Index = Index then
            return (Offset, C);
         end if;
         Offset := Offset + C.Size;
         Comp_Index := Comp_Index + 1;
      end loop;
      raise Program_Error with "Index out of bounds";
   end Component_Location;
end GPU_Types.Structs;
