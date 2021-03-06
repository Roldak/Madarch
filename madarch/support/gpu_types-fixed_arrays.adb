package body GPU_Types.Fixed_Arrays is
   function Create
     (Length : Types.Size; X : GPU_Type) return GPU_Type
   is
   begin
      return (T => new Fixed_Array'
        (GPU_Type_Internal with
            Length    => Length,
            Component => X));
   end Create;

   overriding function Alignment (X : Fixed_Array) return Types.Size is
   begin
      return 16;
   end Alignment;

   overriding function Size (X : Fixed_Array) return Types.Size is
      use type GL.Types.Size;

      Component_Size : Types.Size := X.Component.Size;
   begin
      Pad (Component_Size, 16);
      return Component_Size * X.Length;
   end Size;

   overriding function Component_Location
     (Self  : Fixed_Array; Index : Positive) return Locations.Location
   is
      use type GL.Types.Size;

      Component_Size : Types.Size := Self.Component.Size;
      Offset : Types.Size := 0;
   begin
      Pad (Component_Size, 16);
      Offset := Component_Size * (Types.Size (Index) - 1);
      return (Offset => Offset, Typ => Self.Component);
   end Component_Location;
end GPU_Types.Fixed_Arrays;
