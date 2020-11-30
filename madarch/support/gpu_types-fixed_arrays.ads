package GPU_Types.Fixed_Arrays is
   function Create
     (Length : Types.Size; X : GPU_Type) return GPU_Type;
private
   type Fixed_Array is new GPU_Type_Internal with record
      Length    : Types.Size;
      Component : GPU_Type;
   end record;

   overriding function Alignment (X : Fixed_Array) return Types.Size;
   overriding function Size (X : Fixed_Array) return Types.Size;

   overriding function Component_Location
     (Self  : Fixed_Array; Index : Positive) return Locations.Location;
end GPU_Types.Fixed_Arrays;
