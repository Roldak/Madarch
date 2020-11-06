package GPU_Types.Fixed_Arrays is
   type Fixed_Array is new GPU_Type with private;

   overriding function Alignment (X : Fixed_Array) return Types.Size;
   overriding function Size (X : Fixed_Array) return Types.Size;

   function Create
     (Length : Types.Size; X : GPU_Type_Access) return Fixed_Array;

   overriding function Component_Location
     (Self  : Fixed_Array; Index : Positive) return Locations.Location;
private
   type Fixed_Array is new GPU_Type with record
      Length    : Types.Size;
      Component : GPU_Type_Access;
   end record;
end GPU_Types.Fixed_Arrays;
