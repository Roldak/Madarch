package body GPU_Types is
   procedure Pad (X : in out Types.Size; Amount : Types.Size) is
      use type GL.Types.Size;
   begin
      while X mod Amount /= 0 loop
         X := X + 1;
      end loop;
   end Pad;

   function Allocate
     (X : GPU_Type'Class; Binding : Types.UInt) return UBOs.UBO
   is
   begin
      return UBOs.Create (Binding, Types.Long (X.Size));
   end Allocate;

   package body Locations is
      function Component (Self : Location; Index : Positive) return Location is
         use type GL.Types.Size;

         Comp_Loc : Location := Self.Typ.Component_Location (Index);
      begin
         return (Offset => Self.Offset + Comp_Loc.Offset, Typ => Comp_Loc.Typ);
      end Component;

      procedure Adjust (Self : Location; W : in out UBOs.Writer) is
      begin
         W.Seek (Self.Offset);
      end Adjust;
   end Locations;

   function Component_Location
     (Self : GPU_Type; Component_Index : Positive) return Locations.Location
   is (raise Program_Error with "Invalid operation");
end GPU_Types;
