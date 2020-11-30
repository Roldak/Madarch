package body GPU_Types is
   procedure Pad (X : in out Types.Size; Amount : Types.Size) is
      use type GL.Types.Size;
   begin
      while X mod Amount /= 0 loop
         X := X + 1;
      end loop;
   end Pad;

   function Allocate
     (Typ     : GPU_Type;
      Kind    : GPU_Buffers.GPU_Buffer_Kind;
      Binding : Types.UInt) return GPU_Buffers.GPU_Buffer
   is
   begin
      return GPU_Buffers.Create (Kind, Binding, Types.Long (Typ.Size));
   end Allocate;

   function Named
     (Self : GPU_Type; Name : String) return Named_Component
   is
      R_Name : XString;
   begin
      R_Name.Set (Name);
      return (R_Name, Self);
   end Named;

   package body Locations is
      function Component (Self : Location; Index : Positive) return Location is
         use type GL.Types.Size;

         Comp_Loc : Location := Self.Typ.Component_Location (Index);
      begin
         return (Offset => Self.Offset + Comp_Loc.Offset, Typ => Comp_Loc.Typ);
      end Component;

      function Component (Self : Location; Name  : String) return Location is
         use type GL.Types.Size;

         Comp_Loc : Location := Self.Typ.Component_Location (Name);
      begin
         return (Offset => Self.Offset + Comp_Loc.Offset, Typ => Comp_Loc.Typ);
      end Component;

      procedure Adjust (Self : Location; W : in out GPU_Buffers.Writer) is
      begin
         W.Seek (Self.Offset);
      end Adjust;
   end Locations;

   function Component_Location
     (Self : GPU_Type_Internal;
      Component_Index : Positive) return Locations.Location
   is (raise Program_Error with "Invalid operation");

   function Component_Location
     (Self : GPU_Type_Internal;
      Component_Name : String) return Locations.Location
   is (raise Program_Error with "Invalid operation");
end GPU_Types;
