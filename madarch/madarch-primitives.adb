package body Madarch.Primitives is
   function Create
     (Name     : String;
      Comps    : Components.Component_Array;
      Distance : Struct_Point_Function;
      Normal   : Struct_Point_Function) return Primitive
   is
   begin
      return new Primitive_Internal'
        (Name     => To_Unbounded_String (Name),
         Comps    => new Components.Component_Array'(Comps),
         Distance => Distance,
         Normal   => Normal);
   end Create;
end Madarch.Primitives;
