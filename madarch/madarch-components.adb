package body Madarch.Components is
   function Create
     (Name : String;
      Kind : Values.Value_Kind) return Component
   is
   begin
      return new Component_Internal'
        (Name => To_Unbounded_String (Name),
         Kind => Kind);
   end Create;

   function Get_Name (C : Component) return String is
     (To_String (C.Name));

   function Get_Kind (C : Component) return Values.Value_Kind is
     (C.Kind);
end Madarch.Components;
