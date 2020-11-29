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
   begin
      return To_String (C.Name);
   end Get_Name;
end Madarch.Components;
