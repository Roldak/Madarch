package body Madarch.Entities is
   function Create
     (Values : Components.Component_Value_Array) return Entity
   is
   begin
      return (Values => new Components.Component_Value_Array' (Values));
   end Create;

   function Get
     (Self : Entity; Comp : Components.Component) return Values.Value
   is
      use type Components.Component;
   begin
      for C of Self.Values.all loop
         if C.Comp = Comp then
            return C.Val;
         end if;
      end loop;
      raise Program_Error with "Entity does not have given component.";
   end Get;
end Madarch.Entities;
