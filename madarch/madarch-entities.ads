with Madarch.Components;
with Madarch.Values;

package Madarch.Entities is
   type Entity is private;
   type Entity_Array is array (Positive range <>) of Entity;

   function Create
     (Values : Components.Component_Value_Array) return Entity;

   function Get
     (Self : Entity; Comp : Components.Component) return Values.Value;

   type Component_Value_Procedure is access
      procedure (C : Components.Component; V : Values.Value);

   procedure Foreach
     (Self : Entity; Proc : Component_Value_Procedure);
private
   type Component_Value_Array_Access is
      access Components.Component_Value_Array;

   type Entity is record
      Values : Component_Value_Array_Access;
   end record;
end Madarch.Entities;
