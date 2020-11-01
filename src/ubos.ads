with Ada.Finalization;

with GL.Objects.Buffers;
with GL.Types; use GL.Types;

package UBOs is
   type UBO is private;

   function Create (Binding : UInt; Size : Long) return UBO;

   type Writer is new Ada.Finalization.Limited_Controlled with private;

   function Start (Buffer : UBO) return Writer;

   procedure Pad         (Self : in out Writer; X : Size);
   procedure Seek        (Self : in out Writer; X : Size);
   procedure Write_Int   (Self : in out Writer; X : Int);
   procedure Write_Float (Self : in out Writer; X : Single);
   procedure Write_Vec3  (Self : in out Writer; V : Singles.Vector3);

private
   type UBO is record
      Buffer : GL.Objects.Buffers.Buffer;
   end record;

   type Writer is new Ada.Finalization.Limited_Controlled with record
      Offset : Size := 0;
      Buffer : UBO;
   end record;

   overriding procedure Initialize (Self : in out Writer);
   overriding procedure Finalize   (Self : in out Writer);
end UBOs;
