package body UBOs is
   use GL;
   use GL.Objects;

   Null_Buffer : Buffers.Buffer;

   procedure Bind_UBO (Buf : Buffers.Buffer) is
      use GL.Objects.Buffers;
   begin
      Bind (Uniform_Buffer, Buf);
   end Bind_UBO;

   procedure Unbind_UBO is
      use GL.Objects.Buffers;
   begin
      if not Null_Buffer.Initialized then
         Null_Buffer.Initialize_Id;
      end if;
      Bind_UBO (Null_Buffer);
   end Unbind_UBO;

   procedure Set_Uniform_Int_Data is
      new Objects.Buffers.Set_Sub_Data (Int_Pointers);
   procedure Set_Uniform_Float_Data is
      new Objects.Buffers.Set_Sub_Data (Single_Pointers);

   function Create (Binding : UInt; Size : Long) return UBO
   is
      use GL.Objects.Buffers;

      Buf : Buffer;
   begin
      Buf.Initialize_Id;

      Bind_UBO (Buf);
      Allocate (Uniform_Buffer, Create.Size, Static_Draw);
      Unbind_UBO;

      Bind_Buffer_Base (Uniform_Buffer, Binding, Buf);

      return UBO'(Buffer => Buf);
   end Create;

   function Start (Buffer : UBO) return Writer is
   begin
      return (Ada.Finalization.Limited_Controlled
                with Offset => 0, Buffer => Buffer);
   end Start;

   procedure Pad (Self : in out Writer; X : Size) is
   begin
      while Self.Offset mod X /= 0 loop
         Self.Offset := Self.Offset + 1;
      end loop;
   end Pad;

   procedure Write_Int (Self : in out Writer; X : Int) is
      use GL.Objects.Buffers;
   begin
     Set_Uniform_Int_Data
       (Uniform_Buffer,
        Self.Offset, (0 => X));
     Self.Offset := Self.Offset + 4;
   end Write_Int;

   procedure Write_Float (Self : in out Writer; X : Single) is
      use GL.Objects.Buffers;
   begin
     Set_Uniform_Float_Data
       (Uniform_Buffer,
        Self.Offset, (0 => X));
     Self.Offset := Self.Offset + 4;
   end Write_Float;

   procedure Write_Vec3 (Self : in out Writer; V : Singles.Vector3) is
      use GL.Objects.Buffers;
   begin
      Self.Pad (16);
      Set_Uniform_Float_Data
        (Uniform_Buffer,
         Self.Offset,
         (0 => V (X),
          1 => V (Y),
          2 => V (Z)));
      Self.Offset := Self.Offset + 12;
   end Write_Vec3;

   overriding procedure Initialize (Self : in out Writer) is
      use GL.Objects.Buffers;
   begin
      Bind_UBO (Self.Buffer.Buffer);
   end Initialize;

   overriding procedure Finalize   (Self : in out Writer) is
      use GL.Objects.Buffers;
   begin
      Unbind_UBO;
   end Finalize;
end UBOs;
