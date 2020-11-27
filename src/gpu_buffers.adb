with Interfaces.C.Pointers;

with Ada.Text_IO;

package body GPU_Buffers is
   use GL;
   use GL.Objects;

   Null_Buffer : Buffers.Buffer;

   procedure Bind_Buffer (Kind : GPU_Buffer_Kind; Buf : Buffers.Buffer) is
      use GL.Objects.Buffers;
   begin
      Bind ((if Kind in Uniform_Buffer
             then Buffers.Uniform_Buffer
             else Buffers.Shader_Storage_Buffer), Buf);
   end Bind_Buffer;

   procedure Unbind_Buffer (Kind : GPU_Buffer_Kind) is
      use GL.Objects.Buffers;
   begin
      if not Null_Buffer.Initialized then
         Null_Buffer.Initialize_Id;
      end if;
      Bind_Buffer (Kind, Null_Buffer);
   end Unbind_Buffer;

   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Set_Sub_Data (Kind   : GPU_Buffer_Kind;
                           Offset : Types.Size;
                           Data   : Pointers.Element_Array);

   procedure Set_Sub_Data (Kind   : GPU_Buffer_Kind;
                           Offset : Types.Size;
                           Data   : Pointers.Element_Array)
   is
      procedure Set_It is new Buffers.Set_Sub_Data (Pointers);
   begin
      Set_It
        ((if Kind in Uniform_Buffer
          then Buffers.Uniform_Buffer
          else Buffers.Shader_Storage_Buffer),
         Offset,
         Data);
   end Set_Sub_Data;

   procedure Set_Buffer_Int_Data is new Set_Sub_Data (Int_Pointers);
   procedure Set_Buffer_Float_Data is new Set_Sub_Data (Single_Pointers);

   function Create
     (Kind    : GPU_Buffer_Kind;
      Binding : UInt;
      Size    : Long) return GPU_Buffer
   is
      use GL.Objects.Buffers;

      Buf : Buffer;
   begin
      Buf.Initialize_Id;

      Bind_Buffer (Kind, Buf);

      Allocate
        ((if Kind in Uniform_Buffer
          then Buffers.Uniform_Buffer
          else Buffers.Shader_Storage_Buffer),
         Create.Size,
         Static_Draw);

      Unbind_Buffer (Kind);

      Bind_Buffer_Base
        ((if Kind in Uniform_Buffer
          then Buffers.Uniform_Buffer
          else Buffers.Shader_Storage_Buffer),
         Binding,
         Buf);

      return (Kind => Kind, Buffer => Buf);
   end Create;

   function Start (Buffer : GPU_Buffer) return Writer is
   begin
      return X : Writer :=
        (Ada.Finalization.Limited_Controlled
           with Offset => 0, Buffer => Buffer)
      do
         X.Initialize; --  :/
      end return;
   end Start;

   procedure Pad (Self : in out Writer; X : Size) is
   begin
      while Self.Offset mod X /= 0 loop
         Self.Offset := Self.Offset + 1;
      end loop;
   end Pad;

   procedure Seek (Self : in out Writer; X : Size) is
   begin
      Self.Offset := X;
   end Seek;

   procedure Write_Int (Self : in out Writer; X : Int) is
      use GL.Objects.Buffers;
   begin
     Set_Buffer_Int_Data
       (Self.Buffer.Kind,
        Self.Offset, (0 => X));
     Self.Offset := Self.Offset + 4;
   end Write_Int;

   procedure Write_Float (Self : in out Writer; X : Single) is
      use GL.Objects.Buffers;
   begin
     Set_Buffer_Float_Data
       (Self.Buffer.Kind,
        Self.Offset, (0 => X));
     Self.Offset := Self.Offset + 4;
   end Write_Float;

   procedure Write_Vec3 (Self : in out Writer; V : Singles.Vector3) is
      use GL.Objects.Buffers;
   begin
      Self.Pad (16);
      Set_Buffer_Float_Data
        (Self.Buffer.Kind,
         Self.Offset,
         (0 => V (X),
          1 => V (Y),
          2 => V (Z)));
      Self.Offset := Self.Offset + 12;
   end Write_Vec3;

   overriding procedure Initialize (Self : in out Writer) is
      use GL.Objects.Buffers;
   begin
      Bind_Buffer (Self.Buffer.Kind, Self.Buffer.Buffer);
   end Initialize;

   overriding procedure Finalize   (Self : in out Writer) is
      use GL.Objects.Buffers;
   begin
      Unbind_Buffer (Self.Buffer.Kind);
   end Finalize;
end GPU_Buffers;
