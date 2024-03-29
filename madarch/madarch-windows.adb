with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C;

with Glfw.Input.Mouse;
with Glfw.Windows.Hints;
with Glfw.Windows.Context;

with GLFW_Utils;

package body Madarch.Windows is
   overriding procedure Close_Requested
     (W : not null access Window_Internal)
   is
   begin
      W.Destroy;
   end Close_Requested;

   function Open
     (Width, Height : Natural;
      Title         : String;
      Depth_Bits    : Natural := 0) return Window
   is
   begin
      GLFW_Utils.Init;
      return W : Window := new Window_Internal do
         Glfw.Windows.Hints.Set_Depth_Bits (Depth_Bits);
         W.Init (Glfw.Size (Width), Glfw.Size (Height), Title);
         W.Show;
         W.Enable_Callback (Glfw.Windows.Callbacks.Close);
         Glfw.Windows.Context.Make_Current (W);
      end return;
   end Open;

   function Is_Opened (Self : in out Window_Internal) return Boolean is
     (Self.Initialized and then Self.Visible);

   procedure Poll_Events (Self : Window_Internal) is
   begin
      Glfw.Input.Poll_Events;
   end Poll_Events;

   function Key_Pressed
     (Self : in out Window_Internal; K : Glfw.Input.Keys.Key) return Boolean
   is
      use type Glfw.Input.Button_State;
   begin
      return Self.Key_State (K) = Glfw.Input.Pressed;
   end Key_Pressed;

   procedure Center_Cursor
     (Self : in out Window_Internal; DX, DY : out GL.Types.Single)
   is
      use Interfaces.C;

      SX, SY : Int;
      MX, MY : Double;
   begin
      Self.Get_Size (SX, SY);
      Self.Get_Cursor_Pos (MX, MY);
      declare
         CX : Double := Double (SX) / 2.0;
         CY : Double := Double (SY) / 2.0;
      begin
         Self.Set_Cursor_Pos (CX, CY);
         DX := GL.Types.Single (MX - CX);
         DY := GL.Types.Single (MY - CY);
      end;
   end Center_Cursor;

   procedure Show_Cursor
     (Self : in out Window_Internal; Show : Boolean)
   is
   begin
      if Show then
         Self.Set_Cursor_Mode (Glfw.Input.Mouse.Normal);
      else
         Self.Set_Cursor_Mode (Glfw.Input.Mouse.Hidden);
      end if;
   end Show_Cursor;

   function Width  (Self : in out Window_Internal) return Glfw.Size is
      W, H : Glfw.Size;
   begin
      Self.Get_Size (W, H);
      return W;
   end Width;

   function Height (Self : in out Window_Internal) return Glfw.Size is
      W, H : Glfw.Size;
   begin
      Self.Get_Size (W, H);
      return H;
   end Height;
end Madarch.Windows;
