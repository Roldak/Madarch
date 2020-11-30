with Ada.Text_IO; use Ada.Text_IO;

with Glfw.Input;
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
