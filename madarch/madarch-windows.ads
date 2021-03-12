with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows;

with GL.Types;

package Madarch.Windows is
   type Window_Internal is new Glfw.Windows.Window with private;

   type Window is access Window_Internal;

   function Open
     (Width, Height : Natural;
      Title         : String;
      Depth_Bits    : Natural := 0) return Window;

   function Is_Opened (Self : in out Window_Internal) return Boolean;

   procedure Poll_Events (Self : Window_Internal);

   function Key_Pressed
     (Self : in out Window_Internal; K : Glfw.Input.Keys.Key) return Boolean;

   procedure Center_Cursor
     (Self : in out Window_Internal; DX, DY : out GL.Types.Single);

   function Width  (Self : in out Window_Internal) return Glfw.Size;
   function Height (Self : in out Window_Internal) return Glfw.Size;

private
   type Window_Internal is new Glfw.Windows.Window with null record;

   overriding procedure Close_Requested
     (W : not null access Window_Internal);
end Madarch.Windows;
